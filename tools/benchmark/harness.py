#!/usr/bin/env python3
# tools/benchmark/harness.py --- Time editors on large files through a pseudo-terminal.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Run each editor on each benchmark file in a pseudo-terminal and time what it does.

Usually run by tools/benchmark/run.sh, which makes the files and pins the
editors to the reference environment's cores and memory. By hand:

  python3 -I tools/benchmark/harness.py --data DIR --scratch DIR --out FILE.jsonl
      [--wrap COMMAND] [--editors emacs,emacs-writable,neovim,helix,tuatara]
      [--files csv,log,org,json] [--repetitions 3] [--typing-keys 100] [--page-keys 40]
      [--scenario keys|replace] [--rows 40] [--columns 120] [--limit 120] [--keep-homes]

DIR for --data is what make-data.py wrote. --scratch is a directory the
harness may empty and fill: each editor's home is made there afresh for each
run. One line of JSON is written to --out for the machine and the versions,
then one for each run of an editor on a file.

How it times: a key is written to the editor's terminal and the time noted;
the editor's output is read as it comes into a screen of --rows by --columns.
Each key gets two times. Shown: until output has come (and, for a search,
the match is on the screen) and then none for --quiet-ms, that quiet spell
not counted; this is what a person waits for, and freezes are counted on it.
Idle: until, for --quiet-ms, there was also no output, the editor's
processes used no processor time (one clock tick at most) and none of their
threads waited on the disk; this is the work the key cost, background work
included. Before the next key the harness
waits until the editor has written nothing for --rest-ms, so that a redraw
an editor puts off on purpose (a matching parenthesis shown after a delay,
say) does not land in the next key. A search is typed one key every 150 ms,
as a person types, and only its last key is timed. A key the editor shows no
change for is counted as "no change" rather than timed. A shown time over a
second is a freeze; one over --limit seconds ends the run, which is reported
as timed out. Memory is the largest resident set of the editor's processes
together, read from /proc while it runs.

--scenario replace does the replace run instead (see run_replace), on a copy
of the file: fox replaced by cat everywhere, a sentence typed into a new
buffer while it runs, back to the file, the file saved and the replacement
undone, each step timed until it shows and the editor is idle. Give it a
--limit long enough for a slow editor to finish, an hour say.

--keep-homes leaves each editor's home of the last run in the scratch
directory (fence-EDITOR/home), so that its logs can be read afterwards.
--record DIR writes each editor's timed run of the replace run to
DIR/EDITOR.cast, an asciicast that tools/benchmark/film.py turns into a film.
"""

import argparse
import codecs
import fcntl
import glob
import importlib.util
import json
import os
import platform
import pty
import random
import re
import select
import shlex
import shutil
import signal
import struct
import subprocess
import sys
import termios
import time

HERE = os.path.dirname(os.path.abspath(__file__))


def load(name):
    """Load the module NAME.py that sits beside this file."""
    spec = importlib.util.spec_from_file_location(name, os.path.join(HERE, name + ".py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


Screen = load("screen").Screen
EDITORS = load("keys").EDITORS
PAGE = os.sysconf("SC_PAGE_SIZE")
FREEZE_MS = 1000
REPLACE_SENTENCE = "the quick brown fox jumps over the lazy dog"


class Exited(Exception):
    """The editor ended while the harness waited for it."""


class NotShown(Exception):
    """The editor sat idle without showing the file it was asked to open."""


class Unfinished(Exception):
    """A step of the replace run that the editor did not finish as asked; the argument says how."""


class TimedOut(Exception):
    """The editor took longer than the time limit for one step."""


def now():
    """Return the time in seconds from a clock that only goes forward."""
    return time.perf_counter()


def process_tree(pid):
    """Return PID and the process IDs of all its descendants that are still running."""
    found, todo = [], [pid]
    while todo:
        p = todo.pop()
        found.append(p)
        for children in glob.glob(f"/proc/{p}/task/*/children"):
            try:
                with open(children) as f:
                    todo.extend(int(c) for c in f.read().split())
            except OSError:
                pass
    return found


def tree_rss(pid):
    """Return the resident memory of PID and its descendants together, in bytes."""
    total = 0
    for p in process_tree(pid):
        try:
            with open(f"/proc/{p}/statm") as f:
                total += int(f.read().split()[1]) * PAGE
        except (OSError, IndexError, ValueError):
            pass
    return total


def tree_peak(pid):
    """Return the sum of the largest resident sets (VmHWM) PID and its running descendants have had, in bytes."""
    total = 0
    for p in process_tree(pid):
        try:
            with open(f"/proc/{p}/status") as f:
                for line in f:
                    if line.startswith("VmHWM:"):
                        total += int(line.split()[1]) * 1024
        except (OSError, ValueError):
            pass
    return total


def tree_cpu(pid):
    """Return the processor time PID and its descendants have used, in clock ticks, and whether any of their
    threads is waiting on the disk now (uninterruptible sleep), work the processor time does not show."""
    total = 0
    waiting = False
    for p in process_tree(pid):
        try:
            with open(f"/proc/{p}/stat") as f:
                fields = f.read().rsplit(")", 1)[1].split()
            total += int(fields[11]) + int(fields[12])
            for tid in os.listdir(f"/proc/{p}/task"):
                with open(f"/proc/{p}/task/{tid}/stat") as f:
                    if f.read().rsplit(")", 1)[1].split()[0] == "D":
                        waiting = True
                        break
        except (OSError, IndexError, ValueError):
            pass
    return total, waiting


def cgroup_dir(pid):
    """Return the directory of the control group PID runs in under /sys/fs/cgroup, or None."""
    try:
        with open(f"/proc/{pid}/cgroup") as f:
            for line in f:
                if line.startswith("0::"):
                    return "/sys/fs/cgroup" + line[3:].strip()
    except OSError:
        pass
    return None


def cgroup_numbers(directory):
    """Return the largest memory charged to the control group in DIRECTORY, in bytes, and how many of its
    processes were killed for memory; either is None when it cannot be read."""
    peak = kills = None
    try:
        with open(os.path.join(directory, "memory.peak")) as f:
            peak = int(f.read())
        with open(os.path.join(directory, "memory.events")) as f:
            for line in f:
                if line.startswith("oom_kill "):
                    kills = int(line.split()[1])
    except (OSError, ValueError):
        pass
    return peak, kills


def holds(text, shows):
    """Return whether the screen's TEXT holds SHOWS, a string, or passes it, a function given the text."""
    return shows(text) if callable(shows) else shows in text


class Session:
    """An editor running in a pseudo-terminal of its own, its output read into a Screen."""

    def __init__(self, wrap, argv, env, rows, columns, quiet, rest, record=None, cwd=None):
        """Start ARGV with environment ENV in a ROWS by COLUMNS terminal, run by the command WRAP.

        WRAP (a list, maybe empty) runs with the harness's own environment, as
        systemd-run needs to reach the user's service manager; ARGV gets ENV
        alone, by env -i.

        QUIET and REST are in seconds: how long no output means a key is done,
        and how long to wait for quiet before the next key. RECORD, when
        given, is a path to write the session to as an asciicast (version 2):
        what the editor wrote and the keys it was sent, each with its time.
        CWD, when given, is the directory ARGV starts in, so that a file
        named relative to it shows on the screen by its name alone."""
        self.screen = Screen(rows, columns)
        self.quiet = quiet
        self.rest_time = rest
        self.peak_rss = 0
        self.key_times = []
        self.first_part_sent = None
        self.charged = 0
        self.memory_kills = 0
        self.cgroup = None
        self.last_output = None
        self.outputs = 0
        self._next_sample = 0
        self.started = now()
        self.record = None
        if record:
            self.record = open(record, "w")
            self.record.write(json.dumps({"version": 2, "width": columns, "height": rows,
                                          "timestamp": int(time.time()), "title": argv[0]}) + "\n")
            self._record_decoder = codecs.getincrementaldecoder("utf-8")(errors="replace")
        pid, fd = pty.fork()
        if pid == 0:
            try:
                fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", rows, columns, 0, 0))
                if cwd:
                    os.chdir(cwd)
                command = wrap + ["env", "-i"] + [f"{k}={v}" for k, v in env.items()] + argv
                os.execvp(command[0], command)
            finally:
                os._exit(127)
        self.pid, self.fd = pid, fd
        self.status = None

    def _sample(self):
        """Note the editor's memory, at most every 50 ms: its resident set, and what its control group was
        charged and how many of its processes were killed for memory, when it runs in a scope of its own."""
        t = now()
        if t >= self._next_sample:
            self.peak_rss = max(self.peak_rss, tree_rss(self.pid))
            if self.cgroup is None:
                directory = cgroup_dir(self.pid)
                if directory and directory != cgroup_dir(os.getpid()) and directory.endswith(".scope"):
                    self.cgroup = directory
            if self.cgroup:
                peak, kills = cgroup_numbers(self.cgroup)
                self.charged = max(self.charged, peak or 0)
                self.memory_kills = max(self.memory_kills, kills or 0)
            self._next_sample = t + 0.05

    def _read(self, timeout):
        """Read what the editor wrote within TIMEOUT seconds into the screen; answer its questions."""
        ready, _, _ = select.select([self.fd], [], [], max(0.0, timeout))
        if not ready:
            return False
        try:
            data = os.read(self.fd, 1 << 16)
        except OSError:
            data = b""
        if not data:
            self._reap()
            raise Exited(self.status)
        self.last_output = now()
        self.outputs += 1
        if self.record:
            self.record.write(json.dumps([round(self.last_output - self.started, 4), "o",
                                          self._record_decoder.decode(data)]) + "\n")
        reply = self.screen.feed(data)
        if reply:
            os.write(self.fd, reply)
        return True

    def _reap(self):
        """Note how the editor ended."""
        if self.status is None:
            try:
                _, status = os.waitpid(self.pid, 0)
                self.status = os.waitstatus_to_exitcode(status)
            except ChildProcessError:
                self.status = "unknown"

    def send(self, keys):
        """Write KEYS to the editor's terminal; return the time just before."""
        start = now()
        os.write(self.fd, keys.encode("utf-8"))
        if self.record:
            self.record.write(json.dumps([round(start - self.started, 4), "i", keys]) + "\n")
        return start

    def wait(self, since, limit, shows=None, need_output=True, idle_limit=10.0):
        """Wait for what the editor was sent at SINCE; return (first, shown, idle, changed), times or None.

        SHOWN is when the screen settled: output came after SINCE (when
        NEED_OUTPUT), the screen holds SHOWS (when given), and then no output
        came for the quiet time; SHOWN is the start of that quiet spell, which
        is not counted; SHOWS is a string or a function given the screen's
        text that answers whether it is as asked, a screen that already is
        counting when NEED_OUTPUT is false. IDLE is the start of the first later spell of the quiet
        time in which no output came and the editor's processes used no more
        than one clock tick of processor time: when the work for it was done.
        IDLE is None when that takes over IDLE_LIMIT seconds after SHOWN.
        FIRST is the time of the first output after SINCE. CHANGED is False
        when the editor sat idle for over a second without the screen being
        settled as asked: nothing changed on it, or it does not hold SHOWS.
        Raises TimedOut when the screen has not settled after LIMIT seconds
        and Exited when the editor ends."""
        first = shown_at = None
        outputs = self.outputs
        has_text = shows is None or (not need_output and holds(self.screen.text(), shows))
        cpu, _ = tree_cpu(self.pid)
        idle_from, idle_cpu, idle_outputs = since, cpu, outputs
        while True:
            t = now()
            if shown_at is None and t > since + limit:
                raise TimedOut()
            got = self.outputs > outputs
            if (got or not need_output) and not has_text and holds(self.screen.text(), shows):
                has_text = True
            output_quiet = max(self.last_output or since, since)
            if shown_at is None and (got or not need_output) and has_text and t - output_quiet >= self.quiet:
                shown_at = output_quiet
            cpu, waiting = tree_cpu(self.pid)
            if self.outputs != idle_outputs:
                idle_from, idle_cpu, idle_outputs = self.last_output, cpu, self.outputs
            elif cpu - idle_cpu > 1 or waiting:
                idle_from, idle_cpu = t, cpu
            if t - idle_from >= self.quiet:
                if shown_at is not None:
                    return first, shown_at, max(idle_from, shown_at), True
                if t - idle_from > 1.0:
                    return None, None, None, False
            if shown_at is not None and t - shown_at > idle_limit:
                return first, shown_at, None, True
            self._sample()
            if self._read(0.01) and first is None and self.last_output >= since:
                first = self.last_output

    def rest(self):
        """Wait until the editor has written nothing for the rest time, or two seconds have passed."""
        start = now()
        while now() - start < 2.0:
            left = self.rest_time - (now() - (self.last_output or 0))
            if left <= 0:
                return
            self._sample()
            self._read(min(left, 0.05))

    def key(self, keys, limit, shows=None, paced="", settle=True, idle_limit=10.0):
        """Send KEYS and wait for the editor; return (first, shown, idle) in ms, or None when nothing changed.

        PACED, when given, is sent first one key at a time, one every 150 ms as
        a person types, not timed; the times are those of KEYS alone. SHOWS is
        as for WAIT; IDLE is None when the editor was still busy IDLE_LIMIT
        seconds after the screen settled. SETTLE nil sends at once, without
        first waiting for the rest time, as a person goes on who sees the
        screen ready."""
        if settle:
            self.rest()
        self.paced(paced)
        start = self.send(keys)
        first, shown, idle, changed = self.wait(start, limit, shows, idle_limit=idle_limit)
        if not changed:
            return None
        return (((first or shown) - start) * 1000, (shown - start) * 1000,
                None if idle is None else (idle - start) * 1000)

    def paced(self, keys):
        """Send KEYS one at a time, one every 150 ms as a person types, reading what the editor writes meanwhile."""
        for char in keys:
            self.send(char)
            pause = now() + 0.15
            while now() < pause:
                self._sample()
                self._read(pause - now())

    def typed(self, parts, limit):
        """Send PARTS, a string of keys or a list of them, one key every 150 ms as a person types, and after each
        part but the last wait until the screen shows it; return the time just before the last key is sent.

        The wait is until the editor has written something for the part and
        then nothing for the quiet time, or has sat idle for a second, as a
        person waits to see a command take effect before the next. A part
        ending in Escape needs it: an Escape followed at once by another key
        is taken by the terminal as that key with Meta. How long each part
        took to show, in ms, is added to KEY_TIMES; when the first part's last
        key was sent is kept in FIRST_PART_SENT."""
        keys = [parts] if isinstance(parts, str) else [p for p in parts if p]
        self.paced(keys[0][:-1])
        self.first_part_sent = now()
        if len(keys) == 1:
            return self.send(keys[0][-1])
        keys[0] = keys[0][-1:]
        for part in keys[:-1]:
            self.paced(part[:-1])
            sent = self.send(part[-1])
            _, shown, _, changed = self.wait(sent, limit, idle_limit=0)
            if changed:
                self.key_times.append((shown - sent) * 1000)
        self.paced(keys[-1][:-1])
        return self.send(keys[-1][-1])

    def untimed(self, keys, limit):
        """Send KEYS, not timed, and wait until the editor is done with them."""
        if keys:
            self.rest()
            self.wait(self.send(keys), limit)

    def close(self):
        """End the editor and all its processes; return the sum of their largest resident sets."""
        peak = tree_peak(self.pid) if self.status is None else 0
        try:
            os.killpg(self.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        for p in process_tree(self.pid)[1:]:
            try:
                os.kill(p, signal.SIGKILL)
            except ProcessLookupError:
                pass
        self._reap()
        os.close(self.fd)
        if self.record:
            self.record.close()
            self.record = None
        return peak


def fence(scratch, editor):
    """Make an empty home for EDITOR under SCRATCH and return the environment that points the editor into it.

    Nothing in it leads to the person's own files, settings, agents or display."""
    home = os.path.join(scratch, "fence-" + editor)
    shutil.rmtree(home, ignore_errors=True)
    dirs = {}
    for name in ("home", "config", "state", "data", "cache", "runtime", "tmp", "keys"):
        dirs[name] = os.path.join(home, name)
        os.makedirs(dirs[name], mode=0o700)
    for target, source in EDITORS[editor]["setup"].items():
        path = os.path.join(dirs["home"], target)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        shutil.copyfile(source, path)
    env = {"PATH": "/usr/local/bin:/usr/bin:/bin", "TERM": "xterm-256color", "COLORTERM": "truecolor",
           "LANG": "C.UTF-8", "HOME": dirs["home"], "XDG_CONFIG_HOME": dirs["config"],
           "XDG_STATE_HOME": dirs["state"], "XDG_DATA_HOME": dirs["data"], "XDG_CACHE_HOME": dirs["cache"],
           "XDG_RUNTIME_DIR": dirs["runtime"], "TMPDIR": dirs["tmp"], "GNUPGHOME": dirs["keys"],
           "GIT_CONFIG_GLOBAL": "/dev/null", "GIT_CONFIG_NOSYSTEM": "1"}
    if editor == "emacs" and os.environ.get("BENCH_EMACS_VLF"):
        env["BENCH_EMACS_VLF"] = os.environ["BENCH_EMACS_VLF"]
    return env


def warm_cache(path):
    """Read PATH once, so that every editor opens it from the page cache."""
    with open(path, "rb") as f:
        while f.read(1 << 22):
            pass


def clean_data(directory):
    """Remove the auto-save, lock and backup files editors left beside the benchmark files in DIRECTORY."""
    for name in os.listdir(directory):
        if name.startswith(("#", ".#")) or name.endswith("~") or name.endswith(".swp"):
            os.remove(os.path.join(directory, name))


def typing_text(count, seed):
    """Return COUNT made-up lower-case letters and spaces to type, no eight of them in a row repeated."""
    rng = random.Random(seed)
    while True:
        text = "".join(rng.choice("abcdefghijklmnopqrstuvwxyz   ") for _ in range(count)).replace("  ", " a")
        text = text[:count]
        windows = [text[i:i + 8] for i in range(len(text) - 7)]
        if len(set(windows)) == len(windows) and not text.startswith(" "):
            return text


def warm_up(editor, env, args, wrap):
    """Start EDITOR once on a small file, not timed, so that its caches are made; return why it failed, or None."""
    keys = EDITORS[editor]
    warm = os.path.join(args.scratch, "warm-up.txt")
    with open(warm, "w") as f:
        f.write("warm up the editor\n")
    session = Session(wrap, keys["command"] + [warm], env, args.rows, args.columns, args.quiet_ms / 1000,
                      args.rest_ms / 1000)
    try:
        if not session.wait(session.started, args.limit, shows="warm up the editor")[3]:
            raise NotShown()
    except (NotShown, TimedOut, Exited) as e:
        session.close()
        return f"warm-up start failed ({type(e).__name__})"
    session.close()
    return None


def run_one(editor, entry, data, args, wrap):
    """Run EDITOR on the benchmark file of manifest ENTRY in DATA; return the run's record."""
    keys = EDITORS[editor]
    path = os.path.join(data, entry["path"])
    env = fence(args.scratch, editor)
    record = {"editor": editor, "file": entry["kind"], "status": "ok", "typing": [], "typing_first": [],
              "page": [], "page_first": [], "typing_idle": [], "page_idle": [], "no_change": {},
              "open_ms": None, "end_ms": None, "search_ms": None, "open_idle_ms": None, "end_idle_ms": None,
              "search_idle_ms": None}
    limit = args.limit
    quiet, rest = args.quiet_ms / 1000, args.rest_ms / 1000
    failed = warm_up(editor, env, args, wrap)
    if failed:
        record["status"] = failed
        return record
    clean_data(args.scratch)
    warm_cache(path)
    session = Session(wrap, keys["command"] + [path], env, args.rows, args.columns, quiet, rest)
    case = "open"
    problems = []
    try:
        _, shown, idle, changed = session.wait(session.started, limit, shows=entry["open_marker"])
        if not changed:
            raise NotShown()
        record["open_ms"] = (shown - session.started) * 1000
        record["open_idle_ms"] = None if idle is None else (idle - session.started) * 1000
        if keys.get("only_if") and not re.search(keys["only_if"], session.screen.text(), re.MULTILINE):
            record["status"] = "skipped"
            session.close()
            return record
        case = "typing"
        session.untimed(keys["start_typing"], limit)
        text = typing_text(args.typing_keys, f"{entry['kind']}-{editor}")
        typed = ""
        for i, char in enumerate(text):
            if i and i % 50 == 0:
                timing = session.key(keys["newline"], limit)
                typed = ""
            else:
                typed += char
                timing = session.key(char, limit, shows=typed[-8:] if len(typed) >= 8 else None)
            if timing is None:
                record["no_change"]["typing"] = record["no_change"].get("typing", 0) + 1
            else:
                record["typing_first"].append(timing[0])
                record["typing"].append(timing[1])
                record["typing_idle"].append(timing[2])
        # what was typed last must be in the file on the screen, or none of it counts
        if not record["typing"] or (len(typed) >= 8 and typed[-8:] not in session.screen.text()):
            record["typing"], record["typing_first"], record["typing_idle"] = [], [], []
            problems.append("what was typed is not in the file (is it read-only?)")
        session.untimed(keys["stop_typing"], limit)
        case = "page"
        for _ in range(args.page_keys):
            timing = session.key(keys["page_down"], limit)
            if timing is None:
                record["no_change"]["page"] = record["no_change"].get("page", 0) + 1
            else:
                record["page_first"].append(timing[0])
                record["page"].append(timing[1])
                record["page_idle"].append(timing[2])
        case = "end"
        timing = session.key(keys["to_end"], limit)
        if timing is None:
            record["no_change"]["end"] = 1
        else:
            record["end_ms"], record["end_idle_ms"] = timing[1], timing[2]
        session.untimed(keys["to_start"], limit)
        case = "search"
        search = keys["search"](entry["needle"])
        timing = session.key(search[-1], limit, shows=entry["found"], paced=search[:-1])
        if timing is None:
            problems.append("search did not reach the match")
        else:
            record["search_ms"], record["search_idle_ms"] = timing[1], timing[2]
    except NotShown:
        problems.append("did not show the file's first line")
    except TimedOut:
        problems.append(f"timed out in {case} (over {limit:g} s)")
    except Exited as e:
        problems.append(f"ended in {case} (exit {e.args[0]})")
    if problems:
        record["status"] = "; ".join(problems)
    record["peak_rss"] = max(session.peak_rss, session.close())
    record["freezes"] = sum(1 for t in record["typing"] + record["page"] if t > FREEZE_MS) + sum(
        1 for t in (record["end_ms"], record["search_ms"]) if t is not None and t > FREEZE_MS)
    record["screen_at_end"] = session.screen.text()
    clean_data(data)
    return record


def count_text(path, needle):
    """Return how many times NEEDLE, octets with no repeat inside it, comes in the file at PATH."""
    count = 0
    tail = b""
    with open(path, "rb") as f:
        while chunk := f.read(1 << 24):
            block = tail + chunk
            count += block.count(needle)
            tail = block[-(len(needle) - 1):]
    return count


def changed_at(path):
    """Return when the file at PATH last changed, in nanoseconds, or None while there is none there (an editor
    that saves by renaming a new file over the old)."""
    try:
        return os.stat(path).st_mtime_ns
    except FileNotFoundError:
        return None


def line_reading(text):
    """Return a test of the screen's text: one of its lines reads TEXT and nothing else, after a gutter of
    spaces, line numbers and rules, as the only line of a new buffer does."""
    def test(screen):
        """Return whether a line of SCREEN reads the text alone."""
        return any(line.lstrip(" 0123456789│|").rstrip() == text for line in screen.splitlines())
    return test


def run_replace(editor, entry, data, args, wrap):
    """Run the replace run of EDITOR on a copy of the file of manifest ENTRY in DATA; return the run's record.

    Opens the copy and replaces fox by cat in all of it. At once, as a person
    goes on working, it makes a new buffer (switch_to_ms, from its last key
    until the screen settles) and types a sentence into it, each key timed
    until it shows (typing), the whole sentence too (typing_span_ms). Then it
    goes back to the file (switch_back_ms, until the file's lines show). The
    replacement is finished when the file's lines are on the screen with cat
    in place of fox (every line of the gigantic table has the fox) and the
    editor has stopped working (finished_ms, from the key that gives the
    replacement: the last of its first part). Then it saves the file, done
    when the editor has stopped working and the file on disk has changed
    (save_ms), and undoes the replacement, done when the lines show fox again
    and the editor has stopped working (undo_ms).

    Every step's keys are typed one every 150 ms, each command once the
    screen shows the one before (TYPED); how long each command took to show
    is kept (step_keys). Every key over a second is a freeze. The copy is
    made afresh for each run and removed after it, so the benchmark's own
    file never changes."""
    keys = EDITORS[editor]
    env = fence(args.scratch, editor)
    # the copy in the editor's own empty home, opened by name from there, so that the screen shows it as
    # ~/replace-... and nothing of where the scratch directory is
    copy = os.path.join(env["HOME"], "replace-" + entry["path"])
    record = {"editor": editor, "file": entry["kind"], "scenario": "replace", "status": "ok", "open_ms": None,
              "step_keys": [], "typing": [], "typing_span_ms": None, "switch_to_ms": None,
              "switch_back_ms": None, "finished_ms": None, "save_ms": None, "saved_whole": None,
              "undo_ms": None}
    limit = args.limit

    def showing(old, new):
        """Return a test of the screen's text: the file's lines show NEW where its sentence had OLD."""
        return lambda text: f"brown {new} jumps" in text and f"brown {old} jumps" not in text

    failed = warm_up(editor, env, args, wrap)
    if failed:
        record["status"] = failed
        return record
    clean_data(args.scratch)
    shutil.copyfile(os.path.join(data, entry["path"]), copy)
    warm_cache(copy)
    cast = os.path.join(args.record, editor + ".cast") if args.record else None
    # the copy named from the scratch directory, so that no editor shows where that directory is
    session = Session(wrap, keys["command"] + [os.path.basename(copy)], env, args.rows, args.columns,
                      args.quiet_ms / 1000, args.rest_ms / 1000, cast, cwd=env["HOME"])
    case = "open"
    problems = []
    try:
        _, shown, _, ok = session.wait(session.started, limit, shows=entry["open_marker"])
        if not ok:
            raise NotShown()
        record["open_ms"] = (shown - session.started) * 1000
        case = "replace"
        session.rest()
        session.typed(keys["replace_all"]("fox", "cat"), limit)
        replaced = session.first_part_sent
        case = "new buffer"
        since = session.typed(keys["new_buffer"], limit)
        _, shown, _, ok = session.wait(since, limit, idle_limit=0)
        if ok:
            record["switch_to_ms"] = (shown - since) * 1000
        case = "typing while it replaces"
        if keys["start_typing"]:
            session.wait(session.typed(keys["start_typing"], limit), limit, idle_limit=0)
        typed = ""
        typing_from = now()
        for char in REPLACE_SENTENCE:
            typed += char
            timing = session.key(char, limit, shows=line_reading(typed), settle=False, idle_limit=0)
            if timing is not None:
                record["typing"].append(timing[1])
        record["typing_span_ms"] = (now() - typing_from) * 1000
        if REPLACE_SENTENCE[-8:] not in session.screen.text():
            problems.append("what was typed is not in the new buffer")
        case = "back to the file"
        since = session.typed([keys["stop_typing"], keys["back"]], limit)
        _, shown, _, ok = session.wait(since, limit, shows=lambda text: "lazy dog.," in text, idle_limit=0)
        if ok:
            record["switch_back_ms"] = (shown - since) * 1000
        _, shown, idle, ok = session.wait(since, limit, shows=showing("fox", "cat"), need_output=False,
                                          idle_limit=limit)
        if not ok:
            raise Unfinished("the file did not show cat in place of fox")
        record["finished_ms"] = ((idle or shown) - replaced) * 1000
        case = "save"
        before = changed_at(copy)
        session.rest()
        since = session.typed(keys["save"], limit)
        _, shown, idle, ok = session.wait(since, limit, need_output=False, idle_limit=limit,
                                          shows=lambda text: changed_at(copy) not in (before, None))
        if not ok:
            raise Unfinished("the file was not saved")
        record["save_ms"] = ((idle or shown) - since) * 1000
        case = "undo"
        session.rest()
        since = session.typed(keys["undo"], limit)
        _, shown, idle, ok = session.wait(since, limit, shows=showing("cat", "fox"), need_output=False,
                                          idle_limit=limit)
        if not ok:
            raise Unfinished("undo did not bring fox back")
        record["undo_ms"] = ((idle or shown) - since) * 1000
    except NotShown:
        problems.append("did not show the file's first line")
    except Unfinished as e:
        problems.append(e.args[0])
    except TimedOut:
        problems.append(f"not done after {limit / 60:g} minutes in {case}")
    except Exited as e:
        problems.append(f"ended in {case} (exit {e.args[0]})")
    if problems:
        record["status"] = "; ".join(problems)
    record["step_keys"] = session.key_times
    record["peak_rss"] = max(session.peak_rss, session.close())
    record["charged"] = session.charged
    record["memory_kills"] = session.memory_kills
    record["screen_at_end"] = session.screen.text()
    if record["save_ms"] is not None:
        record["saved_whole"] = count_text(copy, b"fox") == 0 and count_text(copy, b"cat") >= entry["fox_count"]
    os.remove(copy)
    clean_data(args.scratch)
    return record


def version(editor, scratch):
    """Return the first line EDITOR prints for its version, run in its fenced home."""
    try:
        out = subprocess.run(EDITORS[editor]["version"], capture_output=True, text=True, timeout=30,
                             env=fence(scratch, editor)).stdout
        return out.strip().splitlines()[0]
    except (OSError, subprocess.SubprocessError, IndexError):
        return "unknown"


def cpu_list(text):
    """Return the processor numbers a list such as "0,2,16-23" names, as a set."""
    numbers = set()
    for part in text.strip().split(","):
        if "-" in part:
            low, high = part.split("-")
            numbers.update(range(int(low), int(high) + 1))
        elif part:
            numbers.add(int(part))
    return numbers


def read_text(path):
    """Return the text of the file at PATH, or None when it cannot be read."""
    try:
        with open(path) as f:
            return f.read()
    except OSError:
        return None


def processors(wrap):
    """Return what the report says of the processor: its cores, of each kind where they differ (performance and
    efficiency cores), and the threads the editors were pinned to by taskset in WRAP and their kind."""
    cores = set()
    for cpu in range(os.cpu_count()):
        siblings = read_text(f"/sys/devices/system/cpu/cpu{cpu}/topology/core_cpus_list")
        if siblings:
            cores.add(siblings.strip())
    kinds = {}
    for kind, name in (("cpu_core", "performance"), ("cpu_atom", "efficiency")):
        listed = read_text(f"/sys/devices/{kind}/cpus")
        if listed:
            kinds[name] = cpu_list(listed)
    pinned = None
    words = wrap
    if "taskset" in words:
        at = words.index("taskset")
        if at + 2 < len(words) and words[at + 1] == "-c":
            pinned = cpu_list(words[at + 2])

    def split(cpus, of_cores):
        """Return how many of CPUS, threads, are of each kind; whole cores when OF_CORES."""
        out = []
        for name, members in kinds.items():
            if of_cores:
                count = sum(1 for c in cores if cpu_list(c) <= members)
            else:
                count = len(cpus & members)
            if count:
                out.append(f"{count} {name}")
        return out

    return {"cores": len(cores), "core_kinds": split(set(range(os.cpu_count())), True),
            "pinned": len(pinned) if pinned else None, "pinned_kinds": split(pinned, False) if pinned else []}


def storage(path):
    """Return what the report says of where PATH is stored: the file system and its options, the disks under
    it with their model and how they are attached, and the layers between (encryption, partitions)."""
    found = {"file_system": None, "options": None, "disks": [], "layers": []}
    try:
        source, kind, options = subprocess.run(["findmnt", "-no", "SOURCE,FSTYPE,OPTIONS", "-T", path],
                                               capture_output=True, text=True, timeout=30).stdout.split()
        found["file_system"] = kind
        found["options"] = [o for o in options.split(",") if o.split("=")[0] in ("compress", "compress-force",
                                                                                 "ssd", "discard")]
        tree = json.loads(subprocess.run(["lsblk", "-J", "-s", "-o", "NAME,TYPE,TRAN,ROTA,MODEL",
                                          source.split("[")[0]],
                                         capture_output=True, text=True, timeout=30).stdout)
    except (OSError, subprocess.SubprocessError, ValueError):
        return found

    def walk(device):
        """Note DEVICE and the devices under it."""
        if device["type"] == "disk":
            found["disks"].append({"model": (device.get("model") or "unknown").strip(),
                                   "attached": device.get("tran"), "solid": not device.get("rota")})
        elif device["type"] not in found["layers"]:
            found["layers"].append(device["type"])
        for child in device.get("children", []):
            walk(child)

    for device in tree.get("blockdevices", []):
        walk(device)
    return found


def machine(args, wrap):
    """Return what the report says about the machine and the settings of this run."""
    model = "unknown"
    with open("/proc/cpuinfo") as f:
        for line in f:
            if line.startswith("model name"):
                model = line.split(":", 1)[1].strip()
                break
    try:
        with open("/sys/devices/system/cpu/cpu0/cpufreq/scaling_max_freq") as f:
            mhz = int(f.read()) // 1000
    except (OSError, ValueError):
        mhz = None
    with open("/proc/meminfo") as f:
        memory = int(f.readline().split()[1]) * 1024
    return {"cpu": model, "max_mhz": mhz, "threads": os.cpu_count(), "memory": memory,
            "processors": processors(wrap), "storage": storage(args.data),
            "kernel": platform.release(), "python": platform.python_version(), "wrap": " ".join(wrap),
            "rows": args.rows, "columns": args.columns, "quiet_ms": args.quiet_ms, "rest_ms": args.rest_ms,
            "limit_s": args.limit, "repetitions": args.repetitions, "typing_keys": args.typing_keys,
            "page_keys": args.page_keys, "scenario": args.scenario, "date": time.strftime("%Y-%m-%d %H:%M %Z")}


def main():
    """Read the arguments, run every editor on every file the asked number of times, and write the records."""
    parser = argparse.ArgumentParser(description="Time editors on large files through a pseudo-terminal.")
    parser.add_argument("--data", required=True)
    parser.add_argument("--scratch", required=True)
    parser.add_argument("--out", required=True)
    parser.add_argument("--wrap", default="")
    parser.add_argument("--editors", default="emacs,emacs-writable,neovim,helix,tuatara")
    parser.add_argument("--files", default="csv,huge,log,org,json")
    parser.add_argument("--repetitions", type=int, default=3)
    parser.add_argument("--typing-keys", type=int, default=100)
    parser.add_argument("--page-keys", type=int, default=40)
    parser.add_argument("--rows", type=int, default=40)
    parser.add_argument("--columns", type=int, default=120)
    parser.add_argument("--quiet-ms", type=float, default=100)
    parser.add_argument("--rest-ms", type=float, default=400)
    parser.add_argument("--limit", type=float, default=120)
    parser.add_argument("--scenario", choices=("keys", "replace"), default="keys")
    parser.add_argument("--keep-homes", action="store_true")
    parser.add_argument("--record")
    args = parser.parse_args()
    wrap = shlex.split(args.wrap)
    editors = args.editors.split(",")
    with open(os.path.join(args.data, "manifest.json")) as f:
        manifest = json.load(f)
    entries = []
    for kind in args.files.split(","):
        entry = dict(manifest[kind])
        entry["kind"] = kind
        if args.scenario == "replace":
            entry["fox_count"] = count_text(os.path.join(args.data, entry["path"]), b"fox")
        entries.append(entry)
    os.makedirs(args.scratch, exist_ok=True)
    if args.record:
        os.makedirs(args.record, exist_ok=True)
    with open(args.out, "w") as out:
        meta = {"machine": machine(args, wrap), "files": entries, "sentence": REPLACE_SENTENCE,
                "editors": {e: {"name": EDITORS[e]["name"], "version": version(e, args.scratch),
                                "large_file_setup": EDITORS[e]["large_file_setup"]} for e in editors}}
        out.write(json.dumps({"meta": meta}) + "\n")
        for repetition in range(args.repetitions):
            order = editors[repetition % len(editors):] + editors[:repetition % len(editors)]
            for entry in entries:
                for editor in order:
                    print(f"repetition {repetition + 1}: {editor} on {entry['path']}", file=sys.stderr, flush=True)
                    run = run_replace if args.scenario == "replace" else run_one
                    record = run(editor, entry, args.data, args, wrap)
                    record["repetition"] = repetition + 1
                    out.write(json.dumps(record) + "\n")
                    out.flush()
                    print(f"  {record['status']}, open {record['open_ms'] or 0:.0f} ms"
                          + (f", finished {(record['finished_ms'] or 0) / 1000:.1f} s" if args.scenario == "replace"
                             else ""),
                          file=sys.stderr, flush=True)
    if not args.keep_homes:
        for editor in editors:
            shutil.rmtree(os.path.join(args.scratch, "fence-" + editor), ignore_errors=True)


if __name__ == "__main__":
    main()
