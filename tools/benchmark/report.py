#!/usr/bin/env python3
# tools/benchmark/report.py --- Write the editor benchmark's results as a Markdown report.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Turn the records tools/benchmark/harness.py wrote into a Markdown report.

Usage: python3 -I tools/benchmark/report.py RESULTS.jsonl > REPORT.md

The report names the machine, the editors' versions and how each was set up,
the files, how the measuring was done, then a table for each file with every
number, every run that failed and why, and a list of every case where
Tuatara is not the fastest or the smallest. tools/benchmark/run.sh runs it.
For a replace run (harness --scenario replace) it writes that run's report:
each editor's times for each step and whether it got done.
"""

import json
import re
import statistics
import sys

MIB = 1024 * 1024
FREEZE_MS = 1000
BUSY = float("inf")

RULES = """\
- Every editor runs in its terminal mode, in the same {rows} by {columns}
  pseudo-terminal (TERM=xterm-256color), driven by the same harness
  (tools/benchmark/harness.py, Python standard library only), on the same
  files, pinned to the same processors in a systemd user scope whose memory
  is capped as the reference machine's: `{wrap}`.
- Each editor has an empty home of its own, made afresh for every run
  (HOME, the XDG directories, TMPDIR and GNUPGHOME all inside it), so no one's
  own settings are read. Each has its stock setup for large files, as listed
  above. Emacs is started without the files a distribution adds to it
  (--no-site-file, inhibit-default-init): Fedora's bring warnings that take
  half its screen. Before each timed run the editor is started once on a
  small file, not timed, so that its caches are made, and the benchmark file
  is read once so that every editor opens it from the page cache.
- Where so-long makes Emacs's buffer read-only, Emacs cannot type into it as
  it stands; that is reported, and a second row, "made writable", turns
  read-only off (C-x C-q) first and times the typing.
- Open is the time from starting the editor to the first screen showing the
  file's first line, shown and idle as for a key below.
- Each key gets two times. Shown runs from writing the key to the terminal
  until the editor's output for it has settled, no output coming for
  {quiet:g} ms (not counted): what a person waits for. Idle runs on until, for
  {quiet:g} ms, there was also no output, the editor's processes used no
  processor time (one clock tick at most) and none of their threads waited
  on the disk: the work the key cost, including
  work done in the background after the screen settled. Before each key the
  harness waits for {rest:g} ms with no output, so that redraws an editor puts
  off on purpose are not counted in the next key; an editor that collects
  garbage when typing pauses (Tuatara after 0.3 s) may do so in that wait,
  which a person typing steadily would not give it time for. The tables give
  the median, the 95th percentile and the worst of all keys of all
  repetitions. Typing is {typing} letters and spaces at the start of the
  file, a new line every 50; page down is {pages} screens.
- Search is a word found once, near the end of the file, typed into each
  editor's search one key every 150 ms, as a person types, not timed; the
  final RET is timed until the match is on the screen (shown) and until the
  editor is idle as above. Every editor here searches as you type by default.
- Memory is the largest resident set of the editor's processes together,
  read from /proc while it runs.
- A freeze is any key whose shown time is over a second. A run that takes over {limit:g} s
  for one step is stopped and reported as timed out; an editor that crashes
  or refuses is reported so, never left out.
- The editors take turns in a different order in each of the {repetitions}
  repetitions."""


def milliseconds(value):
    """Return VALUE, a time in milliseconds, written for the table; n/a when there is none."""
    if value is None:
        return "n/a"
    if value == BUSY:
        return "over 10 s"
    if value < 10:
        return f"{value:.1f}"
    if value < 10000:
        return f"{value:.0f}"
    if value < 120000:
        return f"{value / 1000:.1f} s"
    return f"{value / 60000:.1f} min"


def percentile(values, fraction):
    """Return the value of VALUES at FRACTION (0.95 for the 95th percentile), by the nearest rank."""
    ordered = sorted(values)
    return ordered[max(0, min(len(ordered) - 1, round(fraction * len(ordered) + 0.5) - 1))]


def summary(runs):
    """Return the numbers of one editor on one file from its RUNS, one record each repetition."""
    def middle(key):
        values = [r[key] for r in runs if r.get(key) is not None]
        return statistics.median(values) if values else None

    def idle(key):
        values = [BUSY if r.get(key) is None else r[key] for r in runs if r.get(key[:-8] + "_ms") is not None]
        return statistics.median(values) if values else None

    out = {"open": middle("open_ms"), "end": middle("end_ms"), "search": middle("search_ms"),
           "open_idle": idle("open_idle_ms"), "end_idle": idle("end_idle_ms"), "search_idle": idle("search_idle_ms"),
           "memory": max((r.get("peak_rss") or 0 for r in runs), default=0) or None,
           "freezes": sum(r.get("freezes", 0) for r in runs),
           "failures": [f"repetition {r['repetition']}: {r['status']}" for r in runs if r["status"] != "ok"],
           "no_change": {}}
    for case in ("typing", "page", "typing_idle", "page_idle"):
        samples = [BUSY if t is None else t for r in runs for t in r.get(case, [])]
        out[case] = (statistics.median(samples), percentile(samples, 0.95), max(samples)) if samples else None
        out[case + "_count"] = len(samples)
    for r in runs:
        for case, count in r.get("no_change", {}).items():
            out["no_change"][case] = out["no_change"].get(case, 0) + count
    return out


def triple(values):
    """Return a median, 95th percentile and worst written as one table cell."""
    return "n/a" if values is None else " / ".join(milliseconds(v) for v in values)


def losses(tables, editors):
    """Return a line for each case where Tuatara is slower or larger than another editor, or has no result."""
    def key(case, index):
        return lambda s: s[case] and s[case][index]

    measures = [("open", lambda s: s["open"]), ("typing, median", key("typing", 0)),
                ("typing, 95th percentile", key("typing", 1)), ("typing, worst", key("typing", 2)),
                ("page down, median", key("page", 0)), ("page down, 95th percentile", key("page", 1)),
                ("page down, worst", key("page", 2)), ("to the end", lambda s: s["end"]),
                ("search", lambda s: s["search"]), ("memory", lambda s: s["memory"]),
                ("freezes", lambda s: s["freezes"])]
    lines = []
    for kind, table in tables.items():
        ours = table.get("tuatara")
        if ours is None:
            continue
        for label, get in measures:
            mine = get(ours)
            others = [(get(s), e) for e, s in table.items() if e != "tuatara" and get(s) is not None]
            if not others:
                continue
            best, who = min(others)
            name = editors[who]["name"]
            if mine is None:
                lines.append(f"- {kind}, {label}: Tuatara has no result; {name} has {show(label, best)}.")
            elif best < mine:
                lines.append(f"- {kind}, {label}: Tuatara {show(label, mine)}, {name} {show(label, best)}.")
    return lines


def show(label, value):
    """Return VALUE written as the measure LABEL is: memory in MiB, freezes counted, else milliseconds."""
    if label == "memory":
        return f"{value / MIB:.0f} MiB"
    if label == "freezes":
        return str(value)
    return milliseconds(value) + ("" if milliseconds(value).endswith((" s", " min")) else " ms")


REPLACE_RULES = """\
- Every editor runs in its terminal mode, in the same {rows} by {columns}
  pseudo-terminal (TERM=xterm-256color), the size a full-screen terminal has
  on a screen of 1920 by 1080, driven by the same harness, pinned to the same
  processors in a systemd user scope whose memory is capped as the
  reference machine's: `{wrap}`. Each has an empty home of its own and its
  stock setup, and is started once on a small file before the timed run.
- Each editor works on a fresh copy of the file, removed after its run, and
  read once before it so that it opens from the page cache.
- The steps, in each editor's own keys, typed one key every 150 ms as a
  person types: open the file; replace fox by cat in all of it ({count});
  at once, while that runs, make a new buffer and type "{sentence}" into
  it; go back to the file; save it; undo the replacement.
- Switching to the new buffer is timed from its last key until the screen
  settles, and switching back from its last key until the file's lines
  show; both while the replacement runs, where it still does.
- Typing is timed key by key, each from sending it until it shows on the
  screen and the output has settled for {quiet:g} ms: median and worst, and
  how long the whole sentence took, under which no finished time can fall,
  since the file is seen again only after it. A key
  over a second is a freeze, here or among the keys of the other steps,
  each also timed until it shows. An editor that cannot take keys while it
  replaces shows it here.
- Finished is from the key that gives the replacement until, back in the
  file after the typing, its lines show cat in place of fox and the
  editor's processes have used no processor time and waited on no disk for
  {quiet:g} ms. Save is from its last key until the file on disk has
  changed and the editor is idle as before. Each editor saves as it does
  by default; Tuatara writes the text to a new file beside the old one and
  renames it over the old, so that a crash leaves one or the other whole.
  Afterwards the whole file is read to check that no fox is left in it.
  Undo is from its last key until the lines show fox again and the editor
  is idle.
- A step not done after {limit:g} minutes ends the run, reported as not
  done; an editor that crashes or runs out of its memory is reported so,
  never left out. Charged is the largest memory the system charged to the
  editor's scope, what counts against its cap; it takes in the file pages
  an editor writes until the system writes them out, so an editor that
  saves may come near the cap without being stopped. Resident is the largest
  resident set of the editor's processes together; it also counts the
  pages of a file the editor maps from disk, which the page cache holds and
  the cap does not charge to it."""

REPLACE_KEYS = {"emacs": "M-x replace-string, fox, cat; C-x b hello; C-x b RET; C-x C-s; C-_",
                "neovim": ":%s/fox/cat/g; :enew, i; Esc, :b#; :w; u",
                "helix": "%, s fox RET, c cat Esc, ,; :new, i; Esc, ga; :w; u",
                "tuatara": "r r %fox/cat RET; C-x b hello, i; Esc, C-x b RET; C-x C-s; u"}


def machine_rows(m):
    """Return the table rows of the machine section of a report from M, what the harness noted of the machine."""
    p = m.get("processors") or {}
    cores = f"{p['cores']} cores ({' and '.join(p['core_kinds'])}), " if p.get("cores") else ""
    found = re.search(r"MemoryMax=(\d+)", m["wrap"])
    cap = f", each editor capped at {int(found.group(1)) / MIB / 1024:.0f} GiB" if found else ""
    pinned = (f"{p['pinned']} of the threads ({' and '.join(p['pinned_kinds'])}){cap}" if p.get("pinned")
              else "all of them" + cap)
    rows = [f"| Processor | {m['cpu']}, {cores}{m['threads']} threads, at most {m['max_mhz']} MHz |",
            f"| Memory | {m['memory'] / MIB / 1024:.0f} GiB |",
            f"| Editors run on | {pinned} |"]
    st = m.get("storage") or {}
    for disk in st.get("disks", []):
        kind = "solid-state" if disk["solid"] else "spinning"
        attached = f", attached by {disk['attached'].upper()}" if disk.get("attached") else ""
        layers = " under disk encryption (LUKS)" if "crypt" in st.get("layers", []) else ""
        rows.append(f"| Disk | {disk['model']}, {kind}{attached}{layers} |")
    if st.get("file_system"):
        options = f" ({', '.join(st['options'])})" if st.get("options") else ""
        rows.append(f"| File system | {st['file_system']}{options} |")
    rows += [f"| Pinning and cap | `{m['wrap']}` |",
             f"| Kernel | Linux {m['kernel']} |",
             f"| Harness | Python {m['python']} |"]
    return rows


def replace_report(meta, records):
    """Return the lines of the report of a replace run, from its META and RECORDS."""
    m = meta["machine"]
    editors = meta["editors"]
    entry = meta["files"][0]
    found = re.search(r"MemoryMax=(\d+)", m["wrap"])
    cap = int(found.group(1)) if found else None
    out = ["# Replacing in a gigantic file in four editors", "",
           f"Measured {m['date']} by tools/benchmark/run.sh --replace in the Tuatara repository.", "",
           "## Machine", "", "| | |", "|---|---|", *machine_rows(m), "",
           "## Editors", "", "| Editor | Version | Keys |", "|---|---|---|"]
    for key, e in editors.items():
        out.append(f"| {e['name']} | {e['version']} | {REPLACE_KEYS.get(key, '')} |")
    out += ["", "## File", "",
            f"{entry['path']}, {entry['size'] / MIB:.1f} MiB, SHA-256 `{entry['sha256'][:16]}`, made by "
            "tools/benchmark/make-data.py from a fixed seed.", "",
            "## How it was measured", "",
            REPLACE_RULES.format(rows=m["rows"], columns=m["columns"], wrap=m["wrap"], quiet=m["quiet_ms"],
                                 limit=m["limit_s"] / 60, count=f"{entry['fox_count']:,} of them",
                                 sentence=meta.get("sentence", "")), "",
            "## Results", "",
            "Times in milliseconds unless marked. Typing is the median / worst key while the replacement runs, "
            "and the sentence's whole time.", "",
            "| Editor | Open | Switch to new buffer | Typing | Sentence | Switch back | Freezes | Finished | "
            "Save as new file | Undo | Charged | Resident | Outcome |",
            "|---|---|---|---|---|---|---|---|---|---|---|---|---|"]
    for key, e in editors.items():
        for r in (r for r in records if r["editor"] == key):
            resident = f"{r['peak_rss'] / MIB:.0f} MiB" if r.get("peak_rss") else "n/a"
            charged = f"{r['charged'] / MIB:.0f} MiB" if r.get("charged") else "n/a"
            outcome = "done" if r["status"] == "ok" else r["status"]
            ended = "ended in" in r["status"]
            if ended and (r.get("memory_kills") or (cap and (r.get("charged") or 0) >= 0.99 * cap)):
                outcome += ", out of memory: the system stopped it at its cap"
            if r.get("saved_whole") is False:
                outcome += "; the saved file still has fox in it"
            typing = r.get("typing") or []
            keys = (f"{milliseconds(statistics.median(typing))} / {milliseconds(max(typing))}" if typing
                    else "n/a")
            switches = [r.get("switch_to_ms") or 0, r.get("switch_back_ms") or 0]
            slow = [t for t in typing + r.get("step_keys", []) + switches if t > FREEZE_MS]
            freezes = f"{len(slow)}, longest {milliseconds(max(slow))}" if slow else "0"
            out.append(f"| {e['name']} | {milliseconds(r['open_ms'])} | {milliseconds(r.get('switch_to_ms'))} | "
                       f"{keys} | {milliseconds(r.get('typing_span_ms'))} | "
                       f"{milliseconds(r.get('switch_back_ms'))} | {freezes} | "
                       f"{milliseconds(r.get('finished_ms'))} | {milliseconds(r.get('save_ms'))} | "
                       f"{milliseconds(r.get('undo_ms'))} | {charged} | {resident} | {outcome} |")
    return out


def main():
    """Read the records named on the command line and print the report."""
    meta, records = None, []
    with open(sys.argv[1]) as f:
        for line in f:
            item = json.loads(line)
            if "meta" in item:
                meta = item["meta"]
            else:
                records.append(item)
    if meta["machine"].get("scenario") == "replace":
        print("\n".join(replace_report(meta, records)))
        return
    m = meta["machine"]
    editors = meta["editors"]
    out = ["# Large files in four editors", "",
           f"Measured {m['date']} by tools/benchmark/run.sh in the Tuatara repository.", "",
           "## Machine", "",
           "| | |", "|---|---|", *machine_rows(m), "",
           "## Editors", "",
           "| Editor | Version | Setup for large files |", "|---|---|---|"]
    for e in editors.values():
        out.append(f"| {e['name']} | {e['version']} | {e['large_file_setup']} |")
    out += ["", "Visual Studio Code is not in the comparison: it is not installed on the machine, and it has no "
            "terminal mode to drive the same way.", "",
            "## Files", "",
            "Made by tools/benchmark/make-data.py from a fixed seed; the same sizes and seed give the same bytes.", "",
            "| File | Size | SHA-256 |", "|---|---|---|"]
    for entry in meta["files"]:
        out.append(f"| {entry['path']} | {entry['size'] / MIB:.1f} MiB | `{entry['sha256'][:16]}` |")
    out += ["", "## How it was measured", "",
            RULES.format(rows=m["rows"], columns=m["columns"], wrap=m["wrap"], quiet=m["quiet_ms"],
                         rest=m["rest_ms"], typing=m["typing_keys"], pages=m["page_keys"], limit=m["limit_s"],
                         repetitions=m["repetitions"]), ""]
    tables = {}
    for entry in meta["files"]:
        kind = entry["kind"]
        tables[kind] = {}
        out += [f"## {entry['path']} ({entry['size'] / MIB:.1f} MiB)", "",
                "Shown: until the screen settled, what a person waits for. Times in milliseconds; for keys "
                "the median / 95th percentile / worst.", "",
                "| Editor | Open | Typing | Page down | To the end | Search | Memory | Freezes |",
                "|---|---|---|---|---|---|---|---|"]
        work = ["", "Idle: until the editor's processes had also stopped using the processor, the work each "
                "step cost, background work included.", "",
                "| Editor | Open | Typing | Page down | To the end | Search |", "|---|---|---|---|---|---|"]
        notes = []
        for key, e in editors.items():
            runs = [r for r in records if r["editor"] == key and r["file"] == kind and r["status"] != "skipped"]
            if not runs:
                continue
            s = summary(runs)
            tables[kind][key] = s
            memory = f"{s['memory'] / MIB:.0f} MiB" if s["memory"] else "n/a"
            out.append(f"| {e['name']} | {milliseconds(s['open'])} | {triple(s['typing'])} | {triple(s['page'])} | "
                       f"{milliseconds(s['end'])} | {milliseconds(s['search'])} | {memory} | {s['freezes']} |")
            work.append(f"| {e['name']} | {milliseconds(s['open_idle'])} | {triple(s['typing_idle'])} | "
                        f"{triple(s['page_idle'])} | {milliseconds(s['end_idle'])} | "
                        f"{milliseconds(s['search_idle'])} |")
            notes += [f"- {e['name']}, {failure}" for failure in s["failures"]]
            for case, count in s["no_change"].items():
                notes.append(f"- {e['name']}: {count} {case} key(s) changed nothing on the screen, not timed.")
        out += work
        if notes:
            out += ["", "Runs that did not go as planned:", ""] + notes
        out.append("")
    out += ["## Where Tuatara loses", "",
            "Every case where another editor is faster or smaller, however slightly; differences of a "
            "millisecond or less are within what the measuring can tell apart.", ""]
    out += losses(tables, editors) or ["- None."]
    print("\n".join(out))


if __name__ == "__main__":
    main()
