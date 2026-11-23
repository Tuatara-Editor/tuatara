"""A stand-in for ssh in the tests of a daemon on another machine.

It takes the command lines a client of such a daemon gives ssh and does what
ssh does with them, the other machine being this one:

  stand-in SERVER-BIN LOG -N -o ControlMaster=yes -o ControlPath=C ... -- HOST
      is the connection: listens on C for -O requests and sessions until
      told to exit or killed; with ForwardAgent=PATH, the first session
      asking for the agent is given a socket passing on to PATH.
  stand-in SERVER-BIN LOG [-n] -o ControlPath=C ... -- HOST COMMAND...
      runs COMMAND over the connection (exit 255 when there is none) with
      sh -c, SERVER-BIN first on its PATH, SSH_AUTH_SOCK the agent's socket
      when it was given one (ForwardAgent=yes); without ControlPath, runs
      it at once.
  stand-in SERVER-BIN LOG -G -- HOST
      says what the configuration STAND_IN_CONFIG makes of HOST: its Host
      block's HostName, HostKeyAlias, User and IdentityFile lines.
  stand-in SERVER-BIN LOG -o ControlPath=C -O forward -L HERE:THERE -- HOST
      has the connection listen on the socket HERE and, for each connection
      to it, connect to THERE and pass octets both ways: one process for all,
      as sshd connects to a daemon's socket.
  stand-in SERVER-BIN LOG -o ControlPath=C -O forward -R THERE:HERE -- HOST
      has the connection listen on the socket THERE, as sshd does on the
      other machine, and pass each connection to it on to HERE.
  stand-in SERVER-BIN LOG -o ControlPath=C -O cancel -R THERE:HERE -- HOST
      has the connection stop listening on THERE and remove the socket.
  stand-in SERVER-BIN LOG -o ControlPath=C -O exit -- HOST
      ends the connection.

  stand-in SERVER-BIN LOG -V
      says it is the OpenSSH STAND_IN_VERSION names (10.2p1 when none).
  stand-in --agent LOG -D -a SOCKET
      is ssh-agent: listens on SOCKET, answers that it holds no keys and
      refuses anything else, and writes to LOG.agent what it was asked and
      that it ended (SIGTERM).
  stand-in --add LOG ARGUMENT...
      is ssh-add: writes its arguments and SSH_AUTH_SOCK to LOG.add, asks
      the agent there for its keys, and exits 0, or 1 when LOG.add-fails is
      there, or 2 when no agent answers.

A HOST ending in .invalid cannot be reached (exit 255). Every command line is
written to LOG, one argument a line, then a line naming the environment's
variables, env NAME..., and a line of --- after each. An -O request without
-F none reads, as ssh does, the configuration STAND_IN_CONFIG names, and its
RemoteForward and LocalForward lines are sent with it; the connection writes
each forward it is asked for to LOG.forwards, one a line. With -n the command
reads nothing; without, it reads the stand-in's standard input. A stand-in
ended by SIGTERM ends its command too, with every program it started.
"""

import os
import signal
import socket
import sys
import threading
import time


def read_arguments(arguments):
    """The options, the -O operation, the -L or -R forward, the host and the command."""
    options, operation, forward = {}, None, None
    flags = set()
    at = 0
    while at < len(arguments):
        argument = arguments[at]
        if argument == "--":
            return options, operation, forward, flags, arguments[at + 1], arguments[at + 2:]
        if argument in ("-o", "-O", "-L", "-R", "-F"):
            value = arguments[at + 1]
            at += 2
            if argument == "-o":
                name, _, setting = value.partition("=")
                options[name] = setting
            elif argument == "-O":
                operation = value
            elif argument == "-F":
                options["-F"] = value
            elif argument == "-R":
                forward = ("R", value)
            else:
                forward = ("L", value)
            continue
        if argument in ("-T", "-n", "-N", "-G"):
            flags.add(argument)
            at += 1
            continue
        sys.stderr.write("stand-in: no such option %s\n" % argument)
        sys.exit(255)
    sys.stderr.write("stand-in: no -- before the host\n")
    sys.exit(255)


def control_path(options):
    """The control socket's path, its %% written %."""
    return options["ControlPath"].replace("%%", "%")


def pass_octets(source, sink):
    """Copy what SOURCE sends to SINK until SOURCE ends, then end SINK's sending."""
    try:
        while True:
            octets = source.recv(65536)
            if not octets:
                break
            sink.sendall(octets)
    except OSError:
        pass
    try:
        sink.shutdown(socket.SHUT_WR)
    except OSError:
        pass


class Connection:
    """The connection that stays: its forwards and their open connections."""

    def __init__(self, keep_seconds, record, agent=None, relay=None):
        self.record = record
        self.keep_seconds = keep_seconds
        self.agent = agent
        self.relay = relay
        self.lock = threading.Lock()
        self.open = 0
        self.last_used = time.monotonic()
        self.listeners = []

    def used(self, change):
        with self.lock:
            self.open += change
            self.last_used = time.monotonic()

    def idle(self):
        with self.lock:
            return (self.keep_seconds is not None and self.open == 0
                    and time.monotonic() - self.last_used > self.keep_seconds)

    def carry(self, here, there):
        self.used(1)
        try:
            with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as far:
                far.connect(there)
                back = threading.Thread(target=pass_octets, args=(far, here), daemon=True)
                back.start()
                pass_octets(here, far)
                back.join()
        except OSError:
            pass
        finally:
            here.close()
            self.used(-1)

    def listen(self, here, there):
        with open(self.record, "a") as record:
            record.write("%s:%s\n" % (here, there))
        listener = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        old = os.umask(0o177)
        try:
            listener.bind(here)
        finally:
            os.umask(old)
        listener.listen(16)
        self.listeners.append((listener, here))

        def take():
            while True:
                try:
                    taken, _ = listener.accept()
                except OSError:
                    return
                threading.Thread(target=self.carry, args=(taken, there), daemon=True).start()

        threading.Thread(target=take, daemon=True).start()

    def cancel(self, here):
        for listener, path in list(self.listeners):
            if path == here:
                listener.close()
                self.listeners.remove((listener, path))
                os.unlink(path)

    def end(self):
        for listener, here in self.listeners:
            listener.close()
            try:
                os.unlink(here)
            except OSError:
                pass


def serve(control, connection):
    """Answer the -O requests on CONTROL until told to exit or idle too long."""
    control.settimeout(0.5)
    while not connection.idle():
        try:
            asker, _ = control.accept()
        except socket.timeout:
            continue
        with asker:
            request = asker.makefile("r").readline().split()
            if request[:1] == ["config"] and len(request) == 2:
                with open(connection.record, "a") as record:
                    record.write("%s\n" % request[1])
                asker.sendall(b"ok\n")
            elif request[:1] == ["forward"] and len(request) == 3:
                try:
                    connection.listen(request[1], request[2])
                    asker.sendall(b"ok\n")
                except OSError as failure:
                    asker.sendall(("failed %s\n" % failure).encode())
            elif request[:1] == ["session"]:
                # as sshd, the agent for the first session asking for it alone
                if request[1:] == ["agent"] and connection.agent and connection.relay:
                    connection.listen(connection.relay, connection.agent)
                    asker.sendall(("ok %s\n" % connection.relay).encode())
                    connection.agent = None
                else:
                    asker.sendall(b"ok -\n")
            elif request[:1] == ["cancel"] and len(request) == 2:
                connection.cancel(request[1])
                with open(connection.record, "a") as record:
                    record.write("cancel %s\n" % request[1])
                asker.sendall(b"ok\n")
            elif request[:1] == ["exit"]:
                asker.sendall(b"ok\n")
                break
            else:
                asker.sendall(b"failed\n")
    connection.end()


def configured_forwards():
    """The forwards the configuration STAND_IN_CONFIG names, as ssh reads them for any host."""
    path = os.environ.get("STAND_IN_CONFIG")
    if not path or not os.path.exists(path):
        return []
    with open(path) as config:
        return ["".join(line.split()) for line in config
                if line.split()[:1] in (["RemoteForward"], ["LocalForward"], ["DynamicForward"])]


def ask(options, words):
    """Send the connection WORDS, with the configuration's forwards unless -F none; the exit code ssh gives."""
    requests = [words]
    if options.get("-F") != "none":
        requests += [["config", forward] for forward in configured_forwards()]
    try:
        for request in requests:
            with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as asker:
                asker.connect(control_path(options))
                asker.sendall((" ".join(request) + "\n").encode())
                answer = asker.makefile("r").readline()
    except OSError as failure:
        sys.stderr.write("Control socket connect(%s): %s\n" % (control_path(options), failure.strerror))
        return 255
    if answer.startswith("ok"):
        return answer.split()[1] if len(answer.split()) > 1 else 0
    sys.stderr.write("stand-in: %s" % answer)
    return 255


def agent_message(connection):
    """One message an agent is sent: its type and contents, or None at the end."""
    head = connection.recv(4, socket.MSG_WAITALL)
    if len(head) < 4:
        return None
    size = int.from_bytes(head, "big")
    if not 0 < size <= 262144:
        return None
    body = connection.recv(size, socket.MSG_WAITALL)
    return body[0], body[1:]


def agent(log, arguments):
    """Be ssh-agent -D -a SOCKET until SIGTERM."""
    path = arguments[arguments.index("-a") + 1]
    record = log + ".agent"

    def note(line):
        with open(record, "a") as out:
            out.write(line + "\n")

    def ended(number, frame):
        note("ended")
        try:
            os.unlink(path)
        except OSError:
            pass
        os._exit(0)

    signal.signal(signal.SIGTERM, ended)
    listener = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    old = os.umask(0o177)
    try:
        listener.bind(path)
    finally:
        os.umask(old)
    listener.listen(4)
    note("listening " + path)
    while True:
        taken, _ = listener.accept()
        with taken:
            while True:
                message = agent_message(taken)
                if message is None:
                    break
                note("asked %d" % message[0])
                # the keys asked for: none; anything else refused
                answer = b"\x0c\x00\x00\x00\x00" if message[0] == 11 else b"\x05"
                taken.sendall(len(answer).to_bytes(4, "big") + answer)


def add(log, arguments):
    """Be ssh-add: note the arguments and ask the agent for its keys."""
    path = os.environ.get("SSH_AUTH_SOCK", "")
    with open(log + ".add", "a") as out:
        out.write(" ".join(arguments) + "\nSSH_AUTH_SOCK=" + path + "\n")
    try:
        with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as asker:
            asker.connect(path)
            asker.sendall(b"\x00\x00\x00\x01\x0b")
            asker.recv(64)
    except OSError:
        sys.stderr.write("Could not open a connection to your authentication agent.\n")
        return 2
    if os.path.exists(log + ".add-fails"):
        sys.stderr.write("No identities found\n")
        return 1
    return 0


def main():
    if sys.argv[1] == "--agent":
        return agent(sys.argv[2], sys.argv[3:])
    if sys.argv[1] == "--add":
        return add(sys.argv[2], sys.argv[3:])
    server_bin, log = sys.argv[1], sys.argv[2]
    arguments = sys.argv[3:]
    if arguments == ["-V"]:
        sys.stderr.write("OpenSSH_%s, OpenSSL 3.5.8 25 Aug 2026\n" % os.environ.get("STAND_IN_VERSION", "10.2p1"))
        return 0
    with open(log, "a") as record:
        record.write("\n".join(arguments) + "\nenv " + " ".join(sorted(os.environ)) + "\n---\n")
    options, operation, forward, flags, host, command = read_arguments(arguments)
    if host.startswith("-"):
        sys.stderr.write("stand-in: a host read as an option\n")
        return 255
    if operation in ("forward", "cancel"):
        direction, value = forward
        first, _, second = value.partition(":")
        # -L HERE:THERE listens here; -R THERE:HERE listens there: either way the first is listened on
        if operation == "cancel":
            return ask(options, ["cancel", first])
        return ask(options, ["forward", first, second])
    if operation == "exit":
        return ask(options, ["exit"])
    if "-G" in flags:
        return say_settings(host)
    if host.endswith(".invalid"):
        sys.stderr.write("ssh: Could not resolve hostname %s: Name or service not known\n" % host)
        return 255
    if options.get("ControlMaster") == "yes":
        control = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        control.bind(control_path(options))
        control.listen(4)
        forwarded = options.get("ForwardAgent", "no")
        persist = options.get("ControlPersist", "no")
        serve(control, Connection(None if persist == "no" else int(persist), log + ".forwards",
                                  forwarded if forwarded.startswith("/") else None,
                                  os.path.join(os.path.dirname(control_path(options)), "r")))
        return 0
    environment = dict(os.environ)
    environment["PATH"] = server_bin + ":/usr/bin:/bin"
    environment.pop("STAND_IN_CONFIG", None)
    environment.pop("SSH_AUTH_SOCK", None)
    if options.get("ControlPath") not in (None, "none"):
        given = ask({"ControlPath": options["ControlPath"], "-F": "none"},
                    ["session", "agent" if options.get("ForwardAgent") == "yes" else "none"])
        if given == 255:
            return 255
        if given not in (0, "-"):
            environment["SSH_AUTH_SOCK"] = given
    child = os.fork()
    if child == 0:
        os.setpgid(0, 0)
        if "-n" in flags:
            nothing = os.open(os.devnull, os.O_RDONLY)
            os.dup2(nothing, 0)
        os.execve("/bin/sh", ["sh", "-c", " ".join(command)], environment)
    signal.signal(signal.SIGTERM, lambda number, frame: (os.killpg(child, signal.SIGKILL), os._exit(143)))
    code = os.waitpid(child, 0)[1]
    return os.waitstatus_to_exitcode(code)


def say_settings(host):
    """Write what STAND_IN_CONFIG's Host block for HOST sets, as ssh -G does."""
    settings, keys = {"hostname": host}, []
    path = os.environ.get("STAND_IN_CONFIG")
    if path and os.path.exists(path):
        inside = False
        with open(path) as config:
            for line in config:
                words = line.split()
                if not words:
                    continue
                if words[0].lower() == "host":
                    inside = host in words[1:]
                elif inside and len(words) == 2:
                    key = words[0].lower()
                    if key == "identityfile":
                        keys.append(words[1])
                    else:
                        settings[key] = words[1]
    for key, value in settings.items():
        print("%s %s" % (key, value))
    for key in keys:
        print("identityfile %s" % key)
    return 0


if __name__ == "__main__":
    sys.exit(main())
