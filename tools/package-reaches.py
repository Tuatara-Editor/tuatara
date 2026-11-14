#!/usr/bin/env python3
# tools/package-reaches.py --- List the names a file takes from another Lisp package with two colons.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""List the references of the form PACKAGE::NAME that cross a package boundary.

Usage: tools/package-reaches.py [--summary] [--packages] [ROOT...]

Each ROOT is a repository (the current directory when left out); with
--packages the package repositories beside this one (../tuatara-packages/*/)
are looked at as well. A file's own
package is the one its first IN-PACKAGE form names; a double-colon reference to
that package is not counted, and neither is one inside a comment or a string
that only mentions it. Test files (test/ and a package's test.lisp) are left
out: they are meant to look inside. Everything else is a reach into a package that did not
export the name. Prints one line per reach as FILE:LINE: PACKAGE::NAME, then
the total. With --summary it prints the count for each file instead. The
count is a fact for the health report: it always exits with 0.
"""
import re
import subprocess
import sys
from pathlib import Path

OWN = re.compile(r"\(in-package\s+#?:?([^\s)]+)", re.I)
REACH = re.compile(r"(?<![\w:.-])#?:?([A-Za-z][\w.-]*)::([^\s()'`,\"]+)")


def package_roots():
    """The package repositories that sit beside this checkout, in name order."""
    beside = Path(__file__).resolve().parent.parent.parent / "tuatara-packages"
    return sorted(p for p in beside.glob("tuatara-*") if (p / ".git").exists() and not p.name.endswith("-pc10"))


def lisp_files(root):
    """The Lisp files ROOT's repository keeps, as relative paths, without its tests."""
    done = subprocess.run(["git", "-C", str(root), "ls-files", "-z", "*.lisp"], capture_output=True)
    names = done.stdout.decode().split("\0") if done.returncode == 0 else []
    return sorted(n for n in names if n and not n.startswith("test/") and n != "test.lisp")


def reaches(text):
    """The (line number, reference) pairs in TEXT that name a package other than the file's own."""
    own = OWN.search(text)
    own = own.group(1).lower() if own else ""
    found = []
    for number, line in enumerate(text.splitlines(), 1):
        code = line.split(";", 1)[0]
        for match in REACH.finditer(code):
            if match.group(1).lower() != own:
                found.append((number, f"{match.group(1)}::{match.group(2)}"))
    return found


def main(argv):
    """Run the check for the command line ARGV; return the exit status."""
    summary = "--summary" in argv
    roots = [Path(a) for a in argv if not a.startswith("--")]
    if "--packages" in argv:
        roots = (roots or [Path(".")]) + package_roots()
    roots = roots or [Path(".")]
    total = 0
    for root in roots:
        for name in lisp_files(root):
            try:
                text = (root / name).read_text(encoding="utf-8")
            except (OSError, UnicodeDecodeError):
                continue
            found = reaches(text)
            total += len(found)
            if summary and found:
                print(f"{len(found):6d} {root.resolve().name}/{name}")
            elif not summary:
                for number, reference in found:
                    print(f"{root.resolve().name}/{name}:{number}: {reference}")
    print(f"{total} references past a package boundary with two colons")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
