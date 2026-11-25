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
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from lisp_text import code_only
from repositories import lisp_files, report

OWN = re.compile(r"\(in-package\s+#?:?([^\s)]+)", re.I)
REACH = re.compile(r"(?<![\w:.-])#?:?([A-Za-z][\w.-]*)::([^\s()'`,\"]+)")
def reaches(text):
    """The (line number, reference) pairs in TEXT that name a package other than the file's own."""
    own = OWN.search(text)
    own = own.group(1).lower() if own else ""
    found = []
    for number, code in enumerate(code_only(text).splitlines(), 1):
        for match in REACH.finditer(code):
            if match.group(1).lower() != own:
                found.append((number, f"{match.group(1)}::{match.group(2)}"))
    return found


def main(argv):
    """Run the check for the command line ARGV; return the exit status."""
    return report(argv, lisp_files, lambda _, text: [f"{n}: {reference}" for n, reference in reaches(text)],
                  "references past a package boundary with two colons")


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
