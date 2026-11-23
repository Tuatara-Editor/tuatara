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
from repositories import lisp_files, package_roots

OWN = re.compile(r"\(in-package\s+#?:?([^\s)]+)", re.I)
REACH = re.compile(r"(?<![\w:.-])#?:?([A-Za-z][\w.-]*)::([^\s()'`,\"]+)")
# what Lisp text is made of, as far as telling code from the rest: a string, a line comment, a block comment
# (not nested), a character such as #\" or #\;
LEXEME = re.compile(r'"(?:[^"\\]|\\.)*"|;[^\n]*|#\|.*?\|#|#\\.', re.S)


def code_only(text):
    """TEXT with every string, comment and character blanked (line ends kept, so line numbers stay), leaving the code
    alone, where a double colon is a reference."""
    return LEXEME.sub(lambda match: re.sub(r"[^\n]", " ", match.group()), text)


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
