#!/usr/bin/env python3
# tools/line-check.py --- List the lines that run past the column set for their kind of file.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""List lines longer than the set columns in the files a repository tracks.

Usage: tools/line-check.py [--summary] [--packages] [ROOT...]

Each ROOT is a repository (the current directory when left out). With
--packages the package repositories next to this one (../tuatara-packages/*/)
are looked at as well. Code files (Lisp,
C, shell, Python, Emacs Lisp, JavaScript, Rust, workflows) are held to 120
columns. Documents (Org, Markdown) are held to 80; their tables, lines that
are only a link, and source, example and quote blocks are left out. The
grammars' queries and licences taken from elsewhere (syntax/rules/ and the
directories beside it), patches, expected outputs and data files are not
looked at; Tuatara's own C in syntax/ is.

Prints one line per long line as REPOSITORY/FILE:LINE: LENGTH, then the
total. With --summary
it prints the count for each file instead. The columns are
targets, so this only lists: it always exits with 0 when it could read ROOT.
"""
import re
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from repositories import report

CODE_COLUMN = 120
DOCUMENT_COLUMN = 80
CODE = {".lisp", ".asd", ".c", ".h", ".sh", ".py", ".el", ".js", ".rs", ".yml", ".yaml"}
DOCUMENTS = {".org", ".md"}
# the grammars' queries and what comes with them, taken from elsewhere as they are; patches; built files
SKIPPED = tuple(f"syntax/{d}/" for d in ("rules", "indents", "textobjects", "injections", "locals", "tags", "patches",
                                        "licenses")) + ("test/syntax/", "build/")
BLOCK_START = re.compile(r"^\s*#\+begin_(src|example|export|quote|verse)\b|^\s*```|^\s*~~~", re.I)
BLOCK_END = re.compile(r"^\s*#\+end_(src|example|export|quote|verse)\b|^\s*```|^\s*~~~", re.I)
ONLY_LINK = re.compile(r"^\s*([-+*]\s+)?(\[\[[^\]]*\](\[[^\]]*\])?\]|<?https?://\S+>?|\[[^\]]*\]\([^)]*\))\s*$")


def tracked_files(root):
    """The files of ROOT's repository that are kept in it, as relative path strings; every file when ROOT has none."""
    done = subprocess.run(["git", "-C", str(root), "ls-files", "-z"], capture_output=True)
    if done.returncode == 0:
        return sorted(name for name in done.stdout.decode().split("\0") if name)
    return sorted(str(p.relative_to(root)) for p in root.rglob("*") if p.is_file() and ".git" not in p.parts)


def long_lines(path, text):
    """The (line number, length) of each line of TEXT, a file at PATH, past the column for that kind of file."""
    if path.suffix in CODE:
        return [(n, len(line)) for n, line in enumerate(text.splitlines(), 1) if len(line) > CODE_COLUMN]
    found, inside = [], False
    for n, line in enumerate(text.splitlines(), 1):
        if inside:
            inside = not BLOCK_END.match(line)
        elif BLOCK_START.match(line):
            inside = True
        elif (len(line) > DOCUMENT_COLUMN and not line.lstrip().startswith(("|", ": "))
              and not ONLY_LINK.match(line)):
            found.append((n, len(line)))
    return found


def checked_files(root):
    """The files of ROOT's repository whose lines are held to a column, as relative path strings."""
    return [name for name in tracked_files(root)
            if Path(name).suffix in CODE | DOCUMENTS and not name.startswith(SKIPPED)]


def main(argv):
    """Run the check for the command line ARGV; return the exit status."""
    return report(argv, checked_files, lambda path, text: [f"{n}: {length}" for n, length in long_lines(path, text)],
                  f"lines past the column (code {CODE_COLUMN}, documents {DOCUMENT_COLUMN})")


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
