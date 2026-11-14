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
are only a link, and source, example and quote blocks are left out. Vendored
grammar queries (syntax/), patches, expected outputs and data files are not
looked at.

Prints one line per long line as FILE:LINE: LENGTH (the file name starts with
the repository's name when there are several), then the total. With --summary
it prints the count for each file instead. The columns are
targets, so this only lists: it always exits with 0 when it could read ROOT.
"""
import re
import subprocess
import sys
from pathlib import Path

CODE_COLUMN = 120
DOCUMENT_COLUMN = 80
CODE = {".lisp", ".asd", ".c", ".h", ".sh", ".py", ".el", ".js", ".rs", ".yml", ".yaml"}
DOCUMENTS = {".org", ".md"}
SKIPPED = ("syntax/", "test/syntax/", "build/")
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


def package_roots():
    """The package repositories that sit beside this checkout, in name order."""
    beside = Path(__file__).resolve().parent.parent.parent / "tuatara-packages"
    return sorted(p for p in beside.glob("tuatara-*") if (p / ".git").exists() and not p.name.endswith("-pc10"))


def count_long_lines(root, label, summary):
    """Print the long lines of repository ROOT, their names starting with LABEL; return how many there are."""
    total = 0
    for name in tracked_files(root):
        path = Path(name)
        if path.suffix not in CODE | DOCUMENTS or name.startswith(SKIPPED):
            continue
        try:
            text = (root / path).read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        found = long_lines(path, text)
        total += len(found)
        if summary and found:
            print(f"{len(found):6d} {label}{name}")
        elif not summary:
            for number, length in found:
                print(f"{label}{name}:{number}: {length}")
    return total


def main(argv):
    """Run the check for the command line ARGV; return the exit status."""
    summary = "--summary" in argv
    roots = [Path(a) for a in argv if not a.startswith("--")]
    if "--packages" in argv:
        roots = (roots or [Path(".")]) + package_roots()
    roots = roots or [Path(".")]
    total = 0
    for root in roots:
        if not root.is_dir():
            print(f"line-check: {root} is not a directory", file=sys.stderr)
            return 2
        total += count_long_lines(root, f"{root.resolve().name}/" if len(roots) > 1 else "", summary)
    print(f"{total} lines past the column (code {CODE_COLUMN}, documents {DOCUMENT_COLUMN})")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
