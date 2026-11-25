# tools/repositories.py --- Find the package repositories beside this checkout, and report over repositories.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Where the package repositories are and which files they keep, and the one way the report scripts of tools/
print what they find in them."""
import subprocess
import sys
from pathlib import Path


def package_roots():
    """The package repositories beside this checkout (../tuatara-packages/tuatara-*/), in name order. Only
    repositories of their own, with a .git directory: the worktrees of their branches, which have a .git file,
    are left out."""
    beside = Path(__file__).resolve().parent.parent.parent / "tuatara-packages"
    return sorted(p for p in beside.glob("tuatara-*") if (p / ".git").is_dir())


def lisp_files(root):
    """The Lisp files ROOT's repository keeps, as relative paths, without its tests (test/ and a package's
    test.lisp)."""
    done = subprocess.run(["git", "-C", str(root), "ls-files", "-z", "*.lisp"], capture_output=True)
    names = done.stdout.decode().split("\0") if done.returncode == 0 else []
    return sorted(n for n in names if n and not n.startswith("test/") and n != "test.lisp")


def report_roots(argv):
    """The repositories a report's command line ARGV names: each argument that is no option, else the current
    directory, and with --packages the package repositories beside this checkout after them."""
    roots = [Path(a) for a in argv if not a.startswith("--")] or [Path(".")]
    return roots + package_roots() if "--packages" in argv else roots


def report(argv, files, find, total):
    """Print a report over the repositories the command line ARGV names (REPORT_ROOTS) and return its exit status,
    2 when one is not a directory, else 0. For each repository FILES gives the relative paths of its files to read,
    and FIND, given a file's path (the repository's joined to it) and its text, the rows found in it, each a
    string. Each row is printed as
    REPOSITORY/PATH:ROW, or with --summary each file's count, then the count of all of them followed by the words
    TOTAL."""
    count = 0
    for root in report_roots(argv):
        if not root.is_dir():
            print(f"{Path(sys.argv[0]).name}: {root} is not a directory", file=sys.stderr)
            return 2
        label = root.resolve().name
        for name in files(root):
            try:
                text = (root / name).read_text(encoding="utf-8")
            except (OSError, UnicodeDecodeError):
                continue
            found = find(root / name, text)
            count += len(found)
            if "--summary" in argv and found:
                print(f"{len(found):6d} {label}/{name}")
            elif "--summary" not in argv:
                for row in found:
                    print(f"{label}/{name}:{row}")
    print(f"{count} {total}")
    return 0
