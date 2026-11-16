# tools/repositories.py --- Find the package repositories that sit beside this checkout.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Where the package repositories are, for the scripts of tools/ that look at them too."""
from pathlib import Path


def package_roots():
    """The package repositories beside this checkout (../tuatara-packages/tuatara-*/), in name order. Only
    repositories of their own, with a .git directory: the worktrees of their branches, which have a .git file,
    are left out."""
    beside = Path(__file__).resolve().parent.parent.parent / "tuatara-packages"
    return sorted(p for p in beside.glob("tuatara-*") if (p / ".git").is_dir())
