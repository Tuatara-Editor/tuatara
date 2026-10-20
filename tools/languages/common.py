# tools/languages/common.py --- What the language generators share: where the Helix fork is, and Lisp text.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""What tools/languages/generate.py, languages.py and tools.py share: where the
Helix fork's sources are (HELIX_SOURCE, else ../helix_editor beside the project,
the project root being where they are run from) and at which commit, and how a
string and a list of strings are written in Lisp."""
import os, subprocess

HELIX = os.environ.get('HELIX_SOURCE') or os.path.join(os.getcwd(), '..', 'helix_editor')

def helix_commit():
    """The first 12 digits of the commit the Helix fork's sources are at."""
    return subprocess.run(['git', '-C', HELIX, 'rev-parse', 'HEAD'], capture_output=True, text=True).stdout.strip()[:12]

def lisp_string(s):
    """S written as a Lisp string, its backslashes and double quotes escaped."""
    return '"' + s.replace('\\', '\\\\').replace('"', '\\"') + '"'

def lisp_list(items):
    """The strings ITEMS written as a Lisp list of strings."""
    return '(' + ' '.join(lisp_string(x) for x in items) + ')'
