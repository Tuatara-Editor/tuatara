# tools/languages/common.py --- What the language generators share: where Helix's sources are, and Lisp text.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""What tools/languages/generate.py, languages.py and tools.py share: where the
Helix's sources are (HELIX_SOURCE, else ../helix beside the project,
the project root being where they are run from) and at which commit, and how a
string and a list of strings are written in Lisp."""
import os, subprocess

HELIX = os.environ.get('HELIX_SOURCE') or os.path.join(os.getcwd(), '..', 'helix')

def helix_commit():
    """The first 12 digits of the commit Helix's sources are at."""
    return subprocess.run(['git', '-C', HELIX, 'rev-parse', 'HEAD'], capture_output=True, text=True).stdout.strip()[:12]

def lisp_string(s):
    """S written as a Lisp string, its backslashes and double quotes escaped."""
    return '"' + s.replace('\\', '\\\\').replace('"', '\\"') + '"'

def lisp_list(items):
    """The strings ITEMS written as a Lisp list of strings."""
    return '(' + ' '.join(lisp_string(x) for x in items) + ')'

def lisp_option(key, items, indent=2, width=119):
    """The option KEY (a keyword's name) with ITEMS, strings, as a list, in a
form indented INDENT columns: on one line when it fits WIDTH columns (one
less than a line holds, for a bracket after it), else filled, the lines after
the first set in under the first item."""
    head = f':{key} ('
    words = [lisp_string(x) for x in items]
    one = head + ' '.join(words) + ')'
    if indent + len(one) <= width:
        return one
    lines, line = [], head
    for k, word in enumerate(words):
        tail = ')' if k == len(words) - 1 else ''
        if line.strip() != head.strip() and indent + len(line) + 1 + len(word) + len(tail) > width:
            lines.append(line)
            line = ' ' * len(head) + word
        else:
            line += ('' if line.endswith('(') else ' ') + word
    lines.append(line + ')')
    return ('\n' + ' ' * indent).join(lines)

def language_options(options, indent=2, width=120):
    """The OPTIONS of a language's definition, strings, as lines in a form
indented INDENT columns, each as full as WIDTH columns allow, the last with
room for the closing bracket."""
    lines, line = [], ''
    for option in options:
        if line and indent + len(line) + 1 + len(option) + 1 > width:
            lines.append(line)
            line = option
        else:
            line = line + ' ' + option if line else option
    lines.append(line)
    return ('\n' + ' ' * indent).join(lines)
