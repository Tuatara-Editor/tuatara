#!/usr/bin/env python3
# tools/languages/licenses.py --- Fill in the grammars' license column.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Fill in the license column of syntax/grammars.tsv for rows that say 'unknown', from
each fetched repository's license file (build/grammar-sources/NAME/), its licences named
by tools/licence-names.sh. Run from the root. A GNU license (GPL, LGPL, AGPL) is written
with its version and whether a later one may be taken (GPL-3.0-only, GPL-3.0-or-later),
as the repository's package.json, Cargo.toml or tree-sitter.json names it; the license
file alone cannot say, and such a row is left 'unknown' to be filled in by hand."""
import glob, os, re, subprocess
ROOT = os.getcwd()
NAMES = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'licence-names.sh')
GNU = re.compile(r'(A|L)?GPL-[0-9.]+')
def names_in(path):
    """The SPDX names of the licences the file at PATH holds, as tools/licence-names.sh
    names them: a GNU licence by its version alone (GPL-3.0)."""
    return subprocess.run(['bash', NAMES, path], capture_output=True, text=True, check=True).stdout.split()
path = os.path.join(ROOT, 'syntax/grammars.tsv')
lines = open(path, encoding='utf-8').read().split('\n')
unknown = []
for i, line in enumerate(lines):
    cols = line.split('\t')
    if len(cols) >= 4 and not line.startswith('#') and cols[3] == 'unknown':
        names = []
        # the grammar's own directory first (a subpath), then the repository's
        dirs = ([os.path.join(ROOT, 'build/grammar-sources', cols[0], cols[4])] if len(cols) > 4 and cols[4] else []) \
               + [os.path.join(ROOT, 'build/grammar-sources', cols[0])]
        for f in [f for d in dirs for f in sorted(glob.glob(os.path.join(d, '*')))]:
            if re.match(r'(?i)(licen[cs]e|copying)', os.path.basename(f)) and os.path.isfile(f):
                names = names_in(f)
                if names: break
        gnu = [name for name in names if GNU.fullmatch(name)]
        found = ' AND '.join(names) if names and not gnu else None
        if not names or gnu:
            # a license named in package.json or Cargo.toml; for a GNU license, only one
            # that names its version and whether a later one may be taken
            for p in [os.path.join(d, meta) for d in dirs for meta in ('package.json', 'Cargo.toml', 'tree-sitter.json')]:
                if os.path.exists(p):
                    m = re.search(r'"?license"?\s*[:=]\s*"([^"]+)"', open(p, encoding='utf-8').read())
                    if m and not gnu:
                        found = m.group(1); break
                    if m and len(gnu) == 1 and re.fullmatch(re.escape(gnu[0]) + r'-(only|or-later)', m.group(1)):
                        found = ' AND '.join(m.group(1) if name in gnu else name for name in names); break
        if found:
            found = found.replace(' AND LLVM-exception', ' WITH LLVM-exception')
            cols[3] = found; lines[i] = '\t'.join(cols)
        else:
            unknown.append(cols[0])
open(path, 'w', encoding='utf-8').write('\n'.join(lines))
print('still unknown:', unknown)
