#!/usr/bin/env python3
# tools/languages/licenses.py --- Fill in the grammars' license column.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Fill in the license column of syntax/grammars.tsv for rows that say 'unknown', from
each fetched repository's license file (build/grammar-sources/NAME/). Run from the root.
A GNU GPL is written with its version and whether a later one may be taken
(GPL-3.0-only, GPL-3.0-or-later), as the repository's package.json, Cargo.toml or
tree-sitter.json names it; the license file alone cannot say, and such a row is
left 'unknown' to be filled in by hand."""
import glob, os, re
ROOT = os.getcwd()
GPL = 'GPL'
def spdx(text):
    """The SPDX id of the license whose full TEXT is given, GPL for any GNU GPL, or None."""
    t = ' '.join(text.split())
    if 'Permission is hereby granted, free of charge' in t: return 'MIT'
    if 'Apache License' in t and 'Version 2.0' in t: return 'Apache-2.0'
    if 'Mozilla Public License Version 2.0' in t or 'Mozilla Public License, v. 2.0' in t: return 'MPL-2.0'
    if 'Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee' in t: return '0BSD' if 'THE SOFTWARE IS PROVIDED "AS IS"' in t and 'copyright notice' not in t.split('Permission')[1][:200] else 'ISC'
    if 'This is free and unencumbered software released into the public domain' in t: return 'Unlicense'
    if 'CC0 1.0' in t or 'Creative Commons Legal Code CC0' in t: return 'CC0-1.0'
    if 'Redistribution and use in source and binary forms' in t:
        return 'BSD-3-Clause' if 'Neither the name' in t or 'names of its contributors' in t else 'BSD-2-Clause'
    if 'GNU GENERAL PUBLIC LICENSE' in t: return GPL
    return None
path = os.path.join(ROOT, 'syntax/grammars.tsv')
lines = open(path, encoding='utf-8').read().split('\n')
unknown = []
for i, line in enumerate(lines):
    cols = line.split('\t')
    if len(cols) >= 4 and not line.startswith('#') and cols[3] == 'unknown':
        found = None
        # the grammar's own directory first (a subpath), then the repository's
        dirs = ([os.path.join(ROOT, 'build/grammar-sources', cols[0], cols[4])] if len(cols) > 4 and cols[4] else []) \
               + [os.path.join(ROOT, 'build/grammar-sources', cols[0])]
        for f in [f for d in dirs for f in sorted(glob.glob(os.path.join(d, '*')))]:
            if re.match(r'(?i)(licen[cs]e|copying)', os.path.basename(f)) and os.path.isfile(f):
                found = spdx(open(f, encoding='utf-8', errors='replace').read())
                if found: break
        if not found or found == GPL:
            # a license named in package.json or Cargo.toml; for a GPL, only one that
            # names its version and whether a later one may be taken
            for p in [os.path.join(d, meta) for d in dirs for meta in ('package.json', 'Cargo.toml', 'tree-sitter.json')]:
                if os.path.exists(p):
                    m = re.search(r'"?license"?\s*[:=]\s*"([^"]+)"', open(p, encoding='utf-8').read())
                    if m and (found != GPL or re.fullmatch(r'GPL-[23]\.0-(only|or-later)', m.group(1))):
                        found = m.group(1); break
            if found == GPL: found = None
        if found:
            cols[3] = found; lines[i] = '\t'.join(cols)
        else:
            unknown.append(cols[0])
open(path, 'w', encoding='utf-8').write('\n'.join(lines))
print('still unknown:', unknown)
