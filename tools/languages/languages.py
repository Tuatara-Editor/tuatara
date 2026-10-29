#!/usr/bin/env python3
# tools/languages/languages.py --- Read Helix's languages.toml into JSON.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Read Helix's languages.toml (read only) into one JSON table of what
Tuatara needs per language: grammar source, file types, comments, indent, queries."""
import json, os, sys, tomllib
from common import HELIX
with open(os.path.join(HELIX, 'languages.toml'), 'rb') as f:
    toml = tomllib.load(f)
grammars = {g['name']: g for g in toml.get('grammar', [])}
out = []
for lang in toml.get('language', []):
    name = lang['name']
    gname = lang.get('grammar', name)
    g = grammars.get(gname)
    src = g.get('source', {}) if g else {}
    qdir = os.path.join(HELIX, 'runtime', 'queries', name)
    queries = sorted(os.path.splitext(q)[0] for q in os.listdir(qdir) if q.endswith('.scm')) if os.path.isdir(qdir) else []
    inherits = {}
    for q in queries:
        with open(os.path.join(qdir, q + '.scm'), encoding='utf-8') as f:
            first = f.readline()
        if first.startswith('; inherits:'):
            inherits[q] = [x.strip() for x in first.split(':', 1)[1].split(',')]
    ft = []
    for t in lang.get('file-types', []):
        ft.append(t if isinstance(t, str) else {'glob': t['glob']} if 'glob' in t else t)
    def first(tokens):
        # a token, or a list of them (the first is the one comments are made with)
        return tokens if isinstance(tokens, str) or tokens is None else (tokens or [None])[0]
    comment = first(lang.get('comment-token')) or first(lang.get('comment-tokens'))
    block = lang.get('block-comment-tokens')
    if isinstance(block, list): block = block[0] if block else None
    indent = lang.get('indent', {})
    out.append({'name': name, 'scope': lang.get('scope'), 'grammar': gname, 'git': src.get('git'), 'rev': src.get('rev'),
                'subpath': src.get('subpath'), 'path': src.get('path'), 'file_types': ft, 'shebangs': lang.get('shebangs', []),
                'comment': comment, 'block_comment': block, 'indent_unit': indent.get('unit'), 'tab_width': indent.get('tab-width'),
                'injection_regex': lang.get('injection-regex'), 'queries': queries, 'inherits': inherits,
                'language_servers': [s if isinstance(s, str) else s.get('name') for s in lang.get('language-servers', [])],
                'formatter': (lang.get('formatter') or {}).get('command')})
json.dump(out, sys.stdout, indent=1, ensure_ascii=False)
