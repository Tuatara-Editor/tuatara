#!/usr/bin/env python3
# tools/languages/tools.py --- Bring Helix's language servers and formatters into Tuatara.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
"""Bring Helix's language servers and formatters into Tuatara: which
programs serve and format each language, how each server is started, and the
files that mark a language's project (lisp/lsp/usual-tools.lisp).

Usage: python3 tools/languages/tools.py, from the project root. The Helix sources
are only read: HELIX_SOURCE names them, ../helix beside the project by
default."""
import json, os, tomllib
from common import HELIX, helix_commit, lisp_list, lisp_string

ROOT = os.getcwd()
HELIX_COMMIT = helix_commit()

# Tuatara's names for languages Helix calls otherwise
ALIASES = {'common-lisp': 'lisp', 'dockerfile': 'containerfile', 'sshclientconfig': 'ssh'}

def main():
    """Write lisp/lsp/usual-tools.lisp from Helix's languages.toml."""
    with open(os.path.join(HELIX, 'languages.toml'), 'rb') as f:
        toml = tomllib.load(f)
    servers = []
    for name, server in sorted(toml.get('language-server', {}).items()):
        parts = [lisp_string(name), lisp_list([server['command']] + server.get('args', []))]
        if server.get('config'):
            parts.append(':options ' + lisp_string(json.dumps(server['config'], separators=(',', ':'), ensure_ascii=False)))
        if server.get('environment'):
            parts.append(':environment ' + lisp_list(f'{k}={v}' for k, v in sorted(server['environment'].items())))
        servers.append('(' + ' '.join(parts) + ')')
    languages = []
    for lang in toml.get('language', []):
        names, features = [], []
        for s in lang.get('language-servers', []):
            if isinstance(s, str):
                names.append(s)
            else:
                names.append(s['name'])
                if s.get('only-features') or s.get('except-features'):
                    features.append((s['name'], s.get('only-features'), s.get('except-features')))
        formatter = lang.get('formatter')
        roots = lang.get('roots', [])
        if not (names or formatter or roots):
            continue
        parts = [lisp_string(ALIASES.get(lang['name'], lang['name']))]
        if lang.get('language-id'): parts.append(':language-id ' + lisp_string(lang['language-id']))
        if names: parts.append(':servers ' + lisp_list(names))
        for name, only, exclude in features:
            if only: parts.append(f':only ({lisp_string(name)} . {lisp_list(only)})')
            if exclude: parts.append(f':except ({lisp_string(name)} . {lisp_list(exclude)})')
        if formatter: parts.append(':formatter ' + lisp_list([formatter['command']] + formatter.get('args', [])))
        if roots: parts.append(':roots ' + lisp_list(roots))
        languages.append('(' + ' '.join(parts) + ')')
    header = (';;;; lisp/lsp/usual-tools.lisp --- The language servers and formatters Helix knows, and what marks a project.\n'
              ';;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.\n;;;;\n'
              f';;;; From the languages.toml of Helix (https://github.com/helix-editor/helix) at\n'
              f';;;; commit {HELIX_COMMIT}, MPL-2.0.\n;;;;\n'
              ';;;; Written by tools/languages/tools.py: change that, not this file.\n\n'
              '(in-package #:tuatara.defaults)\n\n')
    with open(os.path.join(ROOT, 'lisp/lsp/usual-tools.lisp'), 'w', encoding='utf-8') as f:
        f.write(header
                + "(defparameter *usual-language-servers*\n  '(" + '\n    '.join(servers) + ')\n'
                + '  "The language servers each language usually has, by name: each (NAME COMMAND\n'
                + '&key OPTIONS ENVIRONMENT), COMMAND a list of the program and its arguments.")\n\n'
                + "(defparameter *usual-language-tools*\n  '(" + '\n    '.join(languages) + ')\n'
                + '  "What each language\'s tools usually are: each (NAME &key SERVERS ONLY EXCEPT\n'
                + 'FORMATTER ROOTS LANGUAGE-ID), :ONLY and :EXCEPT more than once when there are\n'
                + 'several.")\n\n'
                + '(know-language-servers *usual-language-servers*)\n'
                + '(know-language-tools *usual-language-tools*)\n')
    print(f'{len(servers)} servers, {len(languages)} languages')

main()
