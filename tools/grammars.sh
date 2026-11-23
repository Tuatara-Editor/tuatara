#!/usr/bin/env bash
# tools/grammars.sh --- Fetch and compile the tree-sitter library and grammars.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Fetch the tree-sitter library and the grammars listed in syntax/grammars.tsv,
# each at its pinned commit, and compile them (in the toolbox when there is one; a scanner
# written in C++ with the C++ compiler, the grammar linked with its library) into
# build/grammars/: tuatara-syntax-helper (tree-sitter with syntax/syntax.c and
# syntax/helper.c, the program that runs the grammars for the editor) and
# one NAME.so per grammar, from the directory the manifest's fifth column
# names in its repository when it has one, and build/grammars/licenses/NAME/
# with its licence: its own licence files (LICENSE, COPYING, a LICENSES/
# directory; in that directory, else at the top of its repository), else
# syntax/licenses/NAME.txt for one that has none; that file goes beside its
# own files too when they name a licence without holding its text (an
# SPDX-License-Identifier line, a notice). A grammar without a licence stops
# it, as does one whose licence is named and its text not carried, one with
# a syntax/licenses/NAME.txt it has no need of, and one whose licence the
# manifest's fourth column names otherwise than its licence files do
# (tools/licence-names.sh). A grammar with syntax/patches/NAME.patch is patched
# and its parser generated again with the pinned tree-sitter program first;
# tree-sitter itself is patched with syntax/patches/tree-sitter.patch.
# Each is compiled from a checkout of its pinned commit with nothing changed
# or added (one that has been changed stops it: delete its directory in
# build/grammar-sources/ to fetch it again), and only when what compiling it
# reads (its parser, its scanner, the headers they include, and the
# grammar.js a parser is generated from) hashes as syntax/grammar-inputs.sha256
# records. When a pin moves, tools/check-grammar-sources.sh says what changed
# in that; once it is read, tools/grammars.sh --record NAME... records the new
# hashes (--record --all, every grammar's).
# Nothing is fetched or compiled while Tuatara runs.
# Usage: tools/grammars.sh                 (does nothing when everything is up to date)
#        tools/grammars.sh --record NAME... | --all
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
out="$root/build/grammars"
sources="$root/build/grammar-sources"
stamp="$out/.built-from"
record=""
if [ "${1:-}" = --record ]; then
  shift
  if [ $# -eq 0 ]; then
    echo "Usage: tools/grammars.sh --record NAME... (or --all): name the grammars whose change you have read." >&2
    exit 1
  elif [ "$1" = --all ] && [ $# -eq 1 ]; then
    record="all"
  else
    for name in "$@"; do
      if ! grep -v "^#" "$root/syntax/grammars.tsv" | cut -f1 | grep -qxF -- "$name"; then
        echo "tools/grammars.sh: no grammar called $name in syntax/grammars.tsv: nothing recorded." >&2
        exit 1
      fi
    done
    record="all $*"
  fi
fi
wanted="$(cat "$root/syntax/grammars.tsv" "$root/syntax/grammar-inputs.sha256" "$root/syntax/syntax.c" "$root/syntax/helper.c" "$root/syntax/safe-ctype.h" "$root/syntax/counted-memory.h" "$root"/syntax/patches/*.patch "$root"/syntax/licenses/*.txt "$0" "$root/tools/grammars-inside.sh" "$root/tools/grammar-build.sh" "$root/tools/fetch.sh" "$root/tools/licence-names.sh" 2>/dev/null | sha256sum | cut -c1-16)"
if [ -z "$record" ] && [ -f "$stamp" ] && [ "$(cat "$stamp")" = "$wanted" ]; then exit 0; fi
mkdir -p "$out" "$sources" "$root/build/tools"
. "$root/tools/inside.sh"
inside bash -euo pipefail "$root/tools/grammars-inside.sh" "$root" "$out" "$sources" "$record"
[ -n "$record" ] && exit 0
echo "$wanted" > "$stamp"
