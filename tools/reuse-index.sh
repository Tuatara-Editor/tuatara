#!/usr/bin/env bash
# tools/reuse-index.sh --- Write the reuse index of the code as it is now.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Writes the test/api/reuse-index.txt of the tree it is in, from that tree's
# code alone: one line for each exported function, macro, generic function
# and variable, with the first sentence of its docstring and its file. The
# pre-commit hook (tools/git-hooks/pre-commit) runs it when Lisp files are
# staged; the test (definitions reuse-index-current) fails when it is stale.
# Usage: tools/reuse-index.sh
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
# compiled into a cache of its own, as the tests are, never the one the editor
# and the developer's own Lisp sessions use
in_tree XDG_CACHE_HOME="$HOME/.cache/tuatara-reuse-index" :tuatara/tests \
  '(tuatara.tests::write-reuse-index)' \
  '(format t "~&Wrote ~a~%" (namestring tuatara.tests::*reuse-index-file*))'
