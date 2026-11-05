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
# this tree, never another one Quicklisp or ASDF knows of
tree="$PWD/"
inside sbcl --dynamic-space-size 4GB --non-interactive \
  --load tools/quicklisp.lisp \
  --eval "(push #p\"$tree\" asdf:*central-registry*)" \
  --eval '(ql:quickload :tuatara/tests :silent t)' \
  --eval "(unless (uiop:subpathp tuatara.tests::*reuse-index-file* #p\"$tree\")
            (format *error-output* \"~&The tree loaded is ~a, not $tree: nothing written.~%\"
                    (asdf:system-source-directory :tuatara))
            (uiop:quit 1))" \
  --eval '(tuatara.tests::write-reuse-index)' \
  --eval '(format t "~&Wrote ~a~%" (namestring tuatara.tests::*reuse-index-file*))'
