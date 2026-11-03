#!/usr/bin/env bash
# tools/pin-stable-surface.sh --- Pin the stable surface as it is now.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Writes the test/api/stable-surface.sexp of the tree it is in, from that
# tree's code alone: the package API, the init file's settings and the
# commands' names, with the version in tuatara.asd. Run at each version
# bump, once the changes it shows are meant (git diff).
# Usage: tools/pin-stable-surface.sh
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
# this tree, never another one Quicklisp or ASDF knows of
tree="$PWD/"
inside sbcl --non-interactive \
  --load tools/quicklisp.lisp \
  --eval "(push #p\"$tree\" asdf:*central-registry*)" \
  --eval '(ql:quickload :tuatara/tests :silent t)' \
  --eval "(unless (uiop:subpathp tuatara.tests::*stable-surface-file* #p\"$tree\")
            (format *error-output* \"~&The tree loaded is ~a, not $tree: nothing pinned.~%\"
                    (asdf:system-source-directory :tuatara))
            (uiop:quit 1))" \
  --eval '(tuatara.tests::write-stable-surface)' \
  --eval '(format t "~&Pinned ~a~%" (namestring tuatara.tests::*stable-surface-file*))'
