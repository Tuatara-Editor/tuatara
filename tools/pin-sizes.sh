#!/usr/bin/env bash
# tools/pin-sizes.sh --- Pin the long functions and files of the core as they are now.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Writes test/api/sizes.sexp: each file of the core over 3000 lines and each
# function over 80 lines, with its length. The test (definitions
# sizes-pinned) fails for a new one and for one that grew by more than a
# tenth. Run it after shortening or splitting one, once the changes it
# shows are meant (git diff).
# Usage: tools/pin-sizes.sh
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
# this tree, never another one Quicklisp or ASDF knows of
tree="$PWD/"
inside sbcl --dynamic-space-size 4GB --non-interactive \
  --load tools/quicklisp.lisp \
  --eval "(push #p\"$tree\" asdf:*central-registry*)" \
  --eval '(ql:quickload :tuatara/tests :silent t)' \
  --eval "(unless (uiop:subpathp tuatara.tests::*sizes-file* #p\"$tree\")
            (format *error-output* \"~&The tree loaded is ~a, not $tree: nothing pinned.~%\"
                    (asdf:system-source-directory :tuatara))
            (uiop:quit 1))" \
  --eval '(tuatara.tests::write-sizes)' \
  --eval '(format t "~&Pinned ~a~%" (namestring tuatara.tests::*sizes-file*))'
