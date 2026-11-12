#!/usr/bin/env bash
# tools/pin-kept-files.sh --- Pin the files Tuatara keeps as they are now.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Writes test/api/kept-files.sexp: each list kept between sessions with its
# class and the version of its format (a list of trust with :trust and the
# number of its lines' fields too), and the other files of Tuatara's
# directories with theirs, with the version in tuatara.asd; and into
# test/kept-files/VERSION/ the file of each list as this release writes it,
# none over a file there. Run at each version bump, beside
# tools/pin-stable-surface.sh, once the changes it shows are meant (git diff).
# Usage: tools/pin-kept-files.sh
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
# this tree, never another one Quicklisp or ASDF knows of
tree="$PWD/"
inside sbcl --non-interactive \
  --load tools/quicklisp.lisp \
  --eval "(push #p\"$tree\" asdf:*central-registry*)" \
  --eval '(ql:quickload :tuatara/tests :silent t)' \
  --eval "(unless (uiop:subpathp tuatara.tests::*kept-files-file* #p\"$tree\")
            (format *error-output* \"~&The tree loaded is ~a, not $tree: nothing pinned.~%\"
                    (asdf:system-source-directory :tuatara))
            (uiop:quit 1))" \
  --eval '(format t "~&Samples written: ~{~a~^, ~}~%" (tuatara.tests::write-kept-files))' \
  --eval '(format t "~&Pinned ~a~%" (namestring tuatara.tests::*kept-files-file*))'
