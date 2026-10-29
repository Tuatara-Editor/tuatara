#!/usr/bin/env bash
# tools/pin-stable-surface.sh --- Pin the stable surface as it is now.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Writes test/api/stable-surface.sexp: the package API, the init file's
# settings and the commands' names, with the version in tuatara.asd. Run at
# each version bump, once the changes it shows are meant (git diff).
# Usage: tools/pin-stable-surface.sh
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
inside sbcl --non-interactive \
  --load tools/quicklisp.lisp \
  --eval '(ql:quickload :tuatara/tests :silent t)' \
  --eval '(tuatara.tests::write-stable-surface)' \
  --eval '(format t "~&Pinned ~a~%" (namestring tuatara.tests::*stable-surface-file*))'
