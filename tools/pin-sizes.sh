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
in_tree :tuatara/tests \
  '(tuatara.tests::write-sizes)' \
  '(format t "~&Pinned ~a~%" (namestring tuatara.tests::*sizes-file*))'
