#!/usr/bin/env bash
# tools/test.sh --- Run the test suite.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Run the test suite (in the toolbox when there is one: tools/inside.sh).
# Usage: tools/test.sh
# TUATARA_TEST_MODAL=off tools/test.sh runs the suite with the modal layer off.
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
tools/grammars.sh
tools/terminal-library.sh
# A 4 GB heap: SBCL's default of 1 GB fills up late in the run, and the tests
# that check for room before large edits then fail though they pass alone.
inside env TUATARA_TEST_MODAL="${TUATARA_TEST_MODAL:-}" sbcl --dynamic-space-size 4GB --non-interactive --load test/run.lisp
