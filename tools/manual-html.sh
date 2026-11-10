#!/usr/bin/env bash
# tools/manual-html.sh --- The manual as web pages, by Tuatara's own Org export.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Refresh the generated tables (tools/manual-tables.sh) and write
# build/manual/: a page for each chapter, index.html and the stylesheet.
# The work is done in tools/manual-html.lisp, in the toolbox.
# Usage: tools/manual-html.sh
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
tools/manual-tables.sh > /dev/null
tree="$PWD/"
# none of your own templates or settings in what is made
mkdir -p build
config="$(mktemp -d "$PWD/build/.manual-config.XXXXXX")"
trap 'rm -rf "$config"' EXIT
inside env XDG_CONFIG_HOME="$config" sbcl --dynamic-space-size 4GB --non-interactive \
  --load tools/quicklisp.lisp \
  --eval "(push #p\"$tree\" asdf:*central-registry*)" \
  --eval '(ql:quickload (list :tuatara/defaults :tuatara/frontend-fake :tuatara/terminal) :silent t)' \
  --load tools/manual-html.lisp \
  --eval "(manual-html:run-html #p\"$tree\")"
