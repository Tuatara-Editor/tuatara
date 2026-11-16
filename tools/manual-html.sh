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
# none of your own templates or settings in what is made: tools/manual-html.lisp fences them off
in_tree '(list :tuatara/defaults :tuatara/frontend-fake :tuatara/terminal)' \
  '(load "tools/manual-html.lisp")' \
  "(manual-html:run-html #p\"$PWD/\")"
