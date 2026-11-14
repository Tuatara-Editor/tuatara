#!/usr/bin/env bash
# tools/code-health.sh --- One plain-text report of facts about the code, to start a review from.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Writes, section by section with the counts first: definitions whose bodies
# are alike (the package repositories in ../tuatara-packages/*/ included),
# functions and macros nothing calls, functions and files over the size
# limits, lines past the column limits and names taken from another package
# with two colons (the package repositories included), functions that only
# call another with their own arguments, whether
# the reuse index is current, the definitions this tree adds to main with
# the entries of the index nearest to them and, when the local embedding model
# is there (tools/meaning.py), the pairs of definitions alike by meaning. The
# editor's code is loaded once, for the sections that need it.
# Usage: tools/code-health.sh [REPORT-FILE]
#   Without REPORT-FILE the report goes to standard output.
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
tree="$PWD/"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
# one system load for the sections that need the code
inside sbcl --dynamic-space-size 4GB --non-interactive \
  --load tools/quicklisp.lisp \
  --eval "(push #p\"$tree\" asdf:*central-registry*)" \
  --eval '(ql:quickload :tuatara/tests :silent t)' \
  --eval "(unless (uiop:subpathp tuatara.tests::*reuse-index-file* #p\"$tree\")
            (format *error-output* \"~&The tree loaded is ~a, not $tree.~%\"
                    (asdf:system-source-directory :tuatara))
            (uiop:quit 1))" \
  --eval "(flet ((section (name function)
                   (with-open-file (out (format nil \"$work/~a\" name) :direction :output :if-exists :supersede)
                     (funcall function out))))
            (section \"3-dead\" 'tuatara.tests::dead-code-report)
            (section \"4-sizes\" 'tuatara.tests::sizes-report)
            (section \"6-index\" 'tuatara.tests::reuse-index-report))" >"$work/load.log" 2>&1 ||
  { cat "$work/load.log" >&2; exit 1; }
tools/possible-copies.sh --bodies --packages >"$work/1-bodies"
tools/possible-copies.sh --pass-through >"$work/5-pass"
{ tools/line-check.py --summary --packages | tail -n 1; tools/line-check.py --summary --packages | head -n -1; } >"$work/2-lines"
{ tools/package-reaches.py --summary --packages | tail -n 1; tools/package-reaches.py --summary --packages | head -n -1; } >"$work/8-reaches"
{ tools/possible-copies.sh main || echo "possible-copies.sh main failed"; } >"$work/7-copies.body"
{ echo "$(grep -c '^[^ ]' "$work/7-copies.body") definitions added to main here have near matches in the reuse index"
  cat "$work/7-copies.body"; } >"$work/7-copies"
sections="1-bodies 2-lines 3-dead 4-sizes 5-pass 6-index 7-copies 8-reaches"
tools/possible-copies.sh --alike-meaning --packages >"$work/9-meaning"
if head -n 1 "$work/9-meaning" | grep -q '^no meaning model'; then :; else sections="$sections 9-meaning"; fi
report() {
  echo "Code health of $(git rev-parse --short HEAD) on $(git rev-parse --abbrev-ref HEAD), $(date +%Y-%m-%d)"
  echo
  echo "Counts"
  for section in $sections; do
    echo "  $(head -n 1 "$work/$section")"
  done
  for section in $sections; do
    echo
    echo "== $(head -n 1 "$work/$section")"
    tail -n +2 "$work/$section"
  done
}
if [ -n "${1:-}" ]; then report >"$1"; echo "Wrote $1"; else report; fi
