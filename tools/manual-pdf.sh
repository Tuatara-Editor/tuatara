#!/usr/bin/env bash
# tools/manual-pdf.sh --- The whole manual as one PDF, by Tuatara's own LaTeX export.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Refresh the generated tables, write the manual as one LaTeX document
# (tools/manual-html.lisp, in the toolbox), and make it into
# build/manual/tuatara-manual.pdf with xelatex here, where the fonts are.
# Usage: tools/manual-pdf.sh
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
command -v xelatex >/dev/null || { echo "No xelatex: install TeX Live" >&2; exit 1; }
tools/manual-tables.sh > /dev/null
tree="$PWD/"
out="$PWD/build/manual"
mkdir -p "$out"
config="$(mktemp -d "$PWD/build/.manual-config.XXXXXX")"
work="$(mktemp -d "$PWD/build/.manual-pdf.XXXXXX")"
trap 'rm -rf "$config" "$work"' EXIT
# the export chooses the fonts: in the toolbox, it is shown the fonts of this
# machine, those xelatex finds, and not the toolbox's own, which may differ
fonts=()
if inside test -d /run/host/usr/share/fonts; then
  cat > "$config/fonts.conf" <<XML
<fontconfig>
  <dir>/run/host/usr/share/fonts</dir>
  <dir>$HOME/.local/share/fonts</dir>
  <dir>$HOME/.fonts</dir>
  <cachedir>$config/font-cache</cachedir>
</fontconfig>
XML
  fonts=(FONTCONFIG_FILE="$config/fonts.conf")
fi
inside env XDG_CONFIG_HOME="$config" "${fonts[@]}" sbcl --dynamic-space-size 4GB --non-interactive \
  --load tools/quicklisp.lisp \
  --eval "(push #p\"$tree\" asdf:*central-registry*)" \
  --eval '(ql:quickload (list :tuatara/defaults :tuatara/frontend-fake :tuatara/terminal) :silent t)' \
  --load tools/manual-html.lisp \
  --eval "(manual-html:run-pdf #p\"$tree\")"
cd "$work"
for pass in 1 2 3; do
  xelatex -interaction=nonstopmode -halt-on-error "$out/tuatara-manual.tex" > xelatex-$pass.log ||
    { tail -30 xelatex-$pass.log >&2; exit 1; }
done
mv -f tuatara-manual.pdf "$out/tuatara-manual.pdf"
echo "$out/tuatara-manual.pdf"
