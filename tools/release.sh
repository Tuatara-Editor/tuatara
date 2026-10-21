#!/usr/bin/env bash
# tools/release.sh --- Put together Tuatara as it is given to others.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Put together build/release/tuatara/, Tuatara as it is given to others: the
# program and what it reads beside it, so that the directory runs wherever it
# is copied, nothing compiled there: grammars/ (tree-sitter and the grammars),
# terminal/ (the terminal library, its layout and Ghostty's license), syntax/
# (the grammars' rules), icons/, the license and the README; grammars/licenses/
# holds each grammar's licence (tools/grammars.sh). Beside it,
# build/release/tuatara-sources.tar.zst holds what the release is made from, to
# go with it wherever it is given: Tuatara at the last commit, Ghostty at the
# commit of lisp/terminal/library.pin, and tree-sitter and every grammar at
# the commit of syntax/grammars.tsv, each repository without its history.
# The program is built from the last commit, copied with Quicklisp into a
# folder of its own, /tmp/tuatara-release-build/, so that the places of its
# sources it keeps are that folder's; it is refused when it still names this
# machine's home folder or user, as is the release when any file of it does.
# The grammars and the terminal library are
# built here first and copied there, never compiled again. build/tuatara, the
# program of this checkout, is left as it is. tools/appimage.sh and
# tools/distro-packages.sh make their packages of the release.
# Usage: tools/release.sh
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
. tools/inside.sh
if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
  echo "tools/release.sh: changes not committed are left out of the release" >&2
fi
tools/grammars.sh
tools/terminal-library.sh
# Quicklisp where tools/quicklisp.lisp finds it
quicklisp=""
for setup in "${QUICKLISP_SETUP:-}" "$HOME/quicklisp/setup.lisp" "$HOME/.local/share/quicklisp/setup.lisp"; do
  if [ -n "$setup" ] && [ -f "$setup" ]; then quicklisp="$(dirname "$setup")"; break; fi
done
[ -n "$quicklisp" ] || { echo "tools/release.sh: Quicklisp is not installed: see HACKING.org" >&2; exit 1; }
# a folder the toolbox sees at the same path
work=/tmp/tuatara-release-build
rm -rf "$work"
mkdir -p "$work/src/build" "$work/cache"
git archive HEAD | tar -x -C "$work/src"
cp -a build/grammars build/terminal "$work/src/build/"
cp -a "$quicklisp" "$work/quicklisp"
"$work/src/tools/build-image.sh" \
  QUICKLISP_SETUP="$work/quicklisp/setup.lisp" XDG_CACHE_HOME="$work/cache" \
  CL_SOURCE_REGISTRY="(:source-registry (:tree \"$work/src/\") :ignore-inherited-configuration)" \
  TUATARA_BUILD="$(git rev-parse --short=7 HEAD) nil $(date +%F)"
program="$work/src/build/tuatara"
if names_this_machine "$program"; then
  echo "tools/release.sh: $program names this machine's home folder or user, where shown above: not released" >&2
  exit 1
fi
out="$root/build/release/tuatara"
rm -rf "$out"
mkdir -p "$out/grammars" "$out/terminal" "$out/syntax"
cp "$program" "$out/"
cp build/grammars/*.so "$out/grammars/"
cp -r build/grammars/licenses "$out/grammars/licenses"
cp build/terminal/libghostty-vt.so build/terminal/libtuatara-terminal.so build/terminal/layout.sexp \
   build/terminal/LICENSE-ghostty "$out/terminal/"
# their debugging information names the folders they were built in
strip --strip-debug "$out/terminal/libghostty-vt.so" "$out/terminal/libtuatara-terminal.so"
for kind in rules indents textobjects injections locals tags; do
  cp -r "syntax/$kind" "$out/syntax/$kind"
done
# each grammar's repository, commit and license
cp syntax/grammars.tsv "$out/syntax/"
cp -r data/icons "$out/icons"
cp LICENSE README.org "$out/"
# nothing given to others names this machine, the program checked above
named=0
while IFS= read -r -d '' file; do
  if names_this_machine "$file"; then named=1; fi
done < <(find "$out" -type f ! -path "$out/tuatara" -print0)
if [ "$named" = 1 ]; then
  echo "tools/release.sh: files of $out name this machine's home folder or user, where shown above: not released" >&2
  exit 1
fi
rm -rf "$work"

# the sources, the grammars as tools/grammars.sh left them (patched where
# syntax/patches/ says); a grammar a grammar requires lives in its node_modules/
pack="$(mktemp -d)"
trap 'rm -rf "$pack"' EXIT
top="$pack/tuatara-sources"
mkdir -p "$top/grammars"
git archive --prefix=tuatara-sources/tuatara/ HEAD | tar -x -C "$pack"
git -C build/terminal-sources archive --prefix=tuatara-sources/ghostty/ HEAD | tar -x -C "$pack"
while IFS=$'\t' read -r name url commit licence subpath; do
  [ "$name" = tree-sitter-cli ] && continue
  mkdir -p "$top/grammars/$name"
  tar -C "build/grammar-sources/$name" --exclude=.git --exclude=node_modules -cf - . | tar -x -C "$top/grammars/$name"
done < <(grep -v '^#' syntax/grammars.tsv | grep -v '^[[:space:]]*$')
cat > "$top/README" <<EOF
The sources of Tuatara $(sed -n 's/^ *:version "\(.*\)"$/\1/p' tuatara.asd) ($(git rev-parse --short=7 HEAD)) and of what its
release is built with: tuatara/ (GPL-3.0-or-later), ghostty/ (the terminal
library, MIT), and grammars/, tree-sitter and each grammar at the commit
tuatara/syntax/grammars.tsv names, under the licence it names there, patched
as tuatara/syntax/patches/ says. tuatara/HACKING.org says how to build it.
EOF
tar -C "$pack" -cf - tuatara-sources | zstd -19 -T0 -q -f -o "$root/build/release/tuatara-sources.tar.zst"
echo "$out"
