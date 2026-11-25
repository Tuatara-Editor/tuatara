#!/usr/bin/env bash
# tools/lisp-archives.sh --- Install Quicklisp, and check, fetch and add the archives of the Lisp libraries.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# The libraries are Quicklisp's, each archive named with its SHA-256 in
# tools/lisp-archives.sha256. This is the one place that list is read, by
# tools/ci-setup.sh, tools/release.sh and tools/build-image.sh. Usage:
#   tools/lisp-archives.sh check [QUICKLISP]    every listed archive is there and
#                                               has its hash; stops on any other
#   tools/lisp-archives.sh fetch [QUICKLISP]    download the listed archives over
#                                               HTTPS, kept only with their hash
#   tools/lisp-archives.sh index [QUICKLISP]    download the Quicklisp set's index
#   tools/lisp-archives.sh lock [QUICKLISP]     Quicklisp downloads nothing more,
#                                               so a new library stops a load
#   tools/lisp-archives.sh install QUICKLISP [CACHE]
#                                               make a new Quicklisp in the new
#                                               directory QUICKLISP from the
#                                               pinned client, index and listed
#                                               archives, then check and lock it:
#                                               what a release is compiled with;
#                                               downloads are kept in CACHE and
#                                               taken from it again when their
#                                               hash still matches
#   tools/lisp-archives.sh add LIBRARY [QUICKLISP]
#                                               download LIBRARY over HTTPS from
#                                               the set's index and add its line
#                                               to the list, for review
# QUICKLISP is the directory holding setup.lisp: $QUICKLISP_SETUP's, else
# ~/quicklisp, else ~/.local/share/quicklisp.
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
. "$root/tools/fetch.sh"
list="$root/tools/lisp-archives.sha256"
# the Quicklisp set the list is made from, and the SHA-256 of each of its index files
dist_url=https://beta.quicklisp.org/dist/quicklisp/2026-01-01
index_sha256=(
  "distinfo.txt fc436eb7582c8b69f2ec87d77bdaa4087502328dec09f21c576d78fac8bf5e15"
  "systems.txt cb8309321840887b542fa853dc4fe95f9a4ea2d7ca028c521d32b67d2bdf51c7"
  "releases.txt 9cad59398e760032de0053cafcfdcf16eb437caf1a20d65668a8e3b726485da4")
# the Quicklisp client, as client-info.sexp of its version 2021-02-13 names it
client_url=https://beta.quicklisp.org/client/2021-02-13/quicklisp.tar
client_sha256=a8a3c8c91b51dd185175abad4d7c3999ebb4e2520be5a6cee2127035ac6c87be
setup_url=https://beta.quicklisp.org/client/2021-02-11/setup.lisp
setup_sha256=549fe3e7e0f2669daede98437c99cd60e02c0b8536d3d135c9aa9d346ed951b6
asdf_url=https://beta.quicklisp.org/asdf/3.2.1/asdf.lisp
asdf_sha256=51912f3f7c2c62c204f515d97346d56011528399bbf0a76a123318343ebd8bf0

fail() { echo "tools/lisp-archives.sh: $*" >&2; exit 1; }

quicklisp_dir() {  # the directory given, else where Quicklisp is installed
  if [ -n "${1:-}" ]; then echo "$1"; return; fi
  local setup
  setup="$(quicklisp_setup)" || exit 1
  dirname "$setup"
}

entries() {  # the list's lines: hash, then archive name
  [ -s "$list" ] || fail "tools/lisp-archives.sha256 is missing"
  grep -v '^#' "$list"
}

download() {  # download $1 to $2, kept only when its SHA-256 is $3
  checked_download "$1" "$2" "$3" || exit 1
}

https_url() {  # the archive's address in the index, over HTTPS
  local index="$1" name="$2" url
  url=$(awk -v a="${name%.tgz}" '$6 == a { print $2 }' "$index" | sed 's|^http:|https:|')
  [ -n "$url" ] || fail "$name is not in the Quicklisp set $dist_url"
  echo "$url"
}

check() {
  local archives="$1/dists/quicklisp/archives" sha256 archive bad=0
  while read -r sha256 archive; do
    if [ ! -f "$archives/$archive" ]; then
      echo "tools/lisp-archives.sh: $archive is not in $archives: tools/lisp-archives.sh fetch downloads it" >&2
      bad=1
    elif ! echo "$sha256  $archives/$archive" | sha256sum -c --status; then
      echo "tools/lisp-archives.sh: $archives/$archive is not the archive" \
           "tools/lisp-archives.sha256 lists: not used" >&2
      bad=1
    fi
  done < <(entries)
  [ "$bad" = 0 ] || exit 1
}

index_file() {  # download the set's index file $1 to $2, kept only with its hash
  local entry
  for entry in "${index_sha256[@]}"; do
    if [ "${entry%% *}" = "$1" ]; then download "$dist_url/$1" "$2" "${entry#* }"; return; fi
  done
  fail "$1 is not an index file of the Quicklisp set"
}

index() {  # the set's index files in Quicklisp $1; downloads in $2, else there
  local dist="$1/dists/quicklisp" file
  mkdir -p "$dist/archives" "$1/config" "$1/local-projects"
  for file in distinfo.txt systems.txt releases.txt; do
    if [ -n "${2:-}" ]; then
      index_file "$file" "$2/$file"
      cp "$2/$file" "$dist/$file"
    else
      index_file "$file" "$dist/$file"
    fi
  done
  touch "$dist/enabled.txt"
}

fetch() {  # the listed archives in Quicklisp $1; downloads in $2, else there
  local archives="$1/dists/quicklisp/archives" sha256 archive url
  [ -s "$1/dists/quicklisp/releases.txt" ] || fail "$1 has no Quicklisp index: tools/lisp-archives.sh index makes one"
  mkdir -p "$archives"
  while read -r sha256 archive; do
    url="$(https_url "$1/dists/quicklisp/releases.txt" "$archive")"
    if [ -n "${2:-}" ]; then
      download "$url" "$2/$archive" "$sha256"
      cp "$2/$archive" "$archives/$archive"
    else
      download "$url" "$archives/$archive" "$sha256"
    fi
  done < <(entries)
}

lock() {
  mkdir -p "$1/config"
  # a proxy that answers nothing: anything Quicklisp would still download fails
  echo "http://127.0.0.1:9/" > "$1/config/proxy-url.txt"
}

install() {  # a new Quicklisp in $1 holding only what is pinned here and listed; downloads kept in $2
  local dir="$1" cache="$2"
  [ ! -e "$dir" ] || [ -z "$(ls -A "$dir")" ] || fail "install: $dir is not a new directory"
  mkdir -p "$dir" "$cache"
  download "$client_url" "$cache/quicklisp.tar" "$client_sha256"
  tar -xf "$cache/quicklisp.tar" -C "$dir"
  download "$setup_url" "$cache/setup.lisp" "$setup_sha256"
  cp "$cache/setup.lisp" "$dir/setup.lisp"
  download "$asdf_url" "$cache/asdf.lisp" "$asdf_sha256"
  cp "$cache/asdf.lisp" "$dir/asdf.lisp"
  index "$dir" "$cache"
  fetch "$dir" "$cache"
  check "$dir"
  lock "$dir"
}

add() {
  local name="$1" dir="$2" work line url archive sha256 md5 size
  [ -n "$name" ] || fail "add: which library? (a Quicklisp project name, as releases.txt has it)"
  work="$(mktemp -d)"
  trap "rm -rf '$work'" EXIT
  index_file releases.txt "$work/releases.txt"
  line=$(awk -v p="$name" '$1 == p { print; exit }' "$work/releases.txt")
  [ -n "$line" ] || fail "$name is not in the Quicklisp set $dist_url"
  url=$(echo "$line" | awk '{ print $2 }' | sed 's|^http:|https:|')
  size=$(echo "$line" | awk '{ print $3 }')
  md5=$(echo "$line" | awk '{ print $4 }')
  archive=$(basename "$url")
  if grep -q "  $archive\$" "$list"; then
    echo "$archive is in the list already"
    return
  fi
  https_curl --connect-timeout 30 --max-time 1200 -o "$work/$archive" "$url" || fail "$url could not be downloaded"
  [ "$(stat -c %s "$work/$archive")" = "$size" ] || fail "$archive is not the size the index gives"
  [ "$(md5sum "$work/$archive" | cut -d' ' -f1)" = "$md5" ] || fail "$archive is not the MD5 the index gives"
  sha256=$(sha256sum "$work/$archive" | cut -d' ' -f1)
  mkdir -p "$dir/dists/quicklisp/archives"
  cp "$work/$archive" "$dir/dists/quicklisp/archives/$archive"
  echo "$sha256  $archive" >> "$list"
  echo "Added to tools/lisp-archives.sha256, from $url:"
  echo "$sha256  $archive"
  echo "Review the library (licence, owner, what it loads) before committing the line;"
  echo "its dependencies are not added: run this for each the build reports missing."
}

command="${1:-}"
case "$command" in
  check|fetch|index|lock) shift; dir="$(quicklisp_dir "${1:-}")" || exit 1; "$command" "$dir" ;;
  install)
    [ -n "${2:-}" ] || fail "install: where? (a new directory)"
    if [ -n "${3:-}" ]; then
      install "$2" "$3"
    else
      cache="$(mktemp -d)"
      trap 'rm -rf "$cache"' EXIT
      install "$2" "$cache"
    fi ;;
  add) shift; dir="$(quicklisp_dir "${2:-}")" || exit 1; add "${1:-}" "$dir" ;;
  *) fail "usage: tools/lisp-archives.sh check|fetch|index|lock [QUICKLISP] | install QUICKLISP [CACHE]" \
          "| add LIBRARY [QUICKLISP]" ;;
esac
