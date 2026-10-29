# tools/inside.sh --- Run a command where Tuatara is built, say when build/tuatara is to be built again, and find this machine's names in a program: sourced by the other scripts.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
#
# inside COMMAND...  runs COMMAND in the toolbox container named by
# $TUATARA_CONTAINER (tuatara by default, made by tools/toolbox-setup.sh) when
# that container exists and this is not already inside a container; else here,
# with SBCL, the C compilers and the libraries installed on this system.
inside() {
  local name="${TUATARA_CONTAINER:-tuatara}"
  # grep -c reads to the end: -q would stop at the first match, and under the
  # callers' pipefail toolbox's broken pipe would fail the test
  if [ ! -e /run/.containerenv ] && command -v toolbox >/dev/null &&
     [ "$(toolbox list --containers 2>/dev/null | grep -c "[[:space:]]${name}[[:space:]]")" != 0 ]; then
    toolbox run --container "$name" "$@"
  else
    "$@"
  fi
}
# image_stale ROOT: true when ROOT/build/tuatara is not there, or older than a
# source it is made from: the Lisp, the grammars' list and patches and syntax.c,
# and what data/ puts into it (the starter init file, the LaTeX templates)
image_stale() {
  local image="$1/build/tuatara"
  [ ! -x "$image" ] ||
    [ -n "$(find "$1/tuatara.asd" "$1/src" "$1/lisp" "$1/frontends" "$1/syntax" "$1/data" -newer "$image" \
              \( -name '*.lisp' -o -name '*.asd' -o -name '*.scm' -o -name '*.c' -o -name '*.tsv' -o -name '*.patch' -o -name '*.tex' -o -name '*.txt' \) -print -quit)" ]
}
# names_this_machine FILE: true, and where (the octet each is found at, with
# the text around it), when FILE names this machine's home folder ($HOME, or
# the folder it leads to) or the account's name ($USER); false when it names neither
names_this_machine() {
  local home real user found=1
  home="${HOME%/}"; real="$(realpath -m "$home")"; user="${USER:-$(id -un)}"
  for name in "$home" "$real" "$user"; do
    [ -n "$name" ] || continue
    if [ "$(grep -a -c -F -- "$name" "$1")" != 0 ]; then
      echo "$1 names $name $(grep -a -c -F -- "$name" "$1") times, at:" >&2
      grep -a -o -b -F -- "$name" "$1" | head -n 10 | while IFS=: read -r at _; do
        echo "  $at: $(dd if="$1" bs=1 skip=$((at > 40 ? at - 40 : 0)) count=120 status=none | tr -c '[:print:]' '.')" >&2
      done
      found=0
    fi
  done
  return $found
}
