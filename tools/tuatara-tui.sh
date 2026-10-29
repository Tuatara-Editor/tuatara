#!/usr/bin/env bash
# tools/tuatara-tui.sh --- Run Tuatara in this terminal.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Run Tuatara in this terminal. Usage: tools/tuatara-tui.sh [FILE...]
# It runs build/tuatara, the whole editor saved as one executable, which starts
# at once; first it builds that (tools/build-image.sh) when it is missing or older
# than the sources. It runs on the host when the host has the libraries it
# needs, else in the toolbox (tools/inside.sh). File names are taken from where you are.
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
image="$root/build/tuatara"
. "$root/tools/inside.sh"
if image_stale "$root"; then
  echo "Building Tuatara (after any change to its sources)..." >&2
  (cd "$root" && tools/build-image.sh) >&2
fi
# grep -c reads to the end: with -q it would stop at the first match and, under
# pipefail, the writer's broken pipe would fail the test (and always use the toolbox)
if [ "$(ldd "$image" 2>/dev/null | grep -c "not found")" = 0 ] && [ "$(ldconfig -p 2>/dev/null | grep -c "libncursesw.so.6")" != 0 ]; then
  exec "$image" "$@"
else
  inside "$image" "$@"
fi
