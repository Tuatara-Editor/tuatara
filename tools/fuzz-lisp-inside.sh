#!/usr/bin/env bash
# tools/fuzz-lisp-inside.sh --- Run a Lisp fuzzer, inside the toolbox.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Run by tools/fuzz-lisp.sh, inside the toolbox when there is one, as
# tools/fuzz-lisp-inside.sh ROOT HOME KIND ROUNDS SEED DEADLINE. It runs the
# fuzzer tools/fuzz-KIND.lisp of the tree ROOT, held to its limits, its caches
# in HOME.
set -euo pipefail
root=$1; home=$2; kind=$3; rounds=$4; seed=$5; deadline=$6
ulimit -t 1800
exec nice -n 10 timeout 3600 env FUZZ_ROOT="$root" FUZZ_KIND="$kind" FUZZ_ROUNDS="$rounds" FUZZ_SEED="$seed" \
  FUZZ_DEADLINE="$deadline" FUZZ_HEAP_MB=1024 FUZZ_HOME="$home" XDG_CACHE_HOME="$home/cache" \
  sbcl --dynamic-space-size 2GB --non-interactive \
  --load "$root/tools/fuzz-common.lisp" --load "$root/tools/fuzz-$kind.lisp"
