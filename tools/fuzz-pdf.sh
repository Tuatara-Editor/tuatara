#!/usr/bin/env bash
# tools/fuzz-pdf.sh --- Fuzz the PDF reader.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Reads ROUNDS PDFs (20,000 by default), spoilt from a handful the fuzzer
# makes itself (tools/fuzz-pdf.lisp), from SEED (the time unless given, or
# $FUZZ_SEED), said with the result, so that a failure is made again with
# --seed; each read by every reader of lisp/viewers/pdf-reader.lisp and by
# M-x show-file-metadata's PDF part. Fails (exit 1) on the first error escaping
# a reader, a reader running past DEADLINE seconds (10 unless given) on one
# file, or more than 1 GB of heap left in use; the file that did it is kept
# as build/fuzz/pdf-failed-SEED-ROUND.pdf. The run is held to 30 minutes of
# the processor, a 2 GB heap and an hour in all, at a low priority, its
# caches and logs in build/fuzz/pdf-home/. The log is build/fuzz/pdf.log.
# Usage: tools/fuzz-pdf.sh [-n ROUNDS] [--seed SEED] [--deadline SECONDS]
set -euo pipefail
cd "$(dirname "$0")/.."
rounds=20000 seed="${FUZZ_SEED:-$(date +%s)}" deadline=10
while [ $# -gt 0 ]; do
  case "$1" in
    -n) rounds=$2; shift 2 ;;
    --seed) seed=$2; shift 2 ;;
    --deadline) deadline=$2; shift 2 ;;
    *) echo "Usage: tools/fuzz-pdf.sh [-n ROUNDS] [--seed SEED] [--deadline SECONDS]" >&2; exit 2 ;;
  esac
done
home="$PWD/build/fuzz/pdf-home"
log="$PWD/build/fuzz/pdf.log"
mkdir -p "$home/cache" "$home/state" "$home/data"
. tools/inside.sh
start=$(date +%s)
if inside bash -euo pipefail -c '
     root=$1; home=$2; rounds=$3; seed=$4; deadline=$5
     ulimit -t 1800
     exec nice -n 10 timeout 3600 env FUZZ_ROOT="$root" FUZZ_ROUNDS="$rounds" FUZZ_SEED="$seed" FUZZ_DEADLINE="$deadline" \
       FUZZ_HEAP_MB=1024 XDG_CACHE_HOME="$home/cache" XDG_STATE_HOME="$home/state" XDG_DATA_HOME="$home/data" \
       sbcl --dynamic-space-size 2GB --non-interactive --load "$root/tools/fuzz-pdf.lisp"
   ' fuzz "$PWD" "$home" "$rounds" "$seed" "$deadline" > "$log" 2>&1; then
  status=0
else
  status=$?
fi
said=$(grep -a -E "^(FAILED|[0-9,]+ spoilt)" "$log" | tail -1 || true)
if [ "$status" -ne 0 ] || [ -z "$said" ] || [ "${said#FAILED}" != "$said" ]; then
  echo "pdf reader: FAILED with seed $seed ($([ "$status" -eq 124 ] && echo "no end in an hour" || echo "exit $status")${said:+: $said}), see $log"
  exit 1
fi
echo "pdf reader: passed, $said ($(( $(date +%s) - start )) s)"
