#!/usr/bin/env bash
# tools/fuzz-remote.sh --- Fuzz the messages of a remote frontend and its client.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Reads ROUNDS streams of messages (200,000 by default), spoilt from what an
# editor and a client say to each other (tools/fuzz-remote.lisp), from SEED
# (the time unless given, or $FUZZ_SEED), said with the result, so that a
# failure is made again with --seed; each read as the other end reads it,
# one in fifty over a real socket to a remote frontend or a client with its
# threads. Fails (exit 1) on the first error other than a malformed message,
# a stream read past DEADLINE seconds (10 unless given), a connection not
# ended, or more than 1 GB of heap left in use; the octets that did it are
# kept as build/fuzz/remote-failed-SEED-ROUND.bin. The run is held to 30
# minutes of the processor, a 2 GB heap and an hour in all, at a low
# priority, its caches and logs in build/fuzz/remote-home/. The log is
# build/fuzz/remote.log.
# Usage: tools/fuzz-remote.sh [-n ROUNDS] [--seed SEED] [--deadline SECONDS]
set -euo pipefail
cd "$(dirname "$0")/.."
rounds=200000 seed="${FUZZ_SEED:-$(date +%s)}" deadline=10
while [ $# -gt 0 ]; do
  case "$1" in
    -n) rounds=$2; shift 2 ;;
    --seed) seed=$2; shift 2 ;;
    --deadline) deadline=$2; shift 2 ;;
    *) echo "Usage: tools/fuzz-remote.sh [-n ROUNDS] [--seed SEED] [--deadline SECONDS]" >&2; exit 2 ;;
  esac
done
home="$PWD/build/fuzz/remote-home"
log="$PWD/build/fuzz/remote.log"
mkdir -p "$home/cache" "$home/state" "$home/data"
. tools/inside.sh
start=$(date +%s)
if inside bash -euo pipefail -c '
     root=$1; home=$2; rounds=$3; seed=$4; deadline=$5
     ulimit -t 1800
     exec nice -n 10 timeout 3600 env FUZZ_ROOT="$root" FUZZ_ROUNDS="$rounds" FUZZ_SEED="$seed" FUZZ_DEADLINE="$deadline" \
       FUZZ_HEAP_MB=1024 XDG_CACHE_HOME="$home/cache" XDG_STATE_HOME="$home/state" XDG_DATA_HOME="$home/data" \
       sbcl --dynamic-space-size 2GB --non-interactive --load "$root/tools/fuzz-remote.lisp"
   ' fuzz "$PWD" "$home" "$rounds" "$seed" "$deadline" > "$log" 2>&1; then
  status=0
else
  status=$?
fi
said=$(grep -a -E "^(FAILED|[0-9,]+ spoilt)" "$log" | tail -1 || true)
if [ "$status" -ne 0 ] || [ -z "$said" ] || [ "${said#FAILED}" != "$said" ]; then
  echo "remote frontend: FAILED with seed $seed ($([ "$status" -eq 124 ] && echo "no end in an hour" || echo "exit $status")${said:+: $said}), see $log"
  exit 1
fi
echo "remote frontend: passed, $said ($(( $(date +%s) - start )) s)"
