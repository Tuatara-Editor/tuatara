#!/usr/bin/env bash
# tools/fuzz-lisp.sh --- Fuzz the PDF reader, remote frontend and SFTP client.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Reads ROUNDS spoilt inputs (20,000 PDFs, or 200,000 streams of messages or
# answers, unless given), spoilt from a handful the fuzzer makes itself
# (tools/fuzz-KIND.lisp, with what they share in tools/fuzz-common.lisp),
# from SEED (the time unless given, or $FUZZ_SEED), said with the result, so
# that a failure is made again with --seed.
#   pdf     each PDF read by every reader of lisp/viewers/pdf-reader.lisp and
#           by M-x show-file-metadata's PDF part.
#   remote  each stream read as the other end of a remote frontend reads it,
#           one in fifty over a real socket to a frontend or a client with its
#           threads, and hostile rounds of a whole session.
#   sftp    each stream of answers read as the SFTP client reads it.
# Fails (exit 1) on the first error a reader should not let out, a read past
# DEADLINE seconds (10 unless given), or more than 1 GB of heap left in use;
# what did it is kept as build/fuzz/KIND-failed-SEED-ROUND.pdf or .bin. The run
# is held to 30 minutes of the processor, a 2 GB heap and an hour in all, at a
# low priority, its caches and logs in build/fuzz/KIND-home/. The log is
# build/fuzz/KIND.log.
# Usage: tools/fuzz-lisp.sh pdf|remote|sftp [-n ROUNDS] [--seed SEED]
#                           [--deadline SECONDS]
set -euo pipefail
cd "$(dirname "$0")/.."
usage="Usage: tools/fuzz-lisp.sh pdf|remote|sftp [-n ROUNDS] [--seed SEED] [--deadline SECONDS]"
[ $# -gt 0 ] || { echo "$usage" >&2; exit 2; }
kind=$1; shift
case "$kind" in
  pdf) rounds=20000 label="pdf reader" ;;
  remote) rounds=200000 label="remote frontend" ;;
  sftp) rounds=200000 label="sftp" ;;
  *) echo "$usage" >&2; exit 2 ;;
esac
seed="${FUZZ_SEED:-$(date +%s)}" deadline=10
while [ $# -gt 0 ]; do
  case "$1" in
    -n) rounds=$2; shift 2 ;;
    --seed) seed=$2; shift 2 ;;
    --deadline) deadline=$2; shift 2 ;;
    *) echo "$usage" >&2; exit 2 ;;
  esac
done
home="$PWD/build/fuzz/$kind-home"
log="$PWD/build/fuzz/$kind.log"
# the fuzzer fences the rest (tools/fence.lisp); compiled files go to the
# cache from the start
mkdir -p "$home/cache"
. tools/inside.sh
start=$(date +%s)
if inside bash -euo pipefail tools/fuzz-lisp-inside.sh "$PWD" "$home" "$kind" "$rounds" "$seed" "$deadline" > "$log" 2>&1; then
  status=0
else
  status=$?
fi
said=$(grep -a -E "^(FAILED|[0-9,]+ spoilt)" "$log" | tail -1 || true)
if [ "$status" -ne 0 ] || [ -z "$said" ] || [ "${said#FAILED}" != "$said" ]; then
  ending=$([ "$status" -eq 124 ] && echo "no end in an hour" || echo "exit $status")
  echo "$label: FAILED with seed $seed ($ending${said:+: $said}), see $log"
  exit 1
fi
echo "$label: passed, $said ($(( $(date +%s) - start )) s)"
