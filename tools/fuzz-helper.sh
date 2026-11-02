#!/usr/bin/env bash
# tools/fuzz-helper.sh --- Fuzz the syntax helper's requests.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Builds the program that parses for the editor (syntax/helper.c, with
# tree-sitter as tools/grammars.sh patched it) with AddressSanitizer and
# UndefinedBehaviorSanitizer, and sends it ROUNDS requests (20,000 by default)
# of every kind it takes and some it does not, well made and spoilt
# (tools/fuzz-helper.py), from SEED (the time unless given, or $FUZZ_SEED),
# said with the result, so that a failure is made again with --seed. Fails
# (exit 1) on the first report of a sanitizer, the helper ending, or a reply
# that answers another request. The run is held to 30 minutes of the
# processor, 4 GB of memory as the sanitizer counts it and an hour in all, at
# a low priority, in the container $FUZZ_CONTAINER (tuatara-langs unless
# given), whose compiler has the sanitizers' libraries. Needs tools/grammars.sh
# run first. The log is build/fuzz/helper.log.
# Usage: tools/fuzz-helper.sh [-n ROUNDS] [--seed SEED]
set -euo pipefail
cd "$(dirname "$0")/.."
rounds=20000 seed="${FUZZ_SEED:-$(date +%s)}"
while [ $# -gt 0 ]; do
  case "$1" in
    -n) rounds=$2; shift 2 ;;
    --seed) seed=$2; shift 2 ;;
    *) echo "Usage: tools/fuzz-helper.sh [-n ROUNDS] [--seed SEED]" >&2; exit 2 ;;
  esac
done
ts="$PWD/build/grammar-sources/tree-sitter"
if [ ! -d "$ts/lib/src" ] || [ ! -d build/grammars ]; then
  echo "tools/fuzz-helper.sh: no tree-sitter sources or grammars: run tools/grammars.sh first." >&2
  exit 2
fi
work="$PWD/build/fuzz/helper"
log="$PWD/build/fuzz/helper.log"
mkdir -p "$work"
: > "$log"
. tools/inside.sh
export TUATARA_CONTAINER="${FUZZ_CONTAINER:-tuatara-langs}"
inside bash -euo pipefail -c '
  ts=$1; root=$2; out=$3
  cc -g -O1 -fno-omit-frame-pointer -fsanitize=address,undefined -fno-sanitize-recover=undefined \
     -std=c11 -D_POSIX_C_SOURCE=200112L -D_DEFAULT_SOURCE -I"$ts/lib/include" -I"$ts/lib/src" \
     "$ts/lib/src/lib.c" "$root/syntax/syntax.c" "$root/syntax/helper.c" \
     -rdynamic -lpthread -ldl -o "$out/tuatara-syntax-helper"
' build "$ts" "$PWD" "$work"
start=$(date +%s)
if inside bash -euo pipefail -c '
     helper=$1; grammars=$2; rounds=$3; seed=$4; log=$5; fuzzer=$6
     ulimit -t 1800
     export ASAN_OPTIONS=hard_rss_limit_mb=4096:allocator_may_return_null=1:detect_leaks=0:handle_abort=1
     export UBSAN_OPTIONS=halt_on_error=1:print_stacktrace=1
     exec nice -n 10 timeout 3600 python3 -I "$fuzzer" "$helper" "$grammars" "$rounds" "$seed" "$log"
   ' fuzz "$work/tuatara-syntax-helper" "$PWD/build/grammars" "$rounds" "$seed" "$log" "$PWD/tools/fuzz-helper.py" \
   >> "$log.summary" 2>&1; then
  status=0
else
  status=$?
fi
said=$(tail -1 "$log.summary" 2>/dev/null || true)
rm -f "$log.summary"
why=$(grep -a -m1 -E "ERROR: AddressSanitizer|runtime error:|SUMMARY:" "$log" || true)
if [ "$status" -ne 0 ] || [ -n "$why" ]; then
  echo "helper: FAILED with seed $seed ($([ "$status" -eq 124 ] && echo "no end in an hour" || echo "exit $status")${why:+: $why}; $said), see $log"
  exit 1
fi
echo "helper: passed, $said ($(( $(date +%s) - start )) s)"
