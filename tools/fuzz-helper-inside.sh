#!/usr/bin/env bash
# tools/fuzz-helper-inside.sh --- Build and fuzz the syntax helper, inside the toolbox.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Run by tools/fuzz-helper.sh, inside the container it names, in one of two
# ways.
#   build TREE-SITTER ROOT OUT
#     builds the helper with the sanitizers into OUT.
#   fuzz HELPER GRAMMARS ROUNDS SEED LOG FUZZER CONNECTIONS
#     sends HELPER the fuzzer's requests, held to the run's limits.
set -euo pipefail
kind=$1; shift
case "$kind" in
  build)
    ts=$1; root=$2; out=$3
    . "$root/tools/grammar-build.sh"
    compile_helper "$root" "$ts" "$out/tuatara-syntax-helper" -g -O1 -fno-omit-frame-pointer \
      -fsanitize=address,undefined -fno-sanitize-recover=undefined
    ;;
  fuzz)
    helper=$1; grammars=$2; rounds=$3; seed=$4; log=$5; fuzzer=$6; connections=$7
    ulimit -t 1800
    export ASAN_OPTIONS=hard_rss_limit_mb=4096:allocator_may_return_null=1:detect_leaks=0:handle_abort=1
    export UBSAN_OPTIONS=halt_on_error=1:print_stacktrace=1
    exec nice -n 10 timeout 3600 python3 -I "$fuzzer" "$helper" "$grammars" "$rounds" "$seed" "$log" "$connections"
    ;;
  *) echo "Usage: tools/fuzz-helper-inside.sh build|fuzz ARGUMENTS..." >&2; exit 2 ;;
esac
