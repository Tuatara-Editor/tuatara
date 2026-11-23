#!/usr/bin/env bash
# tools/fuzz-grammars-inside.sh --- Compile and fuzz a grammar, inside the toolbox.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Run by tools/fuzz-grammars.sh, inside the toolbox when there is one, in one
# of three ways.
#   harness TREE-SITTER OUT CFLAGS...
#     compiles tools/grammar-hostile.c with the sanitizers into OUT.
#   hostile SRC LIB NAME ROUNDS HARNESS ROOT GRAMMAR SUBPATH SEED TEXT...
#     compiles a grammar with the sanitizers and parses the hostile texts.
#   fuzz SRC LIB BUILD CASES CLI NAME ITERATIONS SAMPLES ROOT GRAMMAR SUBPATH
#     compiles a grammar with its assertions and runs tree-sitter's fuzzer on
#     its corpus and samples.
set -euo pipefail
kind=$1; shift
case "$kind" in
  harness)
    ts=$1; out=$2; shift 2
    cc "$@" -std=c11 -D_POSIX_C_SOURCE=200112L -D_DEFAULT_SOURCE -I"$ts/lib/include" -I"$ts/lib/src" \
       "$ts/lib/src/lib.c" tools/grammar-hostile.c -ldl -o "$out/grammar-hostile"
    ;;
  hostile)
    src=$1; lib=$2; name=$3; rounds=$4; harness=$5; root=$6; gname=$7; subpath=$8; seed=$9; shift 9
    . "$root/tools/grammar-build.sh"
    # from its pinned commit as recorded, patched as the build patches it
    prepare_grammar "$root" "$gname" "$subpath" "$root/build/tools/tree-sitter" || exit 3
    compile_grammar "$src" "$lib" --sanitize
    ulimit -t 1800
    export ASAN_OPTIONS=hard_rss_limit_mb=4096:allocator_may_return_null=1:detect_leaks=0:handle_abort=1
    export UBSAN_OPTIONS=halt_on_error=1:print_stacktrace=1
    exec nice -n 10 timeout 3600 bash -c ". \"\$0\"; run_shut_in \"\$@\"" "$root/tools/grammar-build.sh" \
      "$root" "$root/build/fuzz/scratch" "$harness" "$lib" "$name" "$rounds" "$seed" "$@"
    ;;
  fuzz)
    src=$1; lib=$2
    . "$3"
    # from its pinned commit as recorded, patched as the build patches it
    prepare_grammar "$9" "${10}" "${11}" "$5" || exit 3
    compile_grammar "$src" "$lib" --assertions
    # the samples as cases, each expecting the tree the grammar gives it; the
    # text of a case ends before the line end ahead of its divider, so one
    # more goes after the sample
    if [ -n "$8" ]; then
      bar=$(printf "%080d" 0 | tr 0 =) dashes=$(printf "%080d" 0 | tr 0 -)
      for sample in "$8"/*.txt; do
        sample_name=$(basename "$sample" .txt)
        tree=$(run_shut_in "$9" "$4" "$5" parse --no-ranges --lib-path "$lib" --lang-name "$6" "$sample" 2>/dev/null || true)
        { printf "%s\n%s\n%s\n" "$bar" "$sample_name" "$bar"; cat "$sample"; printf "\n%s\n\n%s\n" "$dashes" "$tree"; } \
          > "$4/test/corpus/sample-$sample_name.txt"
      done
    fi
    # a grammar gone wrong can build without end: 8 GB at most, then it
    # fails; and at a low priority, the editor and its tests going first
    ulimit -v 8000000
    nice -n 10 timeout 3600 bash -c ". \"\$0\"; run_shut_in \"\$@\"" "$3" "$9" "$4" \
      "$5" fuzz --lib-path "$lib" --lang-name "$6" --iterations "$7" --edits 3
    ;;
  *) echo "Usage: tools/fuzz-grammars-inside.sh harness|hostile|fuzz ARGUMENTS..." >&2; exit 2 ;;
esac
