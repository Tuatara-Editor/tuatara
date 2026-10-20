# tools/grammar-build.sh --- How a tree-sitter grammar is compiled: sourced by tools/grammars.sh and tools/fuzz-grammars.sh.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
#
# The copies of the array.h of tree-sitter in many grammars read one struct
# type as another, which GCC compiles wrongly unless told not to assume that
# never happens; the assertions of a grammar are left out, as a failed one
# would end the editor; and its character classes answer for any code point
# (syntax/safe-ctype.h). A scanner written in C++ (the YAML and Vue grammars
# have one) is compiled apart, with the same care but for the character
# classes: the headers of C++ take those back, and such scanners ask the wide
# ones (iswspace), which answer for any code point; its names are kept inside
# the grammar, as the same ones (Scanner::scan) are in several.
grammar_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
grammar_cflags=(-O2 -fPIC -shared -std=c11 -fno-strict-aliasing -DNDEBUG -include "$grammar_root/syntax/safe-ctype.h")
grammar_cxxflags=(-O2 -fPIC -fno-strict-aliasing -DNDEBUG -fvisibility=hidden)

# compile_grammar SRC LIB [--assertions]: compile the grammar whose parser.c
# (and scanner.c or scanner.cc) are in the directory SRC into the shared
# library LIB; with --assertions, its assertions are kept, as fuzzing wants
compile_grammar() {
  local src="$1" lib="$2" cflags=() cxxflags=() flag
  for flag in "${grammar_cflags[@]}"; do
    [ "${3:-}" = --assertions ] && [ "$flag" = -DNDEBUG ] || cflags+=("$flag")
  done
  for flag in "${grammar_cxxflags[@]}"; do
    [ "${3:-}" = --assertions ] && [ "$flag" = -DNDEBUG ] || cxxflags+=("$flag")
  done
  if [ -f "$src/scanner.cc" ]; then
    cc "${cflags[@]}" -I"$src" -c "$src/parser.c" -o "$lib.parser.o"
    c++ "${cxxflags[@]}" -I"$src" -c "$src/scanner.cc" -o "$lib.scanner.o"
    c++ -shared "$lib.parser.o" "$lib.scanner.o" -o "$lib"
    rm -f "$lib.parser.o" "$lib.scanner.o"
  else
    local files=("$src/parser.c")
    [ -f "$src/scanner.c" ] && files+=("$src/scanner.c")
    cc "${cflags[@]}" -I"$src" "${files[@]}" -o "$lib"
  fi
}
