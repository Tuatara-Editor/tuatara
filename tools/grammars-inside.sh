#!/usr/bin/env bash
# tools/grammars-inside.sh --- Fetch, check and compile the grammars, inside the toolbox.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Run by tools/grammars.sh, inside the toolbox when there is one, as
# tools/grammars-inside.sh ROOT OUT SOURCES RECORD. It does what that script
# says, in the tree ROOT, into OUT, from the checkouts in SOURCES. RECORD is
# empty, or "all" and the grammars whose hashes are recorded.
set -euo pipefail
root="$1"; out="$2"; sources="$3"; record="$4"
. "$root/tools/grammar-build.sh"
fetch() { fetch_grammar "$sources/$1" "$2" "$3"; }  # fetch repository $2 at commit $3 into $sources/$1
cli="$root/build/tools/tree-sitter"
get_cli() {  # download the tree-sitter program from $1 unless it is there, checked against SHA-256 $2
  if [ ! -x "$cli" ] || [ "$(cat "$cli.sha256" 2>/dev/null)" != "$2" ]; then
    checked_download "$1" "$cli.gz" "$2" || exit 1
    gunzip -f "$cli.gz"; chmod +x "$cli"
    echo "$2" > "$cli.sha256"
  fi
}
# first everything is fetched, the grammars before what they require
while IFS=$'\t' read -r name url commit license subpath; do
  echo "  $name ($license) at ${commit:0:12}" >&2
  case "$name" in
    tree-sitter-cli) get_cli "$url" "${commit#sha256:}" ;;
    *) fetch "$name" "$url" "$commit" ;;
  esac
done < <(grammar_manifest)
# then the licences, to go with the release: the licence files of the grammar
# itself, else those at the top of its repository, else the text in
# syntax/licenses/ for a repository that has none or only names its licence;
# nothing compiled without one, with one named and its text not carried, nor
# with one the manifest names otherwise
rm -rf "$out/licenses"; unlicensed=""; misnamed=""; untexted=""; needless=""
while IFS=$'\t' read -r name url commit license subpath; do
  [ "$name" = tree-sitter-cli ] && continue
  dir="$sources/$name"; kept="$out/licenses/$name"; found=""
  for place in "$dir${subpath:+/$subpath}" "$dir"; do
    # LICENSE, COPYING and the like, and the texts of a LICENSES/ directory (REUSE)
    found=$({ find "$place" -maxdepth 1 -type f -iregex ".*/\(licen[cs]e\|copying\|unlicense\)[^/]*"
              if [ -d "$place/LICENSES" ]; then find "$place/LICENSES" -maxdepth 1 -type f; fi; } | sort)
    [ -n "$found" ] && break
  done
  text="$root/syntax/licenses/$name.txt"
  if [ -n "$found" ]; then
    mkdir -p "$kept"
    while IFS= read -r file; do cp "$file" "$kept/"; done <<< "$found"
    mapfile -t files <<< "$found"
    if [ -f "$text" ]; then
      if [ -n "$(bash "$root/tools/licence-names.sh" --untexted "${files[@]}")" ]; then
        cp "$text" "$kept/LICENSE-TEXT"; found="$found"$'\n'"$text"
      else
        needless="$needless $name"
      fi
    fi
  elif [ -f "$text" ]; then
    mkdir -p "$kept"; cp "$root/syntax/licenses/$name.txt" "$kept/LICENSE"
    found="$root/syntax/licenses/$name.txt"
  fi
  if [ -n "$found" ]; then
    mapfile -t files <<< "$found"
    if ! said=$(bash "$root/tools/licence-names.sh" --agrees "$license" "${files[@]}" 2>&1); then
      echo "  $name: $said" >&2
      misnamed="$misnamed $name"
    fi
    without="$(bash "$root/tools/licence-names.sh" --untexted "${files[@]}")"
    if [ -n "$without" ]; then
      echo "  $name: its files name $(echo $without) without the text" >&2
      untexted="$untexted $name"
    fi
  else
    unlicensed="$unlicensed $name"
  fi
done < <(grammar_manifest)
if [ -n "$unlicensed" ]; then
  echo "tools/grammars.sh: no licence for:$unlicensed. Its text goes in syntax/licenses/NAME.txt, or the grammar is left out." >&2
  exit 1
fi
if [ -n "$untexted" ]; then
  echo "tools/grammars.sh: the licence files of$untexted name a licence without its text, as shown above. The text goes in syntax/licenses/NAME.txt, carried beside them." >&2
  exit 1
fi
if [ -n "$needless" ]; then
  echo "tools/grammars.sh: syntax/licenses/ has a text for$needless, whose own licence files hold the texts of their licences: delete it." >&2
  exit 1
fi
if [ -n "$misnamed" ]; then
  echo "tools/grammars.sh: syntax/grammars.tsv names the licence of$misnamed otherwise than its licence files, as shown above. The fourth column is made to say what the files grant, or the grammar is left out." >&2
  exit 1
fi
# then the library and the grammars are compiled, each grammar as
# tools/grammar-build.sh says, from its pinned commit with nothing changed and
# only when what it compiles is what was recorded
recorded="$root/syntax/grammar-inputs.sha256"
pristine() { pristine_grammar "$sources/$1"; }
inputs_of() { grammar_inputs_of "$root" "$1" "$2"; }  # the SHA-256 of what compiling $1 (subdirectory $2) reads
checked() {  # stop unless the checkout of $1 (subpath $2) is its pinned commit as fetched, hashing as recorded
  grammar_checked "$1" "$sources/$1" "$recorded" "$(inputs_of "$1" "$2")" || exit 1
}
if [ -n "$record" ]; then
  # --record: the hashes of those named (all when none), written in the order of the manifest
  set -- $record; shift
  lines="$(while IFS=$'\t' read -r name url commit license subpath; do
    [ "$name" = tree-sitter-cli ] && continue
    wanted=1
    if [ $# -gt 0 ]; then wanted=0; for one in "$@"; do [ "$one" = "$name" ] && wanted=1; done; fi
    if [ "$wanted" = 1 ]; then
      pristine "$name"
      echo "$(inputs_of "$name" "$subpath")  $name"
    else
      grep "  $name\$" "$recorded" || true
    fi
  done < <(grammar_manifest))"
  { grep "^#" "$recorded"; echo "$lines"; } > "$recorded.new"
  mv "$recorded.new" "$recorded"
  echo "Recorded in $recorded: ${*:-every grammar}" >&2
  exit 0
fi
while IFS=$'\t' read -r name url commit license subpath; do
  dir="$sources/$name"
  # a grammar in a directory of its repository (tree-sitter-typescript/tsx)
  src="$dir/${subpath:+$subpath/}src"
  case "$name" in
    tree-sitter-cli|*/*) ;;
    tree-sitter)
      # with syntax/patches/tree-sitter.patch applied, as a grammar is
      pristine "$name"
      checked "$name" ""
      [ -f "$root/syntax/patches/tree-sitter.patch" ] &&
        https_git -C "$dir" apply "$root/syntax/patches/tree-sitter.patch"
      # without its assertions, as the grammars are: one that fails would end
      # the program, and the patch makes the parser check what a grammar gives
      # it instead (a parse that meets a wrong value fails); what names a
      # file names it from the top of the sources, not from this machine;
      # its names seen by the grammars it loads, which take their memory
      # through it (syntax/counted-memory.h)
      compile_helper "$root" "$dir" "$out/tuatara-syntax-helper" -O2 -DNDEBUG \
         -ffile-prefix-map="$root/=" -ffile-prefix-map="$dir/=tree-sitter/"
      rm -f "$out/libtuatara-syntax.so" ;;
    *)
      patch="$root/syntax/patches/$name.patch"
      made="$commit ${grammar_cflags[*]} $(cat "$root/syntax/safe-ctype.h" "$root/syntax/counted-memory.h" | sha256sum | cut -c1-16)"
      [ -f "$patch" ] && made="$made $(sha256sum < "$patch" | cut -c1-16)"
      [ -f "$src/scanner.cc" ] && made="$made c++ ${grammar_cxxflags[*]}"
      if [ ! -f "$out/$name.so" ] || [ "$(cat "$out/$name.commit" 2>/dev/null)" != "$made" ]; then
        prepare_grammar "$root" "$name" "$subpath" "$cli" || exit 1
        compile_grammar "$src" "$out/$name.so"
        echo "$made" > "$out/$name.commit"
      fi ;;
  esac
done < <(grammar_manifest)
# a grammar left out of the manifest is left out of what is built
for made in "$out"/*.so; do
  name="$(basename "$made" .so)"
  grammar_manifest | cut -f1 | grep -qx "$name" || rm -f "$made" "$out/$name.commit"
done
