#!/usr/bin/env bash
# tools/library-sources.sh --- Fetch the source of every library the AppImage bundles.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Run by tools/appimage.sh inside its Ubuntu container, after
# tools/bundle-libraries.sh, with deb-src in apt's sources. For each package of
# LIB_TSV (bundle-libraries.sh's lib.tsv), the Debian source package it was
# built from, at exactly the version installed, which must be the version
# LIB_TSV gives: OUTDIR/SOURCE/ holds its .dsc and the files the .dsc names,
# as apt-get source downloads them from Ubuntu's archive, apt checking each
# against the archive's signed index, and each then checked again against the
# SHA-256 the .dsc gives it. A folder already holding the version wanted is
# kept, its files fetched again only when they are not the ones named; a
# folder of a source package no longer bundled is removed. OUTDIR/README
# says what it is, with a line for each library: its file, package, source
# package, source version and the licences its copyright file names (or
# where that file is in the AppImage, when it names none in a form read). A
# library whose installed version is another, a source apt cannot fetch, or a
# file that differs from its .dsc, stops it.
# DOC_DIR (default /usr/share/doc) is where the copyright files are.
# Usage: tools/library-sources.sh OUTDIR LIB_TSV
set -euo pipefail
out="$1"
table="$2"
docs="${DOC_DIR:-/usr/share/doc}"
mkdir -p "$out"
: > "$out/libraries.part"
declare -A wanted=()
while IFS=$'\t' read -r file package version _; do
  installed="$(dpkg-query -W -f='${Version}\t${source:Package}\t${source:Version}' "$package")"
  IFS=$'\t' read -r installed_version source source_version <<<"$installed"
  [ "$installed_version" = "$version" ] ||
    { echo "$package is $installed_version here, not $version as bundled" >&2; exit 1; }
  wanted[$source]="$source_version"
  licences=""
  [ -f "$docs/$package/copyright" ] &&
    licences="$(sed -n 's/^License: *\(..*\)/\1/p' "$docs/$package/copyright" | sort -u |
                  paste -sd, - | sed 's/,/, /g')"
  [ -n "$licences" ] || licences="see licenses/libraries/$package.copyright"
  printf '%s\t%s\t%s\t%s\t%s\n' "$file" "$package" "$source" "$source_version" "$licences" >> "$out/libraries.part"
done < "$table"
for folder in "$out"/*/; do
  [ -d "$folder" ] || continue
  [ -n "${wanted[$(basename "$folder")]+x}" ] || rm -rf "$folder"
done
for source in $(printf '%s\n' "${!wanted[@]}" | sort); do
  version="${wanted[$source]}"
  dsc="$out/$source/${source}_${version#*:}.dsc"
  [ -f "$dsc" ] || rm -rf "${out:?}/$source"
  mkdir -p "$out/$source"
  (cd "$out/$source" && apt-get source -qq -o APT::Sandbox::User=root --download-only "$source=$version" >/dev/null)
  # the files the .dsc names, each with its SHA-256, and nothing else in the folder
  checks="$(sed -n '/^Checksums-Sha256:/,/^[^ ]/s/^ \([0-9a-f]\{64\}\) [0-9]* \(.*\)/\1  \2/p' "$dsc")"
  [ -n "$checks" ] || { echo "$dsc names no files" >&2; exit 1; }
  (cd "$out/$source" && sha256sum -c --quiet <<<"$checks") ||
    { echo "$source $version differs from its .dsc" >&2; exit 1; }
  for path in "$out/$source"/*; do
    name="$(basename "$path")"
    [ "$path" = "$dsc" ] || grep -qxF "$name" <(sed 's/^.*  //' <<<"$checks") || rm -rf "$path"
  done
done
{
  cat <<'EOF'
The source of every library Tuatara-x86_64.AppImage bundles (its lib/): for
each, exactly the Ubuntu 24.04 source package its binary package was built
from, at the version bundled, as Ubuntu's archive gives it. Each folder holds
one source package, its .dsc and the files the .dsc names, each checked
against the SHA-256 the .dsc gives it; dpkg-source -x NAME_VERSION.dsc
unpacks it. The AppImage's licenses/bundled-libraries.txt names each
library's package, version and SHA-256, and licenses/libraries/ holds each
package's copyright file, whose licences are named below.

EOF
  printf '# file\tpackage\tsource package\tsource version\tlicences\n'
  sort -u "$out/libraries.part"
} > "$out/README"
rm -f "$out/libraries.part"
