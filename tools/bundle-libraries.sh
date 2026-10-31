#!/usr/bin/env bash
# tools/bundle-libraries.sh --- Copy shared libraries with their package, version and copyright.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Run by tools/appimage.sh inside its Ubuntu container. For each library path
# given, in OUTDIR: lib/NAME (the file itself), copyright/PACKAGE.copyright
# (the package's Debian copyright file, one for each package) and lib.tsv,
# a line FILE<TAB>PACKAGE<TAB>VERSION<TAB>SHA-256 per library sorted by file.
# A library no package owns, or whose package has no copyright file, stops it.
# DOC_DIR (default /usr/share/doc) is where the copyright files are, USR_ROOT
# (default /usr) the folder dpkg's /lib and /bin are under.
# Usage: tools/bundle-libraries.sh OUTDIR LIBRARY...
set -euo pipefail
out="$1"
shift
docs="${DOC_DIR:-/usr/share/doc}"
mkdir -p "$out/lib" "$out/copyright"
: > "$out/lib.tsv.part"
for path in "$@"; do
  name="$(basename "$path")"
  cp -L "$path" "$out/lib/$name"
  real="$(readlink -f "$path")"
  # dpkg lists a file under /lib where the library's real path is under /usr/lib
  package="$({ dpkg -S "$real" "${real#"${USR_ROOT:-/usr}"}" 2>/dev/null || true; } | head -1 | sed 's/: .*//; s/:[a-z0-9]*$//')"
  [ -n "$package" ] || { echo "$path belongs to no package" >&2; exit 1; }
  [ -f "$docs/$package/copyright" ] || { echo "$package has no copyright file" >&2; exit 1; }
  cp -L "$docs/$package/copyright" "$out/copyright/$package.copyright"
  printf '%s\t%s\t%s\t%s\n' "$name" "$package" "$(dpkg-query -W -f='${Version}' "$package")" \
    "$(sha256sum "$out/lib/$name" | cut -c1-64)" >> "$out/lib.tsv.part"
done
sort "$out/lib.tsv.part" > "$out/lib.tsv"
rm -f "$out/lib.tsv.part"
