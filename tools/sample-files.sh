#!/usr/bin/env bash
# tools/sample-files.sh --- Write small sample files for trying Tuatara by hand.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Write small sample files for trying Tuatara by hand. Usage: tools/sample-files.sh [DIRECTORY]
# (default tuatara-sample-files in the system's folder for temporary files).
set -euo pipefail
dir=${1:-${TMPDIR:-/tmp}/tuatara-sample-files}
mkdir -p "$dir"
# UTF-8 text: two-, three- and four-octet characters, wide ones, a combining mark
printf 'Plain ASCII line\nLatin: café, Zoë, München, Ångström\nCJK wide: 中文字 日本語 한국어\nEmoji: 😀 🎉 👍\nCombining: e\xcc\x81 (e + U+0301)\nTab:\tafter a tab\n' > "$dir/utf8.txt"
# A CSV saved as Latin-1 by an older program: é and ü are single octets, not UTF-8
printf 'name,city\nJos\xe9,M\xfcnchen\nFran\xe7ois,Qu\xe9bec\nZo\xc3\xab,Wellington\n' > "$dir/latin1.csv"
# A Windows CSV: every line ends with a carriage return and a newline
printf 'id,name,amount\r\n1,Aroha,12.50\r\n2,Tama,7.25\r\n3,Mere,19.99\r\n' > "$dir/crlf.csv"
# Octets that are not text at all, mixed with some that are
printf 'header\n\x00\x01\x02\xff\xfe\x80\x81 bytes \xe2\x82 cut \xf0\x9f\x98 short\ntrailer\n' > "$dir/binary.bin"
echo "Sample files written to $dir:"
ls -l "$dir"
