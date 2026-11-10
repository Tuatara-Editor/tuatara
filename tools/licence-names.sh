#!/usr/bin/env bash
# tools/licence-names.sh --- Name the licences a licence file holds, and check them against a grammar's label.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# A licence is known by sentences only its own text has, read with the lines
# joined and the spaces run together; a file may hold several (a licence
# taken over from an earlier author below the new one), and each is named.
# Each is named as SPDX names it, a GNU licence by its version alone
# (GPL-3.0): whether a later version may be taken is said by a notice the
# text alone does not hold, so GPL-3.0-only and GPL-3.0-or-later both agree
# with it. The W3C Software and Document License is named by the SPDX name
# of its 2015 text, whose terms its later texts keep. A file that gives no
# more than an SPDX-License-Identifier line holds the licences that line
# names. Such a line, and a notice that a work is under a licence (Licensed
# under the Apache License, Version 2.0; the GNU GPL's "as published by the
# Free Software Foundation"), name a licence without holding its text.
# Usage: tools/licence-names.sh FILE...
#          the licences the FILEs hold, one name a line, each once
#        tools/licence-names.sh --untexted FILE...
#          the licences the FILEs name whose text is in none of them
#        tools/licence-names.sh --agrees LABEL FILE...
#          exit 0 when LABEL (an SPDX expression: MIT, Apache-2.0 WITH
#          LLVM-exception, AGPL-3.0-or-later AND ISC) names the licences the
#          FILEs hold, else say how they differ and exit 1. Every licence
#          LABEL names must be in a FILE, and every licence in a FILE must be
#          in LABEL, but for the texts of a LICENSES/ folder (REUSE), which
#          may hold licences of other files of the repository than the
#          grammar's. A FILE that holds no licence known here disagrees.
set -euo pipefail

# the licences the text of FILE holds, one name a line; with a second
# argument, only those whose text it holds, not those it only names
names_in() {
  local text texts_only="${2:-}"
  text="$(tr '\r\n\t\f' '    ' < "$1" | tr -s ' ')"
  has() { grep -qiF -- "$1" <<< "$text"; }       # in any case
  named() { [ -z "$texts_only" ] && has "$1"; }  # named only, by a notice
  heading() { grep -qF -- "$1" <<< "$text"; }    # as a heading, in capitals
  if has "Permission is hereby granted, free of charge, to any person"; then echo MIT; fi
  if has "Apache License Version 2.0, January 2004" || named "Licensed under the Apache License, Version 2.0"; then
    echo Apache-2.0
  fi
  if has "LLVM Exceptions to the Apache 2.0 License"; then echo LLVM-exception; fi
  if has "distribute this software for any purpose with or without fee is hereby granted, provided that the above copyright notice"; then
    echo ISC
  fi
  if has "distribute this software for any purpose with or without fee is hereby granted. THE SOFTWARE IS PROVIDED"; then
    echo 0BSD
  fi
  if has "Redistribution and use in source and binary forms, with or without modification, are permitted"; then
    if has "Neither the name" || has "names of its contributors may be used"; then echo BSD-3-Clause; else echo BSD-2-Clause; fi
  fi
  if has "This is free and unencumbered software released into the public domain"; then echo Unlicense; fi
  if has "CC0 1.0 Universal" || { has "Creative Commons Legal Code" && has "CC0"; }; then echo CC0-1.0; fi
  if has "DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE"; then echo WTFPL; fi
  if has "Mozilla Public License Version 2.0" || has "Mozilla Public License, version 2.0" ||
     named "Mozilla Public License, v. 2.0"; then
    echo MPL-2.0
  fi
  if has "The Artistic License 2.0"; then echo Artistic-2.0; fi
  if has "By obtaining and/or copying this work, you (the licensee) agree" ||
     named "W3C Software and Document" || named "https://www.w3.org/copyright/software-license"; then
    echo W3C-20150513
  fi
  if heading "GNU GENERAL PUBLIC LICENSE Version 3" ||
     named "the GNU General Public License as published by the Free Software Foundation, either version 3"; then
    echo GPL-3.0
  fi
  if heading "GNU GENERAL PUBLIC LICENSE Version 2" ||
     named "the GNU General Public License as published by the Free Software Foundation; either version 2"; then
    echo GPL-2.0
  fi
  if heading "GNU AFFERO GENERAL PUBLIC LICENSE Version 3" ||
     named "the GNU Affero General Public License as published by the Free Software Foundation"; then
    echo AGPL-3.0
  fi
  if heading "GNU LESSER GENERAL PUBLIC LICENSE Version 3" ||
     named "the GNU Lesser General Public License as published by the Free Software Foundation, either version 3"; then
    echo LGPL-3.0
  fi
  if heading "GNU LESSER GENERAL PUBLIC LICENSE Version 2.1" ||
     named "the GNU Lesser General Public License as published by the Free Software Foundation; either version 2.1"; then
    echo LGPL-2.1
  fi
  # a file that names its licence by its SPDX identifier alone
  [ -n "$texts_only" ] || sed -n 's/^.*SPDX-License-Identifier: *//p' "$1" |
    while IFS= read -r expression; do label_names "$expression"; done
  return 0
}

# the names of the SPDX expression $1, each as names_in gives it
label_names() {
  local word
  for word in $(tr '()' '  ' <<< "$1"); do
    case "$word" in
      AND|OR|WITH|and|or|with) ;;
      *) word="${word%+}"; word="${word%-only}"; echo "${word%-or-later}" ;;
    esac
  done
}

if [ "${1:-}" = --agrees ]; then
  label="$2"; shift 2
  wanted="$(label_names "$label" | sort -u)"
  held="" also=""
  for file in "$@"; do
    found="$(names_in "$file")"
    if [ -z "$found" ]; then
      echo "${file}: no licence known in it, where the label says $label" >&2
      exit 1
    fi
    case "$(basename "$(dirname "$file")")" in
      LICENSES) also="$also$found"$'\n' ;;
      *) held="$held$found"$'\n' ;;
    esac
  done
  held="$(grep -v '^$' <<< "$held" | sort -u || true)"
  every="$(printf '%s\n%s\n' "$held" "$also" | grep -v '^$' | sort -u || true)"
  unnamed="$(comm -13 <(echo "$wanted") <(echo "$held") | grep -v '^$' || true)"
  missing="$(comm -23 <(echo "$wanted") <(echo "$every") | grep -v '^$' || true)"
  if [ -n "$unnamed" ] || [ -n "$missing" ]; then
    echo "the label says $label, the licence files hold $(echo $every)${unnamed:+; not in the label: $(echo $unnamed)}${missing:+; in no file: $(echo $missing)}" >&2
    exit 1
  fi
  exit 0
fi
if [ "${1:-}" = --untexted ]; then
  shift
  comm -23 <(for file in "$@"; do names_in "$file"; done | sort -u) \
           <(for file in "$@"; do names_in "$file" texts; done | sort -u)
  exit 0
fi
for file in "$@"; do names_in "$file"; done | sort -u
