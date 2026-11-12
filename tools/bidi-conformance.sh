#!/usr/bin/env bash
# tools/bidi-conformance.sh --- Run the Unicode conformance tests of right-to-left text.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Downloads BidiTest.txt, BidiCharacterTest.txt and BidiBrackets.txt of the
# Unicode version SBCL's character data is (15.0.0), each checked against its
# SHA-256, into build/bidi-conformance/, and runs every case through the order
# Tuatara shows a line in (src/display/bidi.lisp), in the toolbox when there is
# one; prints how many of each passed. The files are under the Unicode License
# v3 (https://www.unicode.org/license.txt) and are not kept in the repository.
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
directory=build/bidi-conformance
mkdir -p "$directory"
fetch() {  # download Unicode's file $1 into the directory, kept only when its SHA-256 is $2
  if ! echo "$2  $directory/$1" | sha256sum -c --status 2>/dev/null; then
    curl -sSfL --retry 3 -o "$directory/$1.part" "https://www.unicode.org/Public/15.0.0/ucd/$1"
    if ! echo "$2  $directory/$1.part" | sha256sum -c --status; then
      rm -f "$directory/$1.part"
      echo "$1 is not the file expected: not used" >&2
      exit 1
    fi
    mv "$directory/$1.part" "$directory/$1"
  fi
}
fetch BidiTest.txt 72a7a509dba0e147322c17997fb5159431042ff4a49fa08c7c25ccc1e291bbfe
fetch BidiCharacterTest.txt 3c423c301f7b8dc41b879062cbf01fd1b4ec2ea4826e20d276c44b52129a01b6
fetch BidiBrackets.txt 333ae1e99db0504ca8a046a07dc45b5e7aa91869c685e6bf955ebe674804827a
inside sbcl --dynamic-space-size 2GB --non-interactive \
  --load tools/quicklisp.lisp \
  --eval "(push (truename \"./\") asdf:*central-registry*)" \
  --eval "(ql:quickload :tuatara/core :silent t)" \
  --load tools/bidi-conformance.lisp \
  --eval "(let ((*print-pretty* nil)) (tuatara.display::run-bidi-conformance (truename \"$directory/\")))"
