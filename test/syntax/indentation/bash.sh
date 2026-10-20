#!/usr/bin/env bash
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.
#
# tests/indent/bash.sh of Helix (https://github.com/YardQuit/helix_editor)
# at commit 9bcdf4d79d14, unchanged but for this notice: every line as
# Helix indents it.

greet() {
  local name="$1"
  echo "hello, $name"
}

if [ -d "$HOME" ]; then
  echo "home exists"
elif [ -f /etc/passwd ]; then
  echo "passwd"
else
  echo "neither"
fi

for f in a b c; do
  echo "$f"
done

while read -r line; do
  echo "$line"
done

case "$1" in
  start)
    echo "starting"
    ;;
  *)
    echo "usage"
    ;;
esac
