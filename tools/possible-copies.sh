#!/usr/bin/env bash
# tools/possible-copies.sh --- Show what looks like a copy: a branch's additions, alike bodies, pass-throughs.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Runs tools/possible-copies.py, which says what it reports and how to ask for it.
# Usage: tools/possible-copies.sh [--meaning] BASE [HEAD] | --where TEXT... | --bodies | --pass-through
#        | --alike-meaning
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here/.."
exec python3 -I "$here/possible-copies.py" "$@"
