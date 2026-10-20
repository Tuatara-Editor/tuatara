#!/usr/bin/env bash
# tools/lisp-server.sh --- Start SBCL with Tuatara and a Swank or Slynk server.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Start SBCL (in the toolbox when there is one) with Tuatara loaded and a Swank (SLIME) or Slynk (SLY)
# server on localhost:4005. Then connect from the host: M-x slime-connect RET localhost RET 4005 RET
set -euo pipefail
cd "$(dirname "$0")/.."
. tools/inside.sh
inside sbcl --noinform --load tools/swank.lisp
