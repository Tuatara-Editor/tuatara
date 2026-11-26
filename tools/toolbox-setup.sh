#!/usr/bin/env bash
# tools/toolbox-setup.sh --- Create the Tuatara development container on Fedora Atomic and install the toolchain.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Usage: tools/toolbox-setup.sh        (run from the host; re-run to update)
# The container is named tuatara, or as $TUATARA_CONTAINER says, as tools/inside.sh finds it.
# At the end it asks whether to start the weekly check for newer dependencies
# (tools/check-updates.sh --install); the answer is no unless you type y.
set -uo pipefail
name="${TUATARA_CONTAINER:-tuatara}"
if [ "$(toolbox list --containers 2>/dev/null | grep -c "[[:space:]]${name}[[:space:]]")" = 0 ]; then
  toolbox --assumeyes create --distro fedora "$name" || exit 1
fi
# --skip-unavailable: a renamed package must not abort the whole install; the check below reports gaps.
# Building: SBCL, the compilers and git. Running: GTK 4, ncurses, and the programs
# the features use (the sandbox, pictures, PDFs, archives, spelling, Office documents, Outlook mailboxes and messages
# drawn as they look, the clipboard).
toolbox run --container "$name" sudo dnf install -y --skip-unavailable \
  sbcl git gcc gcc-c++ make pkg-config \
  gtk4 ncurses-libs \
  bubblewrap ImageMagick poppler-utils bsdtar 7zip \
  enchant2 hunspell hunspell-en \
  libreoffice-writer libreoffice-calc libreoffice-impress libpst weasyprint wl-clipboard xclip || exit 1

# csvdt, which the csvdt package runs and its tests run for real, is no Fedora package: its own
# release's RPM, pinned and checked against the SHA-256 the release publishes (SHA256SUMS-rpms)
. "$(dirname "$0")/fetch.sh"
csvdt_rpm=csvdt-1.2.0-1.fc45.x86_64.rpm
csvdt_url=https://github.com/YardQuit/csvdt/releases/download/1.2.0-2026.11.e29952d-tzdata2026f/$csvdt_rpm
csvdt_sha256=9fefff11386d36e307c07063ac02b0dd4aad460c317a70541e50337d91d86d8e
csvdt_dir="${XDG_CACHE_HOME:-$HOME/.cache}/tuatara-setup"
mkdir -p "$csvdt_dir"
checked_download "$csvdt_url" "$csvdt_dir/$csvdt_rpm" "$csvdt_sha256" || exit 1
toolbox run --container "$name" sudo dnf install -y "$csvdt_dir/$csvdt_rpm" || exit 1

echo "== verification"
toolbox run --container "$name" bash -c '
  printf "sbcl: "; sbcl --version 2>/dev/null || echo MISSING
  # the tree-sitter grammars whose scanners are C++ (YAML, Vue...) need it
  printf "c++: "; if command -v c++ >/dev/null; then c++ --version | head -1; else echo MISSING; fi
  for p in bwrap bsdtar magick pdftoppm weasyprint csvdt; do
    printf "%-10s %s\n" "$p" "$(command -v "$p" || echo MISSING)"
  done
  for l in libgtk-4.so.1 libncursesw.so.6 libenchant-2.so.2; do
    printf "%-20s %s\n" "$l" "$(ldconfig -p | grep -q "$l" && echo found || echo MISSING)"
  done'
echo "== weekly check for newer dependencies"
answer=n
[ -t 0 ] && read -r -p "Check once a week for newer versions of the dependencies \
(a systemd user timer, on the network)? [y/N] " answer
case "$answer" in
  [yY]*) "$(dirname "$0")/check-updates.sh" --install ||
    echo "check-updates.sh --install failed: run it again by hand" ;;
  *) echo "Not started: tools/check-updates.sh --install starts it." ;;
esac
echo "Container '$name' ready. Enter with: toolbox enter $name"
echo "Quicklisp and Zig are not installed by this script: see HACKING.org."
