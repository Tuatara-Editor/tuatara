#!/usr/bin/env bash
# tools/install-desktop.sh --- Put Tuatara in the desktop's list of applications.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Put Tuatara in the desktop's list of applications: writes
# ~/.local/share/applications/tuatara.desktop, which starts the window
# (tools/tuatara-tui.sh --gui) on the files given, and offers Tuatara in "Open
# With" for text, and puts its icon, in every size there is, under
# ~/.local/share/icons/hicolor/. The desktop matches the window to it by the
# name "tuatara". Run it again after moving the project;
# tools/install-desktop.sh --remove takes both away.
#
# tools/install-desktop.sh --appimage FILE installs the AppImage FILE instead:
# copied to ~/.local/bin/tuatara (tuatara in a terminal starts it there), the
# entry and the icons taken from inside it, so that this script and the
# AppImage are all another computer needs; --dark or --light after FILE has
# the entry start it so. --remove takes that copy away too. The AppImage runs
# this itself: Tuatara-x86_64.AppImage --install-desktop [--dark|--light], or
# --remove-desktop.
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
data="${XDG_DATA_HOME:-$HOME/.local/share}"
target="$data/applications/tuatara.desktop"
icons="$data/icons/hicolor"
installed="$HOME/.local/bin/tuatara"
# $1 as one argument of the entry's Exec= line, quoted as desktop entries quote
# one: \ " ` and $ escaped by a backslash, and that and every backslash
# doubled again, since the line is read as a string first; % written %%, so
# that it is no field code. Then made safe to stand in a replacement of sed's s|||
exec_argument() {
  local s="$1"
  s=${s//\\/\\\\\\\\}; s=${s//\"/\\\\\"}; s=${s//\`/\\\\\`}; s=${s//\$/\\\\\$}; s=${s//%/%%}
  printf '"%s"' "$s" | sed 's/[\\|&]/\\&/g'
}
# true when the file $1 is an AppImage: an ELF program with the AppImage type 2 mark
is_appimage() { [ "$(od -An -tx1 -j8 -N3 "$1" 2>/dev/null | tr -d ' \n')" = "414902" ]; }
if [ "${1:-}" = "--remove" ]; then
  rm -f "$target"
  rm -f "$icons"/*/apps/tuatara.*
  # only the AppImage this script put there, never a program of another kind
  if [ -f "$installed" ] && is_appimage "$installed"; then
    rm -f "$installed"
    echo "Removed $installed"
  fi
  echo "Removed $target and the icon"
  exit 0
fi
unpacked=""
trap 'if [ -n "$unpacked" ]; then rm -rf "$unpacked"; fi' EXIT
if [ "${1:-}" = "--appimage" ]; then
  appimage="${2:?--appimage needs the AppImage file}"
  case "${3:-}" in
    "") style="" ;;
    --dark|--light) style=" $3" ;;
    *) echo "$3 is neither --dark nor --light" >&2; exit 1 ;;
  esac
  if ! is_appimage "$appimage"; then
    echo "$appimage is no AppImage" >&2
    exit 1
  fi
  mkdir -p "$(dirname "$installed")"
  # a copy beside it first, renamed over the old one: a running Tuatara keeps its own
  cp "$appimage" "$installed.new"
  chmod 755 "$installed.new"
  mv -f "$installed.new" "$installed"
  # the entry and icons it holds: where it is mounted when it runs this
  # (--install-desktop), else unpacked where nothing else is
  if [ -n "${APPDIR:-}" ] && [ -f "$APPDIR/tuatara.desktop" ]; then
    inside="$APPDIR"
  else
    unpacked="$(mktemp -d)"
    (cd "$unpacked" && "$installed" --appimage-extract 'tuatara.desktop' >/dev/null &&
       "$installed" --appimage-extract 'tuatara/icons/*' >/dev/null)
    inside="$unpacked/squashfs-root"
  fi
  source_icons="$inside/tuatara/icons/hicolor"
  entry() { sed "s|^Exec=tuatara |Exec=$(exec_argument "$installed")$style |" "$inside/tuatara.desktop"; }
else
  source_icons="$root/data/icons/hicolor"
  # the entry as data/tuatara.desktop.in has it (the AppImage's too), started by tools/tuatara-tui.sh
  entry() { sed "s|@EXEC@|$(exec_argument "$root/tools/tuatara-tui.sh")|" "$root/data/tuatara.desktop.in"; }
fi
# the icon, where the desktop looks icons up by name; an earlier one taken away
# first, so that a size or drawing no longer made is not shown in its place
rm -f "$icons"/*/apps/tuatara.*
for f in "$source_icons"/*/apps/tuatara.*; do
  dest="$icons/${f#"$source_icons/"}"
  mkdir -p "$(dirname "$dest")"
  cp "$f" "$dest"
done
mkdir -p "$(dirname "$target")"
entry > "$target"
# a desktop that keeps a cache of the icons sees the new one at once
if command -v gtk-update-icon-cache >/dev/null && [ -f "$icons/index.theme" ]; then
  gtk-update-icon-cache -q -t "$icons" || true
fi
if command -v update-desktop-database >/dev/null; then
  update-desktop-database -q "$(dirname "$target")" || true
fi
[ -n "${appimage:-}" ] && echo "Installed $installed"
echo "Wrote $target and the icon under $icons"
