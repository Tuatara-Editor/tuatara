#!/usr/bin/env bash
# tools/appimage.sh --- Make Tuatara into one AppImage.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Make build/Tuatara-x86_64.AppImage: one file that runs Tuatara on another
# computer, in its terminal or in a window of its own, nothing installed. It
# holds Tuatara as tools/release.sh lays it out (the program, grammars, the
# terminal library, the grammars' rules, icons) and the libraries of its window
# and terminal versions (GTK 4 and what it needs, ncurses, OpenSSL's crypto),
# taken from Ubuntu 24.04 in a podman container, so that it runs where glibc is
# 2.39 or later (Ubuntu 24.04, Debian 13, Fedora 40 and later); tools/distro-packages.sh
# makes Fedora, Ubuntu and Arch packages of the same. The libraries
# every system has its own of (glibc, the graphics drivers, fontconfig and the
# rest of AppImage's exclude list) are the computer's. Spelling uses the
# computer's enchant and dictionaries; the programs Tuatara runs (bwrap for the
# sandbox, ImageMagick, poppler's tools, git, TeX, LibreOffice) are the
# computer's too, each feature saying so when its program is not there.
# appimagetool, the AppImage runtime it puts at the front of the file (what
# runs first when the AppImage is started) and the exclude list are fetched
# once into build/appimage-tools/, each a pinned version checked against its
# SHA-256 before it is used; the
# container is made from the Ubuntu image pinned in tools/containers.sh.
# Usage: tools/appimage.sh
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
. tools/containers.sh
tools_dir="$root/build/appimage-tools"
appdir="$root/build/appimage/Tuatara.AppDir"
out="$root/build/Tuatara-x86_64.AppImage"
container=tuatara-appimage-libs
excludelist_url=https://raw.githubusercontent.com/AppImageCommunity/pkg2appimage/15a64c20dc23a0154622ba25829364323903b6b5/excludelist
excludelist_sha256=50db0f894f34b169c47a5cbc0c17dbab61e9edebca5bc8269a1e6ac1bf4bdad9
appimagetool_url=https://github.com/AppImage/appimagetool/releases/download/1.9.1/appimagetool-x86_64.AppImage
appimagetool_sha256=ed4ce84f0d9caff66f50bcca6ff6f35aae54ce8135408b3fa33abfc3cb384eb0
runtime_url=https://github.com/AppImage/type2-runtime/releases/download/20251108/runtime-x86_64
runtime_sha256=2fca8b443c92510f1483a883f60061ad09b46b978b2631c807cd873a47ec260d

fetch() {  # download $1 to $2 unless it is there with SHA-256 $3; a download that differs is not kept
  if ! echo "$3  $2" | sha256sum -c --status 2>/dev/null; then
    curl -sSfL -o "$2.part" "$1"
    if ! echo "$3  $2.part" | sha256sum -c --status; then
      rm -f "$2.part"
      echo "$1 is not the file expected: not used" >&2
      exit 1
    fi
    mv "$2.part" "$2"
  fi
}
mkdir -p "$tools_dir"
fetch "$excludelist_url" "$tools_dir/excludelist" "$excludelist_sha256"
fetch "$appimagetool_url" "$tools_dir/appimagetool-x86_64.AppImage" "$appimagetool_sha256"
fetch "$runtime_url" "$tools_dir/runtime-x86_64" "$runtime_sha256"
chmod +x "$tools_dir/appimagetool-x86_64.AppImage"

tools/release.sh >/dev/null
rm -rf "$root/build/appimage"
mkdir -p "$appdir/lib"
cp -a build/release/tuatara "$appdir/tuatara"

# the libraries, from Ubuntu 24.04: those the program loads (libzstd, which the
# program itself needs, too) and all they need in turn, but those on the exclude list
pinned_container "$container" "$ubuntu_image" 'DEBIAN_FRONTEND=noninteractive apt-get update -qq &&
  DEBIAN_FRONTEND=noninteractive apt-get install -y -qq libgtk-4-1 libncursesw6 libssl3t64 libzstd1 >/dev/null'
podman cp "$tools_dir/excludelist" "$container:/tmp/excludelist"
podman exec "$container" bash -c '
  set -euo pipefail
  rm -rf /tmp/lib && mkdir -p /tmp/lib
  excluded=" $(grep -v "^#" /tmp/excludelist | sed "s/[ #].*//" | grep . | tr "\n" " ") "
  roots=""
  known=$(ldconfig -p)
  for name in libgtk-4.so.1 libncursesw.so.6 libcrypto.so.3 libzstd.so.1; do
    roots="$roots $(grep -m1 "^\s*$name " <<<"$known" | sed "s/.*=> //")"
  done
  for path in $roots $(ldd $roots | grep "=> /" | sed "s/.*=> //; s/ (.*//" | sort -u); do
    name=$(basename "$path")
    case "$excluded" in *" $name "*) continue ;; esac
    cp -L "$path" "/tmp/lib/$name"
  done'
podman cp "$container:/tmp/lib/." "$appdir/lib/"

# how it starts: the libraries it brings first, the environment as it was given
# back to the programs it starts (LEAVE-APPIMAGE-ENVIRONMENT); --dark or --light
# first shows the window's own parts (its title bar) in GTK's dark or light
# style, which the GTK it brings cannot learn from the desktop; --install-desktop
# and --remove-desktop put it in the desktop's applications and take it out
# (tools/install-desktop.sh, inside it), --install-desktop --dark with the
# style kept in the entry
cat > "$appdir/AppRun" <<'APPRUN'
#!/bin/sh
here="$(dirname "$(readlink -f "$0")")"
case "${1:-}" in
  --dark) export GTK_THEME=Adwaita:dark; shift ;;
  --light) export GTK_THEME=Adwaita; shift ;;
esac
case "${1:-}" in
  --install-desktop) shift; exec "$here/install-desktop.sh" --appimage "${APPIMAGE:?run the AppImage file itself}" "$@" ;;
  --remove-desktop) exec "$here/install-desktop.sh" --remove ;;
esac
export TUATARA_APPIMAGE=1
export TUATARA_LD_LIBRARY_PATH="${LD_LIBRARY_PATH:-}"
export LD_LIBRARY_PATH="$here/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
exec "$here/tuatara/tuatara" "$@"
APPRUN
chmod +x "$appdir/AppRun"
cp tools/install-desktop.sh "$appdir/install-desktop.sh"
sed "s|@EXEC@|tuatara|" data/tuatara.desktop.in > "$appdir/tuatara.desktop"
cp data/icons/hicolor/256x256/apps/tuatara.png "$appdir/tuatara.png"
ln -s tuatara.png "$appdir/.DirIcon"

rm -f "$out"
ARCH=x86_64 "$tools_dir/appimagetool-x86_64.AppImage" --appimage-extract-and-run --no-appstream --runtime-file "$tools_dir/runtime-x86_64" "$appdir" "$out" >/dev/null
echo "$out"
