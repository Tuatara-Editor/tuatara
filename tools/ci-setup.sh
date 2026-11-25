#!/usr/bin/env bash
# tools/ci-setup.sh --- Install what a release is built with, on a new Ubuntu 24.04 machine.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# For .github/workflows/release.yml, on GitHub's machines; on your own, see
# HACKING.org instead. Installs, each pinned and checked by its SHA-256 before
# it is used:
#   - SBCL, its release from sbcl.org, in /usr/local;
#   - Zig, which builds the terminal library, in /opt/zig (on $GITHUB_PATH);
# The release makes its own Quicklisp (tools/release.sh).
# Everything comes over HTTPS. Then bubblewrap (bwrap), which the grammars'
# build shuts each grammar.js in with, from the machine's own Ubuntu archive,
# checked by apt against its signed index; its version goes in the log. Ubuntu
# 24.04 lets a program make a user namespace only when an AppArmor profile
# allows it, so Ubuntu's own profile for bwrap (bwrap-userns-restrict, taken
# out of its apparmor-profiles package, nothing else of it installed) is
# loaded: bwrap alone may, the programs it starts may not, and the rule stays
# as it is for everything else. bwrap is then tried once, and a bwrap that
# cannot shut a program in stops it here.
# Usage: tools/ci-setup.sh
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

sbcl_url=https://downloads.sourceforge.net/project/sbcl/sbcl/2.6.8/sbcl-2.6.8-x86-64-linux-binary.tar.bz2
sbcl_sha256=5391773774b94554a015db9f992370d06937fb6f0cdb0b2142281aebef9e96c1
# the version is the one lisp/terminal/library.pin names
zig_version="$(sed -n 's/^zig[[:space:]]*//p' "$root/lisp/terminal/library.pin")"
zig_url="https://ziglang.org/download/$zig_version/zig-x86_64-linux-$zig_version.tar.xz"
zig_sha256=1cbe9df9f27e6b78d14ccbca43b6703a404ef79ef1c463de901d7f088d4e2026

. "$root/tools/fetch.sh"
fetch() { checked_download "$@" || exit 1; }

echo "== SBCL"
fetch "$sbcl_url" "$work/sbcl.tar.bz2" "$sbcl_sha256"
tar -xjf "$work/sbcl.tar.bz2" -C "$work"
(cd "$work"/sbcl-*-linux && sudo INSTALL_ROOT=/usr/local sh install.sh >/dev/null)
sbcl --version

echo "== Zig"
fetch "$zig_url" "$work/zig.tar.xz" "$zig_sha256"
sudo mkdir -p /opt/zig
sudo tar -xJf "$work/zig.tar.xz" -C /opt/zig --strip-components=1
[ -n "${GITHUB_PATH:-}" ] && echo /opt/zig >> "$GITHUB_PATH"
/opt/zig/zig version

echo "== bubblewrap"
sudo apt-get update -qq
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y -qq bubblewrap >/dev/null
if [ "$(cat /proc/sys/kernel/apparmor_restrict_unprivileged_userns 2>/dev/null || echo 0)" = 1 ]; then
  profile=usr/share/apparmor/extra-profiles/bwrap-userns-restrict
  (cd "$work" && apt-get download -qq apparmor-profiles)
  dpkg-deb --fsys-tarfile "$work"/apparmor-profiles_*.deb | tar -x -C "$work" "./$profile"
  sudo apparmor_parser -r "$work/$profile"
fi
bwrap --unshare-all --die-with-parent --clearenv --proc /proc --dev /dev --ro-bind /usr /usr \
  --symlink usr/lib64 /lib64 --symlink usr/lib /lib --symlink usr/bin /bin -- /usr/bin/true ||
  { echo "bwrap cannot shut a program in on this machine" >&2; exit 1; }
echo "bubblewrap $(dpkg-query -W -f='${Version}' bubblewrap)"
