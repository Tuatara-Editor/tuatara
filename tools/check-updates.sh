#!/usr/bin/env bash
# tools/check-updates.sh --- Report the pinned dependencies that have newer versions.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Compare what Tuatara is built with against what is published upstream, and
# write the result to build/bumpcheck.org:
#   - tree-sitter and each grammar of syntax/grammars.tsv: the pinned commit
#     against the repository's newest commit and newest version tag, and the
#     tree-sitter program's release against the newest;
#   - Ghostty and Zig of lisp/terminal/library.pin;
#   - the Quicklisp release installed against the newest;
#   - the SBCL build/tuatara was saved with against the newest release;
#   - what tools/appimage.sh uses: appimagetool, the AppImage runtime and the
#     list of libraries left out;
#   - the Ubuntu, Fedora and Arch images of tools/containers.sh, whose digest
#     changes with each rebuilt image (security updates), asked of the
#     registry by skopeo.
# Nothing is changed: a pin is moved by hand, a grammar's only after its
# colouring rules are checked against the new version and
# tools/fuzz-grammars.sh passes for it. Only names and versions are asked of
# the servers (git ls-remote, the Quicklisp release list, the image's
# digest). When the list of newer versions differs from the last report, the
# desktop shows a notice.
#
# tools/check-updates.sh --install runs it once a week from now on: writes a
# systemd user service and timer (tuatara-check-updates) under
# ~/.config/systemd/user/ and starts the timer; a week missed while the
# computer was off is caught up at the next start. Run it again after moving
# the project. --remove stops the timer and takes both away.
# Usage: tools/check-updates.sh [--install | --remove]
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
units="${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user"
unit=tuatara-check-updates

case "${1:-}" in
  --install)
    mkdir -p "$units"
    cat > "$units/$unit.service" <<EOF
[Unit]
Description=Report Tuatara's pinned dependencies that have newer versions

[Service]
Type=oneshot
ExecStart="$root/tools/check-updates.sh"
EOF
    cat > "$units/$unit.timer" <<EOF
[Unit]
Description=Check Tuatara's dependencies for newer versions once a week

[Timer]
OnCalendar=weekly
Persistent=true
RandomizedDelaySec=1h

[Install]
WantedBy=timers.target
EOF
    systemctl --user daemon-reload
    systemctl --user enable --now "$unit.timer"
    echo "Installed $units/$unit.{service,timer}; next check:"
    systemctl --user list-timers "$unit.timer" --no-pager | sed -n 2p
    exit 0 ;;
  --remove)
    systemctl --user disable --now "$unit.timer" 2>/dev/null || true
    rm -f "$units/$unit.service" "$units/$unit.timer"
    systemctl --user daemon-reload
    echo "Removed $unit.{service,timer}"
    exit 0 ;;
  "") ;;
  *) echo "Usage: tools/check-updates.sh [--install | --remove]" >&2; exit 1 ;;
esac

report="$root/build/bumpcheck.org"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# the newest version tag among the refs on standard input (v1.2.3, 1.2.3, ...)
newest_tag() {
  sed -n 's|^[0-9a-f]*\trefs/tags/\([^^]*\)$|\1|p' | { grep -E '^v?[0-9]+(\.[0-9]+)+$' || true; } | sort -V | tail -1
}
# the newest release named by its date (20251108) among the refs on standard input
newest_date_tag() {
  sed -n 's|^[0-9a-f]*\trefs/tags/\([0-9]\{8\}\)$|\1|p' | sort | tail -1
}
# one line NAME<TAB>PINNED<TAB>NEWEST for repository $2 pinned at commit $3, or
# nothing when the pin is its newest commit
check_repository() {
  local name="$1" repository="$2" commit="$3" refs head tag pinned
  if ! refs="$(GIT_TERMINAL_PROMPT=0 timeout 60 git ls-remote "$repository" 2>/dev/null)"; then
    printf '%s\t%s\t%s\n' "$name" "${commit:0:12}" "(not reachable)"
    return
  fi
  head="$(awk '$2 == "HEAD" { print $1 }' <<<"$refs")"
  [ "$head" = "$commit" ] && return
  tag="$(newest_tag <<<"$refs")"
  # the pin by its tag where one names it
  pinned="$(awk -v c="$commit" '$1 == c && $2 ~ /^refs\/tags\// { sub(/^refs\/tags\//, "", $2); sub(/\^\{\}$/, "", $2); print $2; exit }' <<<"$refs")"
  printf '%s\t%s\t%s\n' "$name" "${pinned:-${commit:0:12}}" "${head:0:12}${tag:+ (newest tag $tag)}"
}
export -f check_repository newest_tag

# the grammars and tree-sitter, eight at a time; one line per repository and commit
grep -v '^#' "$root/syntax/grammars.tsv" | awk -F'\t' 'NF >= 4 && $1 != "tree-sitter-cli" && $3 ~ /^[0-9a-f]{40}$/ && !seen[$2 "\t" $3]++ { print $1 "\t" $2 "\t" $3 }' |
  xargs -P 8 -d '\n' -n 1 bash -c 'IFS=$'"'"'\t'"'"' read -r n r c <<<"$1"; check_repository "$n" "$r" "$c"' _ |
  sort > "$work/grammars"

# the tree-sitter program: its release, from the address it is fetched from
cli_url="$(awk -F'\t' '$1 == "tree-sitter-cli" { print $2 }' "$root/syntax/grammars.tsv")"
cli_pinned="$(sed -n 's|.*/releases/download/\([^/]*\)/.*|\1|p' <<<"$cli_url")"
cli_newest="$(GIT_TERMINAL_PROMPT=0 timeout 60 git ls-remote --tags https://github.com/tree-sitter/tree-sitter 2>/dev/null | newest_tag || true)"

pin="$root/lisp/terminal/library.pin"
field() { awk -F'\t' -v k="$1" '$1 == k { print $2 }' "$pin"; }
check_repository ghostty "$(field repository)" "$(field commit)" > "$work/ghostty"
zig_pinned="$(field zig)"
zig_newest="$(GIT_TERMINAL_PROMPT=0 timeout 60 git ls-remote --tags https://codeberg.org/ziglang/zig 2>/dev/null | newest_tag || true)"

# where tools/quicklisp.lisp finds Quicklisp
for ql_dist in "$(dirname "${QUICKLISP_SETUP:-/nonexistent/setup.lisp}")" "$HOME/quicklisp" "$HOME/.local/share/quicklisp"; do
  ql_dist="$ql_dist/dists/quicklisp/distinfo.txt"
  [ -f "$ql_dist" ] && break
done
ql_pinned="$(sed -n 's/^version: //p' "$ql_dist" 2>/dev/null || true)"
ql_newest="$(curl -fsS --max-time 60 https://beta.quicklisp.org/dist/quicklisp.txt 2>/dev/null | sed -n 's/^version: //p' || true)"

sbcl_pinned="$("$root/build/tuatara" --version 2>/dev/null | sed -n 's/.* on SBCL \([0-9.]*\).*/\1/p' || true)"
sbcl_newest="$(GIT_TERMINAL_PROMPT=0 timeout 60 git ls-remote --tags https://github.com/sbcl/sbcl 2>/dev/null |
  sed -n 's|^[0-9a-f]*\trefs/tags/sbcl-\([0-9.]*\)$|\1|p' | sort -V | tail -1 || true)"

# what tools/appimage.sh builds the AppImage with: appimagetool's release, the
# runtime's, the commit of the list of libraries left out, and the Ubuntu image, by its digest
appimage_field() { sed -n "s|^$1=||p" "$root/tools/appimage.sh"; }
appimagetool_pinned="$(appimage_field appimagetool_url | sed -n 's|.*/releases/download/\([^/]*\)/.*|\1|p')"
appimagetool_newest="$(GIT_TERMINAL_PROMPT=0 timeout 60 git ls-remote --tags https://github.com/AppImage/appimagetool 2>/dev/null | newest_tag || true)"
runtime_pinned="$(appimage_field runtime_url | sed -n 's|.*/releases/download/\([^/]*\)/.*|\1|p')"
runtime_newest="$(GIT_TERMINAL_PROMPT=0 timeout 60 git ls-remote --tags https://github.com/AppImage/type2-runtime 2>/dev/null | newest_date_tag || true)"
excludelist_commit="$(appimage_field excludelist_url | sed -n 's|.*/pkg2appimage/\([0-9a-f]\{40\}\)/.*|\1|p')"
check_repository "AppImage exclude list" https://github.com/AppImageCommunity/pkg2appimage "$excludelist_commit" > "$work/excludelist"
# the images packages are made in, each a line as version_line writes
. "$root/tools/containers.sh"
for image in "$ubuntu_image" "$fedora_image" "$arch_image"; do
  pinned="${image##*@sha256:}"
  if ! command -v skopeo >/dev/null; then
    printf '%s\t%s\t%s\n' "${image%@*} image" "${pinned:0:12}" "(skopeo not installed)"
    continue
  fi
  newest="$(timeout 60 skopeo inspect --raw "docker://${image%@*}" 2>/dev/null | sha256sum | cut -d' ' -f1 || true)"
  if [ -z "$newest" ] || [ "$newest" = "$(printf '' | sha256sum | cut -d' ' -f1)" ]; then
    printf '%s\t%s\t%s\n' "${image%@*} image" "${pinned:0:12}" "(not reachable)"
  elif [ "$newest" != "$pinned" ]; then
    printf '%s\t%s\t%s\n' "${image%@*} image" "${pinned:0:12}" "${newest:0:12}"
  fi
done > "$work/images"

# one line NAME<TAB>PINNED<TAB>NEWEST when NEWEST is a later version than PINNED
version_line() {
  if [ -z "$3" ]; then printf '%s\t%s\t%s\n' "$1" "${2:-?}" "(not reachable)"
  elif [ "${2#v}" != "${3#v}" ] && [ "$(printf '%s\n%s\n' "${2#v}" "${3#v}" | sort -V | tail -1)" = "${3#v}" ]; then printf '%s\t%s\t%s\n' "$1" "${2:-?}" "$3"; fi
}
{
  version_line "tree-sitter program" "$cli_pinned" "$cli_newest"
  cat "$work/ghostty"
  version_line zig "$zig_pinned" "$zig_newest"
  version_line quicklisp "$ql_pinned" "$ql_newest"
  version_line sbcl "$sbcl_pinned" "$sbcl_newest"
  version_line appimagetool "$appimagetool_pinned" "$appimagetool_newest"
  version_line "AppImage runtime" "$runtime_pinned" "$runtime_newest"
  cat "$work/excludelist"
  cat "$work/images"
} > "$work/tools"

table() { awk -F'\t' '{ printf "| %s | %s | %s |\n", $1, $2, $3 }' "$1"; }
grammars_newer="$(wc -l < "$work/grammars")"
{
  echo "#+title: Newer versions of Tuatara's dependencies"
  echo "#+date: $(date '+%Y-%m-%d %H:%M')"
  echo
  echo "Written by tools/check-updates.sh. Nothing has been changed: a pin is moved"
  echo "by hand, a grammar's only after its rules are checked and"
  echo "tools/fuzz-grammars.sh passes."
  echo
  echo "* Libraries and programs"
  if [ -s "$work/tools" ]; then
    echo "| name | built with | newest |"; echo "|-"; table "$work/tools"
  else
    echo "All are the newest."
  fi
  echo
  echo "* Grammars ($grammars_newer of $(grep -v '^#' "$root/syntax/grammars.tsv" | awk -F'\t' '$3 ~ /^[0-9a-f]{40}$/' | wc -l) with newer commits)"
  if [ -s "$work/grammars" ]; then
    echo "| name | pinned | newest |"; echo "|-"; table "$work/grammars"
  fi
} > "$work/report"

# a notice only when the newer versions are not those of the last report
summary() { sed -n '/^| name |/d; /^| /p' "$1" 2>/dev/null | sha256sum; }
changed=1
[ -f "$report" ] && [ "$(summary "$report")" = "$(summary "$work/report")" ] && changed=0
mkdir -p "$(dirname "$report")"
cp "$work/report" "$report"
tools_newer="$(wc -l < "$work/tools")"
message="$tools_newer libraries or programs and $grammars_newer grammars have newer versions"
echo "$message; see $report"
if [ "$changed" = 1 ] && [ $((tools_newer + grammars_newer)) -gt 0 ] && command -v notify-send >/dev/null; then
  notify-send -a Tuatara "Tuatara: newer dependencies" "$message. See build/bumpcheck.org." || true
fi
