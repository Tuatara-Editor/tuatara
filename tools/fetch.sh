# tools/fetch.sh --- Download a file and check it before it is used, and find Quicklisp: sourced by the other scripts.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
#
# checked_download URL DEST SHA256  makes DEST the file at URL, taken over
# HTTPS only (a redirect too) and kept only when its SHA-256 is SHA256: when
# DEST is there with that hash nothing is downloaded, and a download that has
# another is removed and said so on the error output, returning 1.
#
# quicklisp_setup  prints where Quicklisp's setup.lisp is, as tools/quicklisp.lisp
# finds it: the file QUICKLISP_SETUP names, else ~/quicklisp/ or
# ~/.local/share/quicklisp/; returns 1, saying so, when there is none.

checked_download() {
  if echo "$3  $2" | sha256sum -c --status 2>/dev/null; then return 0; fi
  if ! curl -sSfL --proto '=https' --proto-redir '=https' --retry 3 --connect-timeout 30 --max-time 1200 \
    -o "$2.part" "$1"; then
    rm -f "$2.part"
    echo "$1 could not be downloaded" >&2
    return 1
  fi
  if ! echo "$3  $2.part" | sha256sum -c --status; then
    rm -f "$2.part"
    echo "$1 is not the file expected: not used" >&2
    return 1
  fi
  mv "$2.part" "$2"
}

quicklisp_setup() {
  local setup
  for setup in "${QUICKLISP_SETUP:-}" "$HOME/quicklisp/setup.lisp" "$HOME/.local/share/quicklisp/setup.lisp"; do
    if [ -n "$setup" ] && [ -f "$setup" ]; then echo "$setup"; return 0; fi
  done
  echo "tools/${0##*/}: Quicklisp is not installed: see HACKING.org" >&2
  return 1
}
