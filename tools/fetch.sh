# tools/fetch.sh --- Download a file and check it before it is used, and find Quicklisp: sourced by the other scripts.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
#
# checked_download URL DEST SHA256  makes DEST the file at URL, taken over
# HTTPS only (a redirect too) and kept only when its SHA-256 is SHA256: when
# DEST is there with that hash nothing is downloaded, and a download that has
# another is removed and said so on the error output, returning 1.
#
# https_curl ARGS...  runs curl quietly, failing on an HTTP error, following
# redirects and retrying, over HTTPS only (a redirect too).
#
# https_get URL  prints what is at URL, taken as https_curl does within a
# minute, or nothing when it cannot be had.
#
# github_api PATH  prints the answer of GitHub's REST API for PATH, read-only:
# by gh when it answers (installed and logged in), else by curl over HTTPS,
# with GITHUB_TOKEN when it is set (given to curl on its input, never on a
# command line other accounts can read); prints nothing and returns 1 when
# neither answers.
#
# https_git ARGS...  runs git taking repositories over HTTPS only, and never
# running a program a checkout names for itself (a file monitor, hooks, an
# SSH command), as a fetched repository's .git could name one.
#
# quicklisp_setup  prints where Quicklisp's setup.lisp is, as tools/quicklisp.lisp
# finds it: the file QUICKLISP_SETUP names, else ~/quicklisp/ or
# ~/.local/share/quicklisp/; returns 1, saying so, when there is none.

https_curl() {
  curl -sSfL --proto '=https' --proto-redir '=https' --retry 3 "$@"
}

https_get() {
  https_curl --max-time 60 "$1" 2>/dev/null || true
}

github_api() {
  local answer
  if command -v gh >/dev/null && answer="$(timeout 60 gh api "$1" 2>/dev/null)"; then
    printf '%s\n' "$answer"
    return 0
  fi
  { [ -z "${GITHUB_TOKEN:-}" ] || printf 'header = "Authorization: Bearer %s"\n' "$GITHUB_TOKEN"; } |
    https_curl --max-time 60 --config - -H 'Accept: application/vnd.github+json' "https://api.github.com/$1" 2>/dev/null
}

https_git() {
  git -c protocol.allow=never -c protocol.https.allow=always -c core.fsmonitor=false -c core.hooksPath=/dev/null \
      -c core.sshCommand=false "$@"
}

checked_download() {
  if echo "$3  $2" | sha256sum -c --status 2>/dev/null; then return 0; fi
  if ! https_curl --connect-timeout 30 --max-time 1200 \
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
