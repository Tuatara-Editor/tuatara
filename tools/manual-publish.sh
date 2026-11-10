#!/usr/bin/env bash
# tools/manual-publish.sh --- Publish the manual's web pages on GitHub Pages.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Build the pages (tools/manual-html.sh), refuse them if they name this
# account or its home folder, and commit them as the whole of the branch
# gh-pages, one signed commit on top of what REMOTE has, which is then
# pushed. GitHub Pages serves that branch.
# Usage: tools/manual-publish.sh REMOTE [--no-push]
set -euo pipefail
cd "$(dirname "$0")/.."
remote="${1:?Usage: tools/manual-publish.sh REMOTE [--no-push]}"
push=yes
[ "${2:-}" = --no-push ] && push=no
tools/manual-html.sh
site="$PWD/build/manual"
[ -f "$site/index.html" ] || { echo "No $site/index.html: the build made no pages" >&2; exit 1; }
for word in "$HOME" "$(id -un)"; do
  if grep -rlF -- "$word" "$site" > /dev/null; then
    echo "The pages name \"$word\" in:" >&2
    grep -rlF -- "$word" "$site" >&2
    exit 1
  fi
done
version="$(git describe --tags --always)"
pages="$(mktemp -d "$PWD/build/.manual-pages.XXXXXX")"
# the branch a first publish starts from nothing, gone again when this ends
branch="manual-pages-$$"
trap 'git worktree remove --force "$pages" 2> /dev/null || rm -rf "$pages"
      git branch -D --quiet "$branch" 2> /dev/null || true' EXIT
status=0
git ls-remote --exit-code --heads "$remote" gh-pages > /dev/null || status=$?
case $status in
  0) git fetch --quiet "$remote" gh-pages
     git worktree add --quiet --detach "$pages" FETCH_HEAD ;;
  2) git worktree add --quiet --detach "$pages"
     git -C "$pages" checkout --quiet --orphan "$branch" ;;
  *) echo "Could not read the branches of $remote" >&2; exit 1 ;;
esac
git -C "$pages" rm -r -f --quiet --ignore-unmatch .
cp -r "$site/." "$pages/"
rm -f "$pages/tuatara-manual.pdf"
# GitHub Pages would otherwise run the pages through Jekyll
touch "$pages/.nojekyll"
git -C "$pages" add --all
if git -C "$pages" diff --cached --quiet; then
  echo "The pages are as $remote has them already"
  exit 0
fi
git -C "$pages" commit --quiet -S -m "Manual for Tuatara $version"
if [ "$push" = yes ]; then
  git -C "$pages" push "$remote" HEAD:refs/heads/gh-pages
else
  echo "Committed in $pages, not pushed: git -C $pages push $remote HEAD:refs/heads/gh-pages"
  trap - EXIT
fi
