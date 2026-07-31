#!/bin/sh
# Sync src/ with a remote git repository holding the markdown sources
# (e.g. a private repo for posts, while the generated site/ goes to
# GitHub Pages).
#
# Usage: ./tool/sync-src.sh [push|pull] [repo-url]
#
# The repo URL can also come from BUROGU_SRC_REPO in .env (gitignored);
# a command-line argument always wins. pull replaces local src/ with the
# remote contents; push commits the current src/ with an automatic message.

set -e

if [ -f .env ]; then
    . ./.env
fi

action="${1:-pull}"
repo="${2:-${BUROGU_SRC_REPO:-}}"
if [ -z "$repo" ]; then
    echo "error: no repo URL. Pass it as an argument or set BUROGU_SRC_REPO in .env" >&2
    echo "usage: $0 [push|pull] [repo-url]" >&2
    exit 1
fi

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT INT TERM

case "$action" in
    pull)
        git clone --quiet "$repo" "$tmp/src"
        rsync -a --delete --exclude .git "$tmp/src/" src/
        echo "pulled src/ from $repo"
        ;;
    push)
        git clone --quiet "$repo" "$tmp/src"
        git -C "$tmp/src" rm -rq --ignore-unmatch . 2>/dev/null || true
        rsync -a --exclude .git src/ "$tmp/src"/
        cd "$tmp/src"
        git add -A
        if git diff --cached --quiet; then
            echo "nothing to push"
        else
            git commit -q -m "sync: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
            git push --quiet origin HEAD
            echo "pushed src/ to $repo"
        fi
        ;;
    *)
        echo "error: unknown action $action (expected push or pull)" >&2
        exit 1
        ;;
esac
