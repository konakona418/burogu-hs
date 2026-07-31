#!/usr/bin/env bash
# Generate the source archive and build the Arch package with makepkg.
#
# Usage: packaging/build.sh [makepkg-extra-args...]
#
# Uses the ghcup toolchain: dependency checks are skipped (-d) because
# ghc and cabal-install are not pacman packages. Commit first — the
# archive only contains committed files.

set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root"

version="$(sed -n 's/^version:[[:space:]]*//p' burogu.cabal)"
archive="packaging/burogu-${version}.tar.gz"

if [ -n "$(git status --porcelain)" ]; then
    echo "warning: working tree is not clean; the archive will only contain committed files" >&2
fi

rm -f "$archive" packaging/*.pkg.tar.zst
git archive --format=tar.gz --prefix="burogu-${version}/" -o "$archive" HEAD
echo "created $archive"

cd packaging
exec makepkg -d -i "$@"
