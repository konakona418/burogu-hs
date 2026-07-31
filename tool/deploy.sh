#!/bin/sh
# Build and deploy the site via rsync.
#
# Usage: ./tool/deploy.sh [user@host:/path]
#
# The target can also be set via BUROGU_DEPLOY_TARGET in .env (gitignored);
# a command-line argument always wins.

set -e

if [ -f .env ]; then
    . ./.env
fi

target="${1:-${BUROGU_DEPLOY_TARGET:-}}"
if [ -z "$target" ]; then
    echo "error: no deploy target." >&2
    echo "usage: $0 <user@host:/path>  or set BUROGU_DEPLOY_TARGET in .env" >&2
    exit 1
fi

cabal run burogu
exec rsync -avz --delete site/ "$target/"
