#!/bin/sh
# Build the site and serve it locally for preview.
#
# Usage: ./tool/preview.sh [port]
#
# The site uses root-relative paths (/style.css, /img/...), so it is
# served from the directory root, just like production.

set -e

port="${1:-8000}"

cabal run burogu

echo "Preview: http://127.0.0.1:${port}/  (Ctrl-C to stop)"
exec python3 -m http.server "${port}" --bind 127.0.0.1 --directory site
