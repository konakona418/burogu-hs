#!/bin/sh
# Rebuild the site when sources change; optionally serve it.
#
# Usage: ./tool/watch.sh [--serve PORT]

set -e

serve=""
port="8000"
case "$1" in
    --serve) serve=1; port="${2:-8000}" ;;
esac

marker="/tmp/burogu-watch-marker"
touch "$marker"

rebuild() {
    echo "--- rebuilding ---"
    cabal run burogu
}

rebuild

if [ -n "$serve" ]; then
    python3 -m http.server "$port" --bind 127.0.0.1 --directory site >/dev/null 2>&1 &
    server_pid=$!
    trap 'kill $server_pid 2>/dev/null' EXIT INT TERM
    echo "Preview: http://127.0.0.1:${port}/  (Ctrl-C to stop)"
fi

echo "Watching src/ and config.yaml... (Ctrl-C to stop)"
while true; do
    if find src config.yaml -newer "$marker" 2>/dev/null | grep -q .; then
        touch "$marker"
        rebuild
    fi
    sleep 2
done
