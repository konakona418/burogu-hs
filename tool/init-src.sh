#!/bin/sh
# Initialize an src/ tree in the given directory (default: src).
#
# Usage: ./tool/init-src.sh [dir]
#
# Refuses to run when the directory already exists and is not empty.

set -e

dir="${1:-src}"

if [ -e "$dir" ]; then
    if [ -n "$(ls -A "$dir" 2>/dev/null)" ]; then
        echo "error: $dir is not empty; refusing to initialize" >&2
        exit 1
    fi
else
    mkdir -p "$dir"
fi

mkdir -p "$dir/_post" "$dir/img/00"

cat > "$dir/_post/2026-07-31-hello-world.md" <<'EOF'
---
title: Hello, World
date: 2026-07-31
tags: [getting-started, demo]
description: A sample post showcasing the features of burogu
---

Welcome! This sample post demonstrates what burogu can do.

## Text

**Bold**, *italic*, and a [link](/posts/hello-world/). Bare URLs are linked automatically: https://example.com

> A blockquote, for good measure.

## Code

```c
#include <stdio.h>

int main(void) {
    printf("Hello, burogu!\n");
    return 0;
}
```

## Math

Inline $E = mc^2$ and a display formula:

$$
\int_0^1 x^2 \, dx = \frac{1}{3}
$$

## Image

![Sample image](/img/00/1.png)

## Table

| Feature | Status |
|---------|--------|
| Markdown | yes |
| Syntax highlighting | yes |
| Math | yes |
EOF

printf '%s' 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR4nGNgYGAAAAAEAAH2FzhVAAAAAElFTkSuQmCC' | base64 -d > "$dir/img/00/1.png"

cat > "$dir/CNAME" <<'EOF'
example.com
EOF

cat > "$dir/theme.css" <<'EOF'
/* Your custom styles are appended to style.css.
   Enable them in config.yaml: theme.extraCss: [theme.css] */
EOF

echo "initialized $dir:"
echo "  $dir/_post/2026-07-31-hello-world.md"
echo "  $dir/img/00/1.png"
echo "  $dir/CNAME"
echo "  $dir/theme.css"
echo "edit config.yaml if needed, then run: make dev"
