#!/bin/sh
# Create a new post from a template.
#
# Usage: ./tool/new-post.sh <slug> [--draft]
#
# Regular posts get a YYYY-MM-DD- filename prefix and a date in the
# frontmatter. Drafts get no date at all (the generator exempts drafts
# from the missing-date rule); add the date prefix when publishing.

set -e

draft=""
for arg in "$@"; do
    case "$arg" in
        --draft) draft=1 ;;
        -*) echo "error: unknown option $arg" >&2; exit 1 ;;
        *) slug="$arg" ;;
    esac
done

if [ -z "$slug" ]; then
    echo "usage: $0 <slug> [--draft]" >&2
    exit 1
fi

case "$slug" in
    */*|*'?'*|*'#'*|*'%'*|*' '*)
        echo "error: slug contains a reserved character (/, ?, #, %, space)" >&2
        exit 1 ;;
esac

post_dir="src/_post"

if [ -n "$draft" ]; then
    file="$post_dir/$slug.md"
    frontmatter="---
title: $slug
draft: true
# tags: []
# description: 
---

"
else
    date="$(date +%F)"
    file="$post_dir/${date}-${slug}.md"
    frontmatter="---
title: $slug
date: $date
# tags: []
# description: 
---

"
fi

if [ -e "$file" ]; then
    echo "error: $file already exists" >&2
    exit 1
fi

printf '%s' "$frontmatter" > "$file"
echo "created $file"
