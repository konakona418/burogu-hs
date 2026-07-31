# burogu

Static blog generator in Haskell.

## Usage

```sh
cabal run burogu          # build site/ from src/
make dev                  # rebuild on change + local preview (http://127.0.0.1:8000)
make deploy ARGS="user@host:/path"   # build + rsync to server
./tool/new-post.sh my-slug [--draft]
```

Posts live in `src/_post/*.md` with YAML frontmatter (`title`, `date`, `tags`, `description`, `draft`). Site config in `config.yaml`.

## Features

- Markdown via pandoc, syntax highlighting, MathJax/KaTeX math
- Tags, Atom feed, sitemap, Open Graph, dark mode
- Theme support with design tokens, data hooks, custom CSS/JS

## License

AGPL-3.0
