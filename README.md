# burogu

A static blog generator written in Haskell.

## Quick start

```sh
make init                  # create src/ with a sample post (refuses if not empty)
make dev                   # rebuild on change + preview at http://127.0.0.1:8000
```

## Commands (Makefile)

```sh
make build                 # build site/ from src/ (cabal run burogu)
make test                  # run the test suite
make preview               # build once, then serve site/ at http://127.0.0.1:8000
make watch                 # rebuild automatically when src/ or config.yaml changes
make dev                   # watch + preview together (Ctrl-C stops both)
make clean                 # remove the site/ output directory
make init                  # initialize an src/ tree
make new-post              # create a new post
make deploy                # build + rsync to the server
make format                # fourmolu on all .hs files
```

Targets taking arguments pass them through `ARGS`:

```sh
make init ARGS="mycontent"                 # init into a custom directory
make new-post ARGS="my-slug --draft"       # draft posts get no date
make deploy ARGS="user@host:/var/www/site" # overrides .env target
make sync ARGS="push"                      # commit src/ to the private source repo
```

## CLI

```sh
cabal run burogu -- --help                 # usage and defaults
cabal run burogu -- [--config PATH] [--src DIR] [--out DIR]
```

Defaults: `config.yaml`, `src`, `site`. Example: `cabal run burogu -- --src content --out dist`.

## Tool scripts

```sh
./tool/init-src.sh [dir]                   # initialize src/ structure (default: src)
./tool/new-post.sh <slug> [--draft]        # create a post from a template
./tool/watch.sh [--serve PORT]             # auto-rebuild, optionally serve
./tool/preview.sh [port]                   # build once + serve
./tool/deploy.sh [user@host:/path]         # build + rsync --delete
./tool/sync-src.sh [push|pull] [repo-url]  # sync src/ with a remote git repo
```

Deploy target priority: command-line argument, then `BUROGU_DEPLOY_TARGET` in `.env` (gitignored; see `.env.example`). The private source repo for sync-src.sh is configured the same way via `BUROGU_SRC_REPO` (e.g. a private repo holding `src/`, while `site/` goes to a GitHub Pages repo).

## Content

Posts live in `src/_post/*.md` with YAML frontmatter (`title`, `date`, `tags`, `description`, `draft`). Dates may come from the filename prefix (`YYYY-MM-DD-slug.md`) instead of the frontmatter. Everything else under `src/` is copied to the output verbatim; images and other assets are referenced with root-absolute paths (`/img/00/1.png`).

Site configuration in `config.yaml`:

```yaml
siteName: burogu
baseUrl: https://example.com
siteLang: zh-CN
tagsLabel: Tags
theme:
  math: mathjax          # none | mathjax | katex
  extraCss: [theme.css]  # appended to style.css
  extraJs: [theme.js]    # deferred <script> on every page
```

## Features

- Markdown via pandoc, syntax highlighting, MathJax/KaTeX math
- Tags, Atom feed, sitemap, robots.txt, custom 404, Open Graph
- Dark mode via design tokens; themeable through tokens, data hooks (`--tag-count`) and custom CSS/JS
- Two-phase builds: nothing is written unless every post parses

## License

AGPL-3.0-or-later
