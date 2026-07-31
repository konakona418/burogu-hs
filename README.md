# burogu

A static blog generator written in Haskell.

## Quick start

```sh
make init       # create src/ with a sample post (refuses if not empty)
make build      # build site/ from src/
make preview    # build, then serve at http://127.0.0.1:8000
```

## Commands

Development tasks live in the Makefile:

```sh
make build                  # build site/ from src/
make test                   # run the test suite
make format                 # fourmolu on all .hs files
make init                   # initialize an src/ tree
make preview                # build once, then serve at http://127.0.0.1:8000
make watch                  # rebuild when src/ or config.yaml changes
```

Site operations are subcommands of the binary:

```sh
cabal run burogu -- build [--config PATH] [--src DIR] [--out DIR]
cabal run burogu -- clean [--out DIR]
cabal run burogu -- preview [--port PORT]
cabal run burogu -- watch [--serve PORT]
cabal run burogu -- init [DIR]
cabal run burogu -- new-post <slug> [--draft]
cabal run burogu -- deploy [TARGET]
cabal run burogu -- sync [push|pull] [REPO]
cabal run burogu -- --help                 # full usage
```

Every subcommand supports `--help`. Build paths default to `config.yaml`, `src`, `site`.

Deploy target priority: command-line argument, then `BUROGU_DEPLOY_TARGET` in `.env` (gitignored; see `.env.example`). The private source repo for `sync` is configured the same way via `BUROGU_SRC_REPO` (e.g. a private repo holding `src/`, while `site/` goes to a GitHub Pages repo).

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
