# burogu

A static blog generator written in Haskell.

## Usage

```sh
make build                  # build site/ from src/
make preview                # build + preview at http://127.0.0.1:8000
make watch                  # rebuild on change
make init                   # create src/ with a sample post

cabal run burogu -- new-post <slug> [--draft]   # create a post
cabal run burogu -- deploy                # build + deploy per config.yaml (rsync or git)
cabal run burogu -- sync [push|pull] [repo]      # sync the site repo (config + src) with a git repo
```

Posts live in `src/_post/*.md` with YAML frontmatter (`title`, `date`, `tags`, `description`, `draft`); everything else under `src/` is copied verbatim. Pages live in `src/_pages/*.md`, each becoming `/slug/` with a nav link ordered by the frontmatter `priority` (default 100, ties broken lexicographically; `title` is the nav label). Special pages are declared via `redirectAs`: `/tags/` (tag index), `/archive/` (yearly timeline), `/search/` (client-side search over `search.json`), `/404.html` (404 page); their slug URL redirects there when it differs. A search page can be replaced by defining `window.buroguSearch` in `theme.extraJs`. Site configuration in `config.yaml` (`init` writes a template to the directory above `src`): a `theme` section (`preset` picks the built-in theme `aria` or `shaft`, `fonts` overrides font stacks/sizes and optionally embeds font files, `extraCss`/`extraJs` layer custom styles and scripts), a `deploy` section (either `target` for rsync to a VPS, or `repo` + `branch` for a git branch like `gh-pages`), and optional `srcRepo` for `sync`.

## License

AGPL-3.0-or-later
