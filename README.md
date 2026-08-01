# burogu

A static blog generator written in Haskell.

## Usage

```sh
make build                  # build site/ from src/
make preview                # build + preview at http://127.0.0.1:8000
make watch                  # rebuild on change
make init                   # create src/ with a sample post

cabal run burogu -- new-post <slug> [--draft]   # create a post
cabal run burogu -- deploy [user@host:/path]     # build + rsync to server
cabal run burogu -- sync [push|pull] [repo]      # sync src/ with a git repo
```

Posts live in `src/_post/*.md` with YAML frontmatter (`title`, `date`, `tags`, `description`, `draft`); everything else under `src/` is copied verbatim. Pages live in `src/_pages/*.md` (each becomes `/slug/` with a nav link; `404.md` is special-cased to `/404.html` without a nav link). Site configuration in `config.yaml` (`init` writes a template to the directory above `src`); optional `deployTarget` and `srcRepo` fields give `deploy`/`sync` their targets.

## License

AGPL-3.0-or-later
