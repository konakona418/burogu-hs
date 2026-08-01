# burogu

A static blog generator written in Haskell.

## Quickstart

```sh
cabal run burogu -- init                  # create src/ and a config.yaml template
cabal run burogu -- build                 # build site/ from src/
cabal run burogu -- preview               # build, then view at http://127.0.0.1:8000
cabal run burogu -- new-post hello        # create a new post
cabal run burogu -- deploy                # build and deploy per config.yaml (rsync or git)
cabal run burogu -- sync push             # sync the site repo with a git remote
cabal run burogu -- doc                   # view the man-page style manual
```

Posts live in `src/_post/*.md`; pages in `src/_pages/*.md`.

## License

AGPL-3.0-or-later
