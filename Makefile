.PHONY: build test format

build:
	cabal run burogu -- build

test:
	cabal test

format:
	find . -name "*.hs" -not -path '*/dist-newstyle/*' | xargs fourmolu --mode inplace
