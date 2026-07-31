.PHONY: build test preview watch dev clean new-post deploy init format

build:
	cabal run burogu

test:
	cabal test

preview:
	./tool/preview.sh

watch:
	./tool/watch.sh

dev:
	./tool/watch.sh --serve 8000

clean:
	rm -rf site

init:
	./tool/init-src.sh $(ARGS)

new-post:
	./tool/new-post.sh $(ARGS)

deploy:
	./tool/deploy.sh $(ARGS)

format:
	find . -name "*.hs" -not -path '*/dist-newstyle/*' | xargs fourmolu --mode inplace
