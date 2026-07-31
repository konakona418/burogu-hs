format:
	find . -name "*.hs" -not -path '*/dist-newstyle/*' | xargs fourmolu --mode inplace