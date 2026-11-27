# The port is BACKEND_PORT from the root Makefile
BACKEND_PORT ?= 5001

# Uses GOOGLE_PLACES_KEY from the environment if set, otherwise reads it from pass
all:
	TENDER_PORT=$(BACKEND_PORT) GOOGLE_PLACES_KEY="$${GOOGLE_PLACES_KEY:-$$(pass show GOOGLE_PLACES_KEY | head -n 1)}" cabal run
watch:
	find app src -name "*.hs" | entr -r make all
test:
	cabal test
# fourmolu from mise.toml, the version CI checks with
format:
	mise exec -- fourmolu --mode inplace src app test
