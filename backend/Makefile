# Uses GOOGLE_PLACES_KEY from the environment if set, otherwise reads it from pass
all:
	GOOGLE_PLACES_KEY="$${GOOGLE_PLACES_KEY:-$$(pass show GOOGLE_PLACES_KEY | head -n 1)}" cabal run
watch:
	find app src -name "*.hs" | entr -r make all
test:
	cabal test
