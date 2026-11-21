# Uses YELP_KEY from the environment if set, otherwise reads it from pass
all:
	YELP_KEY="$${YELP_KEY:-$$(pass show YELP_KEY | head -n 1)}" cabal run
watch:
	find app src -name "*.hs" | entr -r make all
test:
	cabal test
