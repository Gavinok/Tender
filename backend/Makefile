all:
	cabal run
watch:
	find app src -name "*.hs" | entr -r make all
test:
	cabal test
