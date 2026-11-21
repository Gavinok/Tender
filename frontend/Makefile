all: build serve

build:
	npm install
	npx spago bundle

watch:
	find src -name "*.purs" -o -name "*.js" | entr -r npx spago bundle

serve:
	npx parcel serve assets/index.html

test:
	npx spago test

clean:
	rm -rf .cache .spago node_modules output dist .parcel-cache
