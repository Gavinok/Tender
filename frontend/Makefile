all: build serve

build:
	npm install
	NODE_OPTIONS=--experimental-fetch spago bundle-app --main Main --to assets/app.js

watch:
	NODE_OPTIONS=--experimental-fetch spago bundle-app --main Main --to assets/app.js --watch

serve:
	npx parcel serve assets/index.html

clean:
	rm -rf .cache .spago node_modules .psci_modules output dist

