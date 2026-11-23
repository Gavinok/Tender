all: build serve

build:
	npm install
	npx spago bundle

watch:
	find src -name "*.purs" -o -name "*.js" | entr -r npx spago bundle

serve:
	npx parcel serve assets/index.html --port 1234

test:
	npx spago test

clean:
	rm -rf .cache .spago node_modules output dist .parcel-cache

# Serve the dev server over HTTPS on your tailnet, e.g. to try it on a
# phone (browsers only give the location to https pages). Only devices on
# the tailnet can reach it. Uses sudo unless you have run
# `sudo tailscale set --operator=$USER` once.
SHARE_PORT ?= 8443

share:
	tailscale serve --bg --https=$(SHARE_PORT) http://localhost:1234 \
	  || sudo tailscale serve --bg --https=$(SHARE_PORT) http://localhost:1234
	@tailscale serve status

unshare:
	tailscale serve --https=$(SHARE_PORT) off \
	  || sudo tailscale serve --https=$(SHARE_PORT) off

.PHONY: all build watch serve test clean share unshare dist

# Production build into dist/, as the Dockerfile does
dist:
	npx spago bundle
	npx parcel build assets/index.html --dist-dir dist --public-url / --no-source-maps
