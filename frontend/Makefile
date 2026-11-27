# Ports come from the root Makefile
BACKEND_PORT ?= 5001
FRONTEND_PORT ?= 1234

all: build serve

build:
	npm install
	npx spago bundle

watch:
	find src -name "*.purs" -o -name "*.js" | entr -r npx spago bundle

# Parcel reads its /api proxy from .proxyrc.json, which is written here so it follows BACKEND_PORT
serve:
	printf '{"/api": {"target": "http://localhost:%s"}}\n' $(BACKEND_PORT) > .proxyrc.json
	npx parcel serve assets/index.html --port $(FRONTEND_PORT)

test: lint
	npx spago test

# ESLint and the type check of the hand-written JavaScript
lint:
	npm run lint

# purs-tidy from package.json, the version CI checks with
format:
	npx purs-tidy format-in-place "src/**/*.purs" "test/**/*.purs"

clean:
	rm -rf .cache .spago node_modules output dist .parcel-cache

# Serve the dev server over HTTPS on your tailnet, e.g. to try it on a
# phone (browsers only give the location to https pages). Only devices on
# the tailnet can reach it. Uses sudo unless you have run
# `sudo tailscale set --operator=$USER` once.
SHARE_PORT ?= 8443

share:
	tailscale serve --bg --https=$(SHARE_PORT) http://localhost:$(FRONTEND_PORT) \
	  || sudo tailscale serve --bg --https=$(SHARE_PORT) http://localhost:$(FRONTEND_PORT)
	@tailscale serve status

unshare:
	tailscale serve --https=$(SHARE_PORT) off \
	  || sudo tailscale serve --https=$(SHARE_PORT) off

.PHONY: all build watch serve test lint format clean share unshare dist fit icons

# Production build into dist/, as the Dockerfile does
dist:
	npx spago bundle
	npx parcel build assets/index.html --dist-dir dist --public-url / --no-source-maps

# Whether each screen fits on a phone (test/fit), against a fresh dist/.
# Needs the browser once: npx playwright install chromium
fit: dist
	npx playwright test

# The app's PNG icons, from the SVGs beside them. Needs rsvg-convert (librsvg).
ICONS = assets/icons

icons:
	rsvg-convert -w 192 $(ICONS)/icon.svg -o $(ICONS)/icon-192.png
	rsvg-convert -w 512 $(ICONS)/icon.svg -o $(ICONS)/icon-512.png
	rsvg-convert -w 512 $(ICONS)/icon-maskable.svg -o $(ICONS)/icon-maskable-512.png
	rsvg-convert -w 180 $(ICONS)/icon-maskable.svg -o $(ICONS)/apple-touch-icon.png
