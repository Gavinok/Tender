# Builds the backend and the frontend, then runs the backend serving both.
# Used by `make deploy` for Google Cloud Run.

# --- Backend ---
FROM haskell:9.12.4-bookworm AS backend
# zlib is frozen to link the system library, found through pkg-config
RUN apt-get update \
 && apt-get install -y --no-install-recommends pkg-config zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /build
# The image's compiler is plain `ghc`, not the ghc-9.12.4 that cabal.project
# names; the pinned versions in cabal.project.freeze keep it reproducible.
# Dependencies are built from the package description alone, so this layer
# is reused until they change. It has to be the library's: the executable's
# dependencies include the library, whose modules aren't copied yet.
COPY backend/tender.cabal backend/cabal.project backend/cabal.project.freeze ./
RUN cabal update && cabal build lib:tender --only-dependencies --with-compiler=ghc
COPY backend/ ./
RUN cabal build exe:tender --with-compiler=ghc \
 && cp "$(cabal list-bin exe:tender --with-compiler=ghc)" /build/tender-server

# --- Frontend ---
FROM node:22-bookworm-slim AS frontend
# spago fetches the package set's registry index with git
RUN apt-get update \
 && apt-get install -y --no-install-recommends git ca-certificates \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /build
COPY frontend/package.json frontend/package-lock.json ./
RUN npm ci
COPY frontend/ ./
RUN npx spago bundle \
 && npx parcel build assets/index.html --dist-dir dist --public-url / --no-source-maps

# --- Runtime ---
FROM debian:bookworm-slim
RUN apt-get update \
 && apt-get install -y --no-install-recommends ca-certificates libgmp10 netbase \
 && rm -rf /var/lib/apt/lists/*
COPY --from=backend /build/tender-server /app/tender-server
COPY --from=frontend /build/dist /app/frontend
# Sessions only need to last as long as this instance, so the database lives
# in /tmp. Cloud Run sets PORT.
ENV PORT=8080 \
    TENDER_LOG_FORMAT=json \
    TENDER_STATIC=/app/frontend \
    TENDER_DB=/tmp/tender.db
USER nobody
CMD ["/app/tender-server"]
