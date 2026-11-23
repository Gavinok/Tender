# Run the whole app from the repository root.
#
#   make         build and start the backend (port 5001) and frontend (port 1234)
#   make phone   the same, also served over https on your tailnet for phones
#   make test    test both halves
#
# Deploying to Google Cloud Run (see README):
#   make deploy-setup   once per project: services, image registry, the key as a secret
#   make deploy         build, push and deploy the current checkout
#
# Ctrl+C stops both servers.

all: dev

# Starts both in the background and waits; stopping make stops both
dev:
	@trap 'kill 0' INT TERM EXIT; \
	$(MAKE) -C backend & \
	$(MAKE) -C frontend & \
	wait

# Sharing stays on after the servers stop; `make -C frontend unshare` turns it off
phone:
	$(MAKE) -C frontend share
	@echo "Still shared after you stop the servers; run 'make -C frontend unshare' to stop sharing."
	$(MAKE) dev

test:
	$(MAKE) -C backend test
	$(MAKE) -C frontend test

# --- Google Cloud Run ---

PROJECT ?= $(shell gcloud config get-value project 2>/dev/null)
REGION ?= us-west1
SERVICE ?= tender
IMAGE = $(REGION)-docker.pkg.dev/$(PROJECT)/tender/tender
TAG ?= $(shell git rev-parse --short HEAD)
SECRET = google-places-key

deploy-setup:
	gcloud services enable run.googleapis.com artifactregistry.googleapis.com secretmanager.googleapis.com --project $(PROJECT)
	gcloud artifacts repositories describe tender --location $(REGION) --project $(PROJECT) >/dev/null 2>&1 \
	  || gcloud artifacts repositories create tender --repository-format docker --location $(REGION) --project $(PROJECT)
	gcloud auth configure-docker $(REGION)-docker.pkg.dev --quiet
	gcloud secrets describe $(SECRET) --project $(PROJECT) >/dev/null 2>&1 \
	  || pass show GOOGLE_PLACES_KEY | head -n 1 | tr -d '\n' | gcloud secrets create $(SECRET) --data-file - --project $(PROJECT)
	@# Cloud Run runs as the project's default compute service account
	gcloud secrets add-iam-policy-binding $(SECRET) --project $(PROJECT) --role roles/secretmanager.secretAccessor \
	  --member serviceAccount:$$(gcloud projects describe $(PROJECT) --format 'value(projectNumber)')-compute@developer.gserviceaccount.com

# Store a new Places key from pass; the next deploy uses it
deploy-secret:
	pass show GOOGLE_PLACES_KEY | head -n 1 | tr -d '\n' | gcloud secrets versions add $(SECRET) --data-file - --project $(PROJECT)

# One instance at most: sessions live in that instance's in-memory database,
# so everyone in a session has to reach the same one. Scales to zero when idle.
deploy:
	@test -n "$(PROJECT)" || (echo "No Google Cloud project: run gcloud config set project <id>" && exit 1)
	docker build -t $(IMAGE):$(TAG) .
	docker push $(IMAGE):$(TAG)
	gcloud run deploy $(SERVICE) --project $(PROJECT) --region $(REGION) --image $(IMAGE):$(TAG) \
	  --allow-unauthenticated --min-instances 0 --max-instances 1 --memory 512Mi --cpu 1 --port 8080 \
	  --set-secrets GOOGLE_PLACES_KEY=$(SECRET):latest

.PHONY: all dev phone test deploy-setup deploy-secret deploy
