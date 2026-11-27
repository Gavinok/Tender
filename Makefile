# Run the whole app from the repository root.
#
#   make         build and start the backend and frontend (ports below)
#   make phone   the same, also served over https on your tailnet for phones
#   make test    test both halves
#   make format  format both halves, as CI checks
#
# Deploying to Google Cloud Run (see README):
#   make deploy-setup   once per project: services, image registry, the key as a secret
#   make deploy         build, push and deploy the current checkout
#
# Ctrl+C stops both servers.

# The dev ports, named once here and handed to both halves.
# Override with e.g. `make BACKEND_PORT=5002`.
BACKEND_PORT ?= 5001
FRONTEND_PORT ?= 1234
export BACKEND_PORT FRONTEND_PORT

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

format:
	$(MAKE) -C backend format
	$(MAKE) -C frontend format

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

# Docker build flags; CI adds its layer cache
BUILD_FLAGS ?=
# Production images and branch previews are kept apart, so cleaning up old
# previews never touches an image production runs (see deploy-ci-setup)
PREVIEW_IMAGE = $(REGION)-docker.pkg.dev/$(PROJECT)/tender/preview

# Each revision counts Google calls in its own memory, so production and
# each preview have their own caps. Set on every deploy: Cloud Run copies a
# deploy's settings from the one before, which may be a preview's.
PRODUCTION_CAPS = TENDER_MAX_SEARCHES_PER_DAY=300,TENDER_MAX_PHOTOS_PER_DAY=3000
PREVIEW_CAPS = TENDER_MAX_SEARCHES_PER_DAY=30,TENDER_MAX_PHOTOS_PER_DAY=300

# One instance at most: sessions live in that instance's in-memory database,
# so everyone in a session has to reach the same one. Scales to zero when idle.
RUN_FLAGS = --project $(PROJECT) --region $(REGION) \
  --allow-unauthenticated --min-instances 0 --max-instances 1 --memory 512Mi --cpu 1 --port 8080 \
  --set-secrets GOOGLE_PLACES_KEY=$(SECRET):latest

# Build and push $(IMAGE_NAME):$(TAG), with the versions mise.toml pins
IMAGE_NAME ?= $(IMAGE)
image:
	@test -n "$(PROJECT)" || (echo "No Google Cloud project: run gcloud config set project <id>" && exit 1)
	docker buildx build $(BUILD_FLAGS) --push -t $(IMAGE_NAME):$(TAG) \
	  --build-arg GHC_VERSION=$$(scripts/tool-version ghc) \
	  --build-arg NODE_VERSION=$$(scripts/tool-version node) .

# Production: a revision named after the commit (CI adds its run, so a rerun
# gets a new name), given all the traffic
REVISION ?= $(TAG)
deploy: image
	gcloud run deploy $(SERVICE) $(RUN_FLAGS) --image $(IMAGE):$(TAG) \
	  --revision-suffix $(REVISION) --update-env-vars $(PRODUCTION_CAPS)
	@# Named, not "latest", since a preview may have been deployed since
	gcloud run services update-traffic $(SERVICE) --project $(PROJECT) --region $(REGION) --to-revisions $(SERVICE)-$(REVISION)=100

# A branch's preview: a revision with no live traffic and its own URL,
# e.g. make deploy-preview PREVIEW=$$(scripts/preview-tag my-branch)
deploy-preview:
	@test -n "$(PREVIEW)" || (echo "Set PREVIEW to the preview's tag" && exit 1)
	$(MAKE) image IMAGE_NAME=$(PREVIEW_IMAGE)
	gcloud run deploy $(SERVICE) $(RUN_FLAGS) --image $(PREVIEW_IMAGE):$(TAG) \
	  --no-traffic --tag $(PREVIEW) --update-env-vars $(PREVIEW_CAPS)

# Where a preview is live
preview-url:
	@gcloud run services describe $(SERVICE) --project $(PROJECT) --region $(REGION) \
	  --format json | jq -r --arg tag "$(PREVIEW)" '.status.traffic[] | select(.tag == $$tag) | .url'

# Take a preview's URL away, e.g. when its branch is deleted
remove-preview:
	gcloud run services update-traffic $(SERVICE) --project $(PROJECT) --region $(REGION) --remove-tags $(PREVIEW)

# Once per project: lets GitHub Actions deploy, signing in as github-deployer
# through Workload Identity Federation (no stored key). Only workflows in this
# repository (by its id, which survives renames) can sign in.
GITHUB_REPO_ID = 667082632
DEPLOYER = github-deployer@$(PROJECT).iam.gserviceaccount.com
PROJECT_NUMBER = $$(gcloud projects describe $(PROJECT) --format 'value(projectNumber)')
deploy-ci-setup:
	gcloud services enable sts.googleapis.com iamcredentials.googleapis.com --project $(PROJECT)
	gcloud iam service-accounts describe $(DEPLOYER) --project $(PROJECT) >/dev/null 2>&1 \
	  || gcloud iam service-accounts create github-deployer --project $(PROJECT) --display-name "Deploys from GitHub Actions"
	gcloud projects add-iam-policy-binding $(PROJECT) --member serviceAccount:$(DEPLOYER) --role roles/run.admin --condition None >/dev/null
	gcloud projects add-iam-policy-binding $(PROJECT) --member serviceAccount:$(DEPLOYER) --role roles/artifactregistry.writer --condition None >/dev/null
	@# To deploy a service that runs as the default compute service account
	gcloud iam service-accounts add-iam-policy-binding $(PROJECT_NUMBER)-compute@developer.gserviceaccount.com \
	  --project $(PROJECT) --member serviceAccount:$(DEPLOYER) --role roles/iam.serviceAccountUser >/dev/null
	gcloud iam workload-identity-pools describe github --project $(PROJECT) --location global >/dev/null 2>&1 \
	  || gcloud iam workload-identity-pools create github --project $(PROJECT) --location global --display-name GitHub
	gcloud iam workload-identity-pools providers describe tender --project $(PROJECT) --location global --workload-identity-pool github >/dev/null 2>&1 \
	  || gcloud iam workload-identity-pools providers create-oidc tender --project $(PROJECT) --location global \
	       --workload-identity-pool github --issuer-uri https://token.actions.githubusercontent.com \
	       --attribute-mapping google.subject=assertion.sub,attribute.repository_id=assertion.repository_id \
	       --attribute-condition "assertion.repository_id == '$(GITHUB_REPO_ID)'"
	gcloud iam service-accounts add-iam-policy-binding $(DEPLOYER) --project $(PROJECT) --role roles/iam.workloadIdentityUser \
	  --member principalSet://iam.googleapis.com/projects/$(PROJECT_NUMBER)/locations/global/workloadIdentityPools/github/attribute.repository_id/$(GITHUB_REPO_ID) >/dev/null
	@# Keep storage within the free 0.5 GB without touching production's image
	gcloud artifacts repositories set-cleanup-policies tender --project $(PROJECT) --location $(REGION) \
	  --policy deploy/cleanup-policy.json --no-dry-run
	@echo "Provider: projects/$(PROJECT_NUMBER)/locations/global/workloadIdentityPools/github/providers/tender"
	@echo "Deployer: $(DEPLOYER)"

.PHONY: all dev phone test format deploy-setup deploy-secret image deploy deploy-preview preview-url remove-preview deploy-ci-setup
