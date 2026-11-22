# Run the whole app from the repository root.
#
#   make         build and start the backend (port 5001) and frontend (port 1234)
#   make phone   the same, also served over https on your tailnet for phones
#   make test    test both halves
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

.PHONY: all dev phone test
