.PHONY: all clean release

# Default target
all: release

# Clean the project
clean:
	@echo "Cleaning project..."
	@rebar3 clean
	@rm -rf _build release

# Compile the project
compile:
	@echo "Compiling project..."
	@rebar3 compile

# Create a release
release: compile
	@echo "Creating release..."
	@rebar3 as prod release
	@rebar3 as prod tar
	@mkdir -p release
	@echo "Copying release tarball..."
	@latest_tarball=$$(ls -t _build/prod/rel/webserver/*.tar.gz | head -n1); \
	if [ -n "$$latest_tarball" ]; then \
		cp "$$latest_tarball" release/; \
		echo "Copied $$(basename $$latest_tarball) to release directory"; \
	else \
		echo "No tarball found in _build/prod/rel/webserver/"; \
		exit 1; \
	fi
	@cp webserver.config release/
	@echo "Release created in ./release directory"

# Run the application (for development)
run:
	@echo "Running application..."
	@rebar3 shell

# Display the current version
version:
	@echo "Current version:"
	@grep -m 1 version src/*.app.src | cut -d'"' -f2