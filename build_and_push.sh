#!/bin/bash

# Exit immediately if a command exits with a non-zero status
set -e

# Check if script is run with sudo
if [ "$EUID" -ne 0 ]; then
    echo "Please run as root or using sudo"
    exit 1
fi

# Docker Hub username
DOCKER_HUB_USERNAME="jcsawyer"
IMAGE_NAME="erlang-example-webserver"

# Read version from rebar.config release configuration
VERSION=$(awk '/\{release,.*\{webserver,\s*"(.+)"\}/ {match($0, /"(.+)"/, a); print a[1]}' rebar.config)
if [ -z "$VERSION" ]; then
    echo "Could not find version in rebar.config release configuration"
    exit 1
fi

echo "Building version: $VERSION"

# Check if multi-arch-builder exists, create if it doesn't
if ! sudo docker buildx inspect multi-arch-builder > /dev/null 2>&1; then
    echo "Creating new buildx instance: multi-arch-builder"
    sudo docker buildx create --name multi-arch-builder
fi

# Use the multi-arch-builder
sudo docker buildx use multi-arch-builder

# Build the multi-architecture Docker image
echo "Building multi-architecture Docker image..."
sudo docker buildx build --platform linux/amd64,linux/arm64 \
  -t ${DOCKER_HUB_USERNAME}/${IMAGE_NAME}:${VERSION} \
  -t ${DOCKER_HUB_USERNAME}/${IMAGE_NAME}:latest \
  --push .

# Pull the image for local testing (this will pull the appropriate architecture for your machine)
echo "Pulling image for local testing..."
sudo docker pull ${DOCKER_HUB_USERNAME}/${IMAGE_NAME}:${VERSION}

# Test the image locally
echo "Testing image locally..."
sudo docker run -d -p 8080:8080 --name test-webserver ${DOCKER_HUB_USERNAME}/${IMAGE_NAME}:${VERSION}
sleep 5  # Wait for the container to start

# Perform a simple test
if curl -s http://localhost:8080 | grep -q "Request Counter"; then
    echo "Local test passed!"
else
    echo "Local test failed!"
    sudo docker stop test-webserver
    sudo docker rm test-webserver
    exit 1
fi

# Stop and remove the test container
sudo docker stop test-webserver
sudo docker rm test-webserver

echo "Build, test, and push completed successfully!"
echo "Multi-architecture image is now available on Docker Hub as:"
echo "  ${DOCKER_HUB_USERNAME}/${IMAGE_NAME}:${VERSION}"
echo "  ${DOCKER_HUB_USERNAME}/${IMAGE_NAME}:latest"