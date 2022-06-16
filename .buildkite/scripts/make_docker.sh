#!/usr/bin/env bash

set -euo pipefail

REGISTRY_HOST="quay.io/team-helium/blockchain-node"
BUILDER_IMAGE="erlang:24-alpine"
RUNNER_IMAGE="alpine:3.16"

BUILD_TARGET=$1
VERSION=$VERSION_TAG
DOCKER_BUILD_ARGS="--build-arg VERSION=$VERSION --build-arg BUILD_TARGET=$BUILD_TARGET"

case "$BUILD_TARGET" in
    "docker_node")
        echo "Building docker node"
        DOCKER_BUILD_ARGS="--build-arg BUILDER_IMAGE=$BUILDER_IMAGE --build-arg RUNNER_IMAGE=$RUNNER_IMAGE $DOCKER_BUILD_ARGS"
        DOCKER_NAME="blockchain-node-$VERSION"
        DOCKERFILE="./Dockerfile"
        ;;
    *)
        echo "I don't know how to build $BUILD_TARGET"
        exit 1
        ;;
esac

docker build $DOCKER_BUILD_ARGS -t "helium:${DOCKER_NAME}" -f "$DOCKERFILE" .
docker tag "helium:$DOCKER_NAME" "$REGISTRY_HOST:$DOCKER_NAME"
docker login -u="team-helium+buildkite" -p="${QUAY_BUILDKITE_PASSWORD}" quay.io
docker push "$REGISTRY_HOST:$DOCKER_NAME"
