#!/usr/bin/env bash

set -euo pipefail

REGISTRY_HOST="quay.io/team-helium/blockchain-node"
UBUNTU_BUILDER="quay.io/team-helium/build-images:ubuntu18-23.3.4.8"
UBUNTU_RUNNER="quay.io/team-helium/build-images:ubuntu18-23.3.4.8"
ALPINE_IMAGE="23.3.4.8-alpine"

BUILD_TARGET=$1
VERSION=$( git describe --abbrev=0 )
DOCKER_BUILD_ARGS="--build-arg VERSION=$VERSION --build-arg BUILD_TARGET=$BUILD_TARGET"

case "$BUILD_TARGET" in
    "docker_node")
        echo "Building docker node"
        DOCKER_BUILD_ARGS="--build-arg BUILDER_IMAGE=$ALPINE_IMAGE --build-arg RUNNER_IMAGE=$ALPINE_IMAGE $DOCKER_BUILD_ARGS"
        DOCKER_NAME="blockchain-node-alpine-$VERSION"
        DOCKERFILE="./Dockerfile"
        ;;
    "rosetta_docker")
        echo "Building rosetta docker"
        DOCKER_BUILD_ARGS="--build-arg BUILDER_IMAGE=$UBUNTU_BUILDER --build-arg RUNNER_IMAGE=$UBUNTU_RUNNER $DOCKER_BUILD_ARGS"
        DOCKER_NAME="blockchain-node-ubuntu18-$VERSION"
        DOCKERFILE=".buildkite/Dockerfile-ubuntu"
        ;;
    "rosetta_testnet_docker")
        echo "Building rosetta testnet docker"
        DOCKER_BUILD_ARGS="--build-arg BUILDER_IMAGE=$UBUNTU_BUILDER --build-arg RUNNER_IMAGE=$UBUNTU_RUNNER $DOCKER_BUILD_ARGS"
        DOCKER_NAME="blockchain-node-testnet-ubuntu18-$VERSION"
        DOCKERFILE=".buildkite/Dockerfile-ubuntu"
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
