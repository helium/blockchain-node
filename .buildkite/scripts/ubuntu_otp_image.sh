#!/usr/bin/env bash

set -xeuo pipefail

REGISTRY_HOST=${REGISTRY_HOST:-"quay.io"}
REGISTRY_ORG=${REGISTRY_ORG:-"team-helium"}
MINER_REGISTRY_NAME="$REGISTRY_HOST/$REGISTRY_ORG/build-images"

OTP_VERSION=$( echo "$BUILDKITE_TAG" | sed -e s/otp-// )

DOCKER_NAME="ubuntu18-${OTP_VERSION}"
SHA256_URL="https://github.com/erlang/otp/releases/download/OTP-${OTP_VERSION}/SHA256.txt"
SRC_SHA256=$( curl -L -s -o - "${SHA256_URL}" | grep otp_src | cut -f1 -d' ' )

echo $SRC_SHA256

# we do _not_ want to pass in a build context to docker here, so
# we will redirect standard in to the docker daemon
docker build --build-arg BUILD_VERSION=$OTP_VERSION \
    --build-arg BUILD_SHA256=$SRC_SHA256 \
    -t "helium:$DOCKER_NAME" - < .buildkite/Dockerfile-ubuntu-erlang
docker tag "helium:$DOCKER_NAME" "$MINER_REGISTRY_NAME:$DOCKER_NAME"

docker login -u="team-helium+buildkite" -p="${QUAY_BUILDKITE_PASSWORD}" ${REGISTRY_HOST}
docker push "$MINER_REGISTRY_NAME:$DOCKER_NAME"
