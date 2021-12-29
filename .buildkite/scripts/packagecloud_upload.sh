#!/usr/bin/env bash

set -euo pipefail

VERSION=$VERSION_TAG

PKGNAME="blockchain-node_${VERSION}_amd64.deb"

buildkite-agent artifact download ${PKGNAME} .

curl -u "${PACKAGECLOUD_API_KEY}:" \
     -F "package[distro_version_id]=190" \
     -F "package[package_file]=@${PKGNAME}" \
     https://packagecloud.io/api/v1/repos/helium/blockchain_node/packages.json
