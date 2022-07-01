#!/usr/bin/env bash

set -euo pipefail

BUILD_NET="${BUILD_NET:-mainnet}"
PKGSTEM="${PKGSTEM:-blockchain-node}"

VERSION=$( echo $VERSION_TAG | sed -e "s,${BUILD_NET},," )

PKGNAME="${PKGSTEM}_${VERSION}_amd64.deb"
REPO=$( echo $PKGSTEM | sed -e "s,-,_," )

buildkite-agent artifact download ${PKGNAME} .

curl -u "${PACKAGECLOUD_API_KEY}:" \
     -F "package[distro_version_id]=210" \
     -F "package[package_file]=@${PKGNAME}" \
     https://packagecloud.io/api/v1/repos/helium/${REPO}/packages.json
