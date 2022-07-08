#!/usr/bin/env bash

set -euo pipefail

BUILD_NET="${BUILD_NET:-mainnet}"
RELEASE_TARGET="${RELEASE_TARGET:-prod}"
PKGSTEM="${PKGSTEM:-blockchain-node}"

VERSION=$( echo $VERSION_TAG | sed -e "s,${BUILD_NET},," )

DIAGNOSTIC=1 ./rebar3 as ${RELEASE_TARGET} release -v ${VERSION} -n blockchain_node

wget -O /tmp/genesis https://snapshots.helium.wtf/genesis.${BUILD_NET}

if [ ! -d /opt/blockchain_node/etc ]; then
    mkdir -p /opt/blockchain_node/etc
fi

if [ ! -f /opt/blockchain_node/etc/node.config ]; then
    touch /opt/blockchain_node/etc/node.config
fi

fpm -n ${PKGSTEM} \
    -v "${VERSION}" \
    -s dir \
    -t deb \
    --depends libssl1.1 \
    --depends libsodium23 \
    --depends libncurses5 \
    --depends dbus \
    --depends libstdc++6 \
    --deb-systemd deb/blockchain-node.service \
    --before-install deb/before_install.sh \
    --after-install deb/after_install.sh \
    --after-remove deb/after_remove.sh \
    --before-upgrade deb/before_upgrade.sh \
    --after-upgrade deb/after_upgrade.sh \
    --deb-no-default-config-files \
    --deb-systemd-enable \
    --deb-systemd-auto-start \
    --deb-systemd-restart-after-upgrade \
    --deb-user helium \
    --deb-group helium \
    --config-files /opt/blockchain_node/etc/node.config \
    _build/${RELEASE_TARGET}/rel/=/opt \
    /tmp/genesis=/opt/miner/update/genesis

rm -f /tmp/genesis
