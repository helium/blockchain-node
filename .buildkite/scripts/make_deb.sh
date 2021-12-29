#!/usr/bin/env bash

set -euo pipefail

VERSION=$VERSION_TAG

DIAGNOSTIC=1 ./rebar3 as $1 release -v ${VERSION} -n blockchain_node

fpm -n blockchain-node \
    -v ${VERSION} \
    -s dir \
    -t deb \
    --depends libssl1.1 \
    --depends libsodium23 \
    --depends libc6 \
    --depends libncurses5 \
    --depends libgcc1 \
    --depends libstdc++6 \
    --depends libsctp1 \
    --deb-systemd deb/blockchain-node.service \
    --deb-no-default-config-files \
    _build/$1/rel/=/var/helium
