FROM erlang:22.3.2-alpine as builder

RUN apk add --no-cache --update \
    git tar build-base linux-headers autoconf automake libtool pkgconfig \
    dbus-dev bzip2 bison flex gmp-dev cmake lz4 libsodium-dev openssl-dev \
    sed curl cargo

# Install Rust toolchain
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

WORKDIR /usr/src/node

ENV CC=gcc CXX=g++ CFLAGS="-U__sun__" \
    ERLANG_ROCKSDB_OPTS="-DWITH_BUNDLE_SNAPPY=ON -DWITH_BUNDLE_LZ4=ON" \
    ERL_COMPILER_OPTIONS="[deterministic]" \
    PATH="/root/.cargo/bin:$PATH" \
    RUSTFLAGS="-C target-feature=-crt-static"

# Add our code
ADD . /usr/src/node/

RUN ./rebar3 as docker_node tar
RUN mkdir -p /opt/docker
RUN tar -zxvf _build/docker_node/rel/*/*.tar.gz -C /opt/docker
RUN mkdir -p /opt/docker/update

FROM erlang:22.3.2-alpine as runner

RUN apk add --no-cache --update ncurses dbus gmp libsodium gcc
RUN ulimit -n 64000

WORKDIR /opt/node

ENV COOKIE=node \
    # Write files generated during startup to /tmp
    RELX_OUT_FILE_PATH=/tmp \
    # add miner to path, for easy interactions
    PATH=$PATH:/opt/node/bin

COPY --from=builder /opt/docker /opt/node

ENTRYPOINT ["/opt/node/bin/blockchain_node"]
CMD ["foreground"]
