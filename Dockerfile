FROM heliumsystems/builder-erlang:2 as builder

WORKDIR /app
ENV REBAR_BASE_DIR /app/_build

RUN apk add --update git tar

# build and cache dependencies as their own layer
COPY rebar3 rebar.config rebar.lock ./
RUN ./rebar3 compile

COPY . .
RUN ./rebar3 compile

RUN mkdir -p /opt/rel && \
    ./rebar3 as prod tar && \
    tar -zxvf $REBAR_BASE_DIR/prod/rel/*/*.tar.gz -C /opt/rel

FROM alpine:3.13 as runner

RUN apk add --update openssl libsodium ncurses libstdc++

ENV COOKIE=blockchain_http \
    RELX_OUT_FILE_PATH=/tmp

WORKDIR /opt/blockchain_http
EXPOSE 44158 4467

COPY --from=builder /opt/rel .



ENTRYPOINT ["/opt/blockchain_http/bin/blockchain_http"]
CMD ["foreground"]
