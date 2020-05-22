FROM ubuntu:18.04

EXPOSE 44158 4467

COPY blockchain-node*.deb /
RUN apt update
RUN apt install -y iproute2
RUN apt install -y ./blockchain-node*.deb
RUN rm -rf /var/apt/cache
RUN rm -f blockchain-node*.deb

COPY .buildkite/config/blockchain_limits.conf /etc/security/limits.d

COPY .buildkite/scripts/start.sh /
