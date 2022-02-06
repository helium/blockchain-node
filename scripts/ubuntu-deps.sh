#!/bin/bash
ERLANG_DEB_PKG="erlang-solutions_2.0_all.deb"
ERLANG_URL_UBUNTU="https://packages.erlang-solutions.com/${ERLANG_DEB_PKG}"
ADDITIONAL_DEPENDENCIES="esl-erlang=1:22.3.4.1-1 cmake libsodium-dev libssl-dev build-essential"
RUST_TOOLCHAIN="https://sh.rustup.rs"

echo "Downloading Erlang deb package"
wget ${ERLANG_URL_UBUNTU}

echo "Installing Erlang deb package"
sudo dpkg -i ${ERLANG_DEB_PKG}


echo "Running apt-update"
sudo apt-get update


echo "Installing additional dependencies"
sudo apt install ${ADDITIONAL_DEPENDENCIES} -y

echo "Installing Rust Toolchain"
curl --proto '=https' --tlsv1.2 -sSf ${RUST_TOOLCHAIN} | sh

echo "Done"
