# blockchain-etl

[![Build status](https://badge.buildkite.com/8f80e5ba2dd64290fb11c5126477a023b0ea75d35f08783085.svg?branch=master)](https://buildkite.com/helium/blockchain-node)

This is an Erlang application that is a Helium Blockchain node. It
follows the blockchain and exposes functionality using a jsonrpc API.


## Developer Usage

* Clone this repository

* Run `make && make release` in the top level folder

* Run `make start` to start the application. Logs will be at
  `_build/dev/rel/blockchain_node/log/*`.

Once started the application will start syncing the blockchain and
loading blocks. If this is done from scrath it can take a number of
days to download all blocks from the network and aobsorb them in the
localq ledger.

#### macOS Note
You may see an error similar to the following during initial sync:

`{error,"IO error: While open a file for appending: data/blockchain.db/020311.sst: Too many open files"}`

Check [this](https://superuser.com/a/443168) Superuser answer for a workaround.

### Installing Ubuntu Required Packages

If running on Ubuntu, you will need the following packages installed before
running `make release`:

```bash
wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb
sudo apt-get update
sudo apt install esl-erlang cmake libsodium-dev libssl-dev build-essential
```
