.PHONY: compile rel cover test typecheck doc ci start stop reset

REBAR=./rebar3
ALPINE_IMAGE=erlang:23.3.4.9-alpine
DOCKERFILE_VERSION=`git describe --abbrev=0`

OS_NAME=$(shell uname -s)
PROFILE ?= dev

ifeq (${OS_NAME},FreeBSD)
make="gmake"
else
MAKE="make"
endif

compile:
	$(REBAR) compile

shell:
	$(REBAR) shell

clean:
	$(REBAR) clean

cover:
	$(REBAR) cover

test:
	$(REBAR) as test do eunit,ct

ci:
	$(REBAR) do xref, dialyzer

typecheck:
	$(REBAR) dialyzer

doc:
	$(REBAR) edoc

release:
	$(REBAR) as $(PROFILE) do release

.PHONY: docs

start:
	./_build/$(PROFILE)/rel/blockchain_node/bin/blockchain_node start

stop:
	-./_build/$(PROFILE)/rel/blockchain_node/bin/blockchain_node stop

console:
	./_build/$(PROFILE)/rel/blockchain_node/bin/blockchain_node remote_console

docker-build:
	docker build \
		--build-arg VERSION=$(DOCKERFILE_VERSION) \
		--build-arg BUILDER_IMAGE=$(ALPINE_IMAGE) \
		--build-arg RUNNER_IMAGE=$(ALPINE_IMAGE) \
		-t helium/node .

docker-clean: docker-stop
	docker rm node

docker-start:
	mkdir -p $(HOME)/node_data
	docker run -d --init \
	--publish 44158:44158/tcp \
	--publish 4467:4467 \
	--name node \
	--mount type=bind,source=$(HOME)/node_data,target=/var/data \
	helium/node

docker-stop:
	docker stop node

docs:
	$(MAKE) -C docs
