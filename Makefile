.PHONY: compile rel cover test typecheck doc ci start stop reset

REBAR=./rebar3
BUILDER_IMAGE=erlang:24-alpine
RUNNER_IMAGE=alpine:3.16
APP_VERSION=$$(git tag --points-at HEAD)

OS_NAME=$(shell uname -s)
PROFILE ?= dev

grpc_services_directory=src/grpc/autogen

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
	rm -rf $(grpc_services_directory)
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

grpc: | $(grpc_services_directory)
	@echo "generating grpc services"
	REBAR_CONFIG="config/grpc_server_gen.config" $(REBAR) grpc gen
	REBAR_CONFIG="config/grpc_client_gen.config" $(REBAR) grpc gen

clean_grpc:
	@echo "cleaning grpc services"
	rm -rf $(grpc_services_directory)

$(grpc_services_directory):
	@echo "grpc service directory $(directory) does not exist"
	$(REBAR) get-deps

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
        --build-arg VERSION=$(APP_VERSION) \
        --build-arg BUILDER_IMAGE=$(BUILDER_IMAGE) \
        --build-arg RUNNER_IMAGE=$(RUNNER_IMAGE) \
        -t helium/node .

docker-clean: docker-stop
	docker rm node

docker-start:
	mkdir -p $(HOME)/node_data
	docker run -d --init \
	--publish 44158:44158/tcp \
	--publish 4467:4467/tcp \
	--name node \
	--mount type=bind,source=$(HOME)/node_data,target=/var/data \
	helium/node

docker-stop:
	docker stop node

docs:
	$(MAKE) -C docs
