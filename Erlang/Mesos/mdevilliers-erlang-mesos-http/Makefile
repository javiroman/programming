.DEFAULT_GOAL := help

build: ## builds erlang-mesos-http only
	./rebar skip_deps=true compile

build-all: bootstrap ## builds all of the source
	./rebar compile

test: ## run tests
	./rebar skip_deps=true eunit

bootstrap: ## bootstraps dependencies
	./rebar get-deps

.PHONY: build build-all test bootstrap

test-environment: zookeeper mesos-master mesos-slave ## install integration environment

test-environment-down: ## remove integration environment
	docker rm -f zookeeper mesos_master mesos_slave

zookeeper:
	docker run -d --net=host --name zookeeper netflixoss/exhibitor:1.5.2

mesos-master:
	docker run -d --name=mesos_master --net=host \
	-e MESOS_PORT=5050 \
	-e MESOS_ZK=zk://127.0.0.1:2181/mesos \
	-e MESOS_QUORUM=1 \
	-e MESOS_REGISTRY=in_memory \
	-e MESOS_LOG_DIR=/var/log/mesos \
	-e MESOS_WORK_DIR=/var/tmp/mesos \
	-v "$(pwd)/log/mesos:/var/log/mesos" \
	-v "$(pwd)/tmp/mesos:/var/tmp/mesos" \
	mesosphere/mesos-master:1.0.0 --ip=127.0.0.1 --hostname=127.0.0.1

mesos-slave:
	docker run -d --net=host --name mesos_slave --privileged \
	-e MESOS_PORT=5051 \
	-e MESOS_MASTER=zk://127.0.0.1:2181/mesos \
	-e MESOS_SWITCH_USER=0 \
	-e MESOS_CONTAINERIZERS=docker,mesos \
	-e MESOS_LOG_DIR=/var/log/mesos \
	-e MESOS_WORK_DIR=/var/tmp/mesos \
	-v "$(pwd)/log/mesos:/var/log/mesos" \
	-v "$(pwd)/tmp/mesos:/var/tmp/mesos" \
	-v /var/run/docker.sock:/var/run/docker.sock \
	-v /cgroup:/cgroup \
	-v /sys:/sys \
	-v /usr/local/bin/docker:/usr/local/bin/docker \
	mesosphere/mesos-slave:1.0.0  --ip=127.0.0.1 --hostname=127.0.0.1

.PHONY: test-environment test-environment-down mesos-master mesos-slave zookeeper

generate-proto-bindings: clean-bindings ## generate the erlang bindings fromthe proto files
	deps/gpb/bin/protoc-erl -I$(PWD)/proto -pkgs -o-erl src -o-hrl include -modsuffix _pb -il $(wildcard $(PWD)/proto/*.proto)

clean-bindings: clean-proto-bindings clean-proto-includes 

clean-proto-bindings:
	rm -f src/*_pb.erl

clean-bindingsn-proto-includes:
	rm -f include/*_pb.hrl

.PHONY: generate-proto-bindings clean-bindings clean-proto-bindings clean-proto-includes

# 'help' parses the Makefile and displays the help text
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PHONY: help
