.DEFAULT_GOAL := build
PROTO_INCLUDE_FOLDERS = $(shell find pkg/proto -type d | tr '\n' ':')
FILES = $(shell find pkg/proto -name *.proto)

all : build generate clean
.PHONY: all

generate:
	protoc --go_out=. --go_opt=paths=source_relative \
		--go-grpc_out=. --go-grpc_opt=paths=source_relative \
		helloworld/*.proto

build:
	cd greeter_client ; go build .
	cd greeter_server ; go build .

clean:
	rm greeter_client/greeter_client 2> /dev/null || true
	rm greeter_server/greeter_server 2> /dev/null || true
	rm helloworld/*.pb.go 2> /dev/null || true

mrproper: clean
	go clean --modcache
