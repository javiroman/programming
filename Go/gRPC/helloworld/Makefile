.DEFAULT_GOAL := build

all : build generate clean
.PHONY: all

generate:
	protoc --go_out=. --go_opt=paths=source_relative \
		--go-grpc_out=. --go-grpc_opt=paths=source_relative \
		proto/*.proto

build:
	cd client ; go build .
	cd server ; go build .

clean:
	rm client/client 2> /dev/null || true
	rm server/server 2> /dev/null || true
	rm proto/*.pb.go 2> /dev/null || true

mrproper: clean
	go clean --modcache
