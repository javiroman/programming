# Testing with grpcurl

```
go install github.com/fullstorydev/grpcurl/cmd/grpcurl@latest
```
```
grpcurl -proto proto/helloworld.proto describe
grpcurl -proto proto/helloworld.proto -plaintext localhost:50051 list
grpcurl -proto proto/helloworld.proto -plaintext localhost:50051 helloworld.Greeter.SayHello

grpcurl -proto proto/helloworld.proto -plaintext -d @ localhost:50051 helloworld.Greeter.SayHello <<!
{
    "name": "world"
}
!
```

