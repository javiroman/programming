# Zeroc Ice Introduction

This README file is derivated from official documentation:

https://doc.zeroc.com/ice/3.7/ice-overview/ice-architecture

- Ice is an object-oriented middleware platform. Fundamentally, this means that 
Ice provides: 
    - tools, 
    - APIs, and 
    - library support for building object-oriented 
client-server applications. 
- Ice applications are suitable for use in heterogeneous environments: client 
and server can be written in different programming languages. 
- Can run on different operating systems and machine architectures,

# Clients and Servers in Ice

- Clients are active entities. They issue requests for service to servers.
- Servers are passive entities. They provide services in response to client 
requests.

Servers are not "pure" servers, in the sense that they never issue requests 
and only respond to requests. Instead, servers often act as a server on 
behalf of some client but, in turn, act as a client to another server in 
order to satisfy their client's request.

Clients often are not "pure" clients, in the sense that they only request 
service from an object. Instead, clients are frequently client-server hybrids. 
For example, a client might start a long-running operation on a server; as 
part of starting the operation, the client can provide a callback object to 
the server that is used by the server to notify the client when the operation 
is complete. In that case, the client acts as a client when it starts the 
operation, and as a server when it is notified that the operation is complete.

# Ice Objects

- An Ice object is an entity in the local or a remote address space that can 
respond to client requests.
- A single Ice object can be instantiated in a single server or, in multiple 
servers. If an object has multiple simultaneous instantiations, it is still a 
single Ice object.
- Each Ice object has one or more **interfaces**. An interface is a collection 
of **named operations** that are supported by an object. Clients issue requests 
by invoking operations.
- An operation has zero or more parameters as well as a return value. 
Parameters and return values have a specific type. Parameters are named and 
have a direction: **in-parameters** are initialized by the client and passed 
to the server; **out-parameters** are initialized by the server and passed 
to the client. (
- An Ice object has a **distinguished interface**, known as its **main interface**. 
In addition, an Ice object can provide zero or more alternate interfaces, known 
as **facets**. Clients can select among the facets of an object to choose the 
interface they want to work with.
- Each Ice object has a **unique object identity**. An object's identity is an 
identifying value that distinguishes the object from all other objects. The 
Ice object model assumes that object identities are globally unique

# Proxies: Ice object link

- For a client to be able to contact an Ice object, the client must hold a 
proxy for the Ice object. A proxy is an artifact that is local to the client's 
address space; it represents the (possibly remote) Ice object for the client. 
- A proxy encapsulates all the necessary information for this sequence of steps 
to take place. In particular, a proxy contains: 
    - Addressing information that allows the client-side run time to contact the
     correct server.
    - An object identity that identifies which particular object in the server 
    is the target of a request.
    - An optional facet identifier that determines which particular facet of 
    an object the proxy refers to.
- **Stringified Proxies**: The information in a proxy can be expressed as a string 
(```SimplePrinter:default -p 10000```), s a human-readable representation of a 
proxy. The Ice run time provides API calls that allow you to convert a proxy 
to its stringified form and vice versa. This is useful, for example, to store 
proxies in database tables or text files.
- **Direct Proxies**: is a proxy that embeds an object's identity, together 
with the address at which its server runs. The address is completely specified 
by: a protocol identifier (such TCP/IP or UDP), a protocol-specific address 
(such as a host name and port number). To contact the object denoted by a 
direct proxy, the Ice run time uses the addressing information in the proxy to 
contact the server; the identity of the object is sent to the server with each 
request made by the client.
- **Indirect Proxies**: Has two forms. It may provide only an object's identity, 
or it may specify an identity together with an object adapter identifier. 
An object that is accessible using only its identity is called a **well-known 
object**, and the corresponding proxy is a well-known proxy. 
For example, the string: ``SimplePrinter`` is a valid proxy for a well-known 
object with the identity SimplePrinter. An indirect proxy that includes an 
object adapter identifier has the stringified form ```SimplePrinter@PrinterAdapter```

TODO

# The Slice Language

Slice (Specification Language for Ice) is the fundamental abstraction mechanism 
for separating object interfaces from their implementations. Slice establishes 
a contract between client and server that describes the interfaces, operations 
and parameter types used by an application. This description is independent of 
the implementation language, so it does not matter whether the client is written 
in the same language as the server.

Slice definitions are compiled for a particular implementation language by a 
compiler. The language-specific Slice compiler translates the 
language-independent Slice definitions into language-specific type definitions 
and APIs. These types and APIs are used by the developer to provide application 
functionality and to interact with Ice. The translation algorithms for various 
implementation languages are known as language mappings, and Ice provides a 
number of language mappings (for C++, C#, Java, JavaScript, Python and more).


  

                      

    
