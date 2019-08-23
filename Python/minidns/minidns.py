#!/usr/bin/env python


from __future__ import print_function


from uuid import uuid4 as uuid


from dnslib import CLASS, QR, QTYPE
from dnslib import DNSHeader, DNSQuestion, DNSRecord


from circuits.net.events import write
from circuits import Component, Debugger, Event
from circuits.net.sockets import UDPClient, UDPServer


class lookup(Event):
    """lookup Event"""


class query(Event):
    """query Event"""


class response(Event):
    """response Event"""


class DNS(Component):

    def read(self, peer, data):
        record = DNSRecord.parse(data)
        if record.header.qr == QR["QUERY"]:
            return self.fire(query(peer, record))
        return self.fire(response(peer, record))


class ReturnResponse(Component):

    def response(self, peer, response):
        return response


class Client(Component):

    channel = "client"

    def init(self, server, port, channel=channel):
        self.server = server
        self.port = int(port)

        self.transport = UDPClient(0, channel=self.channel).register(self)
        self.protocol = DNS(channel=self.channel).register(self)
        self.handler = ReturnResponse(channel=self.channel).register(self)


class Resolver(Component):

    def init(self, server, port):
        self.server = server
        self.port = port

    def lookup(self, qname, qclass="IN", qtype="A"):
        channel = uuid()

        client = Client(
            self.server,
            self.port,
            channel=channel
        ).register(self)

        yield self.wait("ready", channel)

        self.fire(
            write(
                (self.server, self.port),
                DNSRecord(
                    q=DNSQuestion(
                        qname,
                        qclass=CLASS[qclass],
                        qtype=QTYPE[qtype]
                    )
                ).pack()
            )
        )

        yield (yield self.wait("response", channel))

        client.unregister()
        yield self.wait("unregistered", channel)
        del client


class ProcessQuery(Component):

    def query(self, peer, query):
        qname = query.q.qname
        print(qname)
        qtype = QTYPE[query.q.qtype]
        qclass = CLASS[query.q.qclass]

        response = yield self.call(lookup(qname, qclass=qclass, qtype=qtype))

        record = DNSRecord(
            DNSHeader(id=query.header.id, qr=1, aa=1, ra=1),
            q=query.q,
        )

        for rr in response.value.rr:
            record.add_answer(rr)

        yield record.pack()


class Server(Component):

    def init(self, bind=("0.0.0.0", 6666)):
        self.bind = bind

        self.transport = UDPServer(self.bind).register(self)
        self.protocol = DNS().register(self)
        self.handler = ProcessQuery().register(self)


class App(Component):

    def init(self, bind=("0.0.0.0", 6666), server="8.8.8.8", port=53,
             verbose=False):

        if verbose:
            Debugger().register(self)

        self.resolver = Resolver(server, port).register(self)
        self.server = Server(bind).register(self)


def main():
    App().run()


if __name__ == "__main__":
    main()