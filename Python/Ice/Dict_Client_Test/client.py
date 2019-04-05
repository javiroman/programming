#!/usr/bin/env python
import sys
import traceback
import Ice

Ice.loadSlice('Hello.ice')
import Demo

class MiddlewareClient:

    def __init__(self, args):
        self.args = args

    def run(self, communicator):
        hello = Demo.HelloPrx.checkedCast(
            communicator.propertyToProxy('Hello.Proxy'))
        if not hello:
            print(sys.args[0] + ": invalid proxy")
            sys.exit(1)

        if len(self.args) > 1 and self.args[1] == "stop":
            hello.shutdown()
            sys.exit(1)
        try:
                hello.sayHello(0)
        except Ice.Exception as ex:
            print(ex)

#
# Ice.initialize returns an initialized Ice communicator,
# the communicator is destroyed once it goes out of scope.
#
with Ice.initialize(sys.argv, "config.client") as communicator:
    mdc = MiddlewareClient(sys.argv)
    mdc.run(communicator)
