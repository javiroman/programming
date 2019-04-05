#!/usr/bin/env python
import sys
import Ice

Ice.loadSlice('MiddlewareNodoFrontera.ice')
import KeedioMiddlewareNodoFrontera

class MiddlewareClient:

    def __init__(self, args):
        self.args = args

    def run(self, communicator):
        middleware = KeedioMiddlewareNodoFrontera.MiddlewareNodoFronteraPrx.checkedCast(
            communicator.propertyToProxy('MiddlewareNodoFrontera.Proxy'))
        if not middleware:
            print(sys.args[0] + ": invalid proxy")
            sys.exit(1)

        if len(self.args) > 1 and self.args[1] == "stop":
            middleware.shutdown()
            sys.exit(1)
        try:
                ret = middleware.launchJob("hello world", 0)
                print ret
        except Ice.Exception as ex:
            print(ex)

#
# Ice.initialize returns an initialized Ice communicator,
# the communicator is destroyed once it goes out of scope.
#
with Ice.initialize(sys.argv, "config.client") as communicator:
    mdc = MiddlewareClient(sys.argv)
    mdc.run(communicator)
