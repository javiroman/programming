#!/usr/bin/env python
import sys
import traceback
import Ice

Ice.loadSlice('Hello.ice')
import Demo

def run(communicator):
    hello = Demo.HelloPrx.checkedCast(communicator.propertyToProxy('Hello.Proxy'))
    if not hello:
        print(sys.args[0] + ": invalid proxy")
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

    # The communicator initialization removes all Ice-related arguments from argv
    #
    if len(sys.argv) > 1:
        print(sys.argv[0] + ": too many arguments")
        sys.exit(1)

    run(communicator)
