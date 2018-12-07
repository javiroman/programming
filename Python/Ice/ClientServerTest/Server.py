import sys, traceback, Ice
from time import sleep

import Demo

class PrinterI(Demo.Printer):
    def printString(self, s, current=None):
        print("sleeping %s seconds" % s)
        sleep(int(s))

status = 0
ic = None
try:
    ic = Ice.initialize(sys.argv, "resources/server.cfg")
    adapter = ic.createObjectAdapterWithEndpoints("SimplePrinterAdapter", "default -p 10000")
    object = PrinterI()
    adapter.add(object, ic.stringToIdentity("SimplePrinter"))
    adapter.activate()
    ic.waitForShutdown()
except:
    traceback.print_exc()
    status = 1

if ic:
    # Clean up
    try:
        ic.destroy()
    except:
        traceback.print_exc()
        status = 1

sys.exit(status)