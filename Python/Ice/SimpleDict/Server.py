import sys, traceback, Ice

Ice.loadSlice("dict.ice")
import fase.bindings.logging

class LoggingServer(fase.bindings.logging.LoggingServer):
    def log(self, msg, current=None):
        print msg
    def NewMessage(self, msg, current=None):
        print "called"
        return_value = {"hola": "adios"}
        return return_value

status = 0
ic = None

try:
    ic = Ice.initialize(sys.argv)
    adapter = ic.createObjectAdapterWithEndpoints(
        "LoggingServerAdapter", "tcp -p 10000")
    logger = LoggingServer()
    adapter.add(logger, Ice.stringToIdentity("LoggingServer"))
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