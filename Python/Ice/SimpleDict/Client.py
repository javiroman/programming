import sys, traceback, Ice

Ice.loadSlice("dict.ice")
import fase.bindings.logging

status = 0
ic = None

try:
    ic = Ice.initialize(sys.argv)
    base = ic.stringToProxy("LoggingServer:tcp -p 10000")
    logger = fase.bindings.logging.LoggingServerPrx.checkedCast(base)
    if not logger:
        raise RuntimeError("Invalid proxy")

    logger.log({"pippo": "pluto"})
    ret = logger.NewMessage("hola")
    print ret
    print ret["hola"]

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