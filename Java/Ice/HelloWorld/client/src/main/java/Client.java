public class Client
{
    public static void main(String[] args)
    {
        /*
          Every Ice-based application needs to initialize the Ice run time,
          and this initialization returns a com.zeroc.Ice.Communicator object.
          A Communicator is a local Java object that represents an
          instance of the Ice run time. Most Ice-based applications create
          and use a single Communicator object, although it is possible
          and occasionally desirable to have multiple Communicator objects
          in the same application. Util.initialize accepts the argument vector
          that is passed to main by the operating system.
        */
        try(com.zeroc.Ice.Communicator communicator =
                    com.zeroc.Ice.Util.initialize(args))
        {

            /*
             * The communicator operation stringToProxy creates a proxy
             * from its stringified representation.
             *
             * Demo -> Printer -> printString
             */
            com.zeroc.Ice.ObjectPrx base =
                    communicator.stringToProxy("SimplePrinter:default -p 10000");

            Demo.PrinterPrx printer = Demo.PrinterPrx.checkedCast(base);

            if(printer == null)
            {
                throw new Error("Invalid proxy");
            }
            printer.printString("Hello World!");
        }
        /*
          communicator is destroyed automatically here. The Communicator
          interface implements java.lang.AutoCloseable: at the end of a
          try-with-resources statement, the communicator is closed (destroyed)
          automatically, without an explicit call to the destroy method.
         */
    }
}