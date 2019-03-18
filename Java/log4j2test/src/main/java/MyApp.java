import com.foo.Bar;

// Import log4j classes.
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;

public class MyApp {
    static Logger rootLogger = LogManager.getRootLogger();
    /*
     * Define a static logger variable so that it references the
     * Logger instance named "MyApp".
     */
    private static final Logger logger = LogManager.getLogger(MyApp.class);

    public static void main(String args[]) {
        rootLogger.trace("root logger trace");
        // Set up a simple configuration that logs on the console.
        logger.trace("Entering application (trace).");
        Bar bar = new Bar();
        if (!bar.doIt()) {
            logger.error("Didn't do it (error).");
        }
        logger.trace("Exiting application (trace).");

        logger.debug("This Will Be Printed On Debug");
        logger.info("This Will Be Printed On Info");
        logger.warn("This Will Be Printed On Warn");
        logger.error("This Will Be Printed On Error");
        logger.fatal("This Will Be Printed On Fatal");
        logger.info("Appending string: {}.", "Hello, World");

    }
}
