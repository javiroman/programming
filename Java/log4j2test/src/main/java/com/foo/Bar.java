package com.foo;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Bar {
    static final Logger logger = LogManager.getLogger(Bar.class.getName());

    public boolean doIt() {
        logger.info("Logger name -> {}.", logger.getName());
        logger.trace("Logger level -> {}.", logger.getLevel());
        logger.traceEntry();
        logger.error("Did it again! (error)");
        return logger.traceExit(false);
    }
}