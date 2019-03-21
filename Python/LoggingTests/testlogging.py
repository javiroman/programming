#!/bin/env python2.7
# -*- coding: utf-8 -*-

# Logging is performed by calling methods on instances of the Logger class
# (hereafter called loggers). Each instance has a name, and they are
# conceptually arranged in a namespace hierarchy using dots (periods) as
# separators. For example, a logger named ‘scan’ is the parent of loggers
# ‘scan.text’, ‘scan.html’ and ‘scan.pdf’. Logger names can be anything you
# want, and indicate the area of an application in which a logged message
# originates.
import logging
import logging.config
import sys

# With the logging module imported, you can use the called a “logger” to log
# messages that you want to see. By default, there are 5 standard levels
# indicating the severity of events. Each has a corresponding method that
# can be used to log events at that level of severity. The defined levels,
# in order of increasing severity, are the following: DEBUG < INFO < WARNING
# < ERROR < CRITICAL
import time


def main():

    # The logging module provides you with a default logger that allows you
    # to get started without needing to do much configuration.
    logging.debug('This is a debug message')
    logging.info('This is an info message')
    logging.warning('This is a warning message')
    logging.error('This is an error message')
    logging.critical('This is a critical message')

    # ouput:
    #   WARNING:root:This is a warning message
    #   ERROR:root:This is an error message
    #   CRITICAL:root:This is a critical message
    #
    # The output shows the severity level before each message along with root,
    # which is the name the logging module gives to its default logger. This
    # format, which shows the level, name, and message separated by a colon
    # (:), is the default output format that can be configured to include
    # things like timestamp, line number, and other details. Notice that the
    # debug() and info() messages didn’t get logged. This is because,
    # by default, the logging module logs the messages with a severity level
    # of WARNING or above.

    logging.debug('This is a debug message')

    # To modify the default logging level, we have to put this code before using
    # the logging method: logging.basicConfig(level=logging.DEBUG). For changing
    # the default (root logger) in runtime we use this:
    logging.getLogger().setLevel(logging.DEBUG)
    logging.debug('This is a debug message')

    # we mostly deal with the objects of the Logger class, which are
    # instantiated using the module-level function logging.getLogger(name).
    # Multiple calls to getLogger() with the same name will return a reference
    # to the same Logger object.
    logger = logging.getLogger('my_logger')
    logger.warning('This is a warning')

    # Handlers come into the picture when you want to configure your own
    # loggers and send the logs to multiple places when they are generated.
    # Handlers send the log messages to configured destinations like the
    # standard output stream or a file or over HTTP or to your email via SMTP.

    # Create other custom logger
    logger2 = logging.getLogger('other_logger')

    # Create handlers
    c_handler = logging.StreamHandler()
    f_handler = logging.FileHandler('/tmp/file.log')
    c_handler.setLevel(logging.WARNING)
    f_handler.setLevel(logging.ERROR)

    # Create formatters and add it to handlers
    c_format = logging.Formatter('%(name)s - %(levelname)s - %(message)s')
    f_format = logging.Formatter(
        '%(asctime)s - %(name)s - %(levelname)s - %(message)s')
    c_handler.setFormatter(c_format)
    f_handler.setFormatter(f_format)

    # Add handlers to the logger
    logger2.addHandler(c_handler)
    logger2.addHandler(f_handler)

    logger2.warning('This is a warning')
    logger2.error('This is an error')

    # Creating logger with configuration file
    logging.config.fileConfig(fname='logging_config.ini',
                              disable_existing_loggers=False)
    logger3 = logging.getLogger('sampleLogger')
    logger3.debug('This is a debug message configurated by file')

    logger4 = logging.getLogger('rotate_logger')
    for i in range(10):
        logger4.info("This is test log line %s" % i)
        time.sleep(1.5)

    logger4.debug('This is a debug message configurated for rotating file')
    sys.exit(0)


if __name__ == "__main__": main()
