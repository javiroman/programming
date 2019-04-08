#pragma once

module KeedioMiddlewareNodoFrontera {

    exception RequestCanceledException
    {
    }

    interface MiddlewareNodoFrontera {

            ["amd"] int launchJob(string command, int delay)
            throws RequestCanceledException;

        ["amd"] int launchJob2(string command, int delay)
                throws RequestCanceledException;

        void shutdown();
    }
}
