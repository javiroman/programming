#pragma once

module KeedioMiddlewareNodoFrontera {

    exception RequestCanceledException
    {
    }

    interface MiddlewareNodoFrontera {
        ["amd"] int launchJob(string command, int delay)
            throws RequestCanceledException;

        void shutdown();
    }
}
