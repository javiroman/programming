import KeedioMiddlewareNodoFrontera.MiddlewareNodoFrontera;

import java.io.IOException;

public class MiddlewareNodoFronteraI implements MiddlewareNodoFrontera
{
    public MiddlewareNodoFronteraI(WorkQueue workQueue)
    {
        _workQueue = workQueue;
    }

    @Override
    public java.util.concurrent.CompletionStage<Integer>
                    launchJobAsync(String command, int delay,
                                   com.zeroc.Ice.Current current) {

        Process p = null;

        if(delay == 0) {
            System.out.println("executing command: " + command);
            try {
                p = Runtime.getRuntime().exec(command);
                byte[] b = new byte[1];

                while (p.getErrorStream().read(b) > 0)
                    System.out.write(b);
                while (p.getInputStream().read(b) > 0)
                    System.out.write(b);

            } catch (IOException e) {
                e.printStackTrace();
            }

            int output = -1;
            try {
                assert p != null;
                    System.out.println("working command ..");
                    output = p.waitFor();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            return java.util.concurrent.CompletableFuture.completedFuture(
                    (Integer) output);
        } else {
            java.util.concurrent.CompletableFuture<Integer> r = new java.util.concurrent.CompletableFuture<>();
            _workQueue.add(r, delay);
            return r;
        }
    }

    @Override
    public java.util.concurrent.CompletionStage<Integer>
                    launchJob2Async(String command, int delay,
                                   com.zeroc.Ice.Current current) {

        Process p = null;

        if(delay == 0) {
            System.out.println("executing command: " + command);
            try {
                p = Runtime.getRuntime().exec(command);
                byte[] b = new byte[1];

                while (p.getErrorStream().read(b) > 0)
                    System.out.write(b);
                while (p.getInputStream().read(b) > 0)
                    System.out.write(b);

            } catch (IOException e) {
                e.printStackTrace();
            }

            int output = -1;
            try {
                assert p != null;
                    System.out.println("working command ..");
                    output = p.waitFor();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

            return java.util.concurrent.CompletableFuture.completedFuture(
                    (Integer) output);
        } else {
            java.util.concurrent.CompletableFuture<Integer> r = new java.util.concurrent.CompletableFuture<>();
            _workQueue.add(r, delay);
            return r;
        }
    }

    @Override
    public void shutdown(com.zeroc.Ice.Current current) {
        System.out.println("Shutting down...");

        _workQueue._destroy();
        current.adapter.getCommunicator().shutdown();
    }

    private WorkQueue _workQueue;
}
