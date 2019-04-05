import KeedioMiddlewareNodoFrontera.MiddlewareNodoFrontera;

public class MiddlewareNodoFronteraI implements MiddlewareNodoFrontera
{
    public MiddlewareNodoFronteraI(WorkQueue workQueue)
    {
        _workQueue = workQueue;
    }

    @Override
    public java.util.concurrent.CompletionStage<Integer>
                    launchJobAsync(String command, int delay, com.zeroc.Ice.Current current) {
        if(delay == 0) {
            System.out.println(command);
            return java.util.concurrent.CompletableFuture.completedFuture((Integer) null);
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
