import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.*;

public class Main {
    public static void main(String[] args) throws ExecutionException, InterruptedException {

        ExecutorService service = Executors.newFixedThreadPool(10);
        List<Future> allFutures = new ArrayList<>();

        // 20 futures with 20 placeholders, the array list.
        for (int i = 0; i < 20; i++) {
            Future<Integer> future = service.submit(new Task());
            allFutures.add(future);
        }

        // because we have a pool of 10 threads in parallel, this result
        // will be get on chuncks of 10 results.
        for (int i = 0; i < 20; i++) {
            Future<Integer> future = allFutures.get(i);
            Integer result = future.get();
            System.out.println("Result of future: " + i + "=" + result);
        }

        System.out.println("Thread name: " + Thread.currentThread().getName());
        service.shutdown();
    }

    static class Task implements Callable<Integer> {
        @Override
        public Integer call() throws Exception {
            Thread.sleep(3000);
            return new Random().nextInt();
        }
    }
}

