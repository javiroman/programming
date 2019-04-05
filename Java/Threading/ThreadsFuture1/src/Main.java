import java.util.Random;
import java.util.concurrent.*;

public class Main {
    public static void main(String[] args) throws ExecutionException, InterruptedException {
        ExecutorService service = Executors.newSingleThreadExecutor();

        Future<Integer> future = service.submit(new Task());

        System.out.println("Blocking here");
        Integer result = future.get();

        System.out.println(result);
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

