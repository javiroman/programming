package thread.creation.example;

public class Main {

    public static void main(String[] args) throws InterruptedException {
        /*
         * The Thread object is empty by default, so we
         * have to pass an Runnable object to the constructor
         */
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                /*
                 * code that will run in a new thread by the
                 * operating system
                 */
                System.out.println("In the new thread: " +
                        Thread.currentThread().getName());
                System.out.println("My current priority: " +
                        Thread.currentThread().getPriority());
            }
        });

        thread.setName("NewWorker-Thread");

        // modify dynamic priorigy programatically
        // from 1 to 10 values, 10 the max priority.
        thread.setPriority(Thread.MAX_PRIORITY);

        System.out.println("In the thread: " +
            Thread.currentThread().getName() +
            " before staring the new thread");
        /*
         * We have to start the thread in order to run it.
         * We are telling to the JVM to create a new thread
         * and passing it to the OS.
         */
        thread.start();
        System.out.println("In the thread: " +
            Thread.currentThread().getName() +
            " after staring the new thread");

        Thread.sleep(10000);
    }
}
