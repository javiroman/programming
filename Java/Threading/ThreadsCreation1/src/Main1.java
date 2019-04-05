/*
 * MIT License
 *
 * Copyright (c) 2019 Michael Pogrebinsky
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/**
 *  Threads Creation - Part 1, Thread Capabilities & Debugging
 * https://www.udemy.com/java-multithreading-concurrency-performance-optimization
 */
public class Main1 {

    public static void main(String[] args) throws InterruptedException {
        // for creating a thread, we have to create a new Thread object.
        // The thread object by default is empty, so we have to pass an object
        // of the class that implements the Runnable interface into the constructor.
        Thread thread = new Thread(new Runnable() {
            @Override
            public void run() {
                //Code that will run in  a new thread when this thread is scheduled
                // by the OS.
                System.out.println("we are now in thread "+
                        Thread.currentThread().getName());
                System.out.println("Current thread priority is " +
                        Thread.currentThread().getPriority());
            }
        });

        thread.setName("New Worker Thread");

        // Set the static priority of dynamic priority capabilities. This is
        // the value that a programmer can setup. Typically from 1 to 10. From
        // minimus priority to maximum priority.
        thread.setPriority(Thread.MAX_PRIORITY);

        System.out.println("We are in thread: " +
                Thread.currentThread().getName()+ " before starting a new thread");

         /*
         * We have to start the thread in order to run it.
         * We are telling to the JVM to create a new thread
         * and passing it to the OS.
         */
        thread.start();

        // Thre thread is suspended, not consumming any cpu.
        Thread.sleep(10000);

        System.out.println("We are in thread: " +
                Thread.currentThread().getName()+ " after starting a new thread");
    }
}
