package org.acme.vertx;
import javax.enterprise.context.ApplicationScoped;

@ApplicationScoped
public class Work {
    public void doWork(String job) {
        System.out.println("doing job (15 minutes job!)");
        try {
            // 15 minutes
            Thread.sleep(15000);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        System.out.println("job " + job + " done!");
    }
}
