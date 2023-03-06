package org.acme.vertx;

import io.quarkus.vertx.ConsumeEvent;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;


@ApplicationScoped
public class JobService {
    @Inject
    Work work;
    @ConsumeEvent(value= "job", blocking = true)
    public void job(String job) {
        System.out.println("Envent Bus catch!");
        work.doWork(job);
    }
}
