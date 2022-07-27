package org.acme;


import io.smallrye.mutiny.Uni;
import io.smallrye.mutiny.infrastructure.Infrastructure;
import lombok.extern.slf4j.Slf4j;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import java.util.UUID;

@Slf4j
@ApplicationScoped
public class Work {

    @Inject
    StatusService statusService;

    public void doWork() {
        log.info("Do work");

        Uni.createFrom()
                .item(UUID::randomUUID)
                .emitOn(Infrastructure.getDefaultWorkerPool())
                .subscribe()
                .with(this::worker, Throwable::printStackTrace);

    }

    private Uni<Void> worker(UUID uuid) {
        log.info("Starting work: " + uuid);
        try {
            //Thread.sleep((long) Math.random() * 1000);
            Thread.sleep(5000);
            statusService.setStatus1("bingo!!!!");
            Thread.sleep(5000);
            statusService.setStatus2("bingo!!!!");
        } catch (InterruptedException ex) {
            log.info("Could not finish work: " + uuid);
            throw new RuntimeException(ex);
        }
        log.info("Finish work: {}.", uuid);
        return Uni.createFrom().voidItem();
    }
}
