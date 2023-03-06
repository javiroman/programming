package org.acme.vertx;

import io.vertx.mutiny.core.eventbus.EventBus;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

@Path("/job")
@ApplicationScoped
public class JobResource {
    @Inject
    EventBus bus;

    @POST
    @Produces(MediaType.TEXT_PLAIN)
    @Path("{name}")
    public String job(String name) {
        System.out.println("sending Job to bus " + name);
        bus.send("job", name);
        return "ok";
    }
}
