package org.acme;

import lombok.extern.slf4j.Slf4j;

import javax.inject.Inject;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Slf4j
@Path("/api")
public class GreetingResource {

    @Inject
    StatusService statusService;

    @Inject
    Work work;

    @POST
    @Path("/job")
    @Produces(MediaType.TEXT_PLAIN)
    public String runJob() {
        log.info("Processing request.");

        // Aync job
        work.doWork();

        return "Work Accepted";
    }

    @GET
    @Path("/status")
    @Produces(MediaType.TEXT_PLAIN)
    public String getStatus() {
        log.info("Processing request.");

        return statusService.getStatus1() + " " + statusService.getStatus2();
    }
}