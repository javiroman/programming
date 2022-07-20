package org.acme.getting.started;

import infrastructure.SaltService;
import org.jboss.resteasy.reactive.RestStreamElementType;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.sse.Sse;
import javax.ws.rs.sse.SseEventSink;

@Path("/infra")
public class InfrastructureResource {

    @GET
    @Produces(MediaType.SERVER_SENT_EVENTS)
    @RestStreamElementType(MediaType.APPLICATION_JSON)
    @Path("/{name}/run")
    public void infraStepsAsStream(@Context Sse sse, @Context SseEventSink sseEventSink) {
        SaltService saltService = new SaltService(sse, sseEventSink);
        try {
            saltService.executeSaltSteps();
            sseEventSink.close();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
