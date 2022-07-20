package infrastructure;

import javax.ws.rs.sse.Sse;
import javax.ws.rs.sse.SseEventSink;

public class SaltService {
    final Sse sse;
    final SseEventSink sseEventSink;

    public SaltService(Sse sse, SseEventSink sseEventSink) {
        this.sse = sse;
        this.sseEventSink = sseEventSink;
    }

    public void executeSaltSteps() throws InterruptedException {
        stepOne();
        stepTwo();
    }

    public void stepOne() throws InterruptedException {
        // Your real steps here.
        Thread.sleep(5000);
        SaltServiceResponse saltServiceResponse = new SaltServiceResponse(2, 1);
        saltServiceResponse.setTotalSteps(2);
        saltServiceResponse.setCompletedSteps(1);
        saltServiceResponse.setMessage("Minions installation done");

        sseEventSink.send(sse.newEventBuilder()
                .data(SaltServiceResponse.class, saltServiceResponse)
                .build());
    }

    public void stepTwo() throws InterruptedException {
        // Your real steps here.
        Thread.sleep(5000);
        SaltServiceResponse saltServiceResponse = new SaltServiceResponse(2, 1);
        saltServiceResponse.setTotalSteps(2);
        saltServiceResponse.setCompletedSteps(2);
        saltServiceResponse.setMessage("kubeadm installation done");

        sseEventSink.send(sse.newEventBuilder()
                .data(SaltServiceResponse.class, saltServiceResponse)
                .build());
    }
}
