package org.acme;

import javax.enterprise.context.ApplicationScoped;

@ApplicationScoped
public class StatusService {
    String status1;
    String status2;
    String status3;
    String status4;

    public StatusService(){
        this.status1 = "none";
        this.status2 = "none";
        this.status3 = "none";
        this.status4 = "none";
    }

    public String getStatus1() {
        return status1;
    }

    public void setStatus1(String status1) {
        this.status1 = status1;
    }

    public String getStatus2() {
        return status2;
    }

    public void setStatus2(String status2) {
        this.status2 = status2;
    }

    public String getStatus3() {
        return status3;
    }

    public void setStatus3(String status3) {
        this.status3 = status3;
    }

    public String getStatus4() {
        return status4;
    }

    public void setStatus4(String status4) {
        this.status4 = status4;
    }
}
