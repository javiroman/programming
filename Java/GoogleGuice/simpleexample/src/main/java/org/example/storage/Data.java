package org.example.storage;

public class Data {
    String thedata;

    public Data() {

        System.out.println("constructor");
    }

    public Data(String thedata) {
        this.thedata = thedata;
    }

    public String getThedata() {
        return thedata;
    }

    public void setThedata(String thedata) {
        this.thedata = thedata;
    }
}
