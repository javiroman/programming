package org.example.storage;

public interface Storage {

    public void store(String uniqueId, Data data);

    public Data retrieve(String uniqueId);

}

