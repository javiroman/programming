package org.example.storage;

import org.example.storage.Storage;

public class FileStorage implements Storage {

    public void store(String uniqueId, Data data) {
        // Store the object in a file using Java Serialization mechanism.
        data.setThedata(uniqueId);
    }

    public Data retrieve(String uniqueId) {
        // Code to retrieve the object.
        return null;
    }

}