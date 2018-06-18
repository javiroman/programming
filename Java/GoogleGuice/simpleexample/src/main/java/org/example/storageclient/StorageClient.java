package org.example.storageclient;

import org.example.storage.Data;
import org.example.storage.DatabaseStorage;
import org.example.storage.FileStorage;
import org.example.storage.Storage;

public class StorageClient {

    public static void main(String[] args) {

        // Making use of file storage.
        Storage storage = new FileStorage();
        storage.store("123", new Data());

        // Making use of the database.
        storage = new DatabaseStorage();
        storage.store("456", new Data());
    }
}