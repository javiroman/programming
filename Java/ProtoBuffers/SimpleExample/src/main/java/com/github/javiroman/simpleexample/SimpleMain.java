package com.github.javiroman.simpleexample;

import example.simple.Simple.SimpleMessage;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Arrays;

public class SimpleMain {
    public static void main(String[] args) {
        System.out.println("Hello world");

        SimpleMessage.Builder builder = SimpleMessage.newBuilder();

        builder.setId(42)
               .setIsSimple(true)
               .setName("My simple message name");

        // One method for adding sample list
        builder.addSampleList(1)
                .addSampleList(2)
                .addSampleList(3);

        // other method
        builder.addAllSampleList(Arrays.asList(4, 5, 6));

        System.out.println(builder.toString());

        // In order to get the fiels by code we have to build the
        // message
        SimpleMessage message = builder.build();

        System.out.println(message.getId());

        // we can write the message to one file
        try {
            FileOutputStream outputStream = new
                    FileOutputStream("/tmp/simple.message.bin");
            message.writeTo(outputStream);
            outputStream.close();

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

        // for sending the message by network we need send as byte array
        byte[] bytes = message.toByteArray();

    }
}
