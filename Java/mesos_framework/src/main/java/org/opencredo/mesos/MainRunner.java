package org.opencredo.mesos;

import org.apache.mesos.MesosSchedulerDriver;
import org.apache.mesos.Protos;
import org.apache.mesos.Protos.FrameworkInfo;
import org.apache.mesos.Protos.ExecutorInfo;
import org.apache.mesos.Protos.CommandInfo;
import org.apache.mesos.Scheduler;
import org.apache.mesos.Protos.Credential;
import com.google.protobuf.ByteString;
import java.util.Scanner;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

public class MainRunner {

    static String frameworkName = "Framework-MyName";
    static String executorName = "Executor-MyName";
    static String remoteExecutorPath = "/tmp/Executor-MyName/example-framework-1.0-SNAPSHOT.jar";
    static String command = "";

    private static FrameworkInfo getFrameworkInfo() {
        FrameworkInfo.Builder builder = FrameworkInfo.newBuilder();
        builder.setFailoverTimeout(120000);
        builder.setUser("");
        builder.setName(frameworkName);
        return builder.build();
    }

    private static CommandInfo.URI getUri() {
        CommandInfo.URI.Builder uriBuilder = CommandInfo.URI.newBuilder();
        uriBuilder.setValue(remoteExecutorPath);
        uriBuilder.setExtract(false);
        return uriBuilder.build();
    }

    private static CommandInfo getCommandInfo() {
        CommandInfo.Builder cmdInfoBuilder = Protos.CommandInfo.newBuilder();
        cmdInfoBuilder.addUris(getUri());
        cmdInfoBuilder.setValue(command);
        return cmdInfoBuilder.build();
    }

    private static ExecutorInfo getExecutorInfo() {
        ExecutorInfo.Builder builder = ExecutorInfo.newBuilder();
        builder.setExecutorId(Protos.ExecutorID.newBuilder().setValue(executorName));
        builder.setCommand(getCommandInfo());
        builder.setName(executorName);
        builder.setSource("java");
        return builder.build();
    }

    private static void runFramework(String mesosMaster, String commandFile) {
        try { 
            command = new Scanner(new File(commandFile)).useDelimiter("\\Z").next();
            System.out.println(command);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

        MesosSchedulerDriver driver = null;
        if (System.getenv("MESOS_AUTHENTICATE") != null) {
            System.out.println("Enabling authentication for the framework");
        }

        if (System.getenv("DEFAULT_PRINCIPAL") == null) {
            System.err.println("Expecting authentication principal in the environment");
            System.exit(1);
        }

        if (System.getenv("DEFAULT_SECRET") == null) {
            System.err.println("Expecting authentication secret in the environment");
            System.exit(1);
        }

        Credential.Builder credential = Credential.newBuilder()
            .setPrincipal(System.getenv("DEFAULT_PRINCIPAL"))
            .setSecret(System.getenv("DEFAULT_SECRET"));

        Scheduler scheduler = new ExampleScheduler(getExecutorInfo());

        driver = new MesosSchedulerDriver(scheduler, getFrameworkInfo(), mesosMaster, credential.build());
        int status = driver.run() == Protos.Status.DRIVER_STOPPED ? 0 : 1;

        driver.stop();
        System.exit(status);
    }

    public static void main(String[] args) throws Exception {
        runFramework(args[0], args[1]);
    }
}
