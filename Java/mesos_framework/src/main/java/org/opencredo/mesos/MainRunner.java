package org.opencredo.mesos;

import org.apache.mesos.MesosSchedulerDriver;
import org.apache.mesos.Protos;
import org.apache.mesos.Protos.FrameworkInfo;
import org.apache.mesos.Protos.ExecutorInfo;
import org.apache.mesos.Protos.CommandInfo;
import org.apache.mesos.Scheduler;
import java.util.Scanner;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

public class MainRunner {

    static String frameworkName = "Framework-MyName";
    static String executorName = "Executor-MyName";
    static String path = "/opt/programming.git/Java/mesos_framework/build/libs/example-framework-1.0-SNAPSHOT.jar";
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
        uriBuilder.setValue(path);
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
        } catch (IOException e) {
            e.printStackTrace();
        }

        Scheduler scheduler = new ExampleScheduler(getExecutorInfo());
        MesosSchedulerDriver driver = new MesosSchedulerDriver(scheduler, getFrameworkInfo(), mesosMaster);
        int status = driver.run() == Protos.Status.DRIVER_STOPPED ? 0 : 1;

        driver.stop();
        System.exit(status);
    }

    public static void main(String[] args) throws Exception {
        runFramework(args[0], args[1]);
    }
}
