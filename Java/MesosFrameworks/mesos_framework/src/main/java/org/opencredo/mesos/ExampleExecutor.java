/*
 * github.com/apache/mesos/blob/master/include/mesos/executor.hpp
 */
package org.opencredo.mesos;

import org.apache.mesos.*;
import org.apache.mesos.Protos;
import java.util.concurrent.TimeUnit;

public class ExampleExecutor implements Executor {

    /*
     * Invoked once the executor driver has been able to successfully
     * connect with Mesos. In particular, a scheduler can pass some
     * data to its executors through the `FrameworkInfo.ExecutorInfo`'s
     * data field.
     */
    @Override
    public void registered(ExecutorDriver executorDriver,
            Protos.ExecutorInfo executorInfo,
            Protos.FrameworkInfo frameworkInfo, Protos.SlaveInfo slaveInfo) {
    }

    /*
     * Invoked when the executor re-registers with a restarted slave.
     */
    @Override
    public void reregistered(ExecutorDriver executorDriver,
            Protos.SlaveInfo slaveInfo) {
    }

    /*
     * Invoked when the executor becomes "disconnected" from the slave
     * (e.g., the slave is being restarted due to an upgrade).
     */
    @Override
    public void disconnected(ExecutorDriver executorDriver) {
    }

    /*
     * Invoked when a task has been launched on this executor (initiated
     * via Scheduler::launchTasks). Note that this task can be realized
     * with a thread, a process, or some simple computation, however, no
     * other callbacks will be invoked on this executor until this
     * callback has returned.
     */
    @Override
    public void launchTask(ExecutorDriver executorDriver,
            Protos.TaskInfo taskInfo) {

        try {
            TimeUnit.SECONDS.sleep(5);
        } 
        catch(InterruptedException ex) 
        {
                Thread.currentThread().interrupt();

        }
        Integer id = Integer.parseInt(taskInfo.getData().toStringUtf8());
        String reply = id.toString();
        executorDriver.sendFrameworkMessage(reply.getBytes());
        Protos.TaskStatus status = Protos.TaskStatus.newBuilder()
            .setTaskId(taskInfo.getTaskId())
            .setState(Protos.TaskState.TASK_FINISHED).build();
        executorDriver.sendStatusUpdate(status);
    }

    /*
     * Invoked when a task running within this executor has been killed
     * (via SchedulerDriver::killTask). Note that no status update will
     * be sent on behalf of the executor, the executor is responsible
     * for creating a new TaskStatus (i.e., with TASK_KILLED) and
     * invoking ExecutorDriver::sendStatusUpdate.
     */
    @Override
    public void killTask(ExecutorDriver executorDriver, Protos.TaskID taskID) {

    }

    /*
     * Invoked when a framework message has arrived for this
     * executor. These messages are best effort; do not expect a
     * framework message to be retransmitted in any reliable fashion.
     */
    @Override
    public void frameworkMessage(ExecutorDriver executorDriver, byte[] bytes) {

    }

    /*
     * Invoked when the executor should terminate all of its currently
     * running tasks. Note that after a Mesos has determined that an
     * executor has terminated any tasks that the executor did not send
     * terminal status updates for (e.g., TASK_KILLED, TASK_FINISHED,
     * TASK_FAILED, etc) a TASK_LOST status update will be created.
     */
    @Override
    public void shutdown(ExecutorDriver executorDriver) {

    }

    /*
     * Invoked when a fatal error has occurred with the executor and/or
     * executor driver. The driver will be aborted BEFORE invoking this
     * callback.
     */
    @Override
    public void error(ExecutorDriver executorDriver, String s) {

    }

    public static void main(String[] args) throws Exception {
        MesosExecutorDriver driver = new MesosExecutorDriver(
                new ExampleExecutor());
        System.exit(driver.run() == Protos.Status.DRIVER_STOPPED ? 0 : 1);
    }
}
