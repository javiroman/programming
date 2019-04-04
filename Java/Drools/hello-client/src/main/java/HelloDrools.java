import org.kie.api.KieServices;
import org.kie.api.command.BatchExecutionCommand;
import org.kie.api.command.KieCommands;
import org.kie.server.api.marshalling.MarshallingFormat;
import org.kie.server.client.KieServicesClient;
import org.kie.server.client.KieServicesConfiguration;
import org.kie.server.client.KieServicesFactory;
import org.kie.server.client.RuleServicesClient;

import java.util.Arrays;

public class HelloDrools {
    /*
        The Kie Server (Decision Manager Server) Information
     */
    private static final String URL =
            "http://127.0.0.1:8080/kie-server/services/rest/server";

    private static final String USER = "kieserver";
    private static final String PASSWORD = "kieserver";
    private static final String CONTAINER_NAME = "hellowold-test";
    private static final MarshallingFormat FORMAT = MarshallingFormat.JSON;


    public static void main(String[] args) {
        // Server Configuration
        KieServicesConfiguration conf =
                KieServicesFactory.newRestConfiguration(URL, USER, PASSWORD);

        conf.setMarshallingFormat(FORMAT);

        KieServicesClient kieServicesClient =
                KieServicesFactory.newKieServicesClient(conf);

        // To execute rules we must use the Rules Client
        RuleServicesClient ruleClients =
                kieServicesClient.getServicesClient(RuleServicesClient.class);

        KieCommands kieCommands = KieServices.Factory.get().getCommands();

        BatchExecutionCommand batchCommand =
                kieCommands.newBatchExecution(Arrays.asList(
                        kieCommands.newInsert("My first Drools Example"),
                        kieCommands.newFireAllRules()
                ));
        // We now execute the commands, in this case we have no results,
        // the result will be something printed on server console
        ruleClients.executeCommandsWithResults(CONTAINER_NAME, batchCommand);
    }
}