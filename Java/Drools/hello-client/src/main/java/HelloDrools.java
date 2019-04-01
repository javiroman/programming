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
    private static final String URL = "http://localhost:8080/kie-server";
    private static final String USER = "kieserver";
    private static final String PASSWORD = "kieserver";
    private static final MarshallingFormat FORMAT = MarshallingFormat.JAXB;

    public static void main(String[] args) {
        KieServicesConfiguration conf =
                KieServicesFactory.newRestConfiguration(URL, USER, PASSWORD);

        conf.setMarshallingFormat(FORMAT);

        KieServicesClient kieServicesClient =
                KieServicesFactory.newKieServicesClient(conf);

        RuleServicesClient ruleClients =
                kieServicesClient.getServicesClient(RuleServicesClient.class);

        KieCommands kieCommands = KieServices.Factory.get().getCommands();
        BatchExecutionCommand batchCommand =
                kieCommands.newBatchExecution(Arrays.asList(
                        kieCommands.newInsert("Javiroman"),
                        kieCommands.newFireAllRules()
                ));
        ruleClients.executeCommandsWithResults("hello", batchCommand);
    }
}