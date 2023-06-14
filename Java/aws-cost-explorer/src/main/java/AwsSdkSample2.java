import com.amazonaws.AmazonClientException;
import com.amazonaws.AmazonServiceException;
import com.amazonaws.auth.AWSCredentialsProvider;
import com.amazonaws.auth.EnvironmentVariableCredentialsProvider;
import com.amazonaws.services.costexplorer.AWSCostExplorer;
import com.amazonaws.services.costexplorer.AWSCostExplorerClient;
import com.amazonaws.services.costexplorer.model.DateInterval;
import com.amazonaws.services.costexplorer.model.GetCostAndUsageRequest;
import com.amazonaws.services.costexplorer.model.Granularity;

public class AwsSdkSample2 {

    static AWSCostExplorer ce;

    private static void init() {
        AWSCredentialsProvider credentialsProvider = new EnvironmentVariableCredentialsProvider();

        if (credentialsProvider.getCredentials() == null) {
            throw new RuntimeException("No AWS security credentials found\n");
        }

        /*
        File configFile = new File(System.getProperty("user.home"), ".aws/credentials");
        AWSCredentialsProvider credentialsProvider = new ProfileCredentialsProvider(
            new ProfilesConfigFile(configFile), "default");

        if (credentialsProvider.getCredentials() == null) {
            throw new RuntimeException("No AWS security credentials found:\n"
                    + "Make sure you've configured your credentials in: " + configFile.getAbsolutePath() + "\n"
                    + "For more information on configuring your credentials, see "
                    + "http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-getting-started.html");
        }
         */

        ce = AWSCostExplorerClient.builder()
                .withCredentials(credentialsProvider)
                .withRegion("eu-west-1")
                .build();
    }


    public static void main(String[] args) {
        System.out.println("AWS Cost Explorer Simple Example!");
        init();

        try {
            System.out.println("You have " +
                    ce.getCostAndUsage(new GetCostAndUsageRequest()
                            .withTimePeriod(new DateInterval().withStart("2023-05-01").withEnd("2023-05-30"))
                            .withMetrics("BLENDED_COST")
                            .withGranularity(Granularity.DAILY)));

        } catch (AmazonServiceException ase) {
            System.out.println("Server Error Message:    " + ase.getMessage());
            System.out.println("HTTP Status Code: " + ase.getStatusCode());
            System.out.println("AWS Error Code:   " + ase.getErrorCode());
            System.out.println("Error Type:       " + ase.getErrorType());
            System.out.println("Request ID:       " + ase.getRequestId());
        } catch (AmazonClientException ace) {
            System.out.println("Client Error Message: " + ace.getMessage());
        }
    }
}
