package org.example;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import software.amazon.awssdk.auth.credentials.AwsCredentialsProvider;
import software.amazon.awssdk.auth.credentials.EnvironmentVariableCredentialsProvider;
import software.amazon.awssdk.awscore.exception.AwsServiceException;
import software.amazon.awssdk.core.exception.SdkClientException;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.costexplorer.CostExplorerClient;
import software.amazon.awssdk.services.costexplorer.model.DateInterval;
import software.amazon.awssdk.services.costexplorer.model.GetCostAndUsageRequest;
import software.amazon.awssdk.services.costexplorer.model.GetCostAndUsageResponse;
import software.amazon.awssdk.services.costexplorer.model.Granularity;

public class Main {

    static CostExplorerClient ce;

    static Logger logger = LogManager.getLogger();

    private static void init() {
        AwsCredentialsProvider credentialsProvider = EnvironmentVariableCredentialsProvider.create();

        if (credentialsProvider.resolveCredentials() == null) {
            throw new RuntimeException("No AWS security credentials found\n");
        }

        ce = CostExplorerClient.builder()
                .credentialsProvider(credentialsProvider)
                .region(Region.EU_WEST_1)
                .build();
    }


    public static void main(String[] args) throws Exception {
        System.out.println("AWS Cost Explorer Simple Example!");

        logger.error(System.getenv("TEST"));

        init();

        try {
            GetCostAndUsageRequest.Builder requestBuilder = GetCostAndUsageRequest.builder()
                    .granularity(Granularity.DAILY)
                    .metrics("BLENDED_COST")
                    .timePeriod(DateInterval.builder()
                            .start("2023-05-01")
                            .end("2023-05-30")
                            .build());

            GetCostAndUsageResponse response = ce.getCostAndUsage(requestBuilder.build());
            System.out.printf("Yout Have "+ response);

        } catch (AwsServiceException ase) {
            System.out.println("Full Server Error Message:    " + ase.getMessage());
            System.out.println("Service Name:    " + ase.awsErrorDetails().serviceName());
            System.out.println("HTTP Status Code: " + ase.awsErrorDetails().sdkHttpResponse().statusCode());
            System.out.println("AWS Error Code:   " + ase.awsErrorDetails().errorCode());
            System.out.println("Error Type:       " + ase.awsErrorDetails().errorMessage());
            System.out.println("Request ID:       " + ase.requestId());
        } catch (SdkClientException sce) {
            System.out.println("Client Error Message: " + sce.getMessage());
        }
    }
}