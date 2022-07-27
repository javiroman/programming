package io.fabric8;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;
import com.fasterxml.jackson.dataformat.yaml.YAMLParser;
import io.fabric8.kubernetes.api.model.HasMetadata;
import io.fabric8.kubernetes.api.model.KubernetesResourceList;
import io.fabric8.kubernetes.api.model.ListOptions;
import io.fabric8.kubernetes.api.model.Pod;
import io.fabric8.kubernetes.api.model.apps.Deployment;
import io.fabric8.kubernetes.client.*;
import io.smallrye.mutiny.Multi;
import org.jboss.resteasy.annotations.SseElementType;
import org.reactivestreams.Publisher;

import javax.inject.Inject;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.io.*;
import java.util.List;
import java.util.stream.Collectors;

@Path("/kube")
public class KubeResource {

    @Inject
    KubernetesClient client;

    @GET
    @Produces(MediaType.TEXT_PLAIN)
    @Path("/pods")
    public List<String> getPods() throws IOException {
        EnvironmentKubernetesConfig config = new EnvironmentKubernetesConfig();
        client = new DefaultKubernetesClient(config.loadConfig());

        ListOptions options = new ListOptions();
        List<String> hostnames = client.pods().list(/*options*/).getItems()
                .stream()
                .map(item -> item.getMetadata().getName())
                .collect(Collectors.toList());

        return hostnames;
    }

    @GET
    @Produces(MediaType.SERVER_SENT_EVENTS)
    @SseElementType(MediaType.TEXT_PLAIN)
    @Path("/pods/stream")
    public Publisher<String> getPodsMulti() {
        EnvironmentKubernetesConfig config = new EnvironmentKubernetesConfig();
        client = new DefaultKubernetesClient(config.loadConfig());


        Multi<String> multi = Multi.createFrom().emitter(emitter -> {
            Watch watch = client.pods().watch(new Watcher<Pod>() {
                @Override
                public void eventReceived(Action action, Pod resource) {
                    emitter.emit(action.toString() + ": " + resource.getMetadata().getName());
                }

                @Override
                public void onClose(WatcherException e) {

                }
            });
        });

        return multi;
    }

    @POST
    @Produces(MediaType.TEXT_PLAIN)
    @Path("/deploy/{name}")
    public String createDeployment() throws IOException {
        EnvironmentKubernetesConfig config = new EnvironmentKubernetesConfig();
        client = new DefaultKubernetesClient(config.loadConfig());

        /*
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        File file = new File(classLoader.getResource("deployment-example.yaml").getFile());
         */

        // loop for apply every kubernetes object from a single YAML with multiple objects.
        for (Object o: convertYamlToJson("deployment-example.yaml")){
            ObjectMapper jsonWriter = new ObjectMapper();
            String jsonString = jsonWriter.writeValueAsString(o);
            List<HasMetadata> result = client
                    .load(new ByteArrayInputStream(jsonString.getBytes()))
                    .get();

            for (HasMetadata list: result) {
                if (list.getClass() == Deployment.class) {
                    System.out.println(list.getKind());
                    ((Deployment) list).getSpec().setReplicas(2);
                } else {
                        System.out.println("tus muelas");
                    }
            }

            // Apply Kubernetes Resources
            client.resourceList(result).inNamespace("default").createOrReplace();
        }

        return "arza!";
    }

    /*
      Read a YAML from the internal JVM "resources" folder.
     */
    List<Object> convertYamlToJson(String yaml) throws IOException {
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        File file = new File(classLoader.getResource(yaml).getFile());

        YAMLFactory yamlFactory = new YAMLFactory();
        ObjectMapper mapper = new ObjectMapper();

        YAMLParser yamlParser = yamlFactory.createParser(file);
        List<Object> docs = mapper.readValues(yamlParser, Object.class).readAll();

        /*
        for (Object l: docs) {
            System.out.println("################# MANIFEST ################");
            System.out.println(l);
        }
        ObjectMapper yamlReader = new ObjectMapper(new YAMLFactory());
        Object obj = yamlReader.readValue(file, Object.class);
        ObjectMapper jsonWriter = new ObjectMapper();
        return jsonWriter.writeValueAsString(docs);
         */
       return docs;
    }

}