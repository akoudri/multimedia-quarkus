package com.akfc.catalog.infrastructure.consul;

import io.quarkus.runtime.ShutdownEvent;
import io.quarkus.runtime.StartupEvent;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.enterprise.event.Observes;
import org.eclipse.microprofile.config.inject.ConfigProperty;
import org.jboss.logging.Logger;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;

@ApplicationScoped
public class ConsulRegistrationService {

    private static final Logger LOG = Logger.getLogger(ConsulRegistrationService.class);

    @ConfigProperty(name = "quarkus.application.name")
    String serviceName;

    @ConfigProperty(name = "quarkus.http.port")
    int servicePort;

    @ConfigProperty(name = "consul.host", defaultValue = "localhost")
    String consulHost;

    @ConfigProperty(name = "consul.port", defaultValue = "8500")
    int consulPort;

    @ConfigProperty(name = "consul.registration.enabled", defaultValue = "true")
    boolean registrationEnabled;

    private final HttpClient httpClient = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(5))
            .build();

    private String serviceId;

    void onStart(@Observes StartupEvent ev) {
        if (!registrationEnabled) {
            LOG.info("Consul registration is disabled");
            return;
        }

        try {
            registerService();
            LOG.infof("Successfully registered service '%s' with Consul at %s:%d",
                    serviceName, consulHost, consulPort);
        } catch (Exception e) {
            LOG.errorf(e, "Failed to register service '%s' with Consul", serviceName);
        }
    }

    void onStop(@Observes ShutdownEvent ev) {
        if (!registrationEnabled || serviceId == null) {
            return;
        }

        try {
            deregisterService();
            LOG.infof("Successfully deregistered service '%s' from Consul", serviceName);
        } catch (Exception e) {
            LOG.errorf(e, "Failed to deregister service '%s' from Consul", serviceName);
        }
    }

    private void registerService() throws Exception {
        serviceId = serviceName + "-" + servicePort;

        String registrationJson = String.format("""
            {
              "ID": "%s",
              "Name": "%s",
              "Port": %d,
              "Address": "172.22.0.1",
              "Tags": ["quarkus", "microservice"],
              "Check": {
                "TCP": "172.22.0.1:%d",
                "Interval": "10s",
                "Timeout": "3s",
                "DeregisterCriticalServiceAfter": "90s"
              }
            }
            """, serviceId, serviceName, servicePort, servicePort);

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(String.format("http://%s:%d/v1/agent/service/register", consulHost, consulPort)))
                .header("Content-Type", "application/json")
                .PUT(HttpRequest.BodyPublishers.ofString(registrationJson))
                .build();

        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());

        if (response.statusCode() != 200) {
            throw new RuntimeException("Failed to register with Consul: " + response.statusCode() + " - " + response.body());
        }
    }

    private void deregisterService() throws Exception {
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(String.format("http://%s:%d/v1/agent/service/deregister/%s",
                        consulHost, consulPort, serviceId)))
                .PUT(HttpRequest.BodyPublishers.noBody())
                .build();

        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());

        if (response.statusCode() != 200) {
            throw new RuntimeException("Failed to deregister from Consul: " + response.statusCode());
        }
    }
}
