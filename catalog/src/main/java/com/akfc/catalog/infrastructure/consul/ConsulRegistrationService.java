package com.akfc.catalog.infrastructure.consul;

import io.quarkus.runtime.ShutdownEvent;
import io.quarkus.runtime.StartupEvent;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.enterprise.event.Observes;
import org.eclipse.microprofile.config.inject.ConfigProperty;
import org.jboss.logging.Logger;

import java.net.InetAddress;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.Optional;

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

    @ConfigProperty(name = "consul.service.address")
    Optional<String> configuredServiceAddress;

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

    private String getServiceAddress() {
        // Priority: 1. Config property, 2. Environment variable, 3. Auto-detect
        if (configuredServiceAddress.isPresent()) {
            LOG.debugf("Using configured service address: %s", configuredServiceAddress.get());
            return configuredServiceAddress.get();
        }

        String envAddress = System.getenv("SERVICE_ADDRESS");
        if (envAddress != null && !envAddress.isBlank()) {
            LOG.debugf("Using SERVICE_ADDRESS environment variable: %s", envAddress);
            return envAddress;
        }

        try {
            String detectedAddress = InetAddress.getLocalHost().getHostAddress();
            LOG.debugf("Auto-detected service address: %s", detectedAddress);
            return detectedAddress;
        } catch (Exception e) {
            LOG.warn("Failed to auto-detect service address, falling back to localhost", e);
            return "localhost";
        }
    }

    private void registerService() throws Exception {
        serviceId = serviceName + "-" + servicePort;
        String serviceAddress = getServiceAddress();

        String registrationJson = String.format("""
            {
              "ID": "%s",
              "Name": "%s",
              "Port": %d,
              "Address": "%s",
              "Tags": ["quarkus", "microservice"],
              "Check": {
                "TCP": "%s:%d",
                "Interval": "10s",
                "Timeout": "3s",
                "DeregisterCriticalServiceAfter": "90s"
              }
            }
            """, serviceId, serviceName, servicePort, serviceAddress, serviceAddress, servicePort);

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
