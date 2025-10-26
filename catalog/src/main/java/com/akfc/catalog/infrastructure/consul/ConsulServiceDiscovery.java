package com.akfc.catalog.infrastructure.consul;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.enterprise.context.ApplicationScoped;
import org.eclipse.microprofile.config.inject.ConfigProperty;
import org.jboss.logging.Logger;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Random;

/**
 * Consul service discovery client for dynamically discovering service URLs.
 * Queries Consul's health check API to find healthy service instances.
 */
@ApplicationScoped
public class ConsulServiceDiscovery {

    private static final Logger LOG = Logger.getLogger(ConsulServiceDiscovery.class);

    @ConfigProperty(name = "consul.host", defaultValue = "localhost")
    String consulHost;

    @ConfigProperty(name = "consul.port", defaultValue = "8500")
    int consulPort;

    private final HttpClient httpClient = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(5))
            .build();

    private final Random random = new Random();

    /**
     * Discover a service URL from Consul by service name.
     * Returns a random healthy instance if multiple are available.
     *
     * @param serviceName Name of the service registered in Consul
     * @return Optional containing service URL if found, empty if not found or all unhealthy
     */
    public Optional<String> discoverService(String serviceName) {
        try {
            // Query Consul for healthy service instances
            String consulUrl = String.format("http://%s:%d/v1/health/service/%s?passing=true",
                    consulHost, consulPort, serviceName);

            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(consulUrl))
                    .GET()
                    .timeout(Duration.ofSeconds(5))
                    .build();

            HttpResponse<String> response = httpClient.send(request,
                    HttpResponse.BodyHandlers.ofString());

            if (response.statusCode() != 200) {
                LOG.warnf("Consul query failed for service '%s': status %d",
                        serviceName, response.statusCode());
                return Optional.empty();
            }

            // Parse JSON response
            List<ServiceInstance> instances = parseServiceInstances(response.body());

            if (instances.isEmpty()) {
                LOG.warnf("No healthy instances found for service '%s'", serviceName);
                return Optional.empty();
            }

            // Select a random instance (simple load balancing)
            ServiceInstance instance = instances.get(random.nextInt(instances.size()));
            String serviceUrl = String.format("http://%s:%d",
                    instance.address, instance.port);

            LOG.debugf("Discovered service '%s' at %s", serviceName, serviceUrl);
            return Optional.of(serviceUrl);

        } catch (Exception e) {
            LOG.errorf(e, "Error discovering service '%s' from Consul", serviceName);
            return Optional.empty();
        }
    }

    /**
     * Parse Consul health check JSON response to extract service instances.
     */
    private List<ServiceInstance> parseServiceInstances(String json) {
        List<ServiceInstance> instances = new ArrayList<>();
        try {
            // Simple JSON parsing for Consul health endpoint response
            // Format: [{"Service": {"Address": "...", "Port": ...}}, ...]
            com.fasterxml.jackson.databind.ObjectMapper mapper =
                    new com.fasterxml.jackson.databind.ObjectMapper();
            ConsulHealthResponse[] responses = mapper.readValue(json, ConsulHealthResponse[].class);

            for (ConsulHealthResponse response : responses) {
                if (response.service != null) {
                    ServiceInstance instance = new ServiceInstance();
                    instance.address = response.service.address;
                    instance.port = response.service.port;
                    instances.add(instance);
                }
            }
        } catch (Exception e) {
            LOG.errorf(e, "Error parsing Consul response");
        }
        return instances;
    }

    /**
     * DTO for Consul health check response.
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    private static class ConsulHealthResponse {
        @JsonProperty("Service")
        public ServiceInfo service;
    }

    /**
     * DTO for service information in Consul response.
     */
    @JsonIgnoreProperties(ignoreUnknown = true)
    private static class ServiceInfo {
        @JsonProperty("Address")
        public String address;

        @JsonProperty("Port")
        public int port;
    }

    /**
     * Internal representation of a service instance.
     */
    private static class ServiceInstance {
        public String address;
        public int port;
    }
}
