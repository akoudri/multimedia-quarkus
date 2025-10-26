package com.akfc.catalog.infrastructure.clients;

import com.akfc.catalog.dto.UserDto;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import org.eclipse.microprofile.rest.client.inject.RegisterRestClient;

/**
 * REST client for the users-service.
 * Uses dynamic URL resolution via Consul service discovery.
 */
@Path("/users")
@RegisterRestClient(configKey = "users-service")
@Produces(MediaType.APPLICATION_JSON)
public interface UsersClient {

    /**
     * Get a user by ID.
     *
     * @param id User ID
     * @return User information
     */
    @GET
    @Path("/{id}")
    UserDto getUserById(@PathParam("id") Long id);
}
