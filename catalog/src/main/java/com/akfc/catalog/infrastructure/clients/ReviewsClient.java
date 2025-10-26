package com.akfc.catalog.infrastructure.clients;

import com.akfc.catalog.dto.ReviewDto;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import org.eclipse.microprofile.rest.client.inject.RegisterRestClient;

import java.util.List;

/**
 * REST client for the reviews-service.
 * Uses dynamic URL resolution via Consul service discovery.
 */
@Path("/reviews")
@RegisterRestClient(configKey = "reviews-service")
@Produces(MediaType.APPLICATION_JSON)
public interface ReviewsClient {

    /**
     * Get all approved reviews for a specific resource.
     *
     * @param resourceId Work ID
     * @return List of approved reviews
     */
    @GET
    @Path("/by-resource/{resourceId}")
    List<ReviewDto> getReviewsByWorkId(@PathParam("resourceId") Long resourceId);
}
