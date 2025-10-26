package com.akfc.catalog.services;

import com.akfc.catalog.data.Resource;
import com.akfc.catalog.dto.EnrichedResourceResponse;
import com.akfc.catalog.dto.ReviewDto;
import com.akfc.catalog.dto.UserDto;
import com.akfc.catalog.infrastructure.clients.ReviewsClient;
import com.akfc.catalog.infrastructure.clients.UsersClient;
import io.smallrye.faulttolerance.api.CircuitBreakerName;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import org.eclipse.microprofile.faulttolerance.*;
import org.eclipse.microprofile.rest.client.inject.RestClient;
import org.jboss.logging.Logger;

import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.stream.Collectors;

/**
 * Service for enriching catalog resources with data from reviews and users services.
 *
 * Implements complete resilience patterns:
 * - Retry: Automatic retry on transient failures
 * - Circuit Breaker: Prevents cascading failures
 * - Timeout: Limits execution time
 * - Fallback: Provides degraded service when dependencies fail
 * - Bulkhead: Limits concurrent calls to prevent resource exhaustion
 */
@ApplicationScoped
public class ResourceEnrichmentService {

    private static final Logger LOG = Logger.getLogger(ResourceEnrichmentService.class);

    @Inject
    @RestClient
    ReviewsClient reviewsClient;

    @Inject
    @RestClient
    UsersClient usersClient;

    /**
     * Enrich a resource with reviews and user information.
     *
     * Resilience patterns applied:
     * - Timeout: 10 seconds max
     * - Fallback: Returns resource without enrichment if dependencies fail
     *
     * @param resource Resource to enrich
     * @return Enriched resource response
     */
    @Timeout(value = 10, unit = ChronoUnit.SECONDS)
    @Fallback(fallbackMethod = "enrichResourceFallback")
    public EnrichedResourceResponse enrichResource(Resource resource) {
        LOG.infof("Enriching resource %d: %s", resource.id, resource.title);

        EnrichedResourceResponse response = new EnrichedResourceResponse();

        // Copy resource fields
        response.id = resource.id;
        response.title = resource.title;
        response.type = resource.type != null ? resource.type.name() : null;
        response.year = resource.year;
        response.creator = resource.creator;
        response.keywords = new ArrayList<>(resource.keywords);
        response.illustrationUrl = resource.illustrationUrl;
        response.status = resource.status != null ? resource.status.name() : null;

        // Fetch reviews with fault tolerance
        List<ReviewDto> reviews = getReviewsForResource(resource.id);

        // Enrich each review with user information
        for (ReviewDto review : reviews) {
            UserDto user = getUserById(review.userId);
            review.user = user;
        }

        response.reviews = reviews;
        response.statistics = new EnrichedResourceResponse.ReviewStatistics(reviews);

        LOG.infof("Successfully enriched resource %d with %d reviews",
                  resource.id, reviews.size());

        return response;
    }

    /**
     * Fallback method when enrichment fails.
     * Returns resource data without enrichment.
     */
    public EnrichedResourceResponse enrichResourceFallback(Resource resource) {
        LOG.warnf("Using fallback for resource %d enrichment", resource.id);

        EnrichedResourceResponse response = new EnrichedResourceResponse();
        response.id = resource.id;
        response.title = resource.title;
        response.type = resource.type != null ? resource.type.name() : null;
        response.year = resource.year;
        response.creator = resource.creator;
        response.keywords = new ArrayList<>(resource.keywords);
        response.illustrationUrl = resource.illustrationUrl;
        response.status = resource.status != null ? resource.status.name() : null;
        response.reviews = Collections.emptyList();
        response.statistics = new EnrichedResourceResponse.ReviewStatistics(Collections.emptyList());

        return response;
    }

    /**
     * Get reviews for a resource from the reviews service.
     *
     * Resilience patterns:
     * - Retry: 3 attempts with 500ms delay
     * - Circuit Breaker: Opens after 4 failures in 10 requests
     * - Timeout: 5 seconds max
     * - Bulkhead: Max 10 concurrent calls
     * - Fallback: Returns empty list if service unavailable
     */
    @Retry(
        maxRetries = 3,
        delay = 500,
        delayUnit = ChronoUnit.MILLIS,
        jitter = 200
    )
    @CircuitBreaker(
        requestVolumeThreshold = 10,
        failureRatio = 0.4,
        delay = 5000,
        delayUnit = ChronoUnit.MILLIS,
        successThreshold = 2
    )
    @CircuitBreakerName("reviews-service")
    @Timeout(value = 5, unit = ChronoUnit.SECONDS)
    @Bulkhead(value = 10, waitingTaskQueue = 10)
    @Fallback(fallbackMethod = "getReviewsFallback")
    public List<ReviewDto> getReviewsForResource(Long resourceId) {
        LOG.debugf("Fetching reviews for resource %d", resourceId);

        try {
            List<ReviewDto> reviews = reviewsClient.getReviewsByWorkId(resourceId);
            LOG.debugf("Fetched %d reviews for resource %d", reviews.size(), resourceId);
            return reviews != null ? reviews : Collections.emptyList();
        } catch (Exception e) {
            LOG.errorf(e, "Error fetching reviews for resource %d", resourceId);
            throw e;
        }
    }

    /**
     * Fallback for reviews service failure.
     */
    public List<ReviewDto> getReviewsFallback(Long resourceId) {
        LOG.warnf("Using fallback for reviews of resource %d", resourceId);
        return Collections.emptyList();
    }

    /**
     * Get user information by ID from the users service.
     *
     * Resilience patterns:
     * - Retry: 3 attempts with 300ms delay
     * - Circuit Breaker: Opens after 4 failures in 10 requests
     * - Timeout: 3 seconds max
     * - Bulkhead: Max 20 concurrent calls (higher than reviews as we call for each review)
     * - Fallback: Returns placeholder user if service unavailable
     */
    @Retry(
        maxRetries = 3,
        delay = 300,
        delayUnit = ChronoUnit.MILLIS,
        jitter = 100
    )
    @CircuitBreaker(
        requestVolumeThreshold = 10,
        failureRatio = 0.4,
        delay = 5000,
        delayUnit = ChronoUnit.MILLIS,
        successThreshold = 2
    )
    @CircuitBreakerName("users-service")
    @Timeout(value = 3, unit = ChronoUnit.SECONDS)
    @Bulkhead(value = 20, waitingTaskQueue = 20)
    @Fallback(fallbackMethod = "getUserFallback")
    public UserDto getUserById(Long userId) {
        LOG.debugf("Fetching user %d", userId);

        try {
            UserDto user = usersClient.getUserById(userId);
            LOG.debugf("Fetched user %d: %s", userId, user != null ? user.email : "null");
            return user;
        } catch (Exception e) {
            LOG.errorf(e, "Error fetching user %d", userId);
            throw e;
        }
    }

    /**
     * Fallback for users service failure.
     * Returns a placeholder user.
     */
    public UserDto getUserFallback(Long userId) {
        LOG.warnf("Using fallback for user %d", userId);

        UserDto placeholder = new UserDto();
        placeholder.id = userId;
        placeholder.firstName = "Unknown";
        placeholder.lastName = "User";
        placeholder.email = "unavailable@system";
        placeholder.accountStatus = "UNKNOWN";

        return placeholder;
    }

    /**
     * Asynchronous version for better performance when enriching multiple resources.
     * Note: Fallback is handled by the enrichResource method.
     *
     * @param resource Resource to enrich
     * @return CompletionStage with enriched resource
     */
    @Asynchronous
    @Timeout(value = 10, unit = ChronoUnit.SECONDS)
    public CompletionStage<EnrichedResourceResponse> enrichResourceAsync(Resource resource) {
        return CompletableFuture.completedFuture(enrichResource(resource));
    }
}
