package com.akfc.resources;

import com.akfc.dto.CreateReviewRequest;
import com.akfc.dto.ReviewResponse;
import com.akfc.dto.UpdateReviewRequest;
import com.akfc.services.ReviewService;
import io.smallrye.mutiny.Uni;
import jakarta.inject.Inject;
import jakarta.validation.Valid;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import java.util.List;
import java.util.Map;

/**
 * REACTIVE REST endpoints for Review operations.
 *
 * All endpoints return Uni<T> for non-blocking request handling.
 * This enables high concurrency and efficient resource usage.
 */
@Path("/api/reviews")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
@Tag(name = "Reviews", description = "Review management operations")
public class ReviewResource {

    @Inject
    ReviewService reviewService;

    // ========== CREATE ==========

    @POST
    @Operation(summary = "Create a new review")
    public Uni<Response> createReview(@Valid CreateReviewRequest request) {
        return reviewService.createReview(request, "system")
            .map(review -> Response.status(Response.Status.CREATED).entity(review).build());
    }

    // ========== READ ==========

    @GET
    @Path("/{id}")
    @Operation(summary = "Get review by ID")
    public Uni<Response> getReview(@PathParam("id") Long id) {
        return reviewService.getReviewById(id)
            .map(review -> Response.ok(review).build());
    }

    @GET
    @Path("/resource/{resourceId}")
    @Operation(summary = "Get approved reviews for a resource")
    public Uni<Response> getReviewsForResource(@PathParam("resourceId") Long resourceId) {
        return reviewService.getApprovedReviewsForWork(resourceId)
            .map(reviews -> Response.ok(reviews).build());
    }

    @GET
    @Path("/user/{userId}")
    @Operation(summary = "Get reviews by user")
    public Uni<Response> getReviewsByUser(@PathParam("userId") Long userId) {
        return reviewService.getReviewsByUser(userId)
            .map(reviews -> Response.ok(reviews).build());
    }

    @GET
    @Path("/pending")
    @Operation(summary = "Get pending reviews for moderation")
    public Uni<Response> getPendingReviews() {
        return reviewService.getPendingReviews()
            .map(reviews -> Response.ok(reviews).build());
    }

    @GET
    @Path("/flagged")
    @Operation(summary = "Get flagged reviews")
    public Uni<Response> getFlaggedReviews() {
        return reviewService.getFlaggedReviews()
            .map(reviews -> Response.ok(reviews).build());
    }

    @GET
    @Path("/resource/{resourceId}/rating")
    @Operation(summary = "Get average rating for a resource")
    public Uni<Response> getAverageRating(@PathParam("resourceId") Long resourceId) {
        return reviewService.getAverageRating(resourceId)
            .map(rating -> Response.ok(Map.of("averageRating", rating != null ? rating : 0.0)).build());
    }

    @GET
    @Path("/resource/{resourceId}/distribution")
    @Operation(summary = "Get rating distribution for a resource")
    public Uni<Response> getRatingDistribution(@PathParam("resourceId") Long resourceId) {
        return reviewService.getRatingDistribution(resourceId)
            .map(distribution -> Response.ok(distribution).build());
    }

    // ========== UPDATE ==========

    @PUT
    @Path("/{id}")
    @Operation(summary = "Update a review")
    public Uni<Response> updateReview(
            @PathParam("id") Long id,
            @Valid UpdateReviewRequest request) {
        return reviewService.updateReview(id, request, "system")
            .map(review -> Response.ok(review).build());
    }

    // ========== MODERATION ==========

    @POST
    @Path("/{id}/approve")
    @Operation(summary = "Approve a review")
    public Uni<Response> approveReview(@PathParam("id") Long id) {
        return reviewService.approveReview(id, "moderator")
            .map(review -> Response.ok(review).build());
    }

    @POST
    @Path("/{id}/reject")
    @Operation(summary = "Reject a review")
    public Uni<Response> rejectReview(
            @PathParam("id") Long id,
            @QueryParam("reason") String reason) {
        return reviewService.rejectReview(id, reason, "moderator")
            .map(review -> Response.ok(review).build());
    }

    @POST
    @Path("/{id}/flag")
    @Operation(summary = "Flag a review")
    public Uni<Response> flagReview(
            @PathParam("id") Long id,
            @QueryParam("reason") String reason) {
        return reviewService.flagReview(id, reason, "user")
            .map(review -> Response.ok(review).build());
    }

    @POST
    @Path("/{id}/reset")
    @Operation(summary = "Reset review to pending status")
    public Uni<Response> resetToPending(@PathParam("id") Long id) {
        return reviewService.resetToPending(id, "moderator")
            .map(review -> Response.ok(review).build());
    }

    @POST
    @Path("/bulk/approve")
    @Operation(summary = "Bulk approve reviews")
    public Uni<Response> bulkApprove(List<Long> reviewIds) {
        return reviewService.bulkApproveReviews(reviewIds, "moderator")
            .map(count -> Response.ok(Map.of("approvedCount", count)).build());
    }

    @POST
    @Path("/bulk/reject")
    @Operation(summary = "Bulk reject reviews")
    public Uni<Response> bulkReject(
            List<Long> reviewIds,
            @QueryParam("reason") String reason) {
        return reviewService.bulkRejectReviews(reviewIds, reason, "moderator")
            .map(count -> Response.ok(Map.of("rejectedCount", count)).build());
    }

    // ========== DELETE ==========

    @POST
    @Path("/{id}/archive")
    @Operation(summary = "Archive a review (soft delete)")
    public Uni<Response> archiveReview(
            @PathParam("id") Long id,
            @QueryParam("reason") String reason) {
        return reviewService.archiveReview(id, reason, "admin")
            .map(review -> Response.ok(review).build());
    }

    @POST
    @Path("/{id}/restore")
    @Operation(summary = "Restore an archived review")
    public Uni<Response> restoreReview(@PathParam("id") Long id) {
        return reviewService.restoreReview(id, "admin")
            .map(review -> Response.ok(review).build());
    }

    @DELETE
    @Path("/{id}")
    @Operation(summary = "Permanently delete a review")
    public Uni<Response> deleteReview(@PathParam("id") Long id) {
        return reviewService.deleteReview(id)
            .map(v -> Response.noContent().build());
    }

    // ========== UTILITY ==========

    @GET
    @Path("/check")
    @Operation(summary = "Check if user has reviewed a resource")
    public Uni<Response> hasUserReviewedWork(
            @QueryParam("userId") Long userId,
            @QueryParam("resourceId") Long resourceId) {
        return reviewService.hasUserReviewedWork(userId, resourceId)
            .map(hasReviewed -> Response.ok(Map.of("hasReviewed", hasReviewed)).build());
    }
}
