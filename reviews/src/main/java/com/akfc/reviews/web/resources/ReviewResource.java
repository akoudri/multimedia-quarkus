package com.akfc.reviews.web.resources;

import com.akfc.reviews.dto.CreateReviewRequest;
import com.akfc.reviews.dto.ReviewResponse;
import com.akfc.reviews.dto.UpdateReviewRequest;
import com.akfc.reviews.services.ReviewService;
import jakarta.inject.Inject;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.*;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.media.Content;
import org.eclipse.microprofile.openapi.annotations.media.ExampleObject;
import org.eclipse.microprofile.openapi.annotations.media.Schema;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import java.net.URI;
import java.util.List;
import java.util.Map;

/**
 * REST API for Review Management and Moderation.
 *
 * Provides comprehensive review lifecycle management including:
 * - Full CRUD operations (Create, Read, Update, Delete)
 * - Review moderation workflow (approve, reject, flag)
 * - Bulk moderation operations
 * - Rating aggregates and statistics
 * - Work and user filtering
 *
 * Review Moderation Workflow:
 * PENDING → APPROVED (visible to users)
 * PENDING → REJECTED (not visible)
 * APPROVED/PENDING → FLAGGED (requires moderator attention)
 * FLAGGED → PENDING (reset for re-evaluation)
 * ANY → ARCHIVED (soft delete)
 *
 * Business Rules:
 * - Users can only have one review per resource
 * - Rating must be 1-5
 * - Comment must be at least 10 characters
 * - New reviews start with PENDING status
 * - Updated reviews return to PENDING for re-moderation
 * - Only approved reviews count toward aggregates
 *
 * Error Response Format:
 * All error responses follow the standard ErrorResponse structure with:
 * - message: Human-readable error description
 * - code: Machine-readable error code
 * - timestamp: When the error occurred
 * - details: Additional context (field violations, IDs, etc.)
 *
 * @see com.akfc.reviews.services.ReviewService
 * @see com.akfc.reviews.web.mappers - Exception mappers for error handling
 */
@Path("/reviews")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
@Tag(name = "Reviews", description = "Review management and moderation operations")
public class ReviewResource {

    @Inject
    ReviewService reviewService;

    // ========================================
    // CRUD OPERATIONS
    // ========================================

    /**
     * Create a new review.
     *
     * Business Rules:
     * - User can only have one review per resource (409 if duplicate)
     * - Rating must be 1-5 (400 if invalid)
     * - Comment must be at least 10 characters (400 if too short)
     * - New reviews start with PENDING status (require moderation)
     *
     * @param request Review creation request with resourceId, userId, rating, comment
     * @param uriInfo Context for building Location header
     * @return HTTP 201 with created review and Location header
     */
    @POST
    @Operation(
        summary = "Create a new review",
        description = "Creates a new review for a resource. Users can only have one review per resource. " +
                      "New reviews start with PENDING status and require moderation approval."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "201",
            description = "Review created successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ReviewResponse.class),
                examples = @ExampleObject(
                    name = "Created review",
                    value = """
                        {
                          "id": 1,
                          "resourceId": 10,
                          "userId": 5,
                          "rating": 5,
                          "comment": "Excellent book! Highly recommended for anyone interested in dystopian fiction.",
                          "status": "PENDING",
                          "moderationReason": null,
                          "moderatedBy": null,
                          "moderatedAt": null,
                          "archived": false,
                          "createdAt": "2024-03-20T10:15:30",
                          "createdBy": "user:5",
                          "modifiedAt": "2024-03-20T10:15:30",
                          "modifiedBy": "user:5"
                        }
                        """
                )
            ),
            headers = {
                @org.eclipse.microprofile.openapi.annotations.headers.Header(
                    name = "Location",
                    description = "URI of the created review",
                    schema = @Schema(type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.STRING)
                )
            }
        ),
        @APIResponse(
            responseCode = "400",
            description = "Validation error - Invalid input data",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = {
                    @ExampleObject(
                        name = "Rating validation",
                        value = """
                            {
                              "message": "Request validation failed",
                              "code": "VALIDATION_ERROR",
                              "timestamp": "2024-03-20T10:15:30",
                              "details": {
                                "violations": {
                                  "rating": "Rating must be between 1 and 5"
                                }
                              }
                            }
                            """
                    ),
                    @ExampleObject(
                        name = "Comment too short",
                        value = """
                            {
                              "message": "Validation failed for field: comment",
                              "code": "VALIDATION_ERROR",
                              "timestamp": "2024-03-20T10:15:30",
                              "details": {
                                "violations": {
                                  "comment": "Comment must be at least 10 characters"
                                }
                              }
                            }
                            """
                    )
                }
            )
        ),
        @APIResponse(
            responseCode = "409",
            description = "User already reviewed this resource",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Duplicate review",
                    value = """
                        {
                          "message": "User 5 has already reviewed resource 10",
                          "code": "DUPLICATE_REVIEW",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "userId": 5,
                            "resourceId": 10
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "500",
            description = "Internal server error"
        )
    })
    public Response createReview(@Valid CreateReviewRequest request, @Context UriInfo uriInfo) {
        // NOTE: In production, createdBy would come from SecurityIdentity (JWT/OIDC)
        // Format: "user:{userId}" to identify the review author
        String createdBy = "user:" + request.userId;

        ReviewResponse review = reviewService.createReview(request, createdBy);

        // Build Location header: /reviews/{id}
        URI location = uriInfo.getAbsolutePathBuilder()
            .path(review.id.toString())
            .build();

        // Return 201 Created with Location header and created resource
        return Response.created(location)
            .entity(review)
            .build();
    }

    /**
     * Get review by ID.
     *
     * @param id Review ID
     * @return Review response
     */
    @GET
    @Path("/{id}")
    @Operation(
        summary = "Get review by ID",
        description = "Retrieves a specific review by its ID. Only returns active reviews."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Review found",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ReviewResponse.class),
                examples = @ExampleObject(
                    name = "Review details",
                    value = """
                        {
                          "id": 1,
                          "resourceId": 10,
                          "userId": 5,
                          "rating": 5,
                          "comment": "Excellent book! Highly recommended.",
                          "status": "APPROVED",
                          "moderatedBy": "moderator:2",
                          "moderatedAt": "2024-03-20T11:00:00",
                          "createdAt": "2024-03-20T10:15:30"
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "Review not found",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Not found",
                    value = """
                        {
                          "message": "Review not found with ID: 999",
                          "code": "REVIEW_NOT_FOUND",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "reviewId": 999
                          }
                        }
                        """
                )
            )
        )
    })
    public ReviewResponse getReview(
            @PathParam("id")
            @Parameter(description = "Review ID", required = true, example = "1")
            Long id
    ) {
        return reviewService.getReviewById(id);
    }

    /**
     * Update an existing review.
     *
     * Business Rules:
     * - Only the review author can update their review
     * - Updated reviews go back to PENDING status for re-moderation
     * - Cannot update rejected reviews
     *
     * @param id Review ID
     * @param request Update request with new rating and comment
     * @return Updated review response
     */
    @PUT
    @Path("/{id}")
    @Operation(
        summary = "Update a review",
        description = "Updates a review's rating and comment. " +
                      "Updated reviews return to PENDING status for re-moderation. " +
                      "Cannot update rejected reviews."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Review updated successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ReviewResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Validation error - Invalid input data"
        ),
        @APIResponse(
            responseCode = "404",
            description = "Review not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "Cannot update rejected review",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Review rejected",
                    value = """
                        {
                          "message": "Cannot update rejected review",
                          "code": "REVIEW_REJECTED",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "businessRule": "REVIEW_REJECTED"
                          }
                        }
                        """
                )
            )
        )
    })
    public ReviewResponse updateReview(
            @PathParam("id")
            @Parameter(description = "Review ID", required = true, example = "1")
            Long id,

            @Valid UpdateReviewRequest request
    ) {
        // NOTE: In production, modifiedBy would come from SecurityIdentity
        // Should verify that modifiedBy matches the review's userId
        return reviewService.updateReview(id, request, "system");
    }

    /**
     * Delete a review permanently.
     *
     * Business Rule: Review must be archived first before permanent deletion.
     *
     * @param id Review ID
     * @return HTTP 204 No Content on success
     */
    @DELETE
    @Path("/{id}")
    @Operation(
        summary = "Delete a review permanently",
        description = "Permanently deletes a review. Review must be archived first. " +
                      "This operation is irreversible. Use POST /reviews/{id}/archive for soft delete instead."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "204",
            description = "Review deleted successfully"
        ),
        @APIResponse(
            responseCode = "404",
            description = "Review not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "Review must be archived first",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Not archived",
                    value = """
                        {
                          "message": "Cannot permanently delete non-archived review. Archive it first.",
                          "code": "REVIEW_NOT_ARCHIVED",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "businessRule": "REVIEW_NOT_ARCHIVED"
                          }
                        }
                        """
                )
            )
        )
    })
    public Response deleteReview(
            @PathParam("id")
            @Parameter(description = "Review ID", required = true, example = "1")
            Long id
    ) {
        reviewService.deleteReview(id);
        return Response.noContent().build();
    }

    // ========================================
    // BUSINESS OPERATIONS - FILTERING
    // ========================================

    /**
     * Get all approved reviews for a resource.
     *
     * Only returns approved, non-archived reviews visible to users.
     *
     * @param resourceId Work ID
     * @return List of approved reviews
     */
    @GET
    @Path("/by-resource/{resourceId}")
    @Operation(
        summary = "Get approved reviews for a resource",
        description = "Returns all approved reviews for a specific resource. " +
                      "Only approved, non-archived reviews are included."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "List of approved reviews",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ReviewResponse.class, type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.ARRAY),
                examples = @ExampleObject(
                    name = "Review list",
                    value = """
                        [
                          {
                            "id": 1,
                            "resourceId": 10,
                            "userId": 5,
                            "rating": 5,
                            "comment": "Excellent book!",
                            "status": "APPROVED",
                            "createdAt": "2024-03-20T10:15:30"
                          },
                          {
                            "id": 2,
                            "resourceId": 10,
                            "userId": 7,
                            "rating": 4,
                            "comment": "Very good read, highly recommend.",
                            "status": "APPROVED",
                            "createdAt": "2024-03-19T14:20:00"
                          }
                        ]
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Invalid resource ID"
        )
    })
    public List<ReviewResponse> getReviewsByWork(
            @PathParam("resourceId")
            @Parameter(description = "Work ID", required = true, example = "10")
            Long resourceId
    ) {
        return reviewService.getApprovedReviewsForWork(resourceId);
    }

    /**
     * Get all reviews by a user.
     *
     * Returns all of a user's reviews regardless of status.
     *
     * @param userId User ID
     * @return List of user's reviews
     */
    @GET
    @Path("/by-user/{userId}")
    @Operation(
        summary = "Get all reviews by a user",
        description = "Returns all reviews created by a specific user, regardless of moderation status."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "List of user's reviews",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ReviewResponse.class, type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.ARRAY)
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Invalid user ID"
        )
    })
    public List<ReviewResponse> getReviewsByUser(
            @PathParam("userId")
            @Parameter(description = "User ID", required = true, example = "5")
            Long userId
    ) {
        return reviewService.getReviewsByUser(userId);
    }

    /**
     * Get all pending reviews awaiting moderation.
     *
     * Only accessible to moderators.
     *
     * @return List of pending reviews
     */
    @GET
    @Path("/pending")
    @Operation(
        summary = "Get pending reviews",
        description = "Returns all reviews with PENDING status awaiting moderation approval. " +
                      "This endpoint is for moderators only."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "List of pending reviews",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ReviewResponse.class, type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.ARRAY),
                examples = @ExampleObject(
                    name = "Pending reviews",
                    value = """
                        [
                          {
                            "id": 15,
                            "resourceId": 12,
                            "userId": 8,
                            "rating": 3,
                            "comment": "Decent book but could be better.",
                            "status": "PENDING",
                            "createdAt": "2024-03-20T10:15:30"
                          }
                        ]
                        """
                )
            )
        )
    })
    public List<ReviewResponse> getPendingReviews() {
        return reviewService.getPendingReviews();
    }

    /**
     * Get all flagged reviews requiring attention.
     *
     * Only accessible to moderators.
     *
     * @return List of flagged reviews
     */
    @GET
    @Path("/flagged")
    @Operation(
        summary = "Get flagged reviews",
        description = "Returns all reviews with FLAGGED status requiring moderator attention. " +
                      "This endpoint is for moderators only."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "List of flagged reviews",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ReviewResponse.class, type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.ARRAY)
            )
        )
    })
    public List<ReviewResponse> getFlaggedReviews() {
        return reviewService.getFlaggedReviews();
    }

    // ========================================
    // BUSINESS OPERATIONS - AGGREGATES
    // ========================================

    /**
     * Get average rating for a resource.
     *
     * Only includes approved reviews in the calculation.
     *
     * @param resourceId Work ID
     * @return Average rating or null if no reviews
     */
    @GET
    @Path("/by-resource/{resourceId}/average-rating")
    @Operation(
        summary = "Get average rating for a resource",
        description = "Calculates the average rating from all approved reviews for a resource."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Average rating calculated",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Average rating",
                    value = """
                        {
                          "resourceId": 10,
                          "averageRating": 4.5,
                          "totalReviews": 12
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Invalid resource ID"
        )
    })
    public Response getAverageRating(
            @PathParam("resourceId")
            @Parameter(description = "Work ID", required = true, example = "10")
            Long resourceId
    ) {
        Double averageRating = reviewService.getAverageRating(resourceId);

        // Return structured response
        return Response.ok(Map.of(
            "resourceId", resourceId,
            "averageRating", averageRating != null ? averageRating : 0.0,
            "totalReviews", reviewService.getApprovedReviewsForWork(resourceId).size()
        )).build();
    }

    /**
     * Get rating distribution for a resource.
     *
     * Returns count of reviews for each rating (1-5).
     *
     * @param resourceId Work ID
     * @return Rating distribution map
     */
    @GET
    @Path("/by-resource/{resourceId}/rating-distribution")
    @Operation(
        summary = "Get rating distribution for a resource",
        description = "Returns the count of reviews for each rating (1-5) for a specific resource. " +
                      "Only includes approved reviews."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Rating distribution calculated",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Rating distribution",
                    value = """
                        {
                          "resourceId": 10,
                          "distribution": {
                            "1": 0,
                            "2": 1,
                            "3": 2,
                            "4": 5,
                            "5": 4
                          },
                          "totalReviews": 12
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Invalid resource ID"
        )
    })
    public Response getRatingDistribution(
            @PathParam("resourceId")
            @Parameter(description = "Work ID", required = true, example = "10")
            Long resourceId
    ) {
        Map<Integer, Long> distribution = reviewService.getRatingDistribution(resourceId);

        // Return structured response
        return Response.ok(Map.of(
            "resourceId", resourceId,
            "distribution", distribution,
            "totalReviews", distribution.values().stream().mapToLong(Long::longValue).sum()
        )).build();
    }

    // ========================================
    // BUSINESS OPERATIONS - MODERATION
    // ========================================

    /**
     * Approve a review.
     *
     * Business Rule: Only moderators can approve reviews.
     * Approved reviews become visible to all users.
     *
     * @param id Review ID
     * @return Approved review response
     */
    @POST
    @Path("/{id}/approve")
    @Operation(
        summary = "Approve a review",
        description = "Approves a pending or flagged review, making it visible to all users. " +
                      "Only moderators can perform this action."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Review approved successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ReviewResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "Review not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "Review is already approved",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Already approved",
                    value = """
                        {
                          "message": "Review is already approved",
                          "code": "ALREADY_APPROVED",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "businessRule": "ALREADY_APPROVED"
                          }
                        }
                        """
                )
            )
        )
    })
    public ReviewResponse approveReview(
            @PathParam("id")
            @Parameter(description = "Review ID", required = true, example = "1")
            Long id
    ) {
        // NOTE: In production, moderatorId would come from SecurityIdentity
        return reviewService.approveReview(id, "moderator:system");
    }

    /**
     * Reject a review.
     *
     * Business Rule: Only moderators can reject reviews.
     * Rejected reviews are not visible to users.
     * A rejection reason is required.
     *
     * @param id Review ID
     * @param reason Reason for rejection
     * @return Rejected review response
     */
    @POST
    @Path("/{id}/reject")
    @Operation(
        summary = "Reject a review",
        description = "Rejects a review with a mandatory reason. " +
                      "Rejected reviews are not visible to users. Only moderators can perform this action."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Review rejected successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ReviewResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Rejection reason is required",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Missing reason",
                    value = """
                        {
                          "message": "Validation failed for field: reason",
                          "code": "VALIDATION_ERROR",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "violations": {
                              "reason": "Rejection reason is required"
                            }
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "Review not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "Review is already rejected"
        )
    })
    public ReviewResponse rejectReview(
            @PathParam("id")
            @Parameter(description = "Review ID", required = true, example = "1")
            Long id,

            @QueryParam("reason")
            @NotBlank(message = "Rejection reason is required")
            @Parameter(description = "Reason for rejection", required = true, example = "Contains inappropriate content")
            String reason
    ) {
        // NOTE: In production, moderatorId would come from SecurityIdentity
        return reviewService.rejectReview(id, reason, "moderator:system");
    }

    /**
     * Flag a review for moderator attention.
     *
     * Can be done by users or moderators.
     * Flagged reviews require moderator review.
     *
     * @param id Review ID
     * @param reason Reason for flagging
     * @return Flagged review response
     */
    @POST
    @Path("/{id}/flag")
    @Operation(
        summary = "Flag a review",
        description = "Flags a review for moderator attention with a mandatory reason. " +
                      "Can be done by any user to report inappropriate content."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Review flagged successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ReviewResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Flag reason is required",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Missing reason",
                    value = """
                        {
                          "message": "Validation failed for field: reason",
                          "code": "VALIDATION_ERROR",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "violations": {
                              "reason": "Flag reason is required"
                            }
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "Review not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "Review is already flagged or cannot be flagged (rejected)",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = {
                    @ExampleObject(
                        name = "Already flagged",
                        value = """
                            {
                              "message": "Review is already flagged",
                              "code": "ALREADY_FLAGGED",
                              "timestamp": "2024-03-20T10:15:30",
                              "details": {
                                "businessRule": "ALREADY_FLAGGED"
                              }
                            }
                            """
                    ),
                    @ExampleObject(
                        name = "Review rejected",
                        value = """
                            {
                              "message": "Cannot flag rejected review",
                              "code": "REVIEW_REJECTED",
                              "timestamp": "2024-03-20T10:15:30",
                              "details": {
                                "businessRule": "REVIEW_REJECTED"
                              }
                            }
                            """
                    )
                }
            )
        )
    })
    public ReviewResponse flagReview(
            @PathParam("id")
            @Parameter(description = "Review ID", required = true, example = "1")
            Long id,

            @QueryParam("reason")
            @NotBlank(message = "Flag reason is required")
            @Parameter(description = "Reason for flagging", required = true, example = "Contains spam or offensive language")
            String reason
    ) {
        // NOTE: In production, flaggedBy would come from SecurityIdentity
        return reviewService.flagReview(id, reason, "user:system");
    }

    /**
     * Reset review to pending status.
     *
     * Useful for re-evaluating flagged reviews.
     * Only moderators can perform this action.
     *
     * @param id Review ID
     * @return Updated review response
     */
    @POST
    @Path("/{id}/reset-to-pending")
    @Operation(
        summary = "Reset review to pending status",
        description = "Resets a review to PENDING status for re-evaluation. " +
                      "Useful for flagged reviews. Only moderators can perform this action."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Review reset successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ReviewResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "Review not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "Review is already pending"
        )
    })
    public ReviewResponse resetToPending(
            @PathParam("id")
            @Parameter(description = "Review ID", required = true, example = "1")
            Long id
    ) {
        // NOTE: In production, moderatorId would come from SecurityIdentity
        return reviewService.resetToPending(id, "moderator:system");
    }

    // ========================================
    // BUSINESS OPERATIONS - BULK MODERATION
    // ========================================

    /**
     * Bulk approve multiple reviews.
     *
     * Only moderators can perform bulk operations.
     *
     * @param reviewIds List of review IDs to approve
     * @return Number of reviews approved
     */
    @POST
    @Path("/bulk-approve")
    @Operation(
        summary = "Bulk approve reviews",
        description = "Approves multiple reviews in a single operation. " +
                      "Only moderators can perform bulk operations."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Reviews approved successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Bulk approve result",
                    value = """
                        {
                          "approved": 5,
                          "message": "5 reviews approved successfully"
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Review IDs list cannot be empty",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Empty list",
                    value = """
                        {
                          "message": "Validation failed for field: reviewIds",
                          "code": "VALIDATION_ERROR",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "violations": {
                              "reviewIds": "Review IDs list cannot be empty"
                            }
                          }
                        }
                        """
                )
            )
        )
    })
    public Response bulkApproveReviews(
            @NotNull(message = "Review IDs are required")
            @Parameter(description = "List of review IDs to approve", required = true)
            List<Long> reviewIds
    ) {
        // NOTE: In production, moderatorId would come from SecurityIdentity
        int approved = reviewService.bulkApproveReviews(reviewIds, "moderator:system");

        return Response.ok(Map.of(
            "approved", approved,
            "message", approved + " reviews approved successfully"
        )).build();
    }

    /**
     * Bulk reject multiple reviews.
     *
     * Only moderators can perform bulk operations.
     * A rejection reason is required.
     *
     * @param request Bulk reject request with review IDs and reason
     * @return Number of reviews rejected
     */
    @POST
    @Path("/bulk-reject")
    @Operation(
        summary = "Bulk reject reviews",
        description = "Rejects multiple reviews in a single operation with a mandatory reason. " +
                      "Only moderators can perform bulk operations."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Reviews rejected successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Bulk reject result",
                    value = """
                        {
                          "rejected": 3,
                          "message": "3 reviews rejected successfully"
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Validation error - Review IDs or reason required"
        )
    })
    public Response bulkRejectReviews(
            @Valid BulkRejectRequest request
    ) {
        // NOTE: In production, moderatorId would come from SecurityIdentity
        int rejected = reviewService.bulkRejectReviews(request.reviewIds, request.reason, "moderator:system");

        return Response.ok(Map.of(
            "rejected", rejected,
            "message", rejected + " reviews rejected successfully"
        )).build();
    }

    // ========================================
    // BUSINESS OPERATIONS - ARCHIVING
    // ========================================

    /**
     * Archive a review (soft delete).
     *
     * Business Rule: Archive reason is required.
     * Archived reviews can be restored later.
     *
     * @param id Review ID
     * @param reason Reason for archiving
     * @return Archived review response
     */
    @POST
    @Path("/{id}/archive")
    @Operation(
        summary = "Archive a review",
        description = "Soft deletes a review by archiving it. Archived reviews can be restored later. " +
                      "A reason must be provided for audit purposes. Use DELETE /reviews/{id} for permanent deletion."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Review archived successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ReviewResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Archive reason is required",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Missing reason",
                    value = """
                        {
                          "message": "Validation failed for field: reason",
                          "code": "VALIDATION_ERROR",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "violations": {
                              "reason": "Archive reason is required"
                            }
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "Review not found"
        )
    })
    public ReviewResponse archiveReview(
            @PathParam("id")
            @Parameter(description = "Review ID", required = true, example = "1")
            Long id,

            @QueryParam("reason")
            @NotBlank(message = "Archive reason is required")
            @Parameter(description = "Reason for archiving", required = true, example = "User requested deletion")
            String reason
    ) {
        // NOTE: In production, archivedBy would come from SecurityIdentity
        return reviewService.archiveReview(id, reason, "system");
    }

    /**
     * Restore an archived review.
     *
     * Restored reviews go back to PENDING status for re-moderation.
     *
     * @param id Review ID
     * @return Restored review response
     */
    @POST
    @Path("/{id}/restore")
    @Operation(
        summary = "Restore an archived review",
        description = "Restores a previously archived review. " +
                      "Restored reviews return to PENDING status for re-moderation."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Review restored successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ReviewResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "Review not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "Review is not archived",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Not archived",
                    value = """
                        {
                          "message": "Review is not archived: 1",
                          "code": "REVIEW_NOT_ARCHIVED",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "businessRule": "REVIEW_NOT_ARCHIVED"
                          }
                        }
                        """
                )
            )
        )
    })
    public ReviewResponse restoreReview(
            @PathParam("id")
            @Parameter(description = "Review ID", required = true, example = "1")
            Long id
    ) {
        // NOTE: In production, restoredBy would come from SecurityIdentity
        return reviewService.restoreReview(id, "system");
    }

    // ========================================
    // HELPER CLASSES
    // ========================================

    /**
     * Request DTO for bulk reject operation.
     */
    public static class BulkRejectRequest {
        @NotNull(message = "Review IDs are required")
        public List<Long> reviewIds;

        @NotBlank(message = "Rejection reason is required")
        public String reason;
    }
}
