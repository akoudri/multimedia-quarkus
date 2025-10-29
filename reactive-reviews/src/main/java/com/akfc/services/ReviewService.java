package com.akfc.services;

import com.akfc.data.Review;
import com.akfc.data.ReviewRepository;
import com.akfc.data.ReviewStatus;
import com.akfc.dto.CreateReviewRequest;
import com.akfc.dto.ReviewResponse;
import com.akfc.dto.UpdateReviewRequest;
import com.akfc.errors.*;
import io.quarkus.cache.CacheInvalidate;
import io.quarkus.cache.CacheInvalidateAll;
import io.quarkus.cache.CacheResult;
import io.quarkus.hibernate.reactive.panache.common.WithTransaction;
import io.smallrye.mutiny.Uni;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import jakarta.validation.Valid;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * REACTIVE Business service for Review management.
 *
 * Handles all business logic, validation, and orchestration for review operations
 * using reactive patterns with Uni<> return types for non-blocking execution.
 *
 * Uses @WithTransaction for reactive transaction management instead of @Transactional.
 */
@ApplicationScoped
public class ReviewService {

    @Inject
    ReviewRepository reviewRepository;

    // ========== CREATE ==========

    /**
     * REACTIVE: Create a new review.
     *
     * Business Rules:
     * - User can only have one review per resource
     * - Rating must be 1-5
     * - Comment must be at least 10 characters
     * - New reviews start with PENDING status (require moderation)
     *
     * @param request Create request DTO
     * @param createdBy User creating the review (typically same as userId in request)
     * @return Uni containing created review response
     * @throws DuplicateReviewException if user already reviewed this resource
     * @throws ReviewValidationException if validation fails
     */
    @WithTransaction
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    public Uni<ReviewResponse> createReview(@Valid CreateReviewRequest request, String createdBy) {
        // Validate request
        return validateCreateRequest(request)
            .chain(() -> reviewRepository.existsByUserAndWork(request.userId, request.resourceId))
            .chain(exists -> {
                if (exists) {
                    return Uni.createFrom().failure(
                        new DuplicateReviewException(request.userId, request.resourceId)
                    );
                }

                // Create review using repository
                return reviewRepository.createReview(
                    request.resourceId,
                    request.userId,
                    request.rating,
                    request.comment,
                    createdBy
                );
            })
            .map(ReviewResponse::from);
    }

    // ========== READ ==========

    /**
     * REACTIVE: Get review by ID.
     *
     * @param id Review ID
     * @return Uni containing review response
     * @throws ReviewNotFoundException if review not found
     */
    @CacheResult(cacheName = "review-by-id")
    public Uni<ReviewResponse> getReviewById(Long id) {
        return reviewRepository.findActiveById(id)
            .chain(review -> {
                if (review == null) {
                    return Uni.createFrom().failure(new ReviewNotFoundException(id));
                }
                return Uni.createFrom().item(ReviewResponse.from(review));
            });
    }

    /**
     * REACTIVE: Get all approved reviews for a resource.
     *
     * Only returns approved, non-archived reviews visible to users.
     *
     * @param resourceId Work ID
     * @return Uni containing list of approved reviews
     */
    @CacheResult(cacheName = "approved-reviews-by-resource")
    public Uni<List<ReviewResponse>> getApprovedReviewsForWork(Long resourceId) {
        return Review.findApprovedByWorkId(resourceId)
            .map(reviews -> reviews.stream()
                .map(ReviewResponse::from)
                .collect(Collectors.toList()));
    }

    /**
     * REACTIVE: Get all reviews by a user.
     *
     * @param userId User ID
     * @return Uni containing list of user's reviews
     */
    @CacheResult(cacheName = "reviews-by-user")
    public Uni<List<ReviewResponse>> getReviewsByUser(Long userId) {
        return Review.findByUserId(userId)
            .map(reviews -> reviews.stream()
                .map(ReviewResponse::from)
                .collect(Collectors.toList()));
    }

    /**
     * REACTIVE: Get all pending reviews (for moderation).
     *
     * @return Uni containing list of reviews awaiting moderation
     */
    @CacheResult(cacheName = "pending-reviews")
    public Uni<List<ReviewResponse>> getPendingReviews() {
        return reviewRepository.findPendingModeration()
            .map(reviews -> reviews.stream()
                .map(ReviewResponse::from)
                .collect(Collectors.toList()));
    }

    /**
     * REACTIVE: Get all flagged reviews (for moderation).
     *
     * @return Uni containing list of flagged reviews
     */
    @CacheResult(cacheName = "flagged-reviews")
    public Uni<List<ReviewResponse>> getFlaggedReviews() {
        return Review.findFlaggedReviews()
            .map(reviews -> reviews.stream()
                .map(ReviewResponse::from)
                .collect(Collectors.toList()));
    }

    /**
     * REACTIVE: Get average rating for a resource.
     *
     * Only includes approved reviews.
     *
     * @param resourceId Work ID
     * @return Uni containing average rating or null if no reviews
     */
    @CacheResult(cacheName = "average-rating-by-resource")
    public Uni<Double> getAverageRating(Long resourceId) {
        return reviewRepository.getAverageRatingForWork(resourceId);
    }

    /**
     * REACTIVE: Get rating distribution for a resource.
     *
     * Returns map of rating (1-5) to count.
     *
     * @param resourceId Work ID
     * @return Uni containing rating distribution
     */
    @CacheResult(cacheName = "rating-distribution-by-resource")
    public Uni<Map<Integer, Long>> getRatingDistribution(Long resourceId) {
        return reviewRepository.getRatingDistribution(resourceId);
    }

    // ========== UPDATE ==========

    /**
     * REACTIVE: Update an existing review.
     *
     * Business Rules:
     * - Only the review author can update their review
     * - Updated reviews go back to PENDING status for re-moderation
     * - Cannot update rejected reviews
     *
     * @param id Review ID
     * @param request Update request DTO
     * @param modifiedBy User making the update (must be review author)
     * @return Uni containing updated review response
     * @throws ReviewNotFoundException if review not found
     * @throws ReviewBusinessException if business rules violated
     */
    @WithTransaction
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "approved-reviews-by-resource")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    @CacheInvalidateAll(cacheName = "average-rating-by-resource")
    @CacheInvalidateAll(cacheName = "rating-distribution-by-resource")
    public Uni<ReviewResponse> updateReview(Long id, @Valid UpdateReviewRequest request, String modifiedBy) {
        return validateUpdateRequest(request)
            .chain(() -> getReviewEntity(id))
            .chain(review -> {
                // Business rule: Cannot update rejected reviews
                if (review.status == ReviewStatus.REJECTED) {
                    return Uni.createFrom().failure(
                        new ReviewBusinessException(
                            "Cannot update rejected review",
                            "REVIEW_REJECTED"
                        )
                    );
                }

                // Update review (this will reset status to PENDING if it was APPROVED)
                return review.update(request.rating, request.comment, modifiedBy);
            })
            .map(ReviewResponse::from);
    }

    // ========== MODERATION ==========

    /**
     * REACTIVE: Approve a review.
     *
     * Business Rule: Only moderators can approve reviews
     *
     * @param id Review ID
     * @param moderatorId Moderator performing the approval
     * @return Uni containing approved review response
     */
    @WithTransaction
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "approved-reviews-by-resource")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    @CacheInvalidateAll(cacheName = "average-rating-by-resource")
    @CacheInvalidateAll(cacheName = "rating-distribution-by-resource")
    public Uni<ReviewResponse> approveReview(Long id, String moderatorId) {
        return getReviewEntity(id)
            .chain(review -> {
                if (review.status == ReviewStatus.APPROVED) {
                    return Uni.createFrom().failure(
                        new ReviewBusinessException(
                            "Review is already approved",
                            "ALREADY_APPROVED"
                        )
                    );
                }

                return review.approve(moderatorId);
            })
            .map(ReviewResponse::from);
    }

    /**
     * REACTIVE: Reject a review.
     *
     * Business Rule: Only moderators can reject reviews
     *
     * @param id Review ID
     * @param reason Reason for rejection
     * @param moderatorId Moderator performing the rejection
     * @return Uni containing rejected review response
     */
    @WithTransaction
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    public Uni<ReviewResponse> rejectReview(Long id, String reason, String moderatorId) {
        if (reason == null || reason.trim().isEmpty()) {
            return Uni.createFrom().failure(
                new ReviewValidationException("reason", "Rejection reason is required")
            );
        }

        return getReviewEntity(id)
            .chain(review -> {
                if (review.status == ReviewStatus.REJECTED) {
                    return Uni.createFrom().failure(
                        new ReviewBusinessException(
                            "Review is already rejected",
                            "ALREADY_REJECTED"
                        )
                    );
                }

                return review.reject(reason, moderatorId);
            })
            .map(ReviewResponse::from);
    }

    /**
     * REACTIVE: Flag a review for attention.
     *
     * Can be done by users or moderators.
     *
     * @param id Review ID
     * @param reason Reason for flagging
     * @param flaggedBy User or moderator flagging the review
     * @return Uni containing flagged review response
     */
    @WithTransaction
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "approved-reviews-by-resource")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "flagged-reviews")
    public Uni<ReviewResponse> flagReview(Long id, String reason, String flaggedBy) {
        if (reason == null || reason.trim().isEmpty()) {
            return Uni.createFrom().failure(
                new ReviewValidationException("reason", "Flag reason is required")
            );
        }

        return getReviewEntity(id)
            .chain(review -> {
                if (review.status == ReviewStatus.FLAGGED) {
                    return Uni.createFrom().failure(
                        new ReviewBusinessException(
                            "Review is already flagged",
                            "ALREADY_FLAGGED"
                        )
                    );
                }

                if (review.status == ReviewStatus.REJECTED) {
                    return Uni.createFrom().failure(
                        new ReviewBusinessException(
                            "Cannot flag rejected review",
                            "REVIEW_REJECTED"
                        )
                    );
                }

                return review.flag(reason, flaggedBy);
            })
            .map(ReviewResponse::from);
    }

    /**
     * REACTIVE: Reset review to pending status.
     *
     * Useful for re-evaluating flagged reviews.
     *
     * @param id Review ID
     * @param moderatorId Moderator resetting the status
     * @return Uni containing updated review response
     */
    @WithTransaction
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "approved-reviews-by-resource")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    @CacheInvalidateAll(cacheName = "flagged-reviews")
    public Uni<ReviewResponse> resetToPending(Long id, String moderatorId) {
        return getReviewEntity(id)
            .chain(review -> {
                if (review.status == ReviewStatus.PENDING) {
                    return Uni.createFrom().failure(
                        new ReviewBusinessException(
                            "Review is already pending",
                            "ALREADY_PENDING"
                        )
                    );
                }

                return review.resetToPending(moderatorId);
            })
            .map(ReviewResponse::from);
    }

    /**
     * REACTIVE: Bulk approve multiple reviews.
     *
     * @param reviewIds List of review IDs to approve
     * @param moderatorId Moderator performing the approvals
     * @return Uni containing number of reviews approved
     */
    @WithTransaction
    @CacheInvalidateAll(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "approved-reviews-by-resource")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    @CacheInvalidateAll(cacheName = "average-rating-by-resource")
    @CacheInvalidateAll(cacheName = "rating-distribution-by-resource")
    public Uni<Integer> bulkApproveReviews(List<Long> reviewIds, String moderatorId) {
        if (reviewIds == null || reviewIds.isEmpty()) {
            return Uni.createFrom().failure(
                new ReviewValidationException("reviewIds", "Review IDs list cannot be empty")
            );
        }

        return reviewRepository.approveMultiple(reviewIds, moderatorId);
    }

    /**
     * REACTIVE: Bulk reject multiple reviews.
     *
     * @param reviewIds List of review IDs to reject
     * @param reason Reason for rejection
     * @param moderatorId Moderator performing the rejections
     * @return Uni containing number of reviews rejected
     */
    @WithTransaction
    @CacheInvalidateAll(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    public Uni<Integer> bulkRejectReviews(List<Long> reviewIds, String reason, String moderatorId) {
        if (reviewIds == null || reviewIds.isEmpty()) {
            return Uni.createFrom().failure(
                new ReviewValidationException("reviewIds", "Review IDs list cannot be empty")
            );
        }

        if (reason == null || reason.trim().isEmpty()) {
            return Uni.createFrom().failure(
                new ReviewValidationException("reason", "Rejection reason is required")
            );
        }

        return reviewRepository.rejectMultiple(reviewIds, reason, moderatorId);
    }

    // ========== DELETE ==========

    /**
     * REACTIVE: Archive a review (soft delete).
     *
     * @param id Review ID
     * @param reason Reason for archiving
     * @param archivedBy User archiving the review
     * @return Uni containing archived review response
     */
    @WithTransaction
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "approved-reviews-by-resource")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    @CacheInvalidateAll(cacheName = "flagged-reviews")
    @CacheInvalidateAll(cacheName = "average-rating-by-resource")
    @CacheInvalidateAll(cacheName = "rating-distribution-by-resource")
    public Uni<ReviewResponse> archiveReview(Long id, String reason, String archivedBy) {
        if (reason == null || reason.trim().isEmpty()) {
            return Uni.createFrom().failure(
                new ReviewValidationException("reason", "Archive reason is required")
            );
        }

        return getReviewEntity(id)
            .chain(review -> review.archive(reason, archivedBy))
            .map(ReviewResponse::from);
    }

    /**
     * REACTIVE: Restore an archived review.
     *
     * Restored reviews go back to PENDING status.
     *
     * @param id Review ID
     * @param restoredBy User restoring the review
     * @return Uni containing restored review response
     */
    @WithTransaction
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    public Uni<ReviewResponse> restoreReview(Long id, String restoredBy) {
        return reviewRepository.findByIdIncludingArchived(id)
            .chain(review -> {
                if (review == null) {
                    return Uni.createFrom().failure(new ReviewNotFoundException(id));
                }

                if (Boolean.FALSE.equals(review.archived)) {
                    return Uni.createFrom().failure(
                        new ReviewBusinessException(
                            "Review is not archived: " + id,
                            "REVIEW_NOT_ARCHIVED"
                        )
                    );
                }

                return review.restore(restoredBy);
            })
            .map(ReviewResponse::from);
    }

    /**
     * REACTIVE: Permanently delete a review.
     *
     * Business Rule: Review must be archived first
     *
     * @param id Review ID
     * @return Uni<Void> on successful deletion
     * @throws ReviewNotFoundException if review not found
     * @throws ReviewBusinessException if review not archived
     */
    @WithTransaction
    @CacheInvalidate(cacheName = "review-by-id")
    public Uni<Void> deleteReview(Long id) {
        return reviewRepository.findByIdIncludingArchived(id)
            .chain(review -> {
                if (review == null) {
                    return Uni.createFrom().failure(new ReviewNotFoundException(id));
                }

                if (Boolean.FALSE.equals(review.archived)) {
                    return Uni.createFrom().failure(
                        new ReviewBusinessException(
                            "Cannot permanently delete non-archived review. Archive it first.",
                            "REVIEW_NOT_ARCHIVED"
                        )
                    );
                }

                return review.permanentlyDelete();
            });
    }

    // ========== HELPER METHODS ==========

    /**
     * REACTIVE: Get review entity or throw exception.
     *
     * @param id Review ID
     * @return Uni containing review entity
     */
    private Uni<Review> getReviewEntity(Long id) {
        return reviewRepository.findActiveById(id)
            .chain(review -> {
                if (review == null) {
                    return Uni.createFrom().failure(new ReviewNotFoundException(id));
                }
                return Uni.createFrom().item(review);
            });
    }

    /**
     * REACTIVE: Validate create request.
     *
     * @param request Create request DTO
     * @return Uni<Void> on success or Uni with failure
     */
    private Uni<Void> validateCreateRequest(CreateReviewRequest request) {
        if (request.resourceId == null) {
            return Uni.createFrom().failure(
                new ReviewValidationException("resourceId", "Work ID is required")
            );
        }

        if (request.userId == null) {
            return Uni.createFrom().failure(
                new ReviewValidationException("userId", "User ID is required")
            );
        }

        if (request.rating == null) {
            return Uni.createFrom().failure(
                new ReviewValidationException("rating", "Rating is required")
            );
        }

        if (request.rating < 1 || request.rating > 5) {
            return Uni.createFrom().failure(
                new ReviewValidationException("rating", "Rating must be between 1 and 5")
            );
        }

        if (request.comment == null || request.comment.trim().isEmpty()) {
            return Uni.createFrom().failure(
                new ReviewValidationException("comment", "Comment is required")
            );
        }

        if (request.comment.trim().length() < 10) {
            return Uni.createFrom().failure(
                new ReviewValidationException("comment", "Comment must be at least 10 characters")
            );
        }

        if (request.comment.length() > 2000) {
            return Uni.createFrom().failure(
                new ReviewValidationException("comment", "Comment must not exceed 2000 characters")
            );
        }

        return Uni.createFrom().voidItem();
    }

    /**
     * REACTIVE: Validate update request.
     *
     * @param request Update request DTO
     * @return Uni<Void> on success or Uni with failure
     */
    private Uni<Void> validateUpdateRequest(UpdateReviewRequest request) {
        if (request.rating == null) {
            return Uni.createFrom().failure(
                new ReviewValidationException("rating", "Rating is required")
            );
        }

        if (request.rating < 1 || request.rating > 5) {
            return Uni.createFrom().failure(
                new ReviewValidationException("rating", "Rating must be between 1 and 5")
            );
        }

        if (request.comment == null || request.comment.trim().isEmpty()) {
            return Uni.createFrom().failure(
                new ReviewValidationException("comment", "Comment is required")
            );
        }

        if (request.comment.trim().length() < 10) {
            return Uni.createFrom().failure(
                new ReviewValidationException("comment", "Comment must be at least 10 characters")
            );
        }

        return Uni.createFrom().voidItem();
    }

    /**
     * REACTIVE: Check if user has reviewed a resource.
     *
     * @param userId User ID
     * @param resourceId Work ID
     * @return Uni containing true if user has reviewed the resource
     */
    public Uni<Boolean> hasUserReviewedWork(Long userId, Long resourceId) {
        return reviewRepository.existsByUserAndWork(userId, resourceId);
    }
}
