package com.akfc.reviews.services;

import com.akfc.reviews.data.Review;
import com.akfc.reviews.data.ReviewRepository;
import com.akfc.reviews.data.ReviewStatus;
import com.akfc.reviews.dto.CreateReviewRequest;
import com.akfc.reviews.dto.ReviewResponse;
import com.akfc.reviews.dto.UpdateReviewRequest;
import com.akfc.reviews.errors.*;
import io.quarkus.cache.CacheInvalidate;
import io.quarkus.cache.CacheInvalidateAll;
import io.quarkus.cache.CacheResult;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;
import jakarta.validation.Valid;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Business service for Review management.
 *
 * Handles all business logic, validation, and orchestration for review operations.
 * Implements complete CRUD lifecycle with business rules and moderation workflow.
 */
@ApplicationScoped
public class ReviewService {

    @Inject
    ReviewRepository reviewRepository;

    // ========== CREATE ==========

    /**
     * Create a new review.
     *
     * Business Rules:
     * - User can only have one review per resource
     * - Rating must be 1-5
     * - Comment must be at least 10 characters
     * - New reviews start with PENDING status (require moderation)
     *
     * @param request Create request DTO
     * @param createdBy User creating the review (typically same as userId in request)
     * @return Created review response
     * @throws DuplicateReviewException if user already reviewed this resource
     * @throws ReviewValidationException if validation fails
     */
    @Transactional
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    public ReviewResponse createReview(@Valid CreateReviewRequest request, String createdBy) {
        // Validate request
        validateCreateRequest(request);

        // Check if user has already reviewed this resource
        if (reviewRepository.existsByUserAndWork(request.userId, request.resourceId)) {
            throw new DuplicateReviewException(request.userId, request.resourceId);
        }

        // Create review using repository
        Review review = reviewRepository.createReview(
            request.resourceId,
            request.userId,
            request.rating,
            request.comment,
            createdBy
        );

        return ReviewResponse.from(review);
    }

    // ========== READ ==========

    /**
     * Get review by ID.
     *
     * @param id Review ID
     * @return Review response
     * @throws ReviewNotFoundException if review not found
     */
    @CacheResult(cacheName = "review-by-id")
    public ReviewResponse getReviewById(Long id) {
        Review review = reviewRepository.findActiveById(id);
        if (review == null) {
            throw new ReviewNotFoundException(id);
        }
        return ReviewResponse.from(review);
    }

    /**
     * Get all approved reviews for a resource.
     *
     * Only returns approved, non-archived reviews visible to users.
     *
     * @param resourceId Work ID
     * @return List of approved reviews
     */
    @CacheResult(cacheName = "approved-reviews-by-resource")
    public List<ReviewResponse> getApprovedReviewsForWork(Long resourceId) {
        return Review.findApprovedByWorkId(resourceId)
            .stream()
            .map(ReviewResponse::from)
            .collect(Collectors.toList());
    }

    /**
     * Get all reviews by a user.
     *
     * @param userId User ID
     * @return List of user's reviews
     */
    @CacheResult(cacheName = "reviews-by-user")
    public List<ReviewResponse> getReviewsByUser(Long userId) {
        return Review.findByUserId(userId)
            .stream()
            .map(ReviewResponse::from)
            .collect(Collectors.toList());
    }

    /**
     * Get all pending reviews (for moderation).
     *
     * @return List of reviews awaiting moderation
     */
    @CacheResult(cacheName = "pending-reviews")
    public List<ReviewResponse> getPendingReviews() {
        return reviewRepository.findPendingModeration()
            .stream()
            .map(ReviewResponse::from)
            .collect(Collectors.toList());
    }

    /**
     * Get all flagged reviews (for moderation).
     *
     * @return List of flagged reviews
     */
    @CacheResult(cacheName = "flagged-reviews")
    public List<ReviewResponse> getFlaggedReviews() {
        return Review.findFlaggedReviews()
            .stream()
            .map(ReviewResponse::from)
            .collect(Collectors.toList());
    }

    /**
     * Get average rating for a resource.
     *
     * Only includes approved reviews.
     *
     * @param resourceId Work ID
     * @return Average rating or null if no reviews
     */
    @CacheResult(cacheName = "average-rating-by-resource")
    public Double getAverageRating(Long resourceId) {
        return reviewRepository.getAverageRatingForWork(resourceId);
    }

    /**
     * Get rating distribution for a resource.
     *
     * Returns map of rating (1-5) to count.
     *
     * @param resourceId Work ID
     * @return Rating distribution
     */
    @CacheResult(cacheName = "rating-distribution-by-resource")
    public Map<Integer, Long> getRatingDistribution(Long resourceId) {
        return reviewRepository.getRatingDistribution(resourceId);
    }

    // ========== UPDATE ==========

    /**
     * Update an existing review.
     *
     * Business Rules:
     * - Only the review author can update their review
     * - Updated reviews go back to PENDING status for re-moderation
     * - Cannot update rejected reviews
     *
     * @param id Review ID
     * @param request Update request DTO
     * @param modifiedBy User making the update (must be review author)
     * @return Updated review response
     * @throws ReviewNotFoundException if review not found
     * @throws ReviewBusinessException if business rules violated
     */
    @Transactional
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "approved-reviews-by-resource")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    @CacheInvalidateAll(cacheName = "average-rating-by-resource")
    @CacheInvalidateAll(cacheName = "rating-distribution-by-resource")
    public ReviewResponse updateReview(Long id, @Valid UpdateReviewRequest request, String modifiedBy) {
        Review review = getReviewEntity(id);

        // Validate request
        validateUpdateRequest(request);

        // Business rule: Cannot update rejected reviews
        if (review.status == ReviewStatus.REJECTED) {
            throw new ReviewBusinessException(
                "Cannot update rejected review",
                "REVIEW_REJECTED"
            );
        }

        // Update review (this will reset status to PENDING if it was APPROVED)
        review.update(request.rating, request.comment, modifiedBy);

        return ReviewResponse.from(review);
    }

    // ========== MODERATION ==========

    /**
     * Approve a review.
     *
     * Business Rule: Only moderators can approve reviews
     *
     * @param id Review ID
     * @param moderatorId Moderator performing the approval
     * @return Approved review response
     */
    @Transactional
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "approved-reviews-by-resource")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    @CacheInvalidateAll(cacheName = "average-rating-by-resource")
    @CacheInvalidateAll(cacheName = "rating-distribution-by-resource")
    public ReviewResponse approveReview(Long id, String moderatorId) {
        Review review = getReviewEntity(id);

        if (review.status == ReviewStatus.APPROVED) {
            throw new ReviewBusinessException(
                "Review is already approved",
                "ALREADY_APPROVED"
            );
        }

        review.approve(moderatorId);
        return ReviewResponse.from(review);
    }

    /**
     * Reject a review.
     *
     * Business Rule: Only moderators can reject reviews
     *
     * @param id Review ID
     * @param reason Reason for rejection
     * @param moderatorId Moderator performing the rejection
     * @return Rejected review response
     */
    @Transactional
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    public ReviewResponse rejectReview(Long id, String reason, String moderatorId) {
        Review review = getReviewEntity(id);

        if (reason == null || reason.trim().isEmpty()) {
            throw new ReviewValidationException("reason", "Rejection reason is required");
        }

        if (review.status == ReviewStatus.REJECTED) {
            throw new ReviewBusinessException(
                "Review is already rejected",
                "ALREADY_REJECTED"
            );
        }

        review.reject(reason, moderatorId);
        return ReviewResponse.from(review);
    }

    /**
     * Flag a review for attention.
     *
     * Can be done by users or moderators.
     *
     * @param id Review ID
     * @param reason Reason for flagging
     * @param flaggedBy User or moderator flagging the review
     * @return Flagged review response
     */
    @Transactional
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "approved-reviews-by-resource")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "flagged-reviews")
    public ReviewResponse flagReview(Long id, String reason, String flaggedBy) {
        Review review = getReviewEntity(id);

        if (reason == null || reason.trim().isEmpty()) {
            throw new ReviewValidationException("reason", "Flag reason is required");
        }

        if (review.status == ReviewStatus.FLAGGED) {
            throw new ReviewBusinessException(
                "Review is already flagged",
                "ALREADY_FLAGGED"
            );
        }

        if (review.status == ReviewStatus.REJECTED) {
            throw new ReviewBusinessException(
                "Cannot flag rejected review",
                "REVIEW_REJECTED"
            );
        }

        review.flag(reason, flaggedBy);
        return ReviewResponse.from(review);
    }

    /**
     * Reset review to pending status.
     *
     * Useful for re-evaluating flagged reviews.
     *
     * @param id Review ID
     * @param moderatorId Moderator resetting the status
     * @return Updated review response
     */
    @Transactional
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "approved-reviews-by-resource")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    @CacheInvalidateAll(cacheName = "flagged-reviews")
    public ReviewResponse resetToPending(Long id, String moderatorId) {
        Review review = getReviewEntity(id);

        if (review.status == ReviewStatus.PENDING) {
            throw new ReviewBusinessException(
                "Review is already pending",
                "ALREADY_PENDING"
            );
        }

        review.resetToPending(moderatorId);
        return ReviewResponse.from(review);
    }

    /**
     * Bulk approve multiple reviews.
     *
     * @param reviewIds List of review IDs to approve
     * @param moderatorId Moderator performing the approvals
     * @return Number of reviews approved
     */
    @Transactional
    @CacheInvalidateAll(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "approved-reviews-by-resource")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    @CacheInvalidateAll(cacheName = "average-rating-by-resource")
    @CacheInvalidateAll(cacheName = "rating-distribution-by-resource")
    public int bulkApproveReviews(List<Long> reviewIds, String moderatorId) {
        if (reviewIds == null || reviewIds.isEmpty()) {
            throw new ReviewValidationException("reviewIds", "Review IDs list cannot be empty");
        }

        return reviewRepository.approveMultiple(reviewIds, moderatorId);
    }

    /**
     * Bulk reject multiple reviews.
     *
     * @param reviewIds List of review IDs to reject
     * @param reason Reason for rejection
     * @param moderatorId Moderator performing the rejections
     * @return Number of reviews rejected
     */
    @Transactional
    @CacheInvalidateAll(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    public int bulkRejectReviews(List<Long> reviewIds, String reason, String moderatorId) {
        if (reviewIds == null || reviewIds.isEmpty()) {
            throw new ReviewValidationException("reviewIds", "Review IDs list cannot be empty");
        }

        if (reason == null || reason.trim().isEmpty()) {
            throw new ReviewValidationException("reason", "Rejection reason is required");
        }

        return reviewRepository.rejectMultiple(reviewIds, reason, moderatorId);
    }

    // ========== DELETE ==========

    /**
     * Archive a review (soft delete).
     *
     * @param id Review ID
     * @param reason Reason for archiving
     * @param archivedBy User archiving the review
     * @return Archived review response
     */
    @Transactional
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "approved-reviews-by-resource")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    @CacheInvalidateAll(cacheName = "flagged-reviews")
    @CacheInvalidateAll(cacheName = "average-rating-by-resource")
    @CacheInvalidateAll(cacheName = "rating-distribution-by-resource")
    public ReviewResponse archiveReview(Long id, String reason, String archivedBy) {
        Review review = getReviewEntity(id);

        if (reason == null || reason.trim().isEmpty()) {
            throw new ReviewValidationException("reason", "Archive reason is required");
        }

        review.archive(reason, archivedBy);
        return ReviewResponse.from(review);
    }

    /**
     * Restore an archived review.
     *
     * Restored reviews go back to PENDING status.
     *
     * @param id Review ID
     * @param restoredBy User restoring the review
     * @return Restored review response
     */
    @Transactional
    @CacheInvalidate(cacheName = "review-by-id")
    @CacheInvalidateAll(cacheName = "reviews-by-user")
    @CacheInvalidateAll(cacheName = "pending-reviews")
    public ReviewResponse restoreReview(Long id, String restoredBy) {
        Review review = reviewRepository.findByIdIncludingArchived(id);
        if (review == null) {
            throw new ReviewNotFoundException(id);
        }

        if (Boolean.FALSE.equals(review.archived)) {
            throw new ReviewBusinessException(
                "Review is not archived: " + id,
                "REVIEW_NOT_ARCHIVED"
            );
        }

        review.restore(restoredBy);
        return ReviewResponse.from(review);
    }

    /**
     * Permanently delete a review.
     *
     * Business Rule: Review must be archived first
     *
     * @param id Review ID
     * @throws ReviewNotFoundException if review not found
     * @throws ReviewBusinessException if review not archived
     */
    @Transactional
    @CacheInvalidate(cacheName = "review-by-id")
    public void deleteReview(Long id) {
        Review review = reviewRepository.findByIdIncludingArchived(id);
        if (review == null) {
            throw new ReviewNotFoundException(id);
        }

        if (Boolean.FALSE.equals(review.archived)) {
            throw new ReviewBusinessException(
                "Cannot permanently delete non-archived review. Archive it first.",
                "REVIEW_NOT_ARCHIVED"
            );
        }

        review.permanentlyDelete();
    }

    // ========== HELPER METHODS ==========

    /**
     * Get review entity or throw exception.
     */
    private Review getReviewEntity(Long id) {
        Review review = reviewRepository.findActiveById(id);
        if (review == null) {
            throw new ReviewNotFoundException(id);
        }
        return review;
    }

    /**
     * Validate create request.
     */
    private void validateCreateRequest(CreateReviewRequest request) {
        if (request.resourceId == null) {
            throw new ReviewValidationException("resourceId", "Work ID is required");
        }

        if (request.userId == null) {
            throw new ReviewValidationException("userId", "User ID is required");
        }

        if (request.rating == null) {
            throw new ReviewValidationException("rating", "Rating is required");
        }

        if (request.rating < 1 || request.rating > 5) {
            throw new ReviewValidationException("rating", "Rating must be between 1 and 5");
        }

        if (request.comment == null || request.comment.trim().isEmpty()) {
            throw new ReviewValidationException("comment", "Comment is required");
        }

        if (request.comment.trim().length() < 10) {
            throw new ReviewValidationException("comment", "Comment must be at least 10 characters");
        }

        if (request.comment.length() > 2000) {
            throw new ReviewValidationException("comment", "Comment must not exceed 2000 characters");
        }
    }

    /**
     * Validate update request.
     */
    private void validateUpdateRequest(UpdateReviewRequest request) {
        if (request.rating == null) {
            throw new ReviewValidationException("rating", "Rating is required");
        }

        if (request.rating < 1 || request.rating > 5) {
            throw new ReviewValidationException("rating", "Rating must be between 1 and 5");
        }

        if (request.comment == null || request.comment.trim().isEmpty()) {
            throw new ReviewValidationException("comment", "Comment is required");
        }

        if (request.comment.trim().length() < 10) {
            throw new ReviewValidationException("comment", "Comment must be at least 10 characters");
        }
    }

    /**
     * Check if user has reviewed a resource.
     *
     * @param userId User ID
     * @param resourceId Work ID
     * @return True if user has reviewed the resource
     */
    public boolean hasUserReviewedWork(Long userId, Long resourceId) {
        return reviewRepository.existsByUserAndWork(userId, resourceId);
    }
}
