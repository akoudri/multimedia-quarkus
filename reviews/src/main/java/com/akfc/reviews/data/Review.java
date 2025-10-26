package com.akfc.reviews.data;

import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import io.quarkus.panache.common.Page;
import io.quarkus.panache.common.Sort;
import jakarta.persistence.*;
import jakarta.validation.constraints.*;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;

/**
 * Entity representing a review for a multimedia resource.
 * References resource and user by ID only (no JPA relationships as services have separate databases).
 *
 * This entity uses the ACTIVE RECORD pattern, inherited from PanacheEntityBase.
 * Business logic and persistence methods are defined directly in the entity.
 *
 * Lifecycle Management:
 * - Automatic timestamps using Hibernate annotations
 * - Moderation status tracking (PENDING, APPROVED, REJECTED, FLAGGED)
 * - Soft delete with archived flag
 * - Full validation with Jakarta Bean Validation
 * - Moderation audit trail
 *
 * Examples:
 * 1. Book review: id=1, resourceId=1, userId=5, rating=5, comment="Masterpiece of dystopian literature!",
 *    status=APPROVED, publicationDate=2024-03-15
 *
 * 2. Movie review: id=2, resourceId=2, userId=3, rating=4, comment="Mind-bending plot with excellent cinematography.",
 *    status=PENDING, publicationDate=2024-03-20
 */
public class Review extends PanacheEntityBase {

    // ========== PRIMARY KEY ==========
    public Long id;

    // ========== BUSINESS FIELDS ==========

    /**
     * ID of the resource being reviewed (references Catalog service).
     * Renamed from resourceId to resourceId for consistency with documentation.
     */
    public Long resourceId;

    /**
     * ID of the user who wrote the review (references Users service).
     */
    public Long userId;

    /**
     * Rating from 1 to 5 stars.
     */
    public Integer rating;

    /**
     * Review comment/text.
     * Required field with reasonable length constraints.
     */
    public String comment;

    /**
     * Date when the review was published.
     * Defaults to current date on creation.
     */
    public LocalDate publicationDate;

    // ========== STATUS & LIFECYCLE FIELDS ==========

    /**
     * Moderation status of the review.
     * - PENDING: Awaiting moderation
     * - APPROVED: Approved and visible to all users
     * - REJECTED: Rejected by moderator
     * - FLAGGED: Flagged for review (spam, inappropriate content, etc.)
     */
    public ReviewStatus status;

    /**
     * Soft delete flag.
     * When true, review is hidden from normal queries but retained in database.
     */
    public Boolean archived = false;

    /**
     * Reason for archiving (if archived).
     */
    public String archiveReason;

    // ========== MODERATION FIELDS ==========

    /**
     * User (moderator) who approved/rejected/flagged this review.
     */
    public String moderatedBy;

    /**
     * Timestamp when moderation action was taken.
     */
    public LocalDateTime moderatedAt;

    /**
     * Reason for moderation decision (especially for rejection/flagging).
     */
    public String moderationReason;

    // ========== AUDIT/TIMESTAMP FIELDS ==========

    /**
     * Automatic creation timestamp.
     * Set once when entity is first persisted, never updated.
     */
    public LocalDateTime createdAt;

    /**
     * Automatic update timestamp.
     * Updated every time entity is modified.
     */
    public LocalDateTime updatedAt;

    /**
     * User who created this review (for audit trail).
     * Typically same as userId, but tracked separately for audit.
     */
    public String createdBy;

    /**
     * User who last modified this review (for audit trail).
     */
    public String modifiedBy;

    // ========== JPA LIFECYCLE CALLBACKS ==========

    /**
     * Called automatically before entity is persisted for the first time.
     * Initializes default values for status, archived flag, and publication date.
     */
    protected void onCreate() {
        // Set default status if not explicitly set
        if (status == null) {
            status = ReviewStatus.PENDING;
        }

        // Ensure archived flag is set
        if (archived == null) {
            archived = false;
        }

        // Set publication date to today if not specified
        if (publicationDate == null) {
            publicationDate = LocalDate.now();
        }

        // Timestamps handled by @CreationTimestamp/@UpdateTimestamp
    }

    /**
     * Called automatically before entity is updated.
     * Validates business rules before save.
     */
    protected void onUpdate() {
        // Business rule: Cannot modify archived reviews without specifying modifier
        if (Boolean.TRUE.equals(archived) && modifiedBy == null) {
            throw new IllegalStateException("Cannot update archived review without specifying modifier");
        }
    }

    // ========== ACTIVE RECORD PATTERN - BUSINESS METHODS ==========

    /**
     * Find all reviews for a specific resource.
     * Excludes archived reviews.
     *
     * @param resourceId Work ID
     * @return List of reviews for the resource, ordered by publication date descending
     */
    public static List<Review> findByWorkId(Long resourceId) {
        return Collections.emptyList();
    }

    /**
     * Find all reviews by a specific user.
     * Excludes archived reviews.
     *
     * @param userId User ID
     * @return List of reviews by the user, ordered by publication date descending
     */
    public static List<Review> findByUserId(Long userId) {
        return Collections.emptyList();
    }

    /**
     * Find all approved reviews for a specific resource.
     *
     * @param resourceId Work ID
     * @return List of approved reviews
     */
    public static List<Review> findApprovedByWorkId(Long resourceId) {
        return Collections.emptyList();
    }

    /**
     * BUSINESS METHOD: Find pending reviews awaiting moderation.
     *
     * Returns all reviews with PENDING status that need moderator attention.
     * Ordered by creation date (oldest first) for FIFO processing.
     *
     * @return List of pending reviews
     */
    public static List<Review> findPendingReviews() {
        return Collections.emptyList();
    }

    /**
     * BUSINESS METHOD: Find flagged reviews.
     *
     * Returns reviews that have been flagged by users or moderators
     * for inappropriate content, spam, etc.
     *
     * @return List of flagged reviews
     */
    public static List<Review> findFlaggedReviews() {
        return Collections.emptyList();
    }

    /**
     * BUSINESS METHOD: Find recently added reviews.
     *
     * Returns reviews created within the last N days.
     * Useful for "Recent Reviews" features.
     *
     * @param days Number of days to look back
     * @return List of recent reviews
     */
    public static List<Review> findRecentReviews(int days) {
        return Collections.emptyList();
    }

    /**
     * Find all reviews with a specific rating.
     * Excludes archived reviews.
     *
     * @param rating Rating value (1-5)
     * @return List of reviews with that rating
     */
    public static List<Review> findByRating(Integer rating) {
        return Collections.emptyList();
    }

    /**
     * Find reviews for a resource with minimum rating threshold.
     *
     * @param resourceId Work ID
     * @param minRating Minimum rating (inclusive)
     * @return List of reviews meeting criteria
     */
    public static List<Review> findByWorkIdAndMinRating(Long resourceId, Integer minRating) {
        return Collections.emptyList();
    }

    /**
     * Calculate average rating for a resource.
     * Only includes approved, non-archived reviews.
     *
     * @param resourceId Work ID
     * @return Average rating or null if no reviews
     */
    public static Double getAverageRatingForWork(Long resourceId) {
        return null;
    }

    /**
     * Count reviews for a resource.
     * Only includes approved, non-archived reviews.
     *
     * @param resourceId Work ID
     * @return Count of reviews
     */
    public static long countReviewsForWork(Long resourceId) {
        return 0L;
    }

    /**
     * Count reviews by a user.
     * Includes all statuses except archived.
     *
     * @param userId User ID
     * @return Count of reviews
     */
    public static long countReviewsByUser(Long userId) {
        return 0L;
    }

    /**
     * Check if a user has already reviewed a resource.
     * Checks non-archived reviews only.
     *
     * @param userId User ID
     * @param resourceId Work ID
     * @return True if user has reviewed the resource
     */
    public static boolean userHasReviewedWork(Long userId, Long resourceId) {
        return false;
    }

    // ========== PAGINATION EXAMPLES (Active Record Pattern) ==========

    /**
     * PAGINATION: Find all reviews with pagination.
     *
     * @param pageIndex Page number (0-based)
     * @param pageSize Number of items per page
     * @return List of reviews for the requested page
     */
    public static List<Review> findAllPaginated(int pageIndex, int pageSize) {
        return Collections.emptyList();
    }

    /**
     * PAGINATION: Find reviews by resource with pagination.
     *
     * @param resourceId Work ID filter
     * @param pageIndex Page number (0-based)
     * @param pageSize Items per page
     * @return Paginated reviews for the resource
     */
    public static List<Review> findByWorkIdPaginated(Long resourceId, int pageIndex, int pageSize) {
        return Collections.emptyList();
    }

    /**
     * PAGINATION: Find reviews by user with pagination.
     *
     * @param userId User ID filter
     * @param pageIndex Page number (0-based)
     * @param pageSize Items per page
     * @return Paginated reviews by the user
     */
    public static List<Review> findByUserIdPaginated(Long userId, int pageIndex, int pageSize) {
        return Collections.emptyList();
    }

    /**
     * PAGINATION: Get total number of pages for approved reviews.
     *
     * @param pageSize Items per page
     * @return Total number of pages
     */
    public static int getTotalPages(int pageSize) {
        return 0;
    }

    // ========== INSTANCE METHODS FOR LIFECYCLE MANAGEMENT ==========

    // ========== CREATE ==========

    /**
     * Create a new review and persist it to the database.
     *
     * This factory method creates a review with PENDING status by default.
     * Reviews start in PENDING state and require moderator approval.
     *
     * @param resourceId ID of the resource being reviewed
     * @param userId ID of the user writing the review
     * @param rating Rating from 1 to 5 stars
     * @param comment Review text/comment
     * @param createdBy User creating the review (typically same as userId)
     * @return The created and persisted review
     */
    public static Review create(Long resourceId, Long userId, Integer rating,
                               String comment, String createdBy) {
        return null;
    }

    /**
     * Create a review with custom publication date.
     *
     * Useful for importing historical reviews.
     *
     * @param resourceId Work ID
     * @param userId User ID
     * @param rating Rating (1-5)
     * @param comment Review comment
     * @param publicationDate Custom publication date
     * @param createdBy User creating the review
     * @return The created and persisted review
     */
    public static Review create(Long resourceId, Long userId, Integer rating,
                               String comment, LocalDate publicationDate,
                               String createdBy) {
        return null;
    }

    // ========== UPDATE ==========

    /**
     * Update this review's rating and comment.
     *
     * When a review is updated, it goes back to PENDING status
     * to be re-moderated (unless it was already rejected).
     *
     * @param rating New rating (1-5)
     * @param comment New comment text
     * @param modifiedBy User making the update (typically the review author)
     */
    public void update(Integer rating, String comment, String modifiedBy) {
        //TODO
    }

    /**
     * Update only the rating.
     *
     * @param newRating New rating (1-5)
     * @param modifiedBy User making the change
     */
    public void updateRating(Integer newRating, String modifiedBy) {
        //TODO
    }

    /**
     * Update only the comment.
     *
     * @param newComment New comment text
     * @param modifiedBy User making the change
     */
    public void updateComment(String newComment, String modifiedBy) {
        //TODO
    }

    // ========== MODERATION ==========

    /**
     * Approve this review (moderation action).
     *
     * Sets status to APPROVED and records moderator information.
     *
     * @param moderatorId Moderator performing the approval
     */
    public void approve(String moderatorId) {
        //TODO
    }

    /**
     * Reject this review (moderation action).
     *
     * Sets status to REJECTED with a reason.
     *
     * @param reason Reason for rejection
     * @param moderatorId Moderator performing the rejection
     */
    public void reject(String reason, String moderatorId) {
        //TODO
    }

    /**
     * Flag this review for attention (moderation action).
     *
     * Sets status to FLAGGED, indicating it needs review for
     * inappropriate content, spam, etc.
     *
     * @param reason Reason for flagging
     * @param flaggedBy User or moderator flagging the review
     */
    public void flag(String reason, String flaggedBy) {
        //TODO
    }

    /**
     * Reset review to pending status.
     *
     * Useful when a flagged review needs to be re-evaluated.
     *
     * @param moderatorId Moderator resetting the status
     */
    public void resetToPending(String moderatorId) {
        //TODO
    }

    // ========== SOFT DELETE / ARCHIVE ==========

    /**
     * Archive this review (soft delete).
     *
     * Soft delete keeps the record in the database but marks it as archived.
     * The review will be excluded from normal queries.
     *
     * @param reason Reason for archiving
     * @param archivedBy User performing the archive
     */
    public void archive(String reason, String archivedBy) {
        //TODO
    }

    /**
     * Restore an archived review.
     *
     * Unarchives the review and sets it back to pending status
     * for re-moderation.
     *
     * @param restoredBy User performing the restoration
     */
    public void restore(String restoredBy) {
        //TODO
    }

    // ========== HARD DELETE ==========

    /**
     * Permanently delete this review from the database.
     *
     * WARNING: This is a hard delete and cannot be undone!
     * Use archive() for soft delete instead in most cases.
     *
     * This should only be used when:
     * - Review contains illegal content
     * - Legal requirements mandate data removal
     * - The review was created by mistake
     *
     * @throws IllegalStateException if review is not archived first
     */
    public void permanentlyDelete() {
        //TODO
    }

    /**
     * Force delete without archiving first.
     *
     * DANGEROUS: Use with extreme caution!
     * Only for legal compliance or emergency situations.
     *
     * @param confirmedBy Administrator confirming the deletion
     */
    public void forceDelete(String confirmedBy) {
        //TODO
    }

    // ========== UTILITY METHODS ==========

    /**
     * Check if this review is approved and visible to users.
     *
     * @return True if status is APPROVED and not archived
     */
    public boolean isApproved() {
        return this.status == ReviewStatus.APPROVED && Boolean.FALSE.equals(archived);
    }

    /**
     * Check if this review is pending moderation.
     *
     * @return True if status is PENDING
     */
    public boolean isPending() {
        return this.status == ReviewStatus.PENDING && Boolean.FALSE.equals(archived);
    }

    /**
     * Check if this review needs moderator attention.
     *
     * @return True if status is PENDING or FLAGGED
     */
    public boolean needsModeration() {
        return (this.status == ReviewStatus.PENDING || this.status == ReviewStatus.FLAGGED) &&
               Boolean.FALSE.equals(archived);
    }
}
