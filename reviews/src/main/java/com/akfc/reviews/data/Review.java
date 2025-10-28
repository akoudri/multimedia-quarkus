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
@Entity
@Table(name = "reviews", indexes = {
    @Index(name = "idx_review_resource", columnList = "resource_id"),
    @Index(name = "idx_review_user", columnList = "user_id"),
    @Index(name = "idx_review_rating", columnList = "rating"),
    @Index(name = "idx_review_status", columnList = "status"),
    @Index(name = "idx_review_archived", columnList = "archived"),
    @Index(name = "idx_review_publication_date", columnList = "publication_date")
})
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class Review extends PanacheEntityBase {

    // ========== PRIMARY KEY ==========

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    public Long id;

    // ========== BUSINESS FIELDS ==========

    /**
     * ID of the resource being reviewed (references Catalog service).
     * Renamed from resourceId to resourceId for consistency with documentation.
     */
    @NotNull(message = "Work ID is required")
    @Column(name = "resource_id", nullable = false)
    public Long resourceId;

    /**
     * ID of the user who wrote the review (references Users service).
     */
    @NotNull(message = "User ID is required")
    @Column(name = "user_id", nullable = false)
    public Long userId;

    /**
     * Rating from 1 to 5 stars.
     */
    @NotNull(message = "Rating is required")
    @Min(value = 1, message = "Rating must be at least 1")
    @Max(value = 5, message = "Rating must be at most 5")
    @Column(nullable = false)
    public Integer rating;

    /**
     * Review comment/text.
     * Required field with reasonable length constraints.
     */
    @NotBlank(message = "Comment is required")
    @Size(min = 10, max = 2000, message = "Comment must be between 10 and 2000 characters")
    @Column(nullable = false, length = 2000, columnDefinition = "TEXT")
    public String comment;

    /**
     * Date when the review was published.
     * Defaults to current date on creation.
     */
    @NotNull(message = "Publication date is required")
    @Column(name = "publication_date", nullable = false)
    public LocalDate publicationDate;

    // ========== STATUS & LIFECYCLE FIELDS ==========

    /**
     * Moderation status of the review.
     * - PENDING: Awaiting moderation
     * - APPROVED: Approved and visible to all users
     * - REJECTED: Rejected by moderator
     * - FLAGGED: Flagged for review (spam, inappropriate content, etc.)
     */
    @NotNull(message = "Status is required")
    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 20)
    public ReviewStatus status;

    /**
     * Soft delete flag.
     * When true, review is hidden from normal queries but retained in database.
     */
    @Column(nullable = false)
    public Boolean archived = false;

    /**
     * Reason for archiving (if archived).
     */
    @Size(max = 500, message = "Archive reason must not exceed 500 characters")
    @Column(name = "archive_reason", length = 500)
    public String archiveReason;

    // ========== MODERATION FIELDS ==========

    /**
     * User (moderator) who approved/rejected/flagged this review.
     */
    @Size(max = 100, message = "Moderated by must not exceed 100 characters")
    @Column(name = "moderated_by", length = 100)
    public String moderatedBy;

    /**
     * Timestamp when moderation action was taken.
     */
    @Column(name = "moderated_at")
    public LocalDateTime moderatedAt;

    /**
     * Reason for moderation decision (especially for rejection/flagging).
     */
    @Size(max = 500, message = "Moderation reason must not exceed 500 characters")
    @Column(name = "moderation_reason", length = 500)
    public String moderationReason;

    // ========== AUDIT/TIMESTAMP FIELDS ==========

    /**
     * Automatic creation timestamp.
     * Set once when entity is first persisted, never updated.
     */
    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    public LocalDateTime createdAt;

    /**
     * Automatic update timestamp.
     * Updated every time entity is modified.
     */
    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    public LocalDateTime updatedAt;

    /**
     * User who created this review (for audit trail).
     * Typically same as userId, but tracked separately for audit.
     */
    @Size(max = 100, message = "Created by must not exceed 100 characters")
    @Column(name = "created_by", length = 100)
    public String createdBy;

    /**
     * User who last modified this review (for audit trail).
     */
    @Size(max = 100, message = "Modified by must not exceed 100 characters")
    @Column(name = "modified_by", length = 100)
    public String modifiedBy;

    // ========== JPA LIFECYCLE CALLBACKS ==========

    /**
     * Called automatically before entity is persisted for the first time.
     * Initializes default values for status, archived flag, and publication date.
     */
    @PrePersist
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
    @PreUpdate
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
        return list("resourceId = ?1 AND archived = false ORDER BY publicationDate DESC", resourceId);
    }

    /**
     * Find all reviews by a specific user.
     * Excludes archived reviews.
     *
     * @param userId User ID
     * @return List of reviews by the user, ordered by publication date descending
     */
    public static List<Review> findByUserId(Long userId) {
        return list("userId = ?1 AND archived = false ORDER BY publicationDate DESC", userId);
    }

    /**
     * Find all approved reviews for a specific resource.
     *
     * @param resourceId Work ID
     * @return List of approved reviews
     */
    public static List<Review> findApprovedByWorkId(Long resourceId) {
        return list("resourceId = ?1 AND status = ?2 AND archived = false ORDER BY publicationDate DESC",
                   resourceId, ReviewStatus.APPROVED);
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
        return list("status = ?1 AND archived = false ORDER BY createdAt ASC",
                   ReviewStatus.PENDING);
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
        return list("status = ?1 AND archived = false ORDER BY moderatedAt DESC",
                   ReviewStatus.FLAGGED);
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
        LocalDateTime cutoffDate = LocalDateTime.now().minusDays(days);
        return list("createdAt >= ?1 AND archived = false ORDER BY createdAt DESC",
                   cutoffDate);
    }

    /**
     * Find all reviews with a specific rating.
     * Excludes archived reviews.
     *
     * @param rating Rating value (1-5)
     * @return List of reviews with that rating
     */
    public static List<Review> findByRating(Integer rating) {
        return list("rating = ?1 AND archived = false ORDER BY publicationDate DESC", rating);
    }

    /**
     * Find reviews for a resource with minimum rating threshold.
     *
     * @param resourceId Work ID
     * @param minRating Minimum rating (inclusive)
     * @return List of reviews meeting criteria
     */
    public static List<Review> findByWorkIdAndMinRating(Long resourceId, Integer minRating) {
        return list("resourceId = ?1 AND rating >= ?2 AND archived = false ORDER BY publicationDate DESC",
                   resourceId, minRating);
    }

    /**
     * Calculate average rating for a resource.
     * Only includes approved, non-archived reviews.
     *
     * @param resourceId Work ID
     * @return Average rating or null if no reviews
     */
    public static Double getAverageRatingForWork(Long resourceId) {
        return find("SELECT AVG(r.rating) FROM Review r WHERE r.resourceId = ?1 " +
                   "AND r.status = ?2 AND r.archived = false",
                   resourceId, ReviewStatus.APPROVED)
                .project(Double.class)
                .firstResult();
    }

    /**
     * Count reviews for a resource.
     * Only includes approved, non-archived reviews.
     *
     * @param resourceId Work ID
     * @return Count of reviews
     */
    public static long countReviewsForWork(Long resourceId) {
        return count("resourceId = ?1 AND status = ?2 AND archived = false",
                    resourceId, ReviewStatus.APPROVED);
    }

    /**
     * Count reviews by a user.
     * Includes all statuses except archived.
     *
     * @param userId User ID
     * @return Count of reviews
     */
    public static long countReviewsByUser(Long userId) {
        return count("userId = ?1 AND archived = false", userId);
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
        return count("userId = ?1 AND resourceId = ?2 AND archived = false", userId, resourceId) > 0;
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
        return find("archived = false",
                   Sort.by("createdAt").descending())
            .page(Page.of(pageIndex, pageSize))
            .list();
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
        return find("resourceId = ?1 AND status = ?2 AND archived = false",
                   Sort.by("publicationDate").descending(),
                   resourceId, ReviewStatus.APPROVED)
            .page(Page.of(pageIndex, pageSize))
            .list();
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
        return find("userId = ?1 AND archived = false",
                   Sort.by("createdAt").descending(),
                   userId)
            .page(Page.of(pageIndex, pageSize))
            .list();
    }

    /**
     * PAGINATION: Get total number of pages for approved reviews.
     *
     * @param pageSize Items per page
     * @return Total number of pages
     */
    public static int getTotalPages(int pageSize) {
        return find("status = ?1 AND archived = false", ReviewStatus.APPROVED)
            .page(Page.ofSize(pageSize))
            .pageCount();
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
        Review review = new Review();
        review.resourceId = resourceId;
        review.userId = userId;
        review.rating = rating;
        review.comment = comment;
        review.createdBy = createdBy;
        // status, archived, and publicationDate will be set by @PrePersist

        review.persist();
        return review;
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
        Review review = create(resourceId, userId, rating, comment, createdBy);
        review.publicationDate = publicationDate;
        review.persist();
        return review;
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
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot update archived review. Restore it first.");
        }

        this.rating = rating;
        this.comment = comment;
        this.modifiedBy = modifiedBy;

        // Reset to pending for re-moderation (unless rejected)
        if (this.status == ReviewStatus.APPROVED || this.status == ReviewStatus.FLAGGED) {
            this.status = ReviewStatus.PENDING;
            this.moderatedBy = null;
            this.moderatedAt = null;
            this.moderationReason = null;
        }

        // updatedAt will be set automatically by @UpdateTimestamp
        this.persist();
    }

    /**
     * Update only the rating.
     *
     * @param newRating New rating (1-5)
     * @param modifiedBy User making the change
     */
    public void updateRating(Integer newRating, String modifiedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot update archived review");
        }
        this.rating = newRating;
        this.modifiedBy = modifiedBy;
        // Reset moderation status
        if (this.status == ReviewStatus.APPROVED) {
            this.status = ReviewStatus.PENDING;
        }
        this.persist();
    }

    /**
     * Update only the comment.
     *
     * @param newComment New comment text
     * @param modifiedBy User making the change
     */
    public void updateComment(String newComment, String modifiedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot update archived review");
        }
        this.comment = newComment;
        this.modifiedBy = modifiedBy;
        // Reset moderation status
        if (this.status == ReviewStatus.APPROVED) {
            this.status = ReviewStatus.PENDING;
        }
        this.persist();
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
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot approve archived review");
        }
        this.status = ReviewStatus.APPROVED;
        this.moderatedBy = moderatorId;
        this.moderatedAt = LocalDateTime.now();
        this.modifiedBy = moderatorId;
        this.moderationReason = null; // Clear any previous reason
        this.persist();
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
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot reject archived review");
        }
        this.status = ReviewStatus.REJECTED;
        this.moderationReason = reason;
        this.moderatedBy = moderatorId;
        this.moderatedAt = LocalDateTime.now();
        this.modifiedBy = moderatorId;
        this.persist();
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
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot flag archived review");
        }
        this.status = ReviewStatus.FLAGGED;
        this.moderationReason = reason;
        this.moderatedBy = flaggedBy;
        this.moderatedAt = LocalDateTime.now();
        this.modifiedBy = flaggedBy;
        this.persist();
    }

    /**
     * Reset review to pending status.
     *
     * Useful when a flagged review needs to be re-evaluated.
     *
     * @param moderatorId Moderator resetting the status
     */
    public void resetToPending(String moderatorId) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot reset archived review");
        }
        this.status = ReviewStatus.PENDING;
        this.moderationReason = null;
        this.moderatedBy = moderatorId;
        this.moderatedAt = LocalDateTime.now();
        this.modifiedBy = moderatorId;
        this.persist();
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
        this.archived = true;
        this.archiveReason = reason;
        this.modifiedBy = archivedBy;
        this.persist();
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
        this.archived = false;
        this.archiveReason = null;
        this.modifiedBy = restoredBy;
        // Restored reviews go back to pending status
        this.status = ReviewStatus.PENDING;
        this.persist();
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
        if (Boolean.FALSE.equals(archived)) {
            throw new IllegalStateException(
                "Cannot permanently delete a non-archived review. Archive it first for safety.");
        }
        this.delete();
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
        // Log or audit this dangerous operation
        this.modifiedBy = confirmedBy;
        this.delete();
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
