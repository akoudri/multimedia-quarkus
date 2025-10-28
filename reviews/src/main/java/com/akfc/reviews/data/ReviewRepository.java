package com.akfc.reviews.data;

import io.quarkus.hibernate.orm.panache.PanacheRepository;
import io.quarkus.panache.common.Page;
import io.quarkus.panache.common.Sort;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.persistence.EntityManager;
import jakarta.persistence.Query;
import jakarta.persistence.criteria.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Repository for Review entity using the REPOSITORY PATTERN.
 *
 * This demonstrates an alternative to Active Record pattern where
 * data access logic is separated from the entity.
 *
 * Includes examples of:
 * - Native SQL queries (specifically requested)
 * - Criteria API for dynamic queries
 * - Pagination with Panache
 * - Bulk moderation operations
 * - Statistics and aggregations
 */
@ApplicationScoped
public class ReviewRepository implements PanacheRepository<Review> {

    /**
     * EntityManager for advanced queries (Criteria API, native queries).
     * Automatically injected by Quarkus.
     */
    EntityManager em;

    // ========== NATIVE SQL QUERY EXAMPLES ==========

    /**
     * NATIVE QUERY EXAMPLE 1: Get average rating and review count per resource.
     *
     * Native SQL queries bypass JPA/JPQL and execute directly against the database.
     * Use when:
     * - You need database-specific features
     * - Complex aggregations are easier in SQL
     * - Performance optimization requires specific SQL
     *
     * This query demonstrates:
     * - Aggregation functions (AVG, COUNT)
     * - GROUP BY clause
     * - Filtering with WHERE
     * - Result mapping to Object[]
     *
     * @return List of Object arrays: [resourceId, avgRating, reviewCount, lastReviewDate]
     */
    public List<Object[]> getAverageRatingPerWorkNative() {
        String sql = """
            SELECT
                resource_id,
                ROUND(AVG(rating)::numeric, 2) as avg_rating,
                COUNT(*) as review_count,
                MAX(publication_date) as last_review_date
            FROM reviews
            WHERE status = 'APPROVED' AND archived = false
            GROUP BY resource_id
            ORDER BY avg_rating DESC, review_count DESC
            """;

        Query query = em.createNativeQuery(sql);
        return query.getResultList();
    }

    /**
     * NATIVE QUERY EXAMPLE 2: Get detailed statistics for a specific resource.
     *
     * Demonstrates parameterized native query with single resource filter.
     *
     * @param resourceId Work ID to get statistics for
     * @return Object array: [resourceId, avgRating, reviewCount, rating1Count, rating2Count,
     *         rating3Count, rating4Count, rating5Count]
     */
    public Object[] getWorkReviewStatsNative(Long resourceId) {
        String sql = """
            SELECT
                resource_id,
                ROUND(AVG(rating)::numeric, 2) as avg_rating,
                COUNT(*) as total_reviews,
                COUNT(CASE WHEN rating = 1 THEN 1 END) as rating_1_count,
                COUNT(CASE WHEN rating = 2 THEN 1 END) as rating_2_count,
                COUNT(CASE WHEN rating = 3 THEN 1 END) as rating_3_count,
                COUNT(CASE WHEN rating = 4 THEN 1 END) as rating_4_count,
                COUNT(CASE WHEN rating = 5 THEN 1 END) as rating_5_count
            FROM reviews
            WHERE resource_id = ?1 AND status = 'APPROVED' AND archived = false
            GROUP BY resource_id
            """;

        Query query = em.createNativeQuery(sql);
        query.setParameter(1, resourceId);

        List<Object[]> results = query.getResultList();
        return results.isEmpty() ? null : results.get(0);
    }

    /**
     * NATIVE QUERY EXAMPLE 3: Find most active reviewers (users with most reviews).
     *
     * Demonstrates native query for user statistics.
     *
     * @param limit Maximum number of users to return
     * @return List of Object arrays: [userId, reviewCount, avgRating, lastReviewDate]
     */
    public List<Object[]> getMostActiveReviewersNative(int limit) {
        String sql = """
            SELECT
                user_id,
                COUNT(*) as review_count,
                ROUND(AVG(rating)::numeric, 2) as avg_rating_given,
                MAX(created_at) as last_review_date
            FROM reviews
            WHERE archived = false
            GROUP BY user_id
            ORDER BY review_count DESC, last_review_date DESC
            LIMIT ?1
            """;

        Query query = em.createNativeQuery(sql);
        query.setParameter(1, limit);
        return query.getResultList();
    }

    /**
     * NATIVE QUERY EXAMPLE 4: Get review count by status and rating distribution.
     *
     * Complex aggregation showing review distribution by status.
     *
     * @return List of Object arrays: [status, totalCount, avgRating,
     *         rating1-5 counts, oldestDate, newestDate]
     */
    public List<Object[]> getReviewDistributionByStatusNative() {
        String sql = """
            SELECT
                status,
                COUNT(*) as total_count,
                ROUND(AVG(rating)::numeric, 2) as avg_rating,
                COUNT(CASE WHEN rating = 1 THEN 1 END) as rating_1,
                COUNT(CASE WHEN rating = 2 THEN 1 END) as rating_2,
                COUNT(CASE WHEN rating = 3 THEN 1 END) as rating_3,
                COUNT(CASE WHEN rating = 4 THEN 1 END) as rating_4,
                COUNT(CASE WHEN rating = 5 THEN 1 END) as rating_5,
                MIN(created_at) as oldest_review,
                MAX(created_at) as newest_review
            FROM reviews
            WHERE archived = false
            GROUP BY status
            ORDER BY
                CASE status
                    WHEN 'PENDING' THEN 1
                    WHEN 'APPROVED' THEN 2
                    WHEN 'FLAGGED' THEN 3
                    WHEN 'REJECTED' THEN 4
                END
            """;

        Query query = em.createNativeQuery(sql);
        return query.getResultList();
    }

    /**
     * NATIVE QUERY EXAMPLE 5: Find resources with low average ratings.
     *
     * Identifies resources that may need attention based on poor reviews.
     *
     * @param maxRating Maximum average rating threshold
     * @param minReviewCount Minimum number of reviews required
     * @return List of Object arrays: [resourceId, avgRating, reviewCount]
     */
    public List<Object[]> findResourcesWithLowRatingsNative(double maxRating, int minReviewCount) {
        String sql = """
            SELECT
                resource_id,
                ROUND(AVG(rating)::numeric, 2) as avg_rating,
                COUNT(*) as review_count
            FROM reviews
            WHERE status = 'APPROVED' AND archived = false
            GROUP BY resource_id
            HAVING AVG(rating) <= ?1 AND COUNT(*) >= ?2
            ORDER BY avg_rating ASC, review_count DESC
            """;

        Query query = em.createNativeQuery(sql);
        query.setParameter(1, maxRating);
        query.setParameter(2, minReviewCount);
        return query.getResultList();
    }

    // ========== CRITERIA API EXAMPLES ==========

    /**
     * CRITERIA API: Find reviews with dynamic filters using JPA Criteria API.
     *
     * The Criteria API provides type-safe, programmatic query construction.
     * It's more verbose than JPQL but offers:
     * - Compile-time type checking
     * - IDE auto-completion
     * - Easier dynamic query building
     * - Better refactoring support
     *
     * @param resourceId Optional resource ID filter
     * @param userId Optional user ID filter
     * @param minRating Optional minimum rating
     * @param status Optional status filter
     * @param publishedAfter Optional publication date filter
     * @return List of reviews matching all provided criteria
     */
    public List<Review> findWithCriteriaApi(Long resourceId,
                                            Long userId,
                                            Integer minRating,
                                            ReviewStatus status,
                                            LocalDate publishedAfter) {
        CriteriaBuilder cb = em.getCriteriaBuilder();
        CriteriaQuery<Review> cq = cb.createQuery(Review.class);
        Root<Review> review = cq.from(Review.class);

        // Build predicates dynamically
        List<Predicate> predicates = new ArrayList<>();

        // Always exclude archived
        predicates.add(cb.isFalse(review.get("archived")));

        if (resourceId != null) {
            predicates.add(cb.equal(review.get("resourceId"), resourceId));
        }

        if (userId != null) {
            predicates.add(cb.equal(review.get("userId"), userId));
        }

        if (minRating != null) {
            predicates.add(cb.greaterThanOrEqualTo(review.get("rating"), minRating));
        }

        if (status != null) {
            predicates.add(cb.equal(review.get("status"), status));
        }

        if (publishedAfter != null) {
            predicates.add(cb.greaterThanOrEqualTo(
                review.get("publicationDate"), publishedAfter
            ));
        }

        cq.where(cb.and(predicates.toArray(new Predicate[0])));
        cq.orderBy(cb.desc(review.get("createdAt")));

        return em.createQuery(cq).getResultList();
    }

    /**
     * CRITERIA API: Advanced aggregation example.
     *
     * Get rating distribution for a specific resource using Criteria API.
     *
     * @param resourceId Work ID
     * @return Map of rating (1-5) to count
     */
    public Map<Integer, Long> getRatingDistributionCriteria(Long resourceId) {
        CriteriaBuilder cb = em.getCriteriaBuilder();
        CriteriaQuery<Object[]> cq = cb.createQuery(Object[].class);
        Root<Review> review = cq.from(Review.class);

        // SELECT rating, COUNT(*)
        cq.multiselect(
            review.get("rating"),
            cb.count(review)
        );

        // WHERE resourceId = ? AND status = APPROVED AND archived = false
        cq.where(
            cb.and(
                cb.equal(review.get("resourceId"), resourceId),
                cb.equal(review.get("status"), ReviewStatus.APPROVED),
                cb.isFalse(review.get("archived"))
            )
        );

        // GROUP BY rating
        cq.groupBy(review.get("rating"));

        // ORDER BY rating
        cq.orderBy(cb.asc(review.get("rating")));

        List<Object[]> results = em.createQuery(cq).getResultList();

        Map<Integer, Long> distribution = new HashMap<>();
        for (Object[] row : results) {
            distribution.put((Integer) row[0], (Long) row[1]);
        }

        return distribution;
    }

    // ========== PAGINATION WITH REPOSITORY PATTERN ==========

    /**
     * PAGINATION EXAMPLE 1: Simple pagination.
     *
     * @param pageIndex Page number (0-based)
     * @param pageSize Items per page
     * @return Paginated reviews
     */
    public List<Review> findAllPaginated(int pageIndex, int pageSize) {
        return find("archived = false",
                   Sort.by("createdAt").descending())
            .page(Page.of(pageIndex, pageSize))
            .list();
    }

    /**
     * PAGINATION EXAMPLE 2: Paginated search with metadata.
     *
     * Returns paginated results with complete metadata for UI display.
     *
     * @param resourceId Optional resource filter
     * @param status Optional status filter
     * @param pageIndex Page number (0-based)
     * @param pageSize Items per page
     * @return Paginated result with metadata
     */
    public PaginatedResult<Review> findReviewsPaginated(Long resourceId,
                                                        ReviewStatus status,
                                                        int pageIndex,
                                                        int pageSize) {
        // Build dynamic query
        StringBuilder query = new StringBuilder("archived = false");
        List<Object> params = new ArrayList<>();

        if (resourceId != null) {
            query.append(" AND resourceId = ?").append(params.size() + 1);
            params.add(resourceId);
        }

        if (status != null) {
            query.append(" AND status = ?").append(params.size() + 1);
            params.add(status);
        }

        Sort sort = Sort.by("createdAt").descending();

        var paged = find(query.toString(), sort, params.toArray())
            .page(Page.of(pageIndex, pageSize));

        return new PaginatedResult<>(
            paged.list(),
            pageIndex,
            pageSize,
            paged.pageCount(),
            paged.count()
        );
    }

    // ========== CRUD OPERATIONS ==========

    /**
     * CREATE: Create a new review and persist it.
     *
     * This is the repository pattern approach to creating reviews.
     * Alternatively, you can use Review.create() for Active Record pattern.
     *
     * @param resourceId ID of the resource being reviewed
     * @param userId ID of the user writing the review
     * @param rating Rating from 1 to 5
     * @param comment Review comment
     * @param createdBy User creating the review
     * @return The created and persisted review
     */
    public Review createReview(Long resourceId, Long userId, Integer rating,
                              String comment, String createdBy) {
        Review review = new Review();
        review.resourceId = resourceId;
        review.userId = userId;
        review.rating = rating;
        review.comment = comment;
        review.createdBy = createdBy;

        persist(review);
        return review;
    }

    /**
     * CREATE: Create a review with custom publication date.
     *
     * @param resourceId Work ID
     * @param userId User ID
     * @param rating Rating (1-5)
     * @param comment Review comment
     * @param publicationDate Custom publication date
     * @param createdBy User creating the review
     * @return The created and persisted review
     */
    public Review createReview(Long resourceId, Long userId, Integer rating,
                              String comment, LocalDate publicationDate,
                              String createdBy) {
        Review review = createReview(resourceId, userId, rating, comment, createdBy);
        review.publicationDate = publicationDate;
        persist(review);
        return review;
    }

    /**
     * READ: Find all active (non-archived) reviews.
     */
    public List<Review> findAllActive() {
        return list("archived", false);
    }

    /**
     * READ: Find review by ID, only if not archived.
     */
    public Review findActiveById(Long id) {
        return find("id = ?1 AND archived = false", id).firstResult();
    }

    /**
     * READ: Find review by ID, including archived ones.
     *
     * @param id Review ID
     * @return Review or null if not found
     */
    public Review findByIdIncludingArchived(Long id) {
        return findById(id);
    }

    /**
     * UPDATE: Update an existing review.
     *
     * @param id Review ID to update
     * @param rating New rating
     * @param comment New comment
     * @param modifiedBy User making the update
     * @return Updated review
     * @throws IllegalArgumentException if review not found
     */
    public Review updateReview(Long id, Integer rating, String comment, String modifiedBy) {
        Review review = findActiveById(id);
        if (review == null) {
            throw new IllegalArgumentException("Review not found or archived: " + id);
        }

        review.update(rating, comment, modifiedBy);
        return review;
    }

    /**
     * DELETE (soft): Archive a review.
     *
     * @param id Review ID to archive
     * @param reason Reason for archiving
     * @param archivedBy User archiving the review
     * @return Archived review
     * @throws IllegalArgumentException if review not found
     */
    public Review archiveReview(Long id, String reason, String archivedBy) {
        Review review = findActiveById(id);
        if (review == null) {
            throw new IllegalArgumentException("Review not found or already archived: " + id);
        }

        review.archive(reason, archivedBy);
        return review;
    }

    /**
     * UNDELETE: Restore an archived review.
     *
     * @param id Review ID to restore
     * @param restoredBy User restoring the review
     * @return Restored review
     * @throws IllegalArgumentException if review not found
     */
    public Review restoreReview(Long id, String restoredBy) {
        Review review = findById(id);
        if (review == null) {
            throw new IllegalArgumentException("Review not found: " + id);
        }
        if (Boolean.FALSE.equals(review.archived)) {
            throw new IllegalStateException("Review is not archived: " + id);
        }

        review.restore(restoredBy);
        return review;
    }

    /**
     * DELETE (hard): Permanently delete a review.
     *
     * WARNING: This cannot be undone!
     *
     * @param id Review ID to delete
     * @throws IllegalArgumentException if review not found
     * @throws IllegalStateException if review not archived first
     */
    public void permanentlyDeleteReview(Long id) {
        Review review = findById(id);
        if (review == null) {
            throw new IllegalArgumentException("Review not found: " + id);
        }

        review.permanentlyDelete();
    }

    /**
     * EXISTS: Check if review exists by ID.
     *
     * @param id Review ID
     * @return True if exists and not archived
     */
    public boolean existsById(Long id) {
        return count("id = ?1 AND archived = false", id) > 0;
    }

    /**
     * EXISTS: Check if user has already reviewed a resource.
     *
     * @param userId User ID
     * @param resourceId Work ID
     * @return True if user has reviewed the resource
     */
    public boolean existsByUserAndWork(Long userId, Long resourceId) {
        return count("userId = ?1 AND resourceId = ?2 AND archived = false", userId, resourceId) > 0;
    }

    // ========== MODERATION OPERATIONS ==========

    /**
     * MODERATE: Approve a review.
     *
     * @param id Review ID to approve
     * @param moderatorId Moderator performing the approval
     * @return Approved review
     * @throws IllegalArgumentException if review not found
     */
    public Review approveReview(Long id, String moderatorId) {
        Review review = findActiveById(id);
        if (review == null) {
            throw new IllegalArgumentException("Review not found or archived: " + id);
        }

        review.approve(moderatorId);
        return review;
    }

    /**
     * MODERATE: Reject a review.
     *
     * @param id Review ID to reject
     * @param reason Reason for rejection
     * @param moderatorId Moderator performing the rejection
     * @return Rejected review
     * @throws IllegalArgumentException if review not found
     */
    public Review rejectReview(Long id, String reason, String moderatorId) {
        Review review = findActiveById(id);
        if (review == null) {
            throw new IllegalArgumentException("Review not found or archived: " + id);
        }

        review.reject(reason, moderatorId);
        return review;
    }

    /**
     * MODERATE: Flag a review.
     *
     * @param id Review ID to flag
     * @param reason Reason for flagging
     * @param flaggedBy User flagging the review
     * @return Flagged review
     * @throws IllegalArgumentException if review not found
     */
    public Review flagReview(Long id, String reason, String flaggedBy) {
        Review review = findActiveById(id);
        if (review == null) {
            throw new IllegalArgumentException("Review not found or archived: " + id);
        }

        review.flag(reason, flaggedBy);
        return review;
    }

    // ========== BUSINESS METHODS ==========

    /**
     * BUSINESS METHOD: Find pending reviews awaiting moderation.
     */
    public List<Review> findPendingModeration() {
        return list("status = ?1 AND archived = false ORDER BY createdAt ASC",
                   ReviewStatus.PENDING);
    }

    /**
     * BUSINESS METHOD: Get average rating for a resource.
     *
     * Only counts approved, non-archived reviews.
     *
     * @param resourceId Work ID
     * @return Average rating or null if no reviews
     */
    public Double getAverageRatingForWork(Long resourceId) {
        return find("SELECT AVG(r.rating) FROM Review r " +
                   "WHERE r.resourceId = ?1 AND r.status = ?2 AND r.archived = false",
                   resourceId, ReviewStatus.APPROVED)
            .project(Double.class)
            .firstResult();
    }

    /**
     * BUSINESS METHOD: Get rating distribution for a resource.
     *
     * @param resourceId Work ID
     * @return Map of rating (1-5) to count
     */
    public Map<Integer, Long> getRatingDistribution(Long resourceId) {
        List<Object[]> results = em.createQuery(
            "SELECT r.rating, COUNT(r) FROM Review r " +
            "WHERE r.resourceId = ?1 AND r.status = ?2 AND r.archived = false " +
            "GROUP BY r.rating ORDER BY r.rating",
            Object[].class)
            .setParameter(1, resourceId)
            .setParameter(2, ReviewStatus.APPROVED)
            .getResultList();

        Map<Integer, Long> distribution = new HashMap<>();
        // Initialize all ratings to 0
        for (int i = 1; i <= 5; i++) {
            distribution.put(i, 0L);
        }
        // Fill in actual counts
        for (Object[] row : results) {
            distribution.put((Integer) row[0], (Long) row[1]);
        }

        return distribution;
    }

    // ========== BULK OPERATIONS ==========

    /**
     * Approve multiple reviews at once.
     *
     * @param reviewIds List of review IDs to approve
     * @param moderatorId Moderator performing the operation
     * @return Number of reviews approved
     */
    public int approveMultiple(List<Long> reviewIds, String moderatorId) {
        LocalDateTime now = LocalDateTime.now();
        return update(
            "status = ?1, moderatedBy = ?2, moderatedAt = ?3, modifiedBy = ?2 " +
            "WHERE id IN ?4 AND archived = false",
            ReviewStatus.APPROVED, moderatorId, now, reviewIds
        );
    }

    /**
     * Reject multiple reviews at once.
     *
     * @param reviewIds List of review IDs to reject
     * @param reason Reason for rejection
     * @param moderatorId Moderator performing the operation
     * @return Number of reviews rejected
     */
    public int rejectMultiple(List<Long> reviewIds, String reason, String moderatorId) {
        LocalDateTime now = LocalDateTime.now();
        return update(
            "status = ?1, moderationReason = ?2, moderatedBy = ?3, " +
            "moderatedAt = ?4, modifiedBy = ?3 WHERE id IN ?5 AND archived = false",
            ReviewStatus.REJECTED, reason, moderatorId, now, reviewIds
        );
    }

    /**
     * Archive multiple reviews at once.
     *
     * @param reviewIds List of review IDs to archive
     * @param reason Reason for archiving
     * @param archivedBy User performing the operation
     * @return Number of reviews archived
     */
    public int archiveMultiple(List<Long> reviewIds, String reason, String archivedBy) {
        return update(
            "archived = true, archiveReason = ?1, modifiedBy = ?2 " +
            "WHERE id IN ?3 AND archived = false",
            reason, archivedBy, reviewIds
        );
    }

    // ========== STATISTICS ==========

    /**
     * Get review statistics by month for a given year.
     *
     * @param year Year to analyze
     * @return Map of month to review count
     */
    public Map<Integer, Long> getReviewStatsByMonth(int year) {
        List<Object[]> results = em.createQuery(
            "SELECT MONTH(r.createdAt), COUNT(r) FROM Review r " +
            "WHERE YEAR(r.createdAt) = ?1 AND r.archived = false " +
            "GROUP BY MONTH(r.createdAt) ORDER BY MONTH(r.createdAt)",
            Object[].class)
            .setParameter(1, year)
            .getResultList();

        Map<Integer, Long> stats = new HashMap<>();
        for (Object[] row : results) {
            stats.put((Integer) row[0], (Long) row[1]);
        }
        return stats;
    }

    /**
     * Get count of reviews by status.
     *
     * @return Map of status to count
     */
    public Map<ReviewStatus, Long> getCountByStatus() {
        List<Object[]> results = em.createQuery(
            "SELECT r.status, COUNT(r) FROM Review r " +
            "WHERE r.archived = false " +
            "GROUP BY r.status ORDER BY r.status",
            Object[].class)
            .getResultList();

        Map<ReviewStatus, Long> counts = new HashMap<>();
        for (Object[] row : results) {
            counts.put((ReviewStatus) row[0], (Long) row[1]);
        }
        return counts;
    }

    /**
     * Inner class for paginated results with metadata.
     *
     * This provides a clean way to return pagination information
     * along with the actual data.
     */
    public static class PaginatedResult<T> {
        public final List<T> data;
        public final int currentPage;
        public final int pageSize;
        public final int totalPages;
        public final long totalItems;

        public PaginatedResult(List<T> data, int currentPage, int pageSize,
                              int totalPages, long totalItems) {
            this.data = data;
            this.currentPage = currentPage;
            this.pageSize = pageSize;
            this.totalPages = totalPages;
            this.totalItems = totalItems;
        }

        public boolean hasNextPage() {
            return currentPage < totalPages - 1;
        }

        public boolean hasPreviousPage() {
            return currentPage > 0;
        }

        public int getStartItem() {
            return currentPage * pageSize + 1;
        }

        public int getEndItem() {
            return Math.min((currentPage + 1) * pageSize, (int) totalItems);
        }
    }
}
