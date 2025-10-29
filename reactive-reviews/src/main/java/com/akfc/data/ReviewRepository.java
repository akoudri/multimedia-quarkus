package com.akfc.data;

import io.quarkus.hibernate.reactive.panache.PanacheRepository;
import io.quarkus.panache.common.Page;
import io.quarkus.panache.common.Sort;
import io.smallrye.mutiny.Uni;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import org.hibernate.reactive.mutiny.Mutiny;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

/**
 * REACTIVE Repository for Review entity using the REPOSITORY PATTERN.
 *
 * This demonstrates the reactive repository pattern where
 * data access logic is separated from the entity and all operations
 * return Uni<> for non-blocking execution.
 *
 * Includes examples of:
 * - Native SQL queries with Mutiny.SessionFactory
 * - Reactive pagination with Panache
 * - Bulk moderation operations
 * - Statistics and aggregations
 *
 * NOTE: Criteria API is not well-supported in reactive Hibernate,
 * so we use native queries and JPQL instead.
 */
@ApplicationScoped
public class ReviewRepository implements PanacheRepository<Review> {

    /**
     * Mutiny SessionFactory for advanced reactive queries (native queries).
     * Automatically injected by Quarkus.
     */
    @Inject
    Mutiny.SessionFactory sessionFactory;

    // ========== REACTIVE NATIVE SQL QUERY EXAMPLES ==========

    /**
     * REACTIVE NATIVE QUERY EXAMPLE 1: Get average rating and review count per resource.
     *
     * Native SQL queries execute directly against the database reactively.
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
     * @return Uni containing list of Object arrays: [resourceId, avgRating, reviewCount, lastReviewDate]
     */
    public Uni<List<Object[]>> getAverageRatingPerWorkNative() {
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

        return sessionFactory.withSession(session ->
            session.createNativeQuery(sql, Object[].class)
                .getResultList()
        );
    }

    /**
     * REACTIVE NATIVE QUERY EXAMPLE 2: Get detailed statistics for a specific resource.
     *
     * Demonstrates parameterized reactive native query with single resource filter.
     *
     * @param resourceId Work ID to get statistics for
     * @return Uni containing Object array: [resourceId, avgRating, reviewCount, rating1Count, rating2Count,
     *         rating3Count, rating4Count, rating5Count] or null if no reviews
     */
    public Uni<Object[]> getWorkReviewStatsNative(Long resourceId) {
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

        return sessionFactory.withSession(session ->
            session.createNativeQuery(sql, Object[].class)
                .setParameter(1, resourceId)
                .getResultList()
                .map(results -> results.isEmpty() ? null : results.get(0))
        );
    }

    /**
     * REACTIVE NATIVE QUERY EXAMPLE 3: Find most active reviewers (users with most reviews).
     *
     * Demonstrates reactive native query for user statistics.
     *
     * @param limit Maximum number of users to return
     * @return Uni containing list of Object arrays: [userId, reviewCount, avgRating, lastReviewDate]
     */
    public Uni<List<Object[]>> getMostActiveReviewersNative(int limit) {
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

        return sessionFactory.withSession(session ->
            session.createNativeQuery(sql, Object[].class)
                .setParameter(1, limit)
                .getResultList()
        );
    }

    /**
     * REACTIVE NATIVE QUERY EXAMPLE 4: Get review count by status and rating distribution.
     *
     * Complex aggregation showing review distribution by status.
     *
     * @return Uni containing list of Object arrays: [status, totalCount, avgRating,
     *         rating1-5 counts, oldestDate, newestDate]
     */
    public Uni<List<Object[]>> getReviewDistributionByStatusNative() {
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

        return sessionFactory.withSession(session ->
            session.createNativeQuery(sql, Object[].class)
                .getResultList()
        );
    }

    /**
     * REACTIVE NATIVE QUERY EXAMPLE 5: Find resources with low average ratings.
     *
     * Identifies resources that may need attention based on poor reviews.
     *
     * @param maxRating Maximum average rating threshold
     * @param minReviewCount Minimum number of reviews required
     * @return Uni containing list of Object arrays: [resourceId, avgRating, reviewCount]
     */
    public Uni<List<Object[]>> findResourcesWithLowRatingsNative(double maxRating, int minReviewCount) {
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

        return sessionFactory.withSession(session ->
            session.createNativeQuery(sql, Object[].class)
                .setParameter(1, maxRating)
                .setParameter(2, minReviewCount)
                .getResultList()
        );
    }

    // ========== REACTIVE DYNAMIC QUERIES ==========

    /**
     * REACTIVE: Find reviews with dynamic filters.
     *
     * Uses JPQL with reactive Panache for type-safe queries.
     *
     * @param resourceId Optional resource ID filter
     * @param userId Optional user ID filter
     * @param minRating Optional minimum rating
     * @param status Optional status filter
     * @param publishedAfter Optional publication date filter
     * @return Uni containing list of reviews matching all provided criteria
     */
    public Uni<List<Review>> findWithDynamicFilters(Long resourceId,
                                                     Long userId,
                                                     Integer minRating,
                                                     ReviewStatus status,
                                                     LocalDate publishedAfter) {
        // Build dynamic query
        StringBuilder query = new StringBuilder("archived = false");
        List<Object> params = new ArrayList<>();

        if (resourceId != null) {
            query.append(" AND resourceId = ?").append(params.size() + 1);
            params.add(resourceId);
        }

        if (userId != null) {
            query.append(" AND userId = ?").append(params.size() + 1);
            params.add(userId);
        }

        if (minRating != null) {
            query.append(" AND rating >= ?").append(params.size() + 1);
            params.add(minRating);
        }

        if (status != null) {
            query.append(" AND status = ?").append(params.size() + 1);
            params.add(status);
        }

        if (publishedAfter != null) {
            query.append(" AND publicationDate >= ?").append(params.size() + 1);
            params.add(publishedAfter);
        }

        return list(query.toString(), params.toArray());
    }

    /**
     * REACTIVE: Get rating distribution for a resource.
     *
     * Uses JPQL aggregation query.
     *
     * @param resourceId Work ID
     * @return Uni containing map of rating (1-5) to count
     */
    public Uni<Map<Integer, Long>> getRatingDistribution(Long resourceId) {
        return sessionFactory.withSession(session ->
            session.createQuery(
                "SELECT r.rating, COUNT(r) FROM Review r " +
                "WHERE r.resourceId = ?1 AND r.status = ?2 AND r.archived = false " +
                "GROUP BY r.rating ORDER BY r.rating",
                Object[].class)
                .setParameter(1, resourceId)
                .setParameter(2, ReviewStatus.APPROVED)
                .getResultList()
                .map(results -> {
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
                })
        );
    }

    // ========== REACTIVE PAGINATION WITH REPOSITORY PATTERN ==========

    /**
     * REACTIVE PAGINATION EXAMPLE 1: Simple pagination.
     *
     * @param pageIndex Page number (0-based)
     * @param pageSize Items per page
     * @return Uni containing paginated reviews
     */
    public Uni<List<Review>> findAllPaginated(int pageIndex, int pageSize) {
        return find("archived = false",
                   Sort.by("createdAt").descending())
            .page(Page.of(pageIndex, pageSize))
            .list();
    }

    /**
     * REACTIVE PAGINATION EXAMPLE 2: Paginated search with metadata.
     *
     * Returns paginated results with complete metadata for UI display.
     *
     * @param resourceId Optional resource filter
     * @param status Optional status filter
     * @param pageIndex Page number (0-based)
     * @param pageSize Items per page
     * @return Uni containing paginated result with metadata
     */
    public Uni<PaginatedResult<Review>> findReviewsPaginated(Long resourceId,
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

        // Get both list and page count reactively
        return Uni.combine().all().unis(
            paged.list(),
            paged.pageCount(),
            paged.count()
        ).asTuple().map(tuple ->
            new PaginatedResult<>(
                tuple.getItem1(),  // data
                pageIndex,
                pageSize,
                tuple.getItem2(),  // totalPages
                tuple.getItem3()   // totalItems
            )
        );
    }

    // ========== REACTIVE CRUD OPERATIONS ==========

    /**
     * REACTIVE CREATE: Create a new review and persist it.
     *
     * This is the repository pattern approach to creating reviews.
     * Alternatively, you can use Review.create() for Active Record pattern.
     *
     * @param resourceId ID of the resource being reviewed
     * @param userId ID of the user writing the review
     * @param rating Rating from 1 to 5
     * @param comment Review comment
     * @param createdBy User creating the review
     * @return Uni containing the created and persisted review
     */
    public Uni<Review> createReview(Long resourceId, Long userId, Integer rating,
                                    String comment, String createdBy) {
        Review review = new Review();
        review.resourceId = resourceId;
        review.userId = userId;
        review.rating = rating;
        review.comment = comment;
        review.createdBy = createdBy;

        return persist(review);
    }

    /**
     * REACTIVE CREATE: Create a review with custom publication date.
     *
     * @param resourceId Work ID
     * @param userId User ID
     * @param rating Rating (1-5)
     * @param comment Review comment
     * @param publicationDate Custom publication date
     * @param createdBy User creating the review
     * @return Uni containing the created and persisted review
     */
    public Uni<Review> createReview(Long resourceId, Long userId, Integer rating,
                                    String comment, LocalDate publicationDate,
                                    String createdBy) {
        return createReview(resourceId, userId, rating, comment, createdBy)
                .chain(review -> {
                    review.publicationDate = publicationDate;
                    return persist(review);
                });
    }

    /**
     * REACTIVE READ: Find all active (non-archived) reviews.
     *
     * @return Uni containing list of active reviews
     */
    public Uni<List<Review>> findAllActive() {
        return list("archived", false);
    }

    /**
     * REACTIVE READ: Find review by ID, only if not archived.
     *
     * @param id Review ID
     * @return Uni containing review or null if not found/archived
     */
    public Uni<Review> findActiveById(Long id) {
        return find("id = ?1 AND archived = false", id).firstResult();
    }

    /**
     * REACTIVE READ: Find review by ID, including archived ones.
     *
     * @param id Review ID
     * @return Uni containing review or null if not found
     */
    public Uni<Review> findByIdIncludingArchived(Long id) {
        return findById(id);
    }

    /**
     * REACTIVE UPDATE: Update an existing review.
     *
     * @param id Review ID to update
     * @param rating New rating
     * @param comment New comment
     * @param modifiedBy User making the update
     * @return Uni containing updated review
     */
    public Uni<Review> updateReview(Long id, Integer rating, String comment, String modifiedBy) {
        return findActiveById(id)
            .chain(review -> {
                if (review == null) {
                    return Uni.createFrom().failure(
                        new IllegalArgumentException("Review not found or archived: " + id)
                    );
                }
                return review.update(rating, comment, modifiedBy);
            });
    }

    /**
     * REACTIVE DELETE (soft): Archive a review.
     *
     * @param id Review ID to archive
     * @param reason Reason for archiving
     * @param archivedBy User archiving the review
     * @return Uni containing archived review
     */
    public Uni<Review> archiveReview(Long id, String reason, String archivedBy) {
        return findActiveById(id)
            .chain(review -> {
                if (review == null) {
                    return Uni.createFrom().failure(
                        new IllegalArgumentException("Review not found or already archived: " + id)
                    );
                }
                return review.archive(reason, archivedBy);
            });
    }

    /**
     * REACTIVE UNDELETE: Restore an archived review.
     *
     * @param id Review ID to restore
     * @param restoredBy User restoring the review
     * @return Uni containing restored review
     */
    public Uni<Review> restoreReview(Long id, String restoredBy) {
        return findById(id)
            .chain(review -> {
                if (review == null) {
                    return Uni.createFrom().failure(
                        new IllegalArgumentException("Review not found: " + id)
                    );
                }
                if (Boolean.FALSE.equals(review.archived)) {
                    return Uni.createFrom().failure(
                        new IllegalStateException("Review is not archived: " + id)
                    );
                }
                return review.restore(restoredBy);
            });
    }

    /**
     * REACTIVE DELETE (hard): Permanently delete a review.
     *
     * WARNING: This cannot be undone!
     *
     * @param id Review ID to delete
     * @return Uni<Void> on successful deletion
     */
    public Uni<Void> permanentlyDeleteReview(Long id) {
        return findById(id)
            .chain(review -> {
                if (review == null) {
                    return Uni.createFrom().failure(
                        new IllegalArgumentException("Review not found: " + id)
                    );
                }
                return review.permanentlyDelete();
            });
    }

    /**
     * REACTIVE EXISTS: Check if review exists by ID.
     *
     * @param id Review ID
     * @return Uni containing true if exists and not archived
     */
    public Uni<Boolean> existsById(Long id) {
        return count("id = ?1 AND archived = false", id)
            .map(count -> count > 0);
    }

    /**
     * REACTIVE EXISTS: Check if user has already reviewed a resource.
     *
     * @param userId User ID
     * @param resourceId Work ID
     * @return Uni containing true if user has reviewed the resource
     */
    public Uni<Boolean> existsByUserAndWork(Long userId, Long resourceId) {
        return count("userId = ?1 AND resourceId = ?2 AND archived = false", userId, resourceId)
            .map(count -> count > 0);
    }

    // ========== REACTIVE MODERATION OPERATIONS ==========

    /**
     * REACTIVE MODERATE: Approve a review.
     *
     * @param id Review ID to approve
     * @param moderatorId Moderator performing the approval
     * @return Uni containing approved review
     */
    public Uni<Review> approveReview(Long id, String moderatorId) {
        return findActiveById(id)
            .chain(review -> {
                if (review == null) {
                    return Uni.createFrom().failure(
                        new IllegalArgumentException("Review not found or archived: " + id)
                    );
                }
                return review.approve(moderatorId);
            });
    }

    /**
     * REACTIVE MODERATE: Reject a review.
     *
     * @param id Review ID to reject
     * @param reason Reason for rejection
     * @param moderatorId Moderator performing the rejection
     * @return Uni containing rejected review
     */
    public Uni<Review> rejectReview(Long id, String reason, String moderatorId) {
        return findActiveById(id)
            .chain(review -> {
                if (review == null) {
                    return Uni.createFrom().failure(
                        new IllegalArgumentException("Review not found or archived: " + id)
                    );
                }
                return review.reject(reason, moderatorId);
            });
    }

    /**
     * REACTIVE MODERATE: Flag a review.
     *
     * @param id Review ID to flag
     * @param reason Reason for flagging
     * @param flaggedBy User flagging the review
     * @return Uni containing flagged review
     */
    public Uni<Review> flagReview(Long id, String reason, String flaggedBy) {
        return findActiveById(id)
            .chain(review -> {
                if (review == null) {
                    return Uni.createFrom().failure(
                        new IllegalArgumentException("Review not found or archived: " + id)
                    );
                }
                return review.flag(reason, flaggedBy);
            });
    }

    // ========== REACTIVE BUSINESS METHODS ==========

    /**
     * REACTIVE BUSINESS METHOD: Find pending reviews awaiting moderation.
     *
     * @return Uni containing list of pending reviews
     */
    public Uni<List<Review>> findPendingModeration() {
        return list("status = ?1 AND archived = false ORDER BY createdAt ASC",
                   ReviewStatus.PENDING);
    }

    /**
     * REACTIVE BUSINESS METHOD: Get average rating for a resource.
     *
     * Only counts approved, non-archived reviews.
     *
     * @param resourceId Work ID
     * @return Uni containing average rating or null if no reviews
     */
    public Uni<Double> getAverageRatingForWork(Long resourceId) {
        return find("SELECT AVG(r.rating) FROM Review r " +
                   "WHERE r.resourceId = ?1 AND r.status = ?2 AND r.archived = false",
                   resourceId, ReviewStatus.APPROVED)
            .project(Double.class)
            .firstResult();
    }

    // ========== REACTIVE BULK OPERATIONS ==========

    /**
     * REACTIVE: Approve multiple reviews at once.
     *
     * @param reviewIds List of review IDs to approve
     * @param moderatorId Moderator performing the operation
     * @return Uni containing number of reviews approved
     */
    public Uni<Integer> approveMultiple(List<Long> reviewIds, String moderatorId) {
        LocalDateTime now = LocalDateTime.now();
        return update(
            "status = ?1, moderatedBy = ?2, moderatedAt = ?3, modifiedBy = ?2 " +
            "WHERE id IN ?4 AND archived = false",
            ReviewStatus.APPROVED, moderatorId, now, reviewIds
        );
    }

    /**
     * REACTIVE: Reject multiple reviews at once.
     *
     * @param reviewIds List of review IDs to reject
     * @param reason Reason for rejection
     * @param moderatorId Moderator performing the operation
     * @return Uni containing number of reviews rejected
     */
    public Uni<Integer> rejectMultiple(List<Long> reviewIds, String reason, String moderatorId) {
        LocalDateTime now = LocalDateTime.now();
        return update(
            "status = ?1, moderationReason = ?2, moderatedBy = ?3, " +
            "moderatedAt = ?4, modifiedBy = ?3 WHERE id IN ?5 AND archived = false",
            ReviewStatus.REJECTED, reason, moderatorId, now, reviewIds
        );
    }

    /**
     * REACTIVE: Archive multiple reviews at once.
     *
     * @param reviewIds List of review IDs to archive
     * @param reason Reason for archiving
     * @param archivedBy User performing the operation
     * @return Uni containing number of reviews archived
     */
    public Uni<Integer> archiveMultiple(List<Long> reviewIds, String reason, String archivedBy) {
        return update(
            "archived = true, archiveReason = ?1, modifiedBy = ?2 " +
            "WHERE id IN ?3 AND archived = false",
            reason, archivedBy, reviewIds
        );
    }

    // ========== REACTIVE STATISTICS ==========

    /**
     * REACTIVE: Get review statistics by month for a given year.
     *
     * @param year Year to analyze
     * @return Uni containing map of month to review count
     */
    public Uni<Map<Integer, Long>> getReviewStatsByMonth(int year) {
        return sessionFactory.withSession(session ->
            session.createQuery(
                "SELECT MONTH(r.createdAt), COUNT(r) FROM Review r " +
                "WHERE YEAR(r.createdAt) = ?1 AND r.archived = false " +
                "GROUP BY MONTH(r.createdAt) ORDER BY MONTH(r.createdAt)",
                Object[].class)
                .setParameter(1, year)
                .getResultList()
                .map(results -> {
                    Map<Integer, Long> stats = new HashMap<>();
                    for (Object[] row : results) {
                        stats.put((Integer) row[0], (Long) row[1]);
                    }
                    return stats;
                })
        );
    }

    /**
     * REACTIVE: Get count of reviews by status.
     *
     * @return Uni containing map of status to count
     */
    public Uni<Map<ReviewStatus, Long>> getCountByStatus() {
        return sessionFactory.withSession(session ->
            session.createQuery(
                "SELECT r.status, COUNT(r) FROM Review r " +
                "WHERE r.archived = false " +
                "GROUP BY r.status ORDER BY r.status",
                Object[].class)
                .getResultList()
                .map(results -> {
                    Map<ReviewStatus, Long> counts = new HashMap<>();
                    for (Object[] row : results) {
                        counts.put((ReviewStatus) row[0], (Long) row[1]);
                    }
                    return counts;
                })
        );
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
