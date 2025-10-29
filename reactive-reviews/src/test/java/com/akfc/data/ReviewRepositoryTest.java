package com.akfc.data;

import io.quarkus.test.junit.QuarkusTest;
import io.quarkus.test.vertx.RunOnVertxContext;
import io.quarkus.test.vertx.UniAsserter;
import io.smallrye.mutiny.Uni;
import io.smallrye.mutiny.helpers.test.UniAssertSubscriber;
import jakarta.inject.Inject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Integration tests for ReviewRepository demonstrating Mutiny testing patterns with database.
 *
 * This test class illustrates:
 * - Testing reactive repository methods with real database
 * - Using @RunOnVertxContext for reactive database operations
 * - Using UniAsserter for testing within Vert.x context
 * - Testing CRUD operations reactively
 * - Testing complex queries and aggregations
 * - Testing pagination
 * - Testing native SQL queries
 *
 * IMPORTANT: @RunOnVertxContext is crucial for reactive Panache operations
 * as they must run on the Vert.x event loop.
 */
@QuarkusTest
@DisplayName("ReviewRepository Integration Tests - Mutiny & Database")
public class ReviewRepositoryTest {

    @Inject
    ReviewRepository reviewRepository;

    // ==================== PATTERN 1: Basic CRUD with UniAsserter ====================

    @Test
    @DisplayName("Pattern 1: Test createReview - Basic create operation")
    @RunOnVertxContext
    void testCreateReview_Success(UniAsserter asserter) {
        // UniAsserter allows testing reactive operations that must run on Vert.x context

        // Create review using repository
        asserter.execute(() ->
            reviewRepository.createReview(
                1001L, // resourceId
                2001L, // userId
                5,     // rating
                "Absolutely amazing book! Changed my perspective on life.",
                "user2001"
            )
        );

        // Assert the review was created
        asserter.assertThat(
            () -> reviewRepository.findActiveById(1L),
            review -> {
                assertThat(review).isNotNull();
                assertThat(review.resourceId).isEqualTo(1001L);
                assertThat(review.userId).isEqualTo(2001L);
                assertThat(review.rating).isEqualTo(5);
                assertThat(review.status).isEqualTo(ReviewStatus.PENDING);
                assertThat(review.archived).isFalse();
            }
        );
    }

    // ==================== PATTERN 2: Testing Read Operations ====================

    @Test
    @DisplayName("Pattern 2: Test findActiveById - Read operation")
    @RunOnVertxContext
    void testFindActiveById_Success(UniAsserter asserter) {
        // First create a review
        asserter.execute(() ->
            reviewRepository.createReview(
                1002L, 2002L, 4,
                "Great content but could be better organized.",
                "user2002"
            )
        );

        // Then find it
        asserter.assertThat(
            () -> reviewRepository.findActiveById(1L),
            review -> {
                assertThat(review).isNotNull();
                assertThat(review.rating).isEqualTo(4);
            }
        );
    }

    @Test
    @DisplayName("Pattern 3: Test findActiveById - Returns null for non-existent")
    @RunOnVertxContext
    void testFindActiveById_NotFound(UniAsserter asserter) {
        asserter.assertThat(
            () -> reviewRepository.findActiveById(99999L),
            review -> assertThat(review).isNull()
        );
    }

    // ==================== PATTERN 4: Testing Update Operations ====================

    @Test
    @DisplayName("Pattern 4: Test updateReview - Update existing review")
    @RunOnVertxContext
    void testUpdateReview_Success(UniAsserter asserter) {
        // Create initial review
        asserter.execute(() ->
            reviewRepository.createReview(
                1003L, 2003L, 3,
                "Initial comment that will be updated later.",
                "user2003"
            )
        );

        // Update the review
        asserter.execute(() ->
            reviewRepository.updateReview(
                1L, 5,
                "Updated comment with much better rating!",
                "user2003"
            )
        );

        // Verify update
        asserter.assertThat(
            () -> reviewRepository.findActiveById(1L),
            review -> {
                assertThat(review.rating).isEqualTo(5);
                assertThat(review.comment).contains("Updated comment");
                assertThat(review.status).isEqualTo(ReviewStatus.PENDING);
            }
        );
    }

    // ==================== PATTERN 5: Testing Soft Delete (Archive) ====================

    @Test
    @DisplayName("Pattern 5: Test archiveReview - Soft delete")
    @RunOnVertxContext
    void testArchiveReview_Success(UniAsserter asserter) {
        // Create review
        asserter.execute(() ->
            reviewRepository.createReview(
                1004L, 2004L, 2,
                "This review will be archived for testing purposes.",
                "user2004"
            )
        );

        // Archive it
        asserter.execute(() ->
            reviewRepository.archiveReview(
                1L,
                "Test archival",
                "admin"
            )
        );

        // Verify it's not found by findActiveById
        asserter.assertThat(
            () -> reviewRepository.findActiveById(1L),
            review -> assertThat(review).isNull()
        );

        // But IS found by findByIdIncludingArchived
        asserter.assertThat(
            () -> reviewRepository.findByIdIncludingArchived(1L),
            review -> {
                assertThat(review).isNotNull();
                assertThat(review.archived).isTrue();
                assertThat(review.archiveReason).isEqualTo("Test archival");
            }
        );
    }

    // ==================== PATTERN 6: Testing Restore ====================

    @Test
    @DisplayName("Pattern 6: Test restoreReview - Unarchive")
    @RunOnVertxContext
    void testRestoreReview_Success(UniAsserter asserter) {
        // Create and archive
        asserter.execute(() ->
            reviewRepository.createReview(
                1005L, 2005L, 4,
                "Review to be archived and then restored.",
                "user2005"
            ).chain(review ->
                reviewRepository.archiveReview(review.id, "Test", "admin")
            )
        );

        // Restore it
        asserter.execute(() ->
            reviewRepository.restoreReview(1L, "admin")
        );

        // Verify it's active again
        asserter.assertThat(
            () -> reviewRepository.findActiveById(1L),
            review -> {
                assertThat(review).isNotNull();
                assertThat(review.archived).isFalse();
                assertThat(review.status).isEqualTo(ReviewStatus.PENDING);
            }
        );
    }

    // ==================== PATTERN 7: Testing Exists Methods ====================

    @Test
    @DisplayName("Pattern 7: Test existsByUserAndWork - Duplicate check")
    @RunOnVertxContext
    void testExistsByUserAndWork(UniAsserter asserter) {
        Long resourceId = 1006L;
        Long userId = 2006L;

        // Initially should not exist
        asserter.assertThat(
            () -> reviewRepository.existsByUserAndWork(userId, resourceId),
            exists -> assertThat(exists).isFalse()
        );

        // Create review
        asserter.execute(() ->
            reviewRepository.createReview(
                resourceId, userId, 5,
                "Testing duplicate review prevention mechanism.",
                "user2006"
            )
        );

        // Now should exist
        asserter.assertThat(
            () -> reviewRepository.existsByUserAndWork(userId, resourceId),
            exists -> assertThat(exists).isTrue()
        );
    }

    // ==================== PATTERN 8: Testing Moderation ====================

    @Test
    @DisplayName("Pattern 8: Test approveReview - Moderation workflow")
    @RunOnVertxContext
    void testApproveReview_Success(UniAsserter asserter) {
        // Create pending review
        asserter.execute(() ->
            reviewRepository.createReview(
                1007L, 2007L, 5,
                "Excellent work that deserves approval!",
                "user2007"
            )
        );

        // Approve it
        asserter.execute(() ->
            reviewRepository.approveReview(1L, "moderator1")
        );

        // Verify status
        asserter.assertThat(
            () -> reviewRepository.findActiveById(1L),
            review -> {
                assertThat(review.status).isEqualTo(ReviewStatus.APPROVED);
                assertThat(review.moderatedBy).isEqualTo("moderator1");
                assertThat(review.moderatedAt).isNotNull();
            }
        );
    }

    @Test
    @DisplayName("Pattern 9: Test rejectReview - Rejection workflow")
    @RunOnVertxContext
    void testRejectReview_Success(UniAsserter asserter) {
        // Create pending review
        asserter.execute(() ->
            reviewRepository.createReview(
                1008L, 2008L, 1,
                "This content is inappropriate for our platform.",
                "user2008"
            )
        );

        // Reject it
        asserter.execute(() ->
            reviewRepository.rejectReview(
                1L,
                "Violates community guidelines",
                "moderator1"
            )
        );

        // Verify status
        asserter.assertThat(
            () -> reviewRepository.findActiveById(1L),
            review -> {
                assertThat(review.status).isEqualTo(ReviewStatus.REJECTED);
                assertThat(review.moderationReason).contains("guidelines");
            }
        );
    }

    @Test
    @DisplayName("Pattern 10: Test flagReview - Flag for attention")
    @RunOnVertxContext
    void testFlagReview_Success(UniAsserter asserter) {
        // Create and approve review
        asserter.execute(() ->
            reviewRepository.createReview(
                1009L, 2009L, 3,
                "Suspicious content that needs review attention.",
                "user2009"
            ).chain(review ->
                reviewRepository.approveReview(review.id, "moderator1")
            )
        );

        // Flag it
        asserter.execute(() ->
            reviewRepository.flagReview(
                1L,
                "Reported by user",
                "user9999"
            )
        );

        // Verify status
        asserter.assertThat(
            () -> reviewRepository.findActiveById(1L),
            review -> {
                assertThat(review.status).isEqualTo(ReviewStatus.FLAGGED);
                assertThat(review.moderationReason).contains("Reported");
            }
        );
    }

    // ==================== PATTERN 11: Testing Aggregations ====================

    @Test
    @DisplayName("Pattern 11: Test getAverageRatingForWork - Aggregation")
    @RunOnVertxContext
    void testGetAverageRating(UniAsserter asserter) {
        Long resourceId = 1010L;

        // Create multiple reviews for same resource with different ratings
        asserter.execute(() ->
            reviewRepository.createReview(
                resourceId, 2010L, 5,
                "Perfect! Five stars all the way!",
                "user2010"
            ).chain(r1 ->
                reviewRepository.approveReview(r1.id, "mod")
            ).chain(() ->
                reviewRepository.createReview(
                    resourceId, 2011L, 3,
                    "Average content, nothing special here.",
                    "user2011"
                )
            ).chain(r2 ->
                reviewRepository.approveReview(r2.id, "mod")
            )
        );

        // Get average (should be 4.0)
        asserter.assertThat(
            () -> reviewRepository.getAverageRatingForWork(resourceId),
            average -> {
                assertThat(average).isNotNull();
                assertThat(average).isEqualTo(4.0);
            }
        );
    }

    @Test
    @DisplayName("Pattern 12: Test getRatingDistribution - Map aggregation")
    @RunOnVertxContext
    void testGetRatingDistribution(UniAsserter asserter) {
        Long resourceId = 1011L;

        // Create reviews with various ratings
        asserter.execute(() ->
            reviewRepository.createReview(
                resourceId, 2012L, 5,
                "Excellent! Highly recommended for everyone!",
                "user2012"
            ).chain(r -> reviewRepository.approveReview(r.id, "mod"))
            .chain(() ->
                reviewRepository.createReview(
                    resourceId, 2013L, 5,
                    "Best thing ever! Cannot recommend enough!",
                    "user2013"
                )
            ).chain(r -> reviewRepository.approveReview(r.id, "mod"))
            .chain(() ->
                reviewRepository.createReview(
                    resourceId, 2014L, 4,
                    "Very good but has minor issues to fix.",
                    "user2014"
                )
            ).chain(r -> reviewRepository.approveReview(r.id, "mod"))
        );

        // Get distribution
        asserter.assertThat(
            () -> reviewRepository.getRatingDistribution(resourceId),
            distribution -> {
                assertThat(distribution).isNotNull();
                assertThat(distribution.get(5)).isEqualTo(2L);
                assertThat(distribution.get(4)).isEqualTo(1L);
                assertThat(distribution.get(3)).isEqualTo(0L);
                assertThat(distribution.get(2)).isEqualTo(0L);
                assertThat(distribution.get(1)).isEqualTo(0L);
            }
        );
    }

    // ==================== PATTERN 13: Testing Bulk Operations ====================

    @Test
    @DisplayName("Pattern 13: Test approveMultiple - Bulk moderation")
    @RunOnVertxContext
    void testBulkApprove(UniAsserter asserter) {
        // Create multiple pending reviews
        asserter.execute(() ->
            reviewRepository.createReview(
                1012L, 2015L, 5,
                "First review for bulk approval test.",
                "user2015"
            ).chain(() ->
                reviewRepository.createReview(
                    1013L, 2016L, 4,
                    "Second review for bulk approval test.",
                    "user2016"
                )
            ).chain(() ->
                reviewRepository.createReview(
                    1014L, 2017L, 3,
                    "Third review for bulk approval test.",
                    "user2017"
                )
            )
        );

        // Bulk approve
        asserter.execute(() ->
            reviewRepository.approveMultiple(
                Arrays.asList(1L, 2L, 3L),
                "moderator1"
            )
        );

        // Verify all are approved
        asserter.assertThat(
            () -> reviewRepository.findActiveById(1L),
            review -> assertThat(review.status).isEqualTo(ReviewStatus.APPROVED)
        );

        asserter.assertThat(
            () -> reviewRepository.findActiveById(2L),
            review -> assertThat(review.status).isEqualTo(ReviewStatus.APPROVED)
        );

        asserter.assertThat(
            () -> reviewRepository.findActiveById(3L),
            review -> assertThat(review.status).isEqualTo(ReviewStatus.APPROVED)
        );
    }

    // ==================== PATTERN 14: Testing Native Queries ====================

    @Test
    @DisplayName("Pattern 14: Test native query - Get average rating per work")
    @RunOnVertxContext
    void testNativeQuery_AverageRatingPerWork(UniAsserter asserter) {
        Long resourceId1 = 1015L;
        Long resourceId2 = 1016L;

        // Create reviews for two different resources
        asserter.execute(() ->
            reviewRepository.createReview(
                resourceId1, 2018L, 5,
                "Great content for first resource!",
                "user2018"
            ).chain(r -> reviewRepository.approveReview(r.id, "mod"))
            .chain(() ->
                reviewRepository.createReview(
                    resourceId2, 2019L, 3,
                    "Mediocre content for second resource.",
                    "user2019"
                )
            ).chain(r -> reviewRepository.approveReview(r.id, "mod"))
        );

        // Execute native query
        asserter.assertThat(
            () -> reviewRepository.getAverageRatingPerWorkNative(),
            results -> {
                assertThat(results).isNotEmpty();
                // Results are Object[] with: [resourceId, avgRating, reviewCount, lastReviewDate]
                assertThat(results.size()).isGreaterThanOrEqualTo(2);
            }
        );
    }

    // ==================== PATTERN 15: Testing Pagination ====================

    @Test
    @DisplayName("Pattern 15: Test pagination - findAllPaginated")
    @RunOnVertxContext
    void testPagination(UniAsserter asserter) {
        // Create multiple reviews
        asserter.execute(() ->
            reviewRepository.createReview(
                1017L, 2020L, 5,
                "First review for pagination test.",
                "user2020"
            ).chain(() ->
                reviewRepository.createReview(
                    1018L, 2021L, 4,
                    "Second review for pagination test.",
                    "user2021"
                )
            ).chain(() ->
                reviewRepository.createReview(
                    1019L, 2022L, 3,
                    "Third review for pagination test.",
                    "user2022"
                )
            ).chain(() ->
                reviewRepository.createReview(
                    1020L, 2023L, 2,
                    "Fourth review for pagination test.",
                    "user2023"
                )
            ).chain(() ->
                reviewRepository.createReview(
                    1021L, 2024L, 1,
                    "Fifth review for pagination test.",
                    "user2024"
                )
            )
        );

        // Get first page (2 items)
        asserter.assertThat(
            () -> reviewRepository.findAllPaginated(0, 2),
            page1 -> {
                assertThat(page1).hasSize(2);
            }
        );

        // Get second page (2 items)
        asserter.assertThat(
            () -> reviewRepository.findAllPaginated(1, 2),
            page2 -> {
                assertThat(page2).hasSize(2);
            }
        );
    }

    @Test
    @DisplayName("Pattern 16: Test pagination with metadata - PaginatedResult")
    @RunOnVertxContext
    void testPaginationWithMetadata(UniAsserter asserter) {
        Long resourceId = 1022L;

        // Create reviews
        asserter.execute(() ->
            reviewRepository.createReview(
                resourceId, 2025L, 5,
                "Review one for paginated result test.",
                "user2025"
            ).chain(() ->
                reviewRepository.createReview(
                    resourceId, 2026L, 4,
                    "Review two for paginated result test.",
                    "user2026"
                )
            ).chain(() ->
                reviewRepository.createReview(
                    resourceId, 2027L, 3,
                    "Review three for paginated result test.",
                    "user2027"
                )
            )
        );

        // Get paginated result with metadata
        asserter.assertThat(
            () -> reviewRepository.findReviewsPaginated(
                resourceId,
                null,
                0, // page 0
                2  // 2 items per page
            ),
            result -> {
                assertThat(result.data).hasSize(2);
                assertThat(result.currentPage).isEqualTo(0);
                assertThat(result.pageSize).isEqualTo(2);
                assertThat(result.totalItems).isEqualTo(3L);
                assertThat(result.totalPages).isEqualTo(2);
                assertThat(result.hasNextPage()).isTrue();
                assertThat(result.hasPreviousPage()).isFalse();
            }
        );
    }

    // ==================== PATTERN 17: Testing Dynamic Filters ====================

    @Test
    @DisplayName("Pattern 17: Test dynamic filters - findWithDynamicFilters")
    @RunOnVertxContext
    void testDynamicFilters(UniAsserter asserter) {
        Long resourceId = 1023L;

        // Create reviews with different ratings
        asserter.execute(() ->
            reviewRepository.createReview(
                resourceId, 2028L, 5,
                "Five star review for dynamic filter test.",
                "user2028"
            ).chain(() ->
                reviewRepository.createReview(
                    resourceId, 2029L, 3,
                    "Three star review for dynamic filter test.",
                    "user2029"
                )
            ).chain(() ->
                reviewRepository.createReview(
                    resourceId, 2030L, 1,
                    "One star review for dynamic filter test.",
                    "user2030"
                )
            )
        );

        // Filter by resource and minimum rating
        asserter.assertThat(
            () -> reviewRepository.findWithDynamicFilters(
                resourceId, // resourceId
                null,       // userId
                4,          // minRating
                null,       // status
                null        // publishedAfter
            ),
            results -> {
                assertThat(results).hasSize(1); // Only the 5-star review
                assertThat(results.get(0).rating).isGreaterThanOrEqualTo(4);
            }
        );
    }

    // ==================== PATTERN 18: Testing Business Methods ====================

    @Test
    @DisplayName("Pattern 18: Test findPendingModeration - Business query")
    @RunOnVertxContext
    void testFindPendingModeration(UniAsserter asserter) {
        // Create mix of pending and approved reviews
        asserter.execute(() ->
            reviewRepository.createReview(
                1024L, 2031L, 5,
                "Pending review awaiting moderation one.",
                "user2031"
            ).chain(() ->
                reviewRepository.createReview(
                    1025L, 2032L, 4,
                    "Pending review awaiting moderation two.",
                    "user2032"
                )
            ).chain(() ->
                reviewRepository.createReview(
                    1026L, 2033L, 3,
                    "This will be approved immediately.",
                    "user2033"
                )
            ).chain(r -> reviewRepository.approveReview(r.id, "mod"))
        );

        // Get pending reviews
        asserter.assertThat(
            () -> reviewRepository.findPendingModeration(),
            pending -> {
                assertThat(pending).hasSizeGreaterThanOrEqualTo(2);
                assertThat(pending).allMatch(r -> r.status == ReviewStatus.PENDING);
            }
        );
    }

    // ==================== PATTERN 19: Testing Count Operations ====================

    @Test
    @DisplayName("Pattern 19: Test getCountByStatus - Status distribution")
    @RunOnVertxContext
    void testGetCountByStatus(UniAsserter asserter) {
        // Create reviews with different statuses
        asserter.execute(() ->
            reviewRepository.createReview(
                1027L, 2034L, 5,
                "Review that stays pending for count test.",
                "user2034"
            ).chain(() ->
                reviewRepository.createReview(
                    1028L, 2035L, 4,
                    "Review to be approved for count test.",
                    "user2035"
                )
            ).chain(r -> reviewRepository.approveReview(r.id, "mod"))
            .chain(() ->
                reviewRepository.createReview(
                    1029L, 2036L, 1,
                    "Review to be rejected for count test.",
                    "user2036"
                )
            ).chain(r -> reviewRepository.rejectReview(r.id, "Invalid", "mod"))
        );

        // Get counts by status
        asserter.assertThat(
            () -> reviewRepository.getCountByStatus(),
            counts -> {
                assertThat(counts).isNotEmpty();
                assertThat(counts.get(ReviewStatus.PENDING)).isGreaterThanOrEqualTo(1);
                assertThat(counts.get(ReviewStatus.APPROVED)).isGreaterThanOrEqualTo(1);
                assertThat(counts.get(ReviewStatus.REJECTED)).isGreaterThanOrEqualTo(1);
            }
        );
    }

    // ==================== PATTERN 20: Testing Complex Native Queries ====================

    @Test
    @DisplayName("Pattern 20: Test complex native query - Get most active reviewers")
    @RunOnVertxContext
    void testMostActiveReviewers(UniAsserter asserter) {
        Long userId = 2037L;

        // Create multiple reviews from same user
        asserter.execute(() ->
            reviewRepository.createReview(
                1030L, userId, 5,
                "First review from active user test.",
                "user2037"
            ).chain(() ->
                reviewRepository.createReview(
                    1031L, userId, 4,
                    "Second review from active user test.",
                    "user2037"
                )
            ).chain(() ->
                reviewRepository.createReview(
                    1032L, userId, 5,
                    "Third review from active user test.",
                    "user2037"
                )
            )
        );

        // Get most active reviewers
        asserter.assertThat(
            () -> reviewRepository.getMostActiveReviewersNative(10),
            results -> {
                assertThat(results).isNotEmpty();
                // Results are Object[] with: [userId, reviewCount, avgRating, lastReviewDate]
                // Find our user
                boolean foundUser = results.stream()
                    .anyMatch(row -> ((Long) row[0]).equals(userId));
                assertThat(foundUser).isTrue();
            }
        );
    }
}
