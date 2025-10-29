package com.akfc.services;

import com.akfc.data.Review;
import com.akfc.data.ReviewRepository;
import com.akfc.data.ReviewStatus;
import com.akfc.dto.CreateReviewRequest;
import com.akfc.dto.ReviewResponse;
import com.akfc.dto.UpdateReviewRequest;
import com.akfc.errors.*;
import io.quarkus.test.InjectMock;
import io.quarkus.test.junit.QuarkusTest;
import io.smallrye.mutiny.Uni;
import io.smallrye.mutiny.helpers.test.UniAssertSubscriber;
import jakarta.inject.Inject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

/**
 * Unit tests for ReviewService demonstrating Mutiny testing patterns.
 *
 * This test class illustrates:
 * - Testing reactive services with mocked dependencies
 * - Using UniAssertSubscriber for testing Uni<T> return types
 * - Testing success and failure scenarios
 * - Testing validation logic
 * - Testing business rules
 * - Testing reactive chains
 * - Testing error recovery
 */
@QuarkusTest
@DisplayName("ReviewService Unit Tests - Mutiny Patterns")
public class ReviewServiceTest {

    @Inject
    ReviewService reviewService;

    @InjectMock
    ReviewRepository reviewRepository;

    private Review sampleReview;

    @BeforeEach
    void setup() {
        // Create a sample review for testing
        sampleReview = new Review();
        sampleReview.id = 1L;
        sampleReview.resourceId = 100L;
        sampleReview.userId = 50L;
        sampleReview.rating = 5;
        sampleReview.comment = "Excellent book! Highly recommended for everyone.";
        sampleReview.status = ReviewStatus.PENDING;
        sampleReview.publicationDate = LocalDate.now();
        sampleReview.archived = false;
        sampleReview.createdBy = "user50";
        sampleReview.createdAt = LocalDateTime.now();

        // Reset mocks before each test
        Mockito.reset(reviewRepository);
    }

    // ==================== PATTERN 1: Testing Uni Success with Mock ====================

    @Test
    @DisplayName("Pattern 1: Test createReview success - Uni with mocked repository")
    void testCreateReview_Success() {
        // Arrange
        CreateReviewRequest request = new CreateReviewRequest();
        request.resourceId = 100L;
        request.userId = 50L;
        request.rating = 5;
        request.comment = "Excellent book! Highly recommended for everyone.";

        // Mock the repository responses
        when(reviewRepository.existsByUserAndWork(50L, 100L))
            .thenReturn(Uni.createFrom().item(false));
        when(reviewRepository.createReview(anyLong(), anyLong(), anyInt(), anyString(), anyString()))
            .thenReturn(Uni.createFrom().item(sampleReview));

        // Act
        Uni<ReviewResponse> result = reviewService.createReview(request, "user50");

        // Assert using UniAssertSubscriber
        ReviewResponse response = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .assertSubscribed()
            .awaitItem(Duration.ofSeconds(2))
            .assertCompleted()
            .getItem();

        assertThat(response).isNotNull();
        assertThat(response.id).isEqualTo(1L);
        assertThat(response.rating).isEqualTo(5);
        assertThat(response.status).isEqualTo(ReviewStatus.PENDING);
    }

    // ==================== PATTERN 2: Testing Uni Failure Scenarios ====================

    @Test
    @DisplayName("Pattern 2: Test createReview duplicate - Testing expected failure")
    void testCreateReview_DuplicateReview_Failure() {
        // Arrange
        CreateReviewRequest request = new CreateReviewRequest();
        request.resourceId = 100L;
        request.userId = 50L;
        request.rating = 5;
        request.comment = "Excellent book! Highly recommended.";

        // Mock repository to return that review already exists
        when(reviewRepository.existsByUserAndWork(50L, 100L))
            .thenReturn(Uni.createFrom().item(true));

        // Act
        Uni<ReviewResponse> result = reviewService.createReview(request, "user50");

        // Assert using UniAssertSubscriber to test failure
        Throwable failure = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitFailure(Duration.ofSeconds(2))
            .getFailure();

        assertThat(failure)
            .isInstanceOf(DuplicateReviewException.class)
            .hasMessageContaining("already reviewed");
    }

    // ==================== PATTERN 3: Testing Validation Logic ====================

    @Test
    @DisplayName("Pattern 3: Test validation - Rating too low")
    void testCreateReview_InvalidRating_TooLow() {
        // Arrange
        CreateReviewRequest request = new CreateReviewRequest();
        request.resourceId = 100L;
        request.userId = 50L;
        request.rating = 0; // Invalid: too low
        request.comment = "This should fail validation.";

        // Act
        Uni<ReviewResponse> result = reviewService.createReview(request, "user50");

        // Assert
        Throwable failure = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitFailure()
            .getFailure();

        assertThat(failure)
            .isInstanceOf(ReviewValidationException.class)
            .hasMessageContaining("Rating must be between 1 and 5");
    }

    @Test
    @DisplayName("Pattern 4: Test validation - Comment too short")
    void testCreateReview_InvalidComment_TooShort() {
        // Arrange
        CreateReviewRequest request = new CreateReviewRequest();
        request.resourceId = 100L;
        request.userId = 50L;
        request.rating = 5;
        request.comment = "Too short"; // Invalid: less than 10 chars

        // Act
        Uni<ReviewResponse> result = reviewService.createReview(request, "user50");

        // Assert
        Throwable failure = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitFailure()
            .getFailure();

        assertThat(failure)
            .isInstanceOf(ReviewValidationException.class)
            .hasMessageContaining("at least 10 characters");
    }

    // ==================== PATTERN 5: Testing Reactive Chains ====================

    @Test
    @DisplayName("Pattern 5: Test getReviewById - Testing reactive chain")
    void testGetReviewById_Success() {
        // Arrange
        when(reviewRepository.findActiveById(1L))
            .thenReturn(Uni.createFrom().item(sampleReview));

        // Act
        Uni<ReviewResponse> result = reviewService.getReviewById(1L);

        // Assert
        ReviewResponse response = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(response).isNotNull();
        assertThat(response.id).isEqualTo(1L);
        assertThat(response.rating).isEqualTo(5);
    }

    @Test
    @DisplayName("Pattern 6: Test getReviewById not found - Reactive chain with null handling")
    void testGetReviewById_NotFound() {
        // Arrange
        when(reviewRepository.findActiveById(999L))
            .thenReturn(Uni.createFrom().nullItem());

        // Act
        Uni<ReviewResponse> result = reviewService.getReviewById(999L);

        // Assert
        Throwable failure = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitFailure()
            .getFailure();

        assertThat(failure)
            .isInstanceOf(ReviewNotFoundException.class)
            .hasMessageContaining("999");
    }

    // ==================== PATTERN 7: Testing List Operations ====================

    @Test
    @DisplayName("Pattern 7: Test getReviewsByUser - Testing Uni<List<T>>")
    void testGetReviewsByUser() {
        // Arrange
        Review review2 = new Review();
        review2.id = 2L;
        review2.resourceId = 101L;
        review2.userId = 50L;
        review2.rating = 4;
        review2.comment = "Good but not great, needs improvement in some areas.";
        review2.status = ReviewStatus.APPROVED;
        review2.publicationDate = LocalDate.now();

        when(Review.findByUserId(50L))
            .thenReturn(Uni.createFrom().item(Arrays.asList(sampleReview, review2)));

        // Act
        Uni<List<ReviewResponse>> result = reviewService.getReviewsByUser(50L);

        // Assert
        List<ReviewResponse> responses = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(responses).hasSize(2);
        assertThat(responses).extracting("userId")
            .containsOnly(50L);
    }

    // ==================== PATTERN 8: Testing Business Rules ====================

    @Test
    @DisplayName("Pattern 8: Test updateReview - Cannot update rejected review")
    void testUpdateReview_RejectedReview_BusinessRule() {
        // Arrange
        sampleReview.status = ReviewStatus.REJECTED;
        UpdateReviewRequest request = new UpdateReviewRequest();
        request.rating = 4;
        request.comment = "Updated comment that should fail.";

        when(reviewRepository.findActiveById(1L))
            .thenReturn(Uni.createFrom().item(sampleReview));

        // Act
        Uni<ReviewResponse> result = reviewService.updateReview(1L, request, "user50");

        // Assert
        Throwable failure = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitFailure()
            .getFailure();

        assertThat(failure)
            .isInstanceOf(ReviewBusinessException.class)
            .hasMessageContaining("Cannot update rejected review");
    }

    @Test
    @DisplayName("Pattern 9: Test updateReview success - Review goes back to PENDING")
    void testUpdateReview_Success_StatusResetToPending() {
        // Arrange
        sampleReview.status = ReviewStatus.APPROVED; // Was approved
        UpdateReviewRequest request = new UpdateReviewRequest();
        request.rating = 4;
        request.comment = "Updated comment with new information added.";

        when(reviewRepository.findActiveById(1L))
            .thenReturn(Uni.createFrom().item(sampleReview));

        // Mock the update method on the review entity
        Review updatedReview = new Review();
        updatedReview.id = sampleReview.id;
        updatedReview.resourceId = sampleReview.resourceId;
        updatedReview.userId = sampleReview.userId;
        updatedReview.rating = request.rating;
        updatedReview.comment = request.comment;
        updatedReview.status = ReviewStatus.PENDING; // Goes back to pending
        updatedReview.publicationDate = LocalDate.now();

        when(sampleReview.update(anyInt(), anyString(), anyString()))
            .thenReturn(Uni.createFrom().item(updatedReview));

        // Act
        Uni<ReviewResponse> result = reviewService.updateReview(1L, request, "user50");

        // Assert
        ReviewResponse response = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(response.status).isEqualTo(ReviewStatus.PENDING);
        assertThat(response.rating).isEqualTo(4);
    }

    // ==================== PATTERN 10: Testing Moderation Operations ====================

    @Test
    @DisplayName("Pattern 10: Test approveReview success")
    void testApproveReview_Success() {
        // Arrange
        when(reviewRepository.findActiveById(1L))
            .thenReturn(Uni.createFrom().item(sampleReview));

        Review approvedReview = new Review();
        approvedReview.id = sampleReview.id;
        approvedReview.resourceId = sampleReview.resourceId;
        approvedReview.userId = sampleReview.userId;
        approvedReview.rating = sampleReview.rating;
        approvedReview.comment = sampleReview.comment;
        approvedReview.status = ReviewStatus.APPROVED;
        approvedReview.publicationDate = sampleReview.publicationDate;

        when(sampleReview.approve(anyString()))
            .thenReturn(Uni.createFrom().item(approvedReview));

        // Act
        Uni<ReviewResponse> result = reviewService.approveReview(1L, "moderator1");

        // Assert
        ReviewResponse response = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(response.status).isEqualTo(ReviewStatus.APPROVED);
    }

    @Test
    @DisplayName("Pattern 11: Test approveReview - Already approved")
    void testApproveReview_AlreadyApproved_BusinessRule() {
        // Arrange
        sampleReview.status = ReviewStatus.APPROVED;
        when(reviewRepository.findActiveById(1L))
            .thenReturn(Uni.createFrom().item(sampleReview));

        when(sampleReview.approve(anyString()))
            .thenReturn(Uni.createFrom().failure(
                new ReviewBusinessException("Review is already approved", "ALREADY_APPROVED")
            ));

        // Act
        Uni<ReviewResponse> result = reviewService.approveReview(1L, "moderator1");

        // Assert
        Throwable failure = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitFailure()
            .getFailure();

        assertThat(failure)
            .isInstanceOf(ReviewBusinessException.class)
            .hasMessageContaining("already approved");
    }

    // ==================== PATTERN 12: Testing Reject with Reason ====================

    @Test
    @DisplayName("Pattern 12: Test rejectReview success")
    void testRejectReview_Success() {
        // Arrange
        when(reviewRepository.findActiveById(1L))
            .thenReturn(Uni.createFrom().item(sampleReview));

        Review rejectedReview = new Review();
        rejectedReview.id = sampleReview.id;
        rejectedReview.status = ReviewStatus.REJECTED;

        when(sampleReview.reject(anyString(), anyString()))
            .thenReturn(Uni.createFrom().item(rejectedReview));

        // Act
        Uni<ReviewResponse> result = reviewService.rejectReview(1L, "Inappropriate content", "moderator1");

        // Assert
        ReviewResponse response = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(response.status).isEqualTo(ReviewStatus.REJECTED);
    }

    @Test
    @DisplayName("Pattern 13: Test rejectReview - Missing reason validation")
    void testRejectReview_MissingReason() {
        // Act
        Uni<ReviewResponse> result = reviewService.rejectReview(1L, "", "moderator1");

        // Assert
        Throwable failure = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitFailure()
            .getFailure();

        assertThat(failure)
            .isInstanceOf(ReviewValidationException.class)
            .hasMessageContaining("reason");
    }

    // ==================== PATTERN 14: Testing Flag Operations ====================

    @Test
    @DisplayName("Pattern 14: Test flagReview success")
    void testFlagReview_Success() {
        // Arrange
        sampleReview.status = ReviewStatus.APPROVED;
        when(reviewRepository.findActiveById(1L))
            .thenReturn(Uni.createFrom().item(sampleReview));

        Review flaggedReview = new Review();
        flaggedReview.id = sampleReview.id;
        flaggedReview.status = ReviewStatus.FLAGGED;

        when(sampleReview.flag(anyString(), anyString()))
            .thenReturn(Uni.createFrom().item(flaggedReview));

        // Act
        Uni<ReviewResponse> result = reviewService.flagReview(1L, "Suspicious content", "user99");

        // Assert
        ReviewResponse response = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(response.status).isEqualTo(ReviewStatus.FLAGGED);
    }

    @Test
    @DisplayName("Pattern 15: Test flagReview - Cannot flag rejected review")
    void testFlagReview_RejectedReview_BusinessRule() {
        // Arrange
        sampleReview.status = ReviewStatus.REJECTED;
        when(reviewRepository.findActiveById(1L))
            .thenReturn(Uni.createFrom().item(sampleReview));

        when(sampleReview.flag(anyString(), anyString()))
            .thenReturn(Uni.createFrom().failure(
                new ReviewBusinessException("Cannot flag rejected review", "REVIEW_REJECTED")
            ));

        // Act
        Uni<ReviewResponse> result = reviewService.flagReview(1L, "Test reason", "user99");

        // Assert
        Throwable failure = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitFailure()
            .getFailure();

        assertThat(failure)
            .isInstanceOf(ReviewBusinessException.class)
            .hasMessageContaining("Cannot flag rejected");
    }

    // ==================== PATTERN 16: Testing Archive Operations ====================

    @Test
    @DisplayName("Pattern 16: Test archiveReview success - Soft delete")
    void testArchiveReview_Success() {
        // Arrange
        when(reviewRepository.findActiveById(1L))
            .thenReturn(Uni.createFrom().item(sampleReview));

        Review archivedReview = new Review();
        archivedReview.id = sampleReview.id;
        archivedReview.archived = true;
        archivedReview.archiveReason = "Outdated review";

        when(sampleReview.archive(anyString(), anyString()))
            .thenReturn(Uni.createFrom().item(archivedReview));

        // Act
        Uni<ReviewResponse> result = reviewService.archiveReview(1L, "Outdated review", "admin1");

        // Assert
        ReviewResponse response = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(response.archived).isTrue();
    }

    // ==================== PATTERN 17: Testing Aggregation Methods ====================

    @Test
    @DisplayName("Pattern 17: Test getAverageRating - Aggregation query")
    void testGetAverageRating() {
        // Arrange
        when(reviewRepository.getAverageRatingForWork(100L))
            .thenReturn(Uni.createFrom().item(4.5));

        // Act
        Uni<Double> result = reviewService.getAverageRating(100L);

        // Assert
        Double average = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(average).isEqualTo(4.5);
    }

    @Test
    @DisplayName("Pattern 18: Test getRatingDistribution - Map result")
    void testGetRatingDistribution() {
        // Arrange
        Map<Integer, Long> distribution = Map.of(
            1, 2L,
            2, 5L,
            3, 10L,
            4, 15L,
            5, 20L
        );

        when(reviewRepository.getRatingDistribution(100L))
            .thenReturn(Uni.createFrom().item(distribution));

        // Act
        Uni<Map<Integer, Long>> result = reviewService.getRatingDistribution(100L);

        // Assert
        Map<Integer, Long> resultMap = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(resultMap).hasSize(5);
        assertThat(resultMap.get(5)).isEqualTo(20L);
    }

    // ==================== PATTERN 19: Testing Bulk Operations ====================

    @Test
    @DisplayName("Pattern 19: Test bulkApproveReviews - Bulk operation")
    void testBulkApproveReviews() {
        // Arrange
        List<Long> reviewIds = Arrays.asList(1L, 2L, 3L);
        when(reviewRepository.approveMultiple(reviewIds, "moderator1"))
            .thenReturn(Uni.createFrom().item(3));

        // Act
        Uni<Integer> result = reviewService.bulkApproveReviews(reviewIds, "moderator1");

        // Assert
        Integer count = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(count).isEqualTo(3);
    }

    @Test
    @DisplayName("Pattern 20: Test bulkApproveReviews - Empty list validation")
    void testBulkApproveReviews_EmptyList() {
        // Act
        Uni<Integer> result = reviewService.bulkApproveReviews(Arrays.asList(), "moderator1");

        // Assert
        Throwable failure = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitFailure()
            .getFailure();

        assertThat(failure)
            .isInstanceOf(ReviewValidationException.class)
            .hasMessageContaining("cannot be empty");
    }

    // ==================== PATTERN 21: Testing Delete Operations ====================

    @Test
    @DisplayName("Pattern 21: Test deleteReview - Must be archived first")
    void testDeleteReview_NotArchived_BusinessRule() {
        // Arrange
        sampleReview.archived = false; // Not archived
        when(reviewRepository.findByIdIncludingArchived(1L))
            .thenReturn(Uni.createFrom().item(sampleReview));

        when(sampleReview.permanentlyDelete())
            .thenReturn(Uni.createFrom().failure(
                new ReviewBusinessException(
                    "Cannot permanently delete non-archived review. Archive it first.",
                    "REVIEW_NOT_ARCHIVED"
                )
            ));

        // Act
        Uni<Void> result = reviewService.deleteReview(1L);

        // Assert
        Throwable failure = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitFailure()
            .getFailure();

        assertThat(failure)
            .isInstanceOf(ReviewBusinessException.class)
            .hasMessageContaining("Archive it first");
    }

    // ==================== PATTERN 22: Testing Restore Operations ====================

    @Test
    @DisplayName("Pattern 22: Test restoreReview success")
    void testRestoreReview_Success() {
        // Arrange
        sampleReview.archived = true;
        when(reviewRepository.findByIdIncludingArchived(1L))
            .thenReturn(Uni.createFrom().item(sampleReview));

        Review restoredReview = new Review();
        restoredReview.id = sampleReview.id;
        restoredReview.archived = false;
        restoredReview.status = ReviewStatus.PENDING;

        when(sampleReview.restore(anyString()))
            .thenReturn(Uni.createFrom().item(restoredReview));

        // Act
        Uni<ReviewResponse> result = reviewService.restoreReview(1L, "admin1");

        // Assert
        ReviewResponse response = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(response.archived).isFalse();
        assertThat(response.status).isEqualTo(ReviewStatus.PENDING);
    }

    @Test
    @DisplayName("Pattern 23: Test hasUserReviewedWork - Boolean result")
    void testHasUserReviewedWork() {
        // Arrange
        when(reviewRepository.existsByUserAndWork(50L, 100L))
            .thenReturn(Uni.createFrom().item(true));

        // Act
        Uni<Boolean> result = reviewService.hasUserReviewedWork(50L, 100L);

        // Assert
        Boolean hasReviewed = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(hasReviewed).isTrue();
    }

    // ==================== PATTERN 24: Testing Timeout Behavior ====================

    @Test
    @DisplayName("Pattern 24: Test slow operation with timeout")
    void testSlowOperation_WithTimeout() {
        // Arrange
        when(reviewRepository.findActiveById(1L))
            .thenReturn(
                Uni.createFrom().item(sampleReview)
                    .onItem().delayIt().by(Duration.ofSeconds(5))
            );

        // Act - Add timeout
        Uni<ReviewResponse> result = reviewService.getReviewById(1L)
            .ifNoItem().after(Duration.ofMillis(100))
            .fail();

        // Assert
        Throwable failure = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitFailure(Duration.ofSeconds(1))
            .getFailure();

        assertThat(failure).isNotNull();
    }

    // ==================== PATTERN 25: Testing getPendingReviews ====================

    @Test
    @DisplayName("Pattern 25: Test getPendingReviews - List of pending reviews")
    void testGetPendingReviews() {
        // Arrange
        Review pendingReview1 = new Review();
        pendingReview1.id = 1L;
        pendingReview1.status = ReviewStatus.PENDING;
        pendingReview1.rating = 5;
        pendingReview1.comment = "Pending review number one for moderation.";
        pendingReview1.publicationDate = LocalDate.now();

        Review pendingReview2 = new Review();
        pendingReview2.id = 2L;
        pendingReview2.status = ReviewStatus.PENDING;
        pendingReview2.rating = 4;
        pendingReview2.comment = "Pending review number two awaiting approval.";
        pendingReview2.publicationDate = LocalDate.now();

        when(reviewRepository.findPendingModeration())
            .thenReturn(Uni.createFrom().item(Arrays.asList(pendingReview1, pendingReview2)));

        // Act
        Uni<List<ReviewResponse>> result = reviewService.getPendingReviews();

        // Assert
        List<ReviewResponse> responses = result
            .subscribe()
            .withSubscriber(UniAssertSubscriber.create())
            .awaitItem()
            .getItem();

        assertThat(responses).hasSize(2);
        assertThat(responses).extracting("status")
            .containsOnly(ReviewStatus.PENDING);
    }
}
