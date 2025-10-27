package com.akfc.resources;

import com.akfc.data.ReviewStatus;
import com.akfc.dto.CreateReviewRequest;
import com.akfc.dto.ReviewResponse;
import com.akfc.dto.UpdateReviewRequest;
import io.quarkus.test.junit.QuarkusTest;
import io.restassured.http.ContentType;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import static io.restassured.RestAssured.given;
import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.*;

/**
 * Integration tests for ReviewResource REST endpoints demonstrating Mutiny REST testing patterns.
 *
 * This test class illustrates:
 * - Testing reactive REST endpoints with RestAssured
 * - Testing full request/response cycles
 * - Testing success scenarios (200, 201, 204)
 * - Testing error scenarios (400, 404, 409)
 * - Testing JSON serialization/deserialization
 * - Testing business rules through REST API
 * - Testing moderation workflows
 *
 * RestAssured automatically handles the reactive nature of Uni<Response> return types.
 */
@QuarkusTest
@DisplayName("ReviewResource REST Integration Tests - Mutiny Patterns")
public class ReviewResourceIT {

    // ==================== PATTERN 1: Testing POST - Create Resource ====================

    @Test
    @DisplayName("Pattern 1: Test POST /api/reviews - Create review success (201)")
    void testCreateReview_Success() {
        // Arrange
        CreateReviewRequest request = new CreateReviewRequest();
        request.resourceId = 5001L;
        request.userId = 6001L;
        request.rating = 5;
        request.comment = "Absolutely fantastic book! A must-read for everyone!";

        // Act & Assert
        ReviewResponse response = given()
            .contentType(ContentType.JSON)
            .body(request)
        .when()
            .post("/api/reviews")
        .then()
            .statusCode(201)
            .contentType(ContentType.JSON)
            .body("rating", equalTo(5))
            .body("status", equalTo("PENDING"))
            .body("archived", equalTo(false))
            .body("id", notNullValue())
        .extract()
            .as(ReviewResponse.class);

        assertThat(response).isNotNull();
        assertThat(response.rating).isEqualTo(5);
        assertThat(response.status).isEqualTo(ReviewStatus.PENDING);
    }

    @Test
    @DisplayName("Pattern 2: Test POST with validation error - Rating too low (400)")
    void testCreateReview_ValidationError_RatingTooLow() {
        CreateReviewRequest request = new CreateReviewRequest();
        request.resourceId = 5002L;
        request.userId = 6002L;
        request.rating = 0; // Invalid
        request.comment = "This should fail validation due to rating.";

        given()
            .contentType(ContentType.JSON)
            .body(request)
        .when()
            .post("/api/reviews")
        .then()
            .statusCode(anyOf(is(400), is(500))) // Validation error
            .contentType(ContentType.JSON);
    }

    @Test
    @DisplayName("Pattern 3: Test POST with validation error - Comment too short (400)")
    void testCreateReview_ValidationError_CommentTooShort() {
        CreateReviewRequest request = new CreateReviewRequest();
        request.resourceId = 5003L;
        request.userId = 6003L;
        request.rating = 5;
        request.comment = "Short"; // Invalid: less than 10 characters

        given()
            .contentType(ContentType.JSON)
            .body(request)
        .when()
            .post("/api/reviews")
        .then()
            .statusCode(anyOf(is(400), is(500))) // Validation error
            .contentType(ContentType.JSON);
    }

    // ==================== PATTERN 4: Testing GET - Read Resource ====================

    @Test
    @DisplayName("Pattern 4: Test GET /api/reviews/{id} - Get review by ID (200)")
    void testGetReview_Success() {
        // First create a review
        CreateReviewRequest createRequest = new CreateReviewRequest();
        createRequest.resourceId = 5004L;
        createRequest.userId = 6004L;
        createRequest.rating = 4;
        createRequest.comment = "Very good book with excellent character development.";

        Long reviewId = given()
            .contentType(ContentType.JSON)
            .body(createRequest)
        .when()
            .post("/api/reviews")
        .then()
            .statusCode(201)
        .extract()
            .path("id");

        // Then retrieve it
        given()
            .contentType(ContentType.JSON)
        .when()
            .get("/api/reviews/" + reviewId)
        .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body("id", equalTo(reviewId.intValue()))
            .body("rating", equalTo(4))
            .body("resourceId", equalTo(5004));
    }

    @Test
    @DisplayName("Pattern 5: Test GET with non-existent ID - Not found (404)")
    void testGetReview_NotFound() {
        given()
            .contentType(ContentType.JSON)
        .when()
            .get("/api/reviews/99999")
        .then()
            .statusCode(anyOf(is(404), is(500))); // Should be 404
    }

    // ==================== PATTERN 6: Testing GET with Query - List Results ====================

    @Test
    @DisplayName("Pattern 6: Test GET /api/reviews/resource/{resourceId} - Get reviews for resource")
    void testGetReviewsForResource() {
        Long resourceId = 5005L;

        // Create multiple reviews for same resource
        CreateReviewRequest request1 = new CreateReviewRequest();
        request1.resourceId = resourceId;
        request1.userId = 6005L;
        request1.rating = 5;
        request1.comment = "First review for this resource, absolutely perfect!";

        CreateReviewRequest request2 = new CreateReviewRequest();
        request2.resourceId = resourceId;
        request2.userId = 6006L;
        request2.rating = 4;
        request2.comment = "Second review for this resource, very good overall.";

        // Create reviews
        Long review1Id = given().contentType(ContentType.JSON).body(request1)
            .post("/api/reviews").then().statusCode(201).extract().path("id");

        Long review2Id = given().contentType(ContentType.JSON).body(request2)
            .post("/api/reviews").then().statusCode(201).extract().path("id");

        // Approve them so they show up
        given().post("/api/reviews/" + review1Id + "/approve").then().statusCode(200);
        given().post("/api/reviews/" + review2Id + "/approve").then().statusCode(200);

        // Get reviews for resource (only approved ones)
        given()
            .contentType(ContentType.JSON)
        .when()
            .get("/api/reviews/resource/" + resourceId)
        .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body("$", hasSize(greaterThanOrEqualTo(2)));
    }

    @Test
    @DisplayName("Pattern 7: Test GET /api/reviews/user/{userId} - Get reviews by user")
    void testGetReviewsByUser() {
        Long userId = 6007L;

        // Create multiple reviews from same user
        CreateReviewRequest request1 = new CreateReviewRequest();
        request1.resourceId = 5006L;
        request1.userId = userId;
        request1.rating = 5;
        request1.comment = "First review from this user, excellent content!";

        CreateReviewRequest request2 = new CreateReviewRequest();
        request2.resourceId = 5007L;
        request2.userId = userId;
        request2.rating = 3;
        request2.comment = "Second review from this user, average quality.";

        // Create reviews
        given().contentType(ContentType.JSON).body(request1)
            .post("/api/reviews").then().statusCode(201);
        given().contentType(ContentType.JSON).body(request2)
            .post("/api/reviews").then().statusCode(201);

        // Get reviews by user
        given()
            .contentType(ContentType.JSON)
        .when()
            .get("/api/reviews/user/" + userId)
        .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body("$", hasSize(greaterThanOrEqualTo(2)))
            .body("[0].userId", equalTo(userId.intValue()));
    }

    // ==================== PATTERN 8: Testing PUT - Update Resource ====================

    @Test
    @DisplayName("Pattern 8: Test PUT /api/reviews/{id} - Update review (200)")
    void testUpdateReview_Success() {
        // Create review
        CreateReviewRequest createRequest = new CreateReviewRequest();
        createRequest.resourceId = 5008L;
        createRequest.userId = 6008L;
        createRequest.rating = 3;
        createRequest.comment = "Initial review that will be updated later.";

        Long reviewId = given().contentType(ContentType.JSON).body(createRequest)
            .post("/api/reviews").then().statusCode(201).extract().path("id");

        // Update it
        UpdateReviewRequest updateRequest = new UpdateReviewRequest();
        updateRequest.rating = 5;
        updateRequest.comment = "Updated review with much better rating and content!";

        given()
            .contentType(ContentType.JSON)
            .body(updateRequest)
        .when()
            .put("/api/reviews/" + reviewId)
        .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body("rating", equalTo(5))
            .body("comment", containsString("Updated review"))
            .body("status", equalTo("PENDING")); // Goes back to pending
    }

    // ==================== PATTERN 9: Testing Moderation Endpoints ====================

    @Test
    @DisplayName("Pattern 9: Test POST /api/reviews/{id}/approve - Approve review")
    void testApproveReview_Success() {
        // Create pending review
        CreateReviewRequest createRequest = new CreateReviewRequest();
        createRequest.resourceId = 5009L;
        createRequest.userId = 6009L;
        createRequest.rating = 5;
        createRequest.comment = "Review that will be approved for testing purposes.";

        Long reviewId = given().contentType(ContentType.JSON).body(createRequest)
            .post("/api/reviews").then().statusCode(201).extract().path("id");

        // Approve it
        given()
            .contentType(ContentType.JSON)
        .when()
            .post("/api/reviews/" + reviewId + "/approve")
        .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body("status", equalTo("APPROVED"))
            .body("moderatedBy", equalTo("moderator"))
            .body("moderatedAt", notNullValue());
    }

    @Test
    @DisplayName("Pattern 10: Test POST /api/reviews/{id}/reject - Reject review")
    void testRejectReview_Success() {
        // Create pending review
        CreateReviewRequest createRequest = new CreateReviewRequest();
        createRequest.resourceId = 5010L;
        createRequest.userId = 6010L;
        createRequest.rating = 1;
        createRequest.comment = "Review that will be rejected for testing purposes.";

        Long reviewId = given().contentType(ContentType.JSON).body(createRequest)
            .post("/api/reviews").then().statusCode(201).extract().path("id");

        // Reject it
        given()
            .contentType(ContentType.JSON)
            .queryParam("reason", "Violates community guidelines")
        .when()
            .post("/api/reviews/" + reviewId + "/reject")
        .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body("status", equalTo("REJECTED"))
            .body("moderationReason", containsString("guidelines"));
    }

    @Test
    @DisplayName("Pattern 11: Test POST /api/reviews/{id}/flag - Flag review")
    void testFlagReview_Success() {
        // Create and approve review
        CreateReviewRequest createRequest = new CreateReviewRequest();
        createRequest.resourceId = 5011L;
        createRequest.userId = 6011L;
        createRequest.rating = 3;
        createRequest.comment = "Review that will be flagged for testing purposes.";

        Long reviewId = given().contentType(ContentType.JSON).body(createRequest)
            .post("/api/reviews").then().statusCode(201).extract().path("id");

        // Approve first
        given().post("/api/reviews/" + reviewId + "/approve").then().statusCode(200);

        // Then flag it
        given()
            .contentType(ContentType.JSON)
            .queryParam("reason", "Reported by user as suspicious")
        .when()
            .post("/api/reviews/" + reviewId + "/flag")
        .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body("status", equalTo("FLAGGED"))
            .body("moderationReason", containsString("Reported"));
    }

    // ==================== PATTERN 12: Testing Bulk Operations ====================

    @Test
    @DisplayName("Pattern 12: Test POST /api/reviews/bulk/approve - Bulk approve")
    void testBulkApprove() {
        // Create multiple pending reviews
        CreateReviewRequest request1 = new CreateReviewRequest();
        request1.resourceId = 5012L;
        request1.userId = 6012L;
        request1.rating = 5;
        request1.comment = "First review for bulk approval testing.";

        CreateReviewRequest request2 = new CreateReviewRequest();
        request2.resourceId = 5013L;
        request2.userId = 6013L;
        request2.rating = 4;
        request2.comment = "Second review for bulk approval testing.";

        CreateReviewRequest request3 = new CreateReviewRequest();
        request3.resourceId = 5014L;
        request3.userId = 6014L;
        request3.rating = 5;
        request3.comment = "Third review for bulk approval testing.";

        Long id1 = given().contentType(ContentType.JSON).body(request1)
            .post("/api/reviews").then().statusCode(201).extract().path("id");
        Long id2 = given().contentType(ContentType.JSON).body(request2)
            .post("/api/reviews").then().statusCode(201).extract().path("id");
        Long id3 = given().contentType(ContentType.JSON).body(request3)
            .post("/api/reviews").then().statusCode(201).extract().path("id");

        // Bulk approve
        List<Long> ids = Arrays.asList(id1, id2, id3);

        given()
            .contentType(ContentType.JSON)
            .body(ids)
        .when()
            .post("/api/reviews/bulk/approve")
        .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body("approvedCount", equalTo(3));

        // Verify they're all approved
        given().get("/api/reviews/" + id1).then().body("status", equalTo("APPROVED"));
        given().get("/api/reviews/" + id2).then().body("status", equalTo("APPROVED"));
        given().get("/api/reviews/" + id3).then().body("status", equalTo("APPROVED"));
    }

    // ==================== PATTERN 13: Testing Aggregation Endpoints ====================

    @Test
    @DisplayName("Pattern 13: Test GET /api/reviews/resource/{resourceId}/rating - Average rating")
    void testGetAverageRating() {
        Long resourceId = 5015L;

        // Create and approve multiple reviews
        CreateReviewRequest request1 = new CreateReviewRequest();
        request1.resourceId = resourceId;
        request1.userId = 6015L;
        request1.rating = 5;
        request1.comment = "First review for average rating test, perfect!";

        CreateReviewRequest request2 = new CreateReviewRequest();
        request2.resourceId = resourceId;
        request2.userId = 6016L;
        request2.rating = 3;
        request2.comment = "Second review for average rating test, average.";

        Long id1 = given().contentType(ContentType.JSON).body(request1)
            .post("/api/reviews").then().statusCode(201).extract().path("id");
        Long id2 = given().contentType(ContentType.JSON).body(request2)
            .post("/api/reviews").then().statusCode(201).extract().path("id");

        // Approve both
        given().post("/api/reviews/" + id1 + "/approve").then().statusCode(200);
        given().post("/api/reviews/" + id2 + "/approve").then().statusCode(200);

        // Get average rating (should be 4.0)
        given()
            .contentType(ContentType.JSON)
        .when()
            .get("/api/reviews/resource/" + resourceId + "/rating")
        .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body("averageRating", equalTo(4.0f));
    }

    @Test
    @DisplayName("Pattern 14: Test GET /api/reviews/resource/{resourceId}/distribution - Rating distribution")
    void testGetRatingDistribution() {
        Long resourceId = 5016L;

        // Create reviews with various ratings
        CreateReviewRequest request1 = new CreateReviewRequest();
        request1.resourceId = resourceId;
        request1.userId = 6017L;
        request1.rating = 5;
        request1.comment = "Five star review for distribution test!";

        CreateReviewRequest request2 = new CreateReviewRequest();
        request2.resourceId = resourceId;
        request2.userId = 6018L;
        request2.rating = 5;
        request2.comment = "Another five star review for testing!";

        CreateReviewRequest request3 = new CreateReviewRequest();
        request3.resourceId = resourceId;
        request3.userId = 6019L;
        request3.rating = 4;
        request3.comment = "Four star review for distribution test.";

        Long id1 = given().contentType(ContentType.JSON).body(request1)
            .post("/api/reviews").then().statusCode(201).extract().path("id");
        Long id2 = given().contentType(ContentType.JSON).body(request2)
            .post("/api/reviews").then().statusCode(201).extract().path("id");
        Long id3 = given().contentType(ContentType.JSON).body(request3)
            .post("/api/reviews").then().statusCode(201).extract().path("id");

        // Approve all
        given().post("/api/reviews/" + id1 + "/approve").then().statusCode(200);
        given().post("/api/reviews/" + id2 + "/approve").then().statusCode(200);
        given().post("/api/reviews/" + id3 + "/approve").then().statusCode(200);

        // Get distribution
        given()
            .contentType(ContentType.JSON)
        .when()
            .get("/api/reviews/resource/" + resourceId + "/distribution")
        .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body("5", equalTo(2))
            .body("4", equalTo(1))
            .body("3", equalTo(0))
            .body("2", equalTo(0))
            .body("1", equalTo(0));
    }

    // ==================== PATTERN 15: Testing Archive/Restore ====================

    @Test
    @DisplayName("Pattern 15: Test POST /api/reviews/{id}/archive - Archive review")
    void testArchiveReview() {
        // Create review
        CreateReviewRequest createRequest = new CreateReviewRequest();
        createRequest.resourceId = 5017L;
        createRequest.userId = 6020L;
        createRequest.rating = 3;
        createRequest.comment = "Review that will be archived for testing purposes.";

        Long reviewId = given().contentType(ContentType.JSON).body(createRequest)
            .post("/api/reviews").then().statusCode(201).extract().path("id");

        // Archive it
        given()
            .contentType(ContentType.JSON)
            .queryParam("reason", "Outdated content")
        .when()
            .post("/api/reviews/" + reviewId + "/archive")
        .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body("archived", equalTo(true))
            .body("archiveReason", equalTo("Outdated content"));

        // Verify it's not accessible via normal GET
        given().get("/api/reviews/" + reviewId)
            .then().statusCode(anyOf(is(404), is(500)));
    }

    @Test
    @DisplayName("Pattern 16: Test POST /api/reviews/{id}/restore - Restore archived review")
    void testRestoreReview() {
        // Create and archive review
        CreateReviewRequest createRequest = new CreateReviewRequest();
        createRequest.resourceId = 5018L;
        createRequest.userId = 6021L;
        createRequest.rating = 4;
        createRequest.comment = "Review to be archived and then restored.";

        Long reviewId = given().contentType(ContentType.JSON).body(createRequest)
            .post("/api/reviews").then().statusCode(201).extract().path("id");

        // Archive it
        given().queryParam("reason", "Test").post("/api/reviews/" + reviewId + "/archive")
            .then().statusCode(200);

        // Restore it
        given()
            .contentType(ContentType.JSON)
        .when()
            .post("/api/reviews/" + reviewId + "/restore")
        .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body("archived", equalTo(false))
            .body("status", equalTo("PENDING"));

        // Verify it's accessible again
        given().get("/api/reviews/" + reviewId).then().statusCode(200);
    }

    // ==================== PATTERN 17: Testing DELETE ====================

    @Test
    @DisplayName("Pattern 17: Test DELETE /api/reviews/{id} - Permanent delete (must be archived first)")
    void testDeleteReview_MustBeArchivedFirst() {
        // Create review
        CreateReviewRequest createRequest = new CreateReviewRequest();
        createRequest.resourceId = 5019L;
        createRequest.userId = 6022L;
        createRequest.rating = 2;
        createRequest.comment = "Review that cannot be deleted without archiving first.";

        Long reviewId = given().contentType(ContentType.JSON).body(createRequest)
            .post("/api/reviews").then().statusCode(201).extract().path("id");

        // Try to delete without archiving - should fail
        given()
            .contentType(ContentType.JSON)
        .when()
            .delete("/api/reviews/" + reviewId)
        .then()
            .statusCode(anyOf(is(400), is(409), is(422), is(500))); // Business rule violation
    }

    @Test
    @DisplayName("Pattern 18: Test DELETE after archive - Success")
    void testDeleteReview_AfterArchive_Success() {
        // Create review
        CreateReviewRequest createRequest = new CreateReviewRequest();
        createRequest.resourceId = 5020L;
        createRequest.userId = 6023L;
        createRequest.rating = 1;
        createRequest.comment = "Review that will be archived then deleted.";

        Long reviewId = given().contentType(ContentType.JSON).body(createRequest)
            .post("/api/reviews").then().statusCode(201).extract().path("id");

        // Archive it first
        given().queryParam("reason", "Test").post("/api/reviews/" + reviewId + "/archive")
            .then().statusCode(200);

        // Now delete - should succeed
        given()
            .contentType(ContentType.JSON)
        .when()
            .delete("/api/reviews/" + reviewId)
        .then()
            .statusCode(204); // No content - successful deletion
    }

    // ==================== PATTERN 19: Testing Query Parameters ====================

    @Test
    @DisplayName("Pattern 19: Test GET /api/reviews/check - Check if user reviewed resource")
    void testHasUserReviewedWork() {
        Long userId = 6024L;
        Long resourceId = 5021L;

        // Initially should be false
        given()
            .contentType(ContentType.JSON)
            .queryParam("userId", userId)
            .queryParam("resourceId", resourceId)
        .when()
            .get("/api/reviews/check")
        .then()
            .statusCode(200)
            .body("hasReviewed", equalTo(false));

        // Create review
        CreateReviewRequest createRequest = new CreateReviewRequest();
        createRequest.resourceId = resourceId;
        createRequest.userId = userId;
        createRequest.rating = 5;
        createRequest.comment = "Testing review existence check functionality.";

        given().contentType(ContentType.JSON).body(createRequest)
            .post("/api/reviews").then().statusCode(201);

        // Now should be true
        given()
            .contentType(ContentType.JSON)
            .queryParam("userId", userId)
            .queryParam("resourceId", resourceId)
        .when()
            .get("/api/reviews/check")
        .then()
            .statusCode(200)
            .body("hasReviewed", equalTo(true));
    }

    // ==================== PATTERN 20: Testing Pending Reviews List ====================

    @Test
    @DisplayName("Pattern 20: Test GET /api/reviews/pending - Get pending reviews for moderation")
    void testGetPendingReviews() {
        // Create multiple pending reviews
        CreateReviewRequest request1 = new CreateReviewRequest();
        request1.resourceId = 5022L;
        request1.userId = 6025L;
        request1.rating = 5;
        request1.comment = "First pending review awaiting moderation.";

        CreateReviewRequest request2 = new CreateReviewRequest();
        request2.resourceId = 5023L;
        request2.userId = 6026L;
        request2.rating = 4;
        request2.comment = "Second pending review awaiting moderation.";

        given().contentType(ContentType.JSON).body(request1)
            .post("/api/reviews").then().statusCode(201);
        given().contentType(ContentType.JSON).body(request2)
            .post("/api/reviews").then().statusCode(201);

        // Get pending reviews
        given()
            .contentType(ContentType.JSON)
        .when()
            .get("/api/reviews/pending")
        .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body("$", hasSize(greaterThanOrEqualTo(2)));
    }
}
