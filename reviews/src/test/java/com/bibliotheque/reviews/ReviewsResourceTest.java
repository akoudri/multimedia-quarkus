package com.bibliotheque.reviews;

import io.quarkus.test.junit.QuarkusTest;
import org.junit.jupiter.api.Test;

import static io.restassured.RestAssured.given;

@QuarkusTest
public class ReviewsResourceTest {

    @Test
    public void testReviewsEndpoint() {
        // TODO: Add your endpoint tests here
        // Example:
        // given()
        //     .when().get("/reviews")
        //     .then()
        //         .statusCode(200);
    }
}
