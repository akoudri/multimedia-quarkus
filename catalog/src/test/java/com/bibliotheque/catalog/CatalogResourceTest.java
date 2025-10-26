package com.bibliotheque.catalog;

import io.quarkus.test.junit.QuarkusTest;
import org.junit.jupiter.api.Test;

import static io.restassured.RestAssured.given;

@QuarkusTest
public class CatalogResourceTest {

    @Test
    public void testCatalogEndpoint() {
        // TODO: Add your endpoint tests here
        // Example:
        // given()
        //     .when().get("/catalog")
        //     .then()
        //         .statusCode(200);
    }
}
