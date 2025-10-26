package com.akfc.catalog.web;

import com.akfc.catalog.data.Resource;
import com.akfc.catalog.data.ResourceStatus;
import com.akfc.catalog.data.ResourceType;
import com.akfc.catalog.dto.CreateResourceRequest;
import com.akfc.catalog.services.ResourceService;
import io.quarkus.test.junit.QuarkusTest;
import io.restassured.http.ContentType;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;
import org.junit.jupiter.api.*;

import java.util.Arrays;

import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.*;

/**
 * API tests for CatalogWebController using RestAssured.
 *
 * These tests demonstrate:
 * - HTTP endpoint testing with RestAssured
 * - HTML response validation
 * - Form submission testing
 * - Query parameter handling
 * - HTTP status code verification
 * - Response body content validation
 *
 * RestAssured provides a fluent API for testing REST/HTTP endpoints.
 */
@QuarkusTest
@DisplayName("CatalogWebController API Tests (RestAssured)")
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class CatalogWebControllerApiTest {

    @Inject
    ResourceService resourceService;

    private Long testResourceId;

    @BeforeEach
    @Transactional
    void setUp() {
        // Clean database before each test
        Resource.deleteAll();

        // Create a test resource for GET/PUT/DELETE tests
        CreateResourceRequest request = new CreateResourceRequest();
        request.title = "The Hobbit";
        request.type = ResourceType.BOOK;
        request.year = 1937;
        request.creator = "J.R.R. Tolkien";
        request.keywords = Arrays.asList("fantasy", "adventure");
        request.illustrationUrl = "http://example.com/hobbit.jpg";

        testResourceId = resourceService.createResource(request, "test").id;
    }

    // ========== GET TESTS ==========

    @Test
    @Order(1)
    @DisplayName("GET /web/catalog - Should return catalog list page")
    void shouldReturnCatalogListPage() {
        given()
            .when()
                .get("/web/catalog")
            .then()
                .statusCode(200)
                .contentType(ContentType.HTML)
                .body(containsString("Catalog"))
                .body(containsString("The Hobbit"))
                .body(containsString("J.R.R. Tolkien"));
    }

    @Test
    @Order(2)
    @DisplayName("GET /web/catalog - Should handle empty catalog")
    @Transactional
    void shouldHandleEmptyCatalog() {
        // Given: Empty catalog
        Resource.deleteAll();

        given()
            .when()
                .get("/web/catalog")
            .then()
                .statusCode(200)
                .contentType(ContentType.HTML)
                .body(containsString("Catalog"));
    }

    @Test
    @Order(3)
    @DisplayName("GET /web/catalog - Should support search query parameter")
    void shouldSupportSearchQueryParameter() {
        given()
            .queryParam("search", "Hobbit")
            .when()
                .get("/web/catalog")
            .then()
                .statusCode(200)
                .contentType(ContentType.HTML)
                .body(containsString("The Hobbit"));
    }

    @Test
    @Order(4)
    @DisplayName("GET /web/catalog - Should support type filter")
    void shouldSupportTypeFilter() {
        given()
            .queryParam("type", "BOOK")
            .when()
                .get("/web/catalog")
            .then()
                .statusCode(200)
                .contentType(ContentType.HTML)
                .body(containsString("The Hobbit"));
    }

    @Test
    @Order(5)
    @DisplayName("GET /web/catalog - Should support pagination parameters")
    void shouldSupportPaginationParameters() {
        given()
            .queryParam("page", "0")
            .queryParam("size", "10")
            .when()
                .get("/web/catalog")
            .then()
                .statusCode(200)
                .contentType(ContentType.HTML);
    }

    @Test
    @Order(6)
    @DisplayName("GET /web/catalog/new - Should return create form")
    void shouldReturnCreateForm() {
        given()
            .when()
                .get("/web/catalog/new")
            .then()
                .statusCode(200)
                .contentType(ContentType.HTML)
                .body(containsString("Add New Resource"))
                .body(containsString("<form"))
                .body(containsString("title"))
                .body(containsString("type"))
                .body(containsString("year"))
                .body(containsString("creator"));
    }

    @Test
    @Order(7)
    @DisplayName("GET /web/catalog/{id} - Should return resource details")
    void shouldReturnResourceDetails() {
        given()
            .pathParam("id", testResourceId)
            .when()
                .get("/web/catalog/{id}")
            .then()
                .statusCode(200)
                .contentType(ContentType.HTML)
                .body(containsString("The Hobbit"))
                .body(containsString("J.R.R. Tolkien"))
                .body(containsString("1937"))
                .body(containsString("BOOK"));
    }

    @Test
    @Order(8)
    @DisplayName("GET /web/catalog/{id} - Should redirect when resource not found")
    void shouldRedirectWhenResourceNotFound() {
        given()
            .redirects().follow(false)  // Don't follow redirects
            .pathParam("id", 99999L)
            .when()
                .get("/web/catalog/{id}")
            .then()
                .statusCode(303)  // See Other redirect
                .header("Location", containsString("/web/catalog"));
    }

    @Test
    @Order(9)
    @DisplayName("GET /web/catalog/{id}/edit - Should return edit form")
    void shouldReturnEditForm() {
        given()
            .pathParam("id", testResourceId)
            .when()
                .get("/web/catalog/{id}/edit")
            .then()
                .statusCode(200)
                .contentType(ContentType.HTML)
                .body(containsString("Edit Resource"))
                .body(containsString("The Hobbit"))
                .body(containsString("<form"))
                .body(containsString("value=\"The Hobbit\""));
    }

    // ========== POST TESTS (Form Submission) ==========

    @Test
    @Order(10)
    @DisplayName("POST /web/catalog - Should create new resource")
    void shouldCreateNewResource() {
        given()
            .redirects().follow(false)
            .contentType("application/x-www-form-urlencoded")
            .formParam("title", "1984")
            .formParam("type", "BOOK")
            .formParam("year", 1949)
            .formParam("creator", "George Orwell")
            .formParam("keywords", "dystopia,classic")
            .formParam("illustrationUrl", "http://example.com/1984.jpg")
            .when()
                .post("/web/catalog")
            .then()
                .statusCode(303)  // Redirect after POST
                .header("Location", containsString("/web/catalog"));
    }

    @Test
    @Order(11)
    @DisplayName("POST /web/catalog - Should handle validation errors")
    void shouldHandleValidationErrors() {
        given()
            .redirects().follow(false)
            .contentType("application/x-www-form-urlencoded")
            .formParam("title", "")  // Empty title (invalid)
            .formParam("type", "BOOK")
            .formParam("year", 2020)
            .formParam("creator", "Author")
            .when()
                .post("/web/catalog")
            .then()
                .statusCode(303)  // Redirect with error
                .header("Location", containsString("error="));
    }

    @Test
    @Order(12)
    @DisplayName("POST /web/catalog - Should handle missing required fields")
    void shouldHandleMissingRequiredFields() {
        given()
            .redirects().follow(false)
            .contentType("application/x-www-form-urlencoded")
            .formParam("title", "Test Book")
            // Missing type, year, creator
            .when()
                .post("/web/catalog")
            .then()
                .statusCode(303)
                .header("Location", containsString("error="));
    }

    @Test
    @Order(13)
    @DisplayName("POST /web/catalog/{id} - Should update existing resource")
    void shouldUpdateExistingResource() {
        given()
            .redirects().follow(false)
            .pathParam("id", testResourceId)
            .contentType("application/x-www-form-urlencoded")
            .formParam("title", "The Hobbit (Updated)")
            .formParam("year", 1937)
            .formParam("creator", "J.R.R. Tolkien")
            .formParam("keywords", "fantasy,adventure,classic")
            .formParam("illustrationUrl", "http://example.com/hobbit-new.jpg")
            .when()
                .post("/web/catalog/{id}")
            .then()
                .statusCode(303)
                .header("Location", containsString("/web/catalog/" + testResourceId));
    }

    @Test
    @Order(14)
    @DisplayName("POST /web/catalog/{id}/delete - Should delete resource")
    void shouldDeleteResource() {
        given()
            .redirects().follow(false)
            .pathParam("id", testResourceId)
            .when()
                .post("/web/catalog/{id}/delete")
            .then()
                .statusCode(303)
                .header("Location", containsString("/web/catalog"))
                .header("Location", containsString("message="));
    }

    @Test
    @Order(15)
    @DisplayName("POST /web/catalog/{id}/delete - Should handle non-existent resource")
    void shouldHandleDeleteNonExistentResource() {
        given()
            .redirects().follow(false)
            .pathParam("id", 99999L)
            .when()
                .post("/web/catalog/{id}/delete")
            .then()
                .statusCode(303)
                .header("Location", containsString("error="));
    }

    // ========== SUCCESS/ERROR MESSAGE TESTS ==========

    @Test
    @Order(16)
    @DisplayName("Should display success message in query parameter")
    void shouldDisplaySuccessMessage() {
        given()
            .queryParam("message", "Resource created successfully")
            .when()
                .get("/web/catalog")
            .then()
                .statusCode(200)
                .body(containsString("Resource created successfully"));
    }

    @Test
    @Order(17)
    @DisplayName("Should display error message in query parameter")
    void shouldDisplayErrorMessage() {
        given()
            .queryParam("error", "Something went wrong")
            .when()
                .get("/web/catalog")
            .then()
                .statusCode(200)
                .body(containsString("Something went wrong"));
    }

    // ========== CONTENT TYPE TESTS ==========

    @Test
    @Order(18)
    @DisplayName("Should return HTML content type for all pages")
    void shouldReturnHtmlContentType() {
        // List page
        given()
            .when().get("/web/catalog")
            .then().contentType(ContentType.HTML);

        // Create form
        given()
            .when().get("/web/catalog/new")
            .then().contentType(ContentType.HTML);

        // View page
        given()
            .pathParam("id", testResourceId)
            .when().get("/web/catalog/{id}")
            .then().contentType(ContentType.HTML);

        // Edit form
        given()
            .pathParam("id", testResourceId)
            .when().get("/web/catalog/{id}/edit")
            .then().contentType(ContentType.HTML);
    }

    // ========== RESPONSE TIME TESTS ==========

    @Test
    @Order(19)
    @DisplayName("Should respond within acceptable time")
    void shouldRespondWithinAcceptableTime() {
        given()
            .when()
                .get("/web/catalog")
            .then()
                .statusCode(200)
                .time(lessThan(2000L));  // Less than 2 seconds
    }

    // ========== MULTIPLE RESOURCES TESTS ==========

    @Test
    @Order(20)
    @DisplayName("Should display multiple resources in catalog")
    void shouldDisplayMultipleResources() {
        // Given: Multiple resources
        CreateResourceRequest req1 = new CreateResourceRequest();
        req1.title = "Book 1";
        req1.type = ResourceType.BOOK;
        req1.year = 2020;
        req1.creator = "Author 1";

        CreateResourceRequest req2 = new CreateResourceRequest();
        req2.title = "Movie 1";
        req2.type = ResourceType.MOVIE;
        req2.year = 2021;
        req2.creator = "Director 1";

        resourceService.createResource(req1, "test");
        resourceService.createResource(req2, "test");

        // When & Then: All resources should appear in list
        given()
            .when()
                .get("/web/catalog")
            .then()
                .statusCode(200)
                .body(containsString("The Hobbit"))
                .body(containsString("Book 1"))
                .body(containsString("Movie 1"));
    }

    // ========== CUSTOM ASSERTIONS WITH EXTRACT ==========

    @Test
    @Order(21)
    @DisplayName("Should extract and validate response body")
    void shouldExtractAndValidateResponseBody() {
        String responseBody = given()
            .when()
                .get("/web/catalog")
            .then()
                .statusCode(200)
                .extract().body().asString();

        // Custom assertions on response body
        Assertions.assertTrue(responseBody.contains("Catalog"));
        Assertions.assertTrue(responseBody.contains("<!DOCTYPE html>"));
        Assertions.assertTrue(responseBody.length() > 100);
    }
}
