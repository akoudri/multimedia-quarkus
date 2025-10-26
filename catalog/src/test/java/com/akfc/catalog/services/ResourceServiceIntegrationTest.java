package com.akfc.catalog.services;

import com.akfc.catalog.data.Resource;
import com.akfc.catalog.data.ResourceRepository;
import com.akfc.catalog.data.ResourceStatus;
import com.akfc.catalog.data.ResourceType;
import com.akfc.catalog.dto.CreateResourceRequest;
import com.akfc.catalog.dto.ResourceResponse;
import com.akfc.catalog.dto.UpdateResourceRequest;
import com.akfc.catalog.errors.ResourceAlreadyArchivedException;
import com.akfc.catalog.errors.ResourceNotFoundException;
import io.quarkus.test.junit.QuarkusTest;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;
import org.junit.jupiter.api.*;

import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

/**
 * Integration tests for ResourceService using @QuarkusTest.
 *
 * These tests demonstrate:
 * - Full Quarkus application context
 * - Real database transactions
 * - CDI dependency injection
 * - Transaction management
 * - Cache behavior (if configured)
 * - End-to-end business logic testing
 *
 * NOTE: @QuarkusTest starts the full Quarkus application, so these tests are slower
 * but more comprehensive than unit tests.
 */
@QuarkusTest
@DisplayName("ResourceService Integration Tests (@QuarkusTest)")
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class ResourceServiceIntegrationTest {

    @Inject
    ResourceService resourceService;

    @Inject
    ResourceRepository resourceRepository;

    private Long createdResourceId;

    @BeforeEach
    @Transactional
    void setUp() {
        // Clean up database before each test
        Resource.deleteAll();
    }

    // ========== CREATE TESTS ==========

    @Test
    @Order(1)
    @DisplayName("Should create resource with full integration")
    void shouldCreateResourceWithFullIntegration() {
        // Given: Valid create request
        CreateResourceRequest request = new CreateResourceRequest();
        request.title = "The Hobbit";
        request.type = ResourceType.BOOK;
        request.year = 1937;
        request.creator = "J.R.R. Tolkien";
        request.keywords = Arrays.asList("fantasy", "adventure", "classic");
        request.illustrationUrl = "http://example.com/hobbit.jpg";

        // When: Create resource through service
        ResourceResponse response = resourceService.createResource(request, "integration-test");

        // Then: Resource should be created and persisted
        assertThat(response).isNotNull();
        assertThat(response.id).isNotNull();
        assertThat(response.title).isEqualTo("The Hobbit");
        assertThat(response.type).isEqualTo(ResourceType.BOOK);
        assertThat(response.year).isEqualTo(1937);
        assertThat(response.creator).isEqualTo("J.R.R. Tolkien");
        assertThat(response.keywords).containsExactlyInAnyOrder("fantasy", "adventure", "classic");
        assertThat(response.status).isEqualTo(ResourceStatus.AVAILABLE);
        assertThat(response.archived).isFalse();
        assertThat(response.createdAt).isNotNull();
        assertThat(response.updatedAt).isNotNull();

        // Save ID for later tests
        createdResourceId = response.id;

        // Verify: Resource exists in database
        Resource foundResource = Resource.findById(createdResourceId);
        assertThat(foundResource).isNotNull();
        assertThat(foundResource.title).isEqualTo("The Hobbit");
    }

    @Test
    @Order(2)
    @DisplayName("Should create multiple resources")
    void shouldCreateMultipleResources() {
        // Given: Multiple create requests
        CreateResourceRequest book = createBookRequest("1984", 1949, "George Orwell");
        CreateResourceRequest movie = createMovieRequest("Inception", 2010, "Christopher Nolan");
        CreateResourceRequest music = createMusicRequest("Abbey Road", 1969, "The Beatles");

        // When: Create all resources
        ResourceResponse bookResponse = resourceService.createResource(book, "test");
        ResourceResponse movieResponse = resourceService.createResource(movie, "test");
        ResourceResponse musicResponse = resourceService.createResource(music, "test");

        // Then: All should be created
        assertThat(bookResponse.type).isEqualTo(ResourceType.BOOK);
        assertThat(movieResponse.type).isEqualTo(ResourceType.MOVIE);
        assertThat(musicResponse.type).isEqualTo(ResourceType.MUSIC);

        // Verify: All exist in database
        List<ResourceResponse> allResources = resourceService.getAllResources();
        assertThat(allResources).hasSize(3);
    }

    // ========== READ TESTS ==========

    @Test
    @Order(3)
    @DisplayName("Should retrieve resource by ID")
    void shouldRetrieveResourceById() {
        // Given: Resource exists
        CreateResourceRequest request = createBookRequest("Test Book", 2024, "Test Author");
        ResourceResponse created = resourceService.createResource(request, "test");

        // When: Get by ID
        ResourceResponse retrieved = resourceService.getResourceById(created.id);

        // Then: Should match created resource
        assertThat(retrieved).isNotNull();
        assertThat(retrieved.id).isEqualTo(created.id);
        assertThat(retrieved.title).isEqualTo("Test Book");
        assertThat(retrieved.creator).isEqualTo("Test Author");
    }

    @Test
    @Order(4)
    @DisplayName("Should throw exception when resource not found")
    void shouldThrowExceptionWhenResourceNotFound() {
        // Given: Non-existent ID
        Long nonExistentId = 99999L;

        // When & Then: Should throw ResourceNotFoundException
        assertThatThrownBy(() -> resourceService.getResourceById(nonExistentId))
            .isInstanceOf(ResourceNotFoundException.class)
            .hasMessageContaining("99999");
    }

    @Test
    @Order(5)
    @DisplayName("Should retrieve all active resources")
    void shouldRetrieveAllActiveResources() {
        // Given: Multiple resources (some archived)
        ResourceResponse r1 = resourceService.createResource(
            createBookRequest("Book 1", 2020, "Author 1"), "test"
        );
        ResourceResponse r2 = resourceService.createResource(
            createBookRequest("Book 2", 2021, "Author 2"), "test"
        );
        ResourceResponse r3 = resourceService.createResource(
            createBookRequest("Book 3", 2022, "Author 3"), "test"
        );

        // Archive one resource
        resourceService.archiveResource(r2.id, "Test archive", "test");

        // When: Get all resources
        List<ResourceResponse> active = resourceService.getAllResources();

        // Then: Should only return active resources (not archived)
        assertThat(active).hasSize(2);
        assertThat(active).extracting(r -> r.title)
            .containsExactlyInAnyOrder("Book 1", "Book 3");
    }

    @Test
    @Order(6)
    @DisplayName("Should filter resources by type")
    void shouldFilterResourcesByType() {
        // Given: Resources of different types
        resourceService.createResource(createBookRequest("Book 1", 2020, "Author"), "test");
        resourceService.createResource(createBookRequest("Book 2", 2021, "Author"), "test");
        resourceService.createResource(createMovieRequest("Movie 1", 2020, "Director"), "test");

        // When: Get resources by type
        List<ResourceResponse> books = resourceService.getResourcesByType(ResourceType.BOOK);
        List<ResourceResponse> movies = resourceService.getResourcesByType(ResourceType.MOVIE);

        // Then: Should be filtered correctly
        assertThat(books).hasSize(2);
        assertThat(movies).hasSize(1);
        assertThat(books).allMatch(r -> r.type == ResourceType.BOOK);
        assertThat(movies).allMatch(r -> r.type == ResourceType.MOVIE);
    }

    // ========== UPDATE TESTS ==========

    @Test
    @Order(7)
    @DisplayName("Should update resource successfully")
    void shouldUpdateResourceSuccessfully() {
        // Given: Existing resource
        CreateResourceRequest createReq = createBookRequest("Original Title", 2020, "Original Author");
        ResourceResponse created = resourceService.createResource(createReq, "test");

        UpdateResourceRequest updateReq = new UpdateResourceRequest();
        updateReq.title = "Updated Title";
        updateReq.year = 2024;
        updateReq.creator = "Updated Author";
        updateReq.keywords = Arrays.asList("updated", "keywords");
        updateReq.illustrationUrl = "http://example.com/updated.jpg";

        // When: Update resource
        ResourceResponse updated = resourceService.updateResource(created.id, updateReq, "updater");

        // Then: Changes should be persisted
        assertThat(updated.title).isEqualTo("Updated Title");
        assertThat(updated.year).isEqualTo(2024);
        assertThat(updated.creator).isEqualTo("Updated Author");
        assertThat(updated.keywords).containsExactlyInAnyOrder("updated", "keywords");

        // Verify: Changes persisted in database
        ResourceResponse retrieved = resourceService.getResourceById(created.id);
        assertThat(retrieved.title).isEqualTo("Updated Title");
    }

    @Test
    @Order(8)
    @DisplayName("Should throw exception when updating archived resource")
    void shouldThrowExceptionWhenUpdatingArchivedResource() {
        // Given: Archived resource
        CreateResourceRequest createReq = createBookRequest("Test Book", 2020, "Author");
        ResourceResponse created = resourceService.createResource(createReq, "test");
        resourceService.archiveResource(created.id, "Outdated", "test");

        UpdateResourceRequest updateReq = new UpdateResourceRequest();
        updateReq.title = "Updated Title";
        updateReq.year = 2024;
        updateReq.creator = "Updated Author";

        // When & Then: Update should fail (entity's update() throws IllegalStateException)
        assertThatThrownBy(() ->
            resourceService.updateResource(created.id, updateReq, "updater")
        ).isInstanceOf(IllegalStateException.class)
         .hasMessageContaining("archived resource");
    }

    // ========== STATUS CHANGE TESTS ==========

    @Test
    @Order(9)
    @DisplayName("Should change resource status via entity methods")
    @Transactional
    void shouldChangeResourceStatusViaEntityMethods() {
        // Given: Resource with AVAILABLE status
        CreateResourceRequest createReq = createBookRequest("Test Book", 2020, "Author");
        ResourceResponse created = resourceService.createResource(createReq, "test");
        assertThat(created.status).isEqualTo(ResourceStatus.AVAILABLE);

        // When: Change status directly on entity (bypassing service)
        Resource resource = Resource.findById(created.id);
        resource.markReserved("admin");

        // Then: Status should be updated
        ResourceResponse retrieved = resourceService.getResourceById(created.id);
        assertThat(retrieved.status).isEqualTo(ResourceStatus.RESERVED);

        // When: Change to UNAVAILABLE
        resource = Resource.findById(created.id);
        resource.markUnavailable("admin");

        // Then: Status should be updated again
        retrieved = resourceService.getResourceById(created.id);
        assertThat(retrieved.status).isEqualTo(ResourceStatus.UNAVAILABLE);
    }

    // ========== ARCHIVE TESTS ==========

    @Test
    @Order(10)
    @DisplayName("Should archive and restore resource")
    void shouldArchiveAndRestoreResource() {
        // Given: Active resource
        CreateResourceRequest createReq = createBookRequest("Test Book", 2020, "Author");
        ResourceResponse created = resourceService.createResource(createReq, "test");
        assertThat(created.archived).isFalse();

        // When: Archive resource
        resourceService.archiveResource(created.id, "No longer needed", "admin");

        // Then: Resource should be archived and not in active list
        List<ResourceResponse> active = resourceService.getAllResources();
        assertThat(active).noneMatch(r -> r.id.equals(created.id));

        // When: Restore resource
        resourceService.restoreResource(created.id, "admin");

        // Then: Resource should be active again
        active = resourceService.getAllResources();
        assertThat(active).anyMatch(r -> r.id.equals(created.id));

        ResourceResponse restored = resourceService.getResourceById(created.id);
        assertThat(restored.archived).isFalse();
        assertThat(restored.status).isEqualTo(ResourceStatus.AVAILABLE);
    }

    // ========== TRANSACTION TESTS ==========

    @Test
    @Order(11)
    @DisplayName("Should rollback transaction on error")
    @Transactional
    void shouldRollbackTransactionOnError() {
        // Given: Initial count
        long initialCount = Resource.count();

        // When: Create resource with invalid data (simulating error)
        CreateResourceRequest invalidRequest = new CreateResourceRequest();
        invalidRequest.title = null;  // This will fail validation
        invalidRequest.type = ResourceType.BOOK;
        invalidRequest.year = 2020;
        invalidRequest.creator = "Author";

        // Then: Should throw exception and not persist
        try {
            resourceService.createResource(invalidRequest, "test");
        } catch (Exception e) {
            // Expected
        }

        // Verify: No new resource was persisted
        long finalCount = Resource.count();
        assertThat(finalCount).isEqualTo(initialCount);
    }

    // ========== HELPER METHODS ==========

    private CreateResourceRequest createBookRequest(String title, Integer year, String author) {
        CreateResourceRequest request = new CreateResourceRequest();
        request.title = title;
        request.type = ResourceType.BOOK;
        request.year = year;
        request.creator = author;
        request.keywords = Arrays.asList("book", "literature");
        return request;
    }

    private CreateResourceRequest createMovieRequest(String title, Integer year, String director) {
        CreateResourceRequest request = new CreateResourceRequest();
        request.title = title;
        request.type = ResourceType.MOVIE;
        request.year = year;
        request.creator = director;
        request.keywords = Arrays.asList("movie", "film");
        return request;
    }

    private CreateResourceRequest createMusicRequest(String title, Integer year, String artist) {
        CreateResourceRequest request = new CreateResourceRequest();
        request.title = title;
        request.type = ResourceType.MUSIC;
        request.year = year;
        request.creator = artist;
        request.keywords = Arrays.asList("music", "album");
        return request;
    }
}
