package com.akfc.catalog.data;

import io.quarkus.test.common.QuarkusTestResource;
import io.quarkus.test.junit.QuarkusTest;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;
import org.junit.jupiter.api.*;

import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

/**
 * Integration tests for ResourceRepository using Testcontainers.
 *
 * These tests demonstrate:
 * - Using Testcontainers for PostgreSQL database
 * - Testing repository layer with real database
 * - Database operations (CRUD)
 * - Complex queries and filtering
 * - Transaction isolation
 *
 * Testcontainers spins up a real PostgreSQL Docker container for testing,
 * providing a production-like environment without mocking.
 */
@QuarkusTest
@QuarkusTestResource(PostgresTestResource.class)
@DisplayName("ResourceRepository Tests (Testcontainers)")
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class ResourceRepositoryTestcontainersTest {

    @Inject
    ResourceRepository resourceRepository;

    @BeforeEach
    @Transactional
    void setUp() {
        // Clean database before each test
        Resource.deleteAll();
    }

    // ========== CREATE TESTS ==========

    @Test
    @Order(1)
    @DisplayName("Should create resource in PostgreSQL container")
    void shouldCreateResourceInPostgresContainer() {
        // Given: Resource data
        String title = "The Hobbit";
        ResourceType type = ResourceType.BOOK;
        Integer year = 1937;
        String creator = "J.R.R. Tolkien";
        List<String> keywords = Arrays.asList("fantasy", "adventure");
        String illustrationUrl = "http://example.com/hobbit.jpg";

        // When: Create resource via repository
        Resource created = resourceRepository.createResource(
            title, type, year, creator, keywords, illustrationUrl, "test-user"
        );

        // Then: Resource should be persisted with ID
        assertThat(created).isNotNull();
        assertThat(created.id).isNotNull();
        assertThat(created.title).isEqualTo(title);
        assertThat(created.type).isEqualTo(type);
        assertThat(created.year).isEqualTo(year);
        assertThat(created.creator).isEqualTo(creator);
        assertThat(created.keywords).containsExactlyInAnyOrder("fantasy", "adventure");
        assertThat(created.status).isEqualTo(ResourceStatus.AVAILABLE);
        assertThat(created.archived).isFalse();
        assertThat(created.createdAt).isNotNull();
        assertThat(created.updatedAt).isNotNull();

        // Verify: Can be retrieved from database
        Resource found = Resource.findById(created.id);
        assertThat(found).isNotNull();
        assertThat(found.title).isEqualTo(title);
    }

    @Test
    @Order(2)
    @DisplayName("Should handle keywords as ElementCollection in PostgreSQL")
    void shouldHandleKeywordsAsElementCollection() {
        // Given: Resource with multiple keywords
        Resource resource = resourceRepository.createResource(
            "Test Book",
            ResourceType.BOOK,
            2024,
            "Test Author",
            Arrays.asList("keyword1", "keyword2", "keyword3", "keyword4"),
            null,
            "test"
        );

        // When: Retrieve from database
        Resource found = Resource.findById(resource.id);

        // Then: Keywords should be stored and retrieved correctly
        assertThat(found.keywords).hasSize(4);
        assertThat(found.keywords).containsExactlyInAnyOrder(
            "keyword1", "keyword2", "keyword3", "keyword4"
        );
    }

    // ========== READ TESTS ==========

    @Test
    @Order(3)
    @DisplayName("Should find all active resources")
    void shouldFindAllActiveResources() {
        // Given: Multiple resources (some archived)
        Resource r1 = resourceRepository.createResource(
            "Book 1", ResourceType.BOOK, 2020, "Author 1", null, null, "test"
        );
        Resource r2 = resourceRepository.createResource(
            "Book 2", ResourceType.BOOK, 2021, "Author 2", null, null, "test"
        );
        Resource r3 = resourceRepository.createResource(
            "Book 3", ResourceType.BOOK, 2022, "Author 3", null, null, "test"
        );

        // Archive one
        resourceRepository.archiveResource(r2.id, "Test", "test");

        // When: Find all active
        List<Resource> active = resourceRepository.findAllActive();

        // Then: Only non-archived resources returned
        assertThat(active).hasSize(2);
        assertThat(active).extracting(r -> r.title)
            .containsExactlyInAnyOrder("Book 1", "Book 3");
    }

    @Test
    @Order(4)
    @DisplayName("Should find resources by type")
    void shouldFindResourcesByType() {
        // Given: Resources of different types
        resourceRepository.createResource(
            "Book 1", ResourceType.BOOK, 2020, "Author", null, null, "test"
        );
        resourceRepository.createResource(
            "Book 2", ResourceType.BOOK, 2021, "Author", null, null, "test"
        );
        resourceRepository.createResource(
            "Movie 1", ResourceType.MOVIE, 2020, "Director", null, null, "test"
        );
        resourceRepository.createResource(
            "Music 1", ResourceType.MUSIC, 2020, "Artist", null, null, "test"
        );

        // When: Query by type using Panache
        List<Resource> books = Resource.findByType(ResourceType.BOOK);
        List<Resource> movies = Resource.findByType(ResourceType.MOVIE);
        List<Resource> music = Resource.findByType(ResourceType.MUSIC);

        // Then: Correct filtering
        assertThat(books).hasSize(2);
        assertThat(movies).hasSize(1);
        assertThat(music).hasSize(1);
    }

    @Test
    @Order(5)
    @DisplayName("Should search by keyword using JOIN query")
    void shouldSearchByKeywordUsingJoinQuery() {
        // Given: Resources with different keywords
        resourceRepository.createResource(
            "Fantasy Book 1",
            ResourceType.BOOK,
            2020,
            "Author 1",
            Arrays.asList("fantasy", "adventure"),
            null,
            "test"
        );
        resourceRepository.createResource(
            "Fantasy Book 2",
            ResourceType.BOOK,
            2021,
            "Author 2",
            Arrays.asList("fantasy", "magic"),
            null,
            "test"
        );
        resourceRepository.createResource(
            "Sci-Fi Book",
            ResourceType.BOOK,
            2022,
            "Author 3",
            Arrays.asList("sci-fi", "space"),
            null,
            "test"
        );

        // When: Search by keyword
        List<Resource> fantasyBooks = Resource.findByKeyword("fantasy");
        List<Resource> scifiBooks = Resource.findByKeyword("sci-fi");

        // Then: Correct results
        assertThat(fantasyBooks).hasSize(2);
        assertThat(scifiBooks).hasSize(1);
    }

    @Test
    @Order(6)
    @DisplayName("Should perform case-insensitive search")
    void shouldPerformCaseInsensitiveSearch() {
        // Given: Resource with mixed-case title
        resourceRepository.createResource(
            "The Lord of the Rings",
            ResourceType.BOOK,
            1954,
            "J.R.R. Tolkien",
            null,
            null,
            "test"
        );

        // When: Search with different cases
        List<Resource> result1 = Resource.searchByTitle("lord");
        List<Resource> result2 = Resource.searchByTitle("LORD");
        List<Resource> result3 = Resource.searchByTitle("Lord");

        // Then: All should find the resource
        assertThat(result1).hasSize(1);
        assertThat(result2).hasSize(1);
        assertThat(result3).hasSize(1);
    }

    // ========== UPDATE TESTS ==========

    @Test
    @Order(7)
    @DisplayName("Should update resource and persist changes")
    @Transactional
    void shouldUpdateResourceAndPersistChanges() {
        // Given: Existing resource
        Resource resource = resourceRepository.createResource(
            "Original Title", ResourceType.BOOK, 2020, "Original Author", null, null, "test"
        );
        Long resourceId = resource.id;

        // When: Update resource
        resource.title = "Updated Title";
        resource.creator = "Updated Author";
        resource.year = 2024;
        resource.keywords = Arrays.asList("updated", "keywords");
        resource.modifiedBy = "updater";
        resource.persist();

        // Then: Changes should be persisted
        Resource updated = Resource.findById(resourceId);
        assertThat(updated.title).isEqualTo("Updated Title");
        assertThat(updated.creator).isEqualTo("Updated Author");
        assertThat(updated.year).isEqualTo(2024);
        assertThat(updated.keywords).containsExactlyInAnyOrder("updated", "keywords");
    }

    // ========== DELETE/ARCHIVE TESTS ==========

    @Test
    @Order(8)
    @DisplayName("Should archive resource (soft delete)")
    void shouldArchiveResource() {
        // Given: Active resource
        Resource resource = resourceRepository.createResource(
            "Test Book", ResourceType.BOOK, 2020, "Author", null, null, "test"
        );
        Long resourceId = resource.id;

        // When: Archive resource
        resourceRepository.archiveResource(resourceId, "No longer needed", "admin");

        // Then: Resource should be marked as archived
        Resource archived = Resource.findById(resourceId);
        assertThat(archived.archived).isTrue();
        assertThat(archived.archiveReason).isEqualTo("No longer needed");
        assertThat(archived.status).isEqualTo(ResourceStatus.UNAVAILABLE);

        // And: Should not appear in active resources
        List<Resource> active = resourceRepository.findAllActive();
        assertThat(active).noneMatch(r -> r.id.equals(resourceId));
    }

    @Test
    @Order(9)
    @DisplayName("Should restore archived resource")
    @Transactional
    void shouldRestoreArchivedResource() {
        // Given: Archived resource
        Resource resource = resourceRepository.createResource(
            "Test Book", ResourceType.BOOK, 2020, "Author", null, null, "test"
        );
        resourceRepository.archiveResource(resource.id, "Test", "admin");

        // When: Restore resource
        Resource restoredResource = Resource.findById(resource.id);
        restoredResource.restore("admin");

        // Then: Resource should be active again
        Resource found = Resource.findById(resource.id);
        assertThat(found.archived).isFalse();
        assertThat(found.archiveReason).isNull();
        assertThat(found.status).isEqualTo(ResourceStatus.AVAILABLE);
    }

    @Test
    @Order(10)
    @DisplayName("Should permanently delete archived resource")
    @Transactional
    void shouldPermanentlyDeleteArchivedResource() {
        // Given: Archived resource
        Resource resource = resourceRepository.createResource(
            "Test Book", ResourceType.BOOK, 2020, "Author", null, null, "test"
        );
        Long resourceId = resource.id;
        resourceRepository.archiveResource(resourceId, "Test", "admin");

        // When: Permanently delete
        Resource toDelete = Resource.findById(resourceId);
        toDelete.permanentlyDelete();

        // Then: Resource should not exist in database
        Resource notFound = Resource.findById(resourceId);
        assertThat(notFound).isNull();
    }

    // ========== STATUS TESTS ==========

    @Test
    @Order(11)
    @DisplayName("Should change resource status")
    @Transactional
    void shouldChangeResourceStatus() {
        // Given: Resource with AVAILABLE status
        Resource resource = resourceRepository.createResource(
            "Test Book", ResourceType.BOOK, 2020, "Author", null, null, "test"
        );

        // When: Change status to RESERVED
        resource.markReserved("admin");

        // Then: Status should be updated
        Resource found = Resource.findById(resource.id);
        assertThat(found.status).isEqualTo(ResourceStatus.RESERVED);

        // When: Change to UNAVAILABLE
        found.markUnavailable("admin");

        // Then: Status should be updated
        found = Resource.findById(resource.id);
        assertThat(found.status).isEqualTo(ResourceStatus.UNAVAILABLE);
    }

    // ========== PAGINATION TESTS ==========

    @Test
    @Order(12)
    @DisplayName("Should paginate results using Panache")
    void shouldPaginateResultsUsingPanache() {
        // Given: 15 resources
        for (int i = 1; i <= 15; i++) {
            resourceRepository.createResource(
                "Book " + i, ResourceType.BOOK, 2020 + i, "Author " + i, null, null, "test"
            );
        }

        // When: Get first page (page 0, size 5)
        List<Resource> page1 = Resource.findAllPaginated(0, 5);

        // Then: Should return 5 resources
        assertThat(page1).hasSize(5);

        // When: Get second page
        List<Resource> page2 = Resource.findAllPaginated(1, 5);

        // Then: Should return 5 different resources
        assertThat(page2).hasSize(5);
        assertThat(page2).doesNotContainAnyElementsOf(page1);
    }

    // ========== TRANSACTION TESTS ==========

    @Test
    @Order(13)
    @DisplayName("Should maintain transaction isolation")
    @Transactional
    void shouldMaintainTransactionIsolation() {
        // Given: Initial count
        long initialCount = Resource.count();

        // When: Create resource in transaction
        Resource resource = resourceRepository.createResource(
            "Test Book", ResourceType.BOOK, 2020, "Author", null, null, "test"
        );

        // Then: Count should increase
        long newCount = Resource.count();
        assertThat(newCount).isEqualTo(initialCount + 1);

        // Note: If transaction rolls back, changes are undone
    }

    // ========== TIMESTAMP TESTS ==========

    @Test
    @Order(14)
    @DisplayName("Should automatically set timestamps")
    void shouldAutomaticallySetTimestamps() {
        // When: Create resource
        Resource resource = resourceRepository.createResource(
            "Test Book", ResourceType.BOOK, 2020, "Author", null, null, "test"
        );

        // Then: Timestamps should be set
        assertThat(resource.createdAt).isNotNull();
        assertThat(resource.updatedAt).isNotNull();
        assertThat(resource.createdAt).isBeforeOrEqualTo(resource.updatedAt);
    }
}
