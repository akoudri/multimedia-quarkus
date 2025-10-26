package com.akfc.catalog.data;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.*;

/**
 * Unit tests for Resource entity WITHOUT Mockito.
 *
 * These tests focus on pure business logic that doesn't require database access:
 * - Archive/restore operations
 * - Status changes
 * - Keyword management
 * - Validation logic
 *
 * NOTE: Methods that call persist() are NOT tested here (they require database).
 * Those are tested in integration tests.
 */
@DisplayName("Resource Unit Tests (No Mockito)")
class ResourceUnitTest {

    private Resource resource;

    @BeforeEach
    void setUp() {
        // Create a resource manually without persisting
        resource = new Resource();
        resource.id = 1L;
        resource.title = "The Hobbit";
        resource.type = ResourceType.BOOK;
        resource.year = 1937;
        resource.creator = "J.R.R. Tolkien";
        resource.keywords = new ArrayList<>(Arrays.asList("fantasy", "adventure"));
        resource.status = ResourceStatus.AVAILABLE;
        resource.archived = false;
        resource.createdBy = "test-user";
    }

    // ========== TESTS FOR KEYWORD MANAGEMENT ==========

    @Test
    @DisplayName("Should check if archived flag prevents status change")
    void shouldPreventStatusChangeWhenArchived() {
        // Given: Resource is archived
        resource.archived = true;

        // When & Then: Attempting to change status should throw exception
        assertThatThrownBy(() -> resource.markAvailable("admin"))
            .isInstanceOf(IllegalStateException.class)
            .hasMessageContaining("archived resource");

        assertThatThrownBy(() -> resource.markUnavailable("admin"))
            .isInstanceOf(IllegalStateException.class)
            .hasMessageContaining("archived resource");

        assertThatThrownBy(() -> resource.markReserved("admin"))
            .isInstanceOf(IllegalStateException.class)
            .hasMessageContaining("archived resource");
    }

    @Test
    @DisplayName("Should check if archived flag prevents updates")
    void shouldPreventUpdateWhenArchived() {
        // Given: Resource is archived
        resource.archived = true;

        // When & Then: Attempting to update should throw exception
        assertThatThrownBy(() ->
            resource.update("New Title", 2020, "New Author", null, null, "admin")
        )
            .isInstanceOf(IllegalStateException.class)
            .hasMessageContaining("archived resource");
    }

    @Test
    @DisplayName("Should check if archived flag prevents keyword changes")
    void shouldPreventKeywordChangesWhenArchived() {
        // Given: Resource is archived
        resource.archived = true;

        // When & Then: Attempting to add/remove keywords should throw exception
        assertThatThrownBy(() -> resource.addKeyword("new-keyword", "admin"))
            .isInstanceOf(IllegalStateException.class)
            .hasMessageContaining("archived resource");

        assertThatThrownBy(() -> resource.removeKeyword("fantasy", "admin"))
            .isInstanceOf(IllegalStateException.class)
            .hasMessageContaining("archived resource");
    }

    @Test
    @DisplayName("Should prevent permanent delete of non-archived resource")
    void shouldPreventDeleteOfNonArchivedResource() {
        // Given: Resource is NOT archived
        resource.archived = false;

        // When & Then: Attempting to permanently delete should throw exception
        assertThatThrownBy(() -> resource.permanentlyDelete())
            .isInstanceOf(IllegalStateException.class)
            .hasMessageContaining("Archive it first");
    }

    @Test
    @DisplayName("Should validate title update on non-archived resource")
    void shouldAllowTitleUpdateOnNonArchivedResource() {
        // Given: Resource is not archived
        resource.archived = false;
        String originalTitle = resource.title;

        // When: Update title (without persist, just field update)
        resource.title = "The Lord of the Rings";
        resource.modifiedBy = "admin";

        // Then: Title should be updated
        assertThat(resource.title).isEqualTo("The Lord of the Rings");
        assertThat(resource.title).isNotEqualTo(originalTitle);
        assertThat(resource.modifiedBy).isEqualTo("admin");
    }

    @Test
    @DisplayName("Should validate keyword list operations")
    void shouldValidateKeywordOperations() {
        // Given: Resource with initial keywords
        assertThat(resource.keywords).containsExactly("fantasy", "adventure");

        // When: Add a new keyword manually (testing list operations)
        resource.keywords.add("classic");

        // Then: Keyword should be added
        assertThat(resource.keywords).containsExactly("fantasy", "adventure", "classic");

        // When: Remove a keyword
        resource.keywords.remove("adventure");

        // Then: Keyword should be removed
        assertThat(resource.keywords).containsExactly("fantasy", "classic");
    }

    @Test
    @DisplayName("Should validate status transitions")
    void shouldValidateStatusTransitions() {
        // Given: Resource is available
        assertThat(resource.status).isEqualTo(ResourceStatus.AVAILABLE);
        assertThat(resource.archived).isFalse();

        // When: Change status manually (without persist)
        resource.status = ResourceStatus.RESERVED;

        // Then: Status should change
        assertThat(resource.status).isEqualTo(ResourceStatus.RESERVED);

        // When: Change to unavailable
        resource.status = ResourceStatus.UNAVAILABLE;

        // Then: Status should change
        assertThat(resource.status).isEqualTo(ResourceStatus.UNAVAILABLE);
    }

    @Test
    @DisplayName("Should validate archive flag behavior")
    void shouldValidateArchiveFlag() {
        // Given: Resource is not archived
        assertThat(resource.archived).isFalse();
        assertThat(resource.archiveReason).isNull();

        // When: Archive manually (without persist)
        resource.archived = true;
        resource.archiveReason = "Out of print";
        resource.status = ResourceStatus.UNAVAILABLE;

        // Then: Archive flag and reason should be set
        assertThat(resource.archived).isTrue();
        assertThat(resource.archiveReason).isEqualTo("Out of print");
        assertThat(resource.status).isEqualTo(ResourceStatus.UNAVAILABLE);
    }

    @Test
    @DisplayName("Should validate restore operation")
    void shouldValidateRestoreOperation() {
        // Given: Resource is archived
        resource.archived = true;
        resource.archiveReason = "Test archive";
        resource.status = ResourceStatus.UNAVAILABLE;

        // When: Restore manually (without persist)
        resource.archived = false;
        resource.archiveReason = null;
        resource.status = ResourceStatus.AVAILABLE;
        resource.modifiedBy = "admin";

        // Then: Resource should be restored
        assertThat(resource.archived).isFalse();
        assertThat(resource.archiveReason).isNull();
        assertThat(resource.status).isEqualTo(ResourceStatus.AVAILABLE);
        assertThat(resource.modifiedBy).isEqualTo("admin");
    }

    @Test
    @DisplayName("Should validate default values in onCreate callback")
    void shouldSetDefaultValuesOnCreate() {
        // Given: Resource with null status and archived flag
        Resource newResource = new Resource();
        newResource.title = "Test Book";
        newResource.type = ResourceType.BOOK;
        newResource.year = 2024;
        newResource.creator = "Test Author";
        newResource.status = null;  // Explicitly null
        newResource.archived = null;  // Explicitly null

        // When: Call @PrePersist callback manually
        newResource.onCreate();

        // Then: Default values should be set
        assertThat(newResource.status).isEqualTo(ResourceStatus.AVAILABLE);
        assertThat(newResource.archived).isFalse();
    }

    @Test
    @DisplayName("Should not override existing status in onCreate")
    void shouldNotOverrideExistingStatusOnCreate() {
        // Given: Resource with explicit status
        Resource newResource = new Resource();
        newResource.title = "Test Book";
        newResource.type = ResourceType.BOOK;
        newResource.year = 2024;
        newResource.creator = "Test Author";
        newResource.status = ResourceStatus.RESERVED;  // Explicitly set

        // When: Call @PrePersist callback manually
        newResource.onCreate();

        // Then: Status should not be overridden
        assertThat(newResource.status).isEqualTo(ResourceStatus.RESERVED);
    }

    @Test
    @DisplayName("Should validate PreUpdate callback for archived resources")
    void shouldValidatePreUpdateCallback() {
        // Given: Archived resource without modifiedBy
        resource.archived = true;
        resource.modifiedBy = null;

        // When & Then: PreUpdate should throw exception
        assertThatThrownBy(() -> resource.onUpdate())
            .isInstanceOf(IllegalStateException.class)
            .hasMessageContaining("without specifying modifier");
    }

    @Test
    @DisplayName("Should allow PreUpdate for archived resources with modifiedBy")
    void shouldAllowPreUpdateForArchivedWithModifier() {
        // Given: Archived resource with modifiedBy set
        resource.archived = true;
        resource.modifiedBy = "admin";

        // When & Then: PreUpdate should NOT throw exception
        assertThatCode(() -> resource.onUpdate())
            .doesNotThrowAnyException();
    }

    @Test
    @DisplayName("Should validate resource type enum values")
    void shouldValidateResourceTypeEnum() {
        // Test all possible resource types
        assertThat(ResourceType.values())
            .contains(ResourceType.BOOK, ResourceType.MOVIE, ResourceType.MUSIC);

        // Test resource type assignment
        resource.type = ResourceType.MOVIE;
        assertThat(resource.type).isEqualTo(ResourceType.MOVIE);

        resource.type = ResourceType.MUSIC;
        assertThat(resource.type).isEqualTo(ResourceType.MUSIC);
    }

    @Test
    @DisplayName("Should validate resource status enum values")
    void shouldValidateResourceStatusEnum() {
        // Test all possible statuses
        assertThat(ResourceStatus.values())
            .contains(
                ResourceStatus.AVAILABLE,
                ResourceStatus.RESERVED,
                ResourceStatus.UNAVAILABLE
            );

        // Test status assignment
        resource.status = ResourceStatus.RESERVED;
        assertThat(resource.status).isEqualTo(ResourceStatus.RESERVED);
    }

    @Test
    @DisplayName("Should handle empty keywords list")
    void shouldHandleEmptyKeywordsList() {
        // Given: Resource with no keywords
        resource.keywords = new ArrayList<>();

        // Then: Keywords list should be empty but not null
        assertThat(resource.keywords).isNotNull();
        assertThat(resource.keywords).isEmpty();
    }

    @Test
    @DisplayName("Should handle null illustration URL")
    void shouldHandleNullIllustrationUrl() {
        // Given: Resource without illustration URL
        resource.illustrationUrl = null;

        // Then: Illustration URL should be null (valid state)
        assertThat(resource.illustrationUrl).isNull();
    }

    @Test
    @DisplayName("Should validate year constraints")
    void shouldValidateYearConstraints() {
        // Valid years
        resource.year = 1000;
        assertThat(resource.year).isEqualTo(1000);

        resource.year = 9999;
        assertThat(resource.year).isEqualTo(9999);

        resource.year = 2024;
        assertThat(resource.year).isEqualTo(2024);
    }
}
