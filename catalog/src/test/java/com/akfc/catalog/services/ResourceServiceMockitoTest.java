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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;
import static org.mockito.AdditionalMatchers.gt;

/**
 * Unit tests for ResourceService using Mockito.
 *
 * These tests demonstrate:
 * - Mocking dependencies (ResourceRepository)
 * - Verifying method calls
 * - Testing service business logic in isolation
 * - Exception handling
 * - Stubbing repository responses
 */
@ExtendWith(MockitoExtension.class)
@DisplayName("ResourceService Unit Tests (With Mockito)")
class ResourceServiceMockitoTest {

    @Mock
    private ResourceRepository resourceRepository;

    @InjectMocks
    private ResourceService resourceService;

    private Resource testResource;
    private CreateResourceRequest createRequest;
    private UpdateResourceRequest updateRequest;

    @BeforeEach
    void setUp() {
        // Create test resource
        testResource = new Resource();
        testResource.id = 1L;
        testResource.title = "The Hobbit";
        testResource.type = ResourceType.BOOK;
        testResource.year = 1937;
        testResource.creator = "J.R.R. Tolkien";
        testResource.keywords = new ArrayList<>(Arrays.asList("fantasy", "adventure"));
        testResource.illustrationUrl = "http://example.com/hobbit.jpg";
        testResource.status = ResourceStatus.AVAILABLE;
        testResource.archived = false;
        testResource.createdAt = LocalDateTime.now();
        testResource.updatedAt = LocalDateTime.now();
        testResource.createdBy = "test-user";

        // Create test DTOs
        createRequest = new CreateResourceRequest();
        createRequest.title = "The Hobbit";
        createRequest.type = ResourceType.BOOK;
        createRequest.year = 1937;
        createRequest.creator = "J.R.R. Tolkien";
        createRequest.keywords = Arrays.asList("fantasy", "adventure");
        createRequest.illustrationUrl = "http://example.com/hobbit.jpg";

        updateRequest = new UpdateResourceRequest();
        updateRequest.title = "The Hobbit (Updated)";
        updateRequest.year = 1937;
        updateRequest.creator = "J.R.R. Tolkien";
        updateRequest.keywords = Arrays.asList("fantasy", "adventure", "classic");
        updateRequest.illustrationUrl = "http://example.com/hobbit-new.jpg";
    }

    // ========== CREATE TESTS ==========

    @Test
    @DisplayName("Should create resource successfully")
    void shouldCreateResourceSuccessfully() {
        // Given: Repository will return a created resource
        when(resourceRepository.createResource(
            anyString(), any(ResourceType.class), anyInt(),
            anyString(), anyList(), anyString(), anyString()
        )).thenReturn(testResource);

        // When: Create resource via service
        ResourceResponse response = resourceService.createResource(createRequest, "test-user");

        // Then: Response should match created resource
        assertThat(response).isNotNull();
        assertThat(response.id).isEqualTo(1L);
        assertThat(response.title).isEqualTo("The Hobbit");
        assertThat(response.type).isEqualTo(ResourceType.BOOK);

        // Verify: Repository method was called exactly once with correct parameters
        verify(resourceRepository, times(1)).createResource(
            eq("The Hobbit"),
            eq(ResourceType.BOOK),
            eq(1937),
            eq("J.R.R. Tolkien"),
            eq(Arrays.asList("fantasy", "adventure")),
            eq("http://example.com/hobbit.jpg"),
            eq("test-user")
        );
    }

    @Test
    @DisplayName("Should create resource without optional fields")
    void shouldCreateResourceWithoutOptionalFields() {
        // Given: Request without keywords and illustration
        createRequest.keywords = null;
        createRequest.illustrationUrl = null;
        testResource.keywords = new ArrayList<>();
        testResource.illustrationUrl = null;

        when(resourceRepository.createResource(
            anyString(), any(ResourceType.class), anyInt(),
            anyString(), any(), any(), anyString()
        )).thenReturn(testResource);

        // When: Create resource
        ResourceResponse response = resourceService.createResource(createRequest, "test-user");

        // Then: Resource should be created
        assertThat(response).isNotNull();
        assertThat(response.keywords).isEmpty();

        // Verify: Repository was called with null values for optional fields
        verify(resourceRepository).createResource(
            eq("The Hobbit"),
            eq(ResourceType.BOOK),
            eq(1937),
            eq("J.R.R. Tolkien"),
            isNull(),
            isNull(),
            eq("test-user")
        );
    }

    // ========== READ TESTS ==========

    @Test
    @DisplayName("Should get resource by ID successfully")
    void shouldGetResourceByIdSuccessfully() {
        // Given: Repository returns resource
        when(resourceRepository.findActiveById(1L)).thenReturn(testResource);

        // When: Get resource by ID
        ResourceResponse response = resourceService.getResourceById(1L);

        // Then: Response should match resource
        assertThat(response).isNotNull();
        assertThat(response.id).isEqualTo(1L);
        assertThat(response.title).isEqualTo("The Hobbit");

        // Verify: Repository was called once
        verify(resourceRepository, times(1)).findActiveById(1L);
    }

    @Test
    @DisplayName("Should throw ResourceNotFoundException when resource not found")
    void shouldThrowNotFoundExceptionWhenResourceNotFound() {
        // Given: Repository returns null
        when(resourceRepository.findActiveById(999L)).thenReturn(null);

        // When & Then: Should throw exception
        assertThatThrownBy(() -> resourceService.getResourceById(999L))
            .isInstanceOf(ResourceNotFoundException.class)
            .hasMessageContaining("999");

        // Verify: Repository was called
        verify(resourceRepository, times(1)).findActiveById(999L);
    }

    @Test
    @DisplayName("Should get all resources successfully")
    void shouldGetAllResourcesSuccessfully() {
        // Given: Repository returns list of resources
        Resource resource2 = new Resource();
        resource2.id = 2L;
        resource2.title = "1984";
        resource2.type = ResourceType.BOOK;
        resource2.year = 1949;
        resource2.creator = "George Orwell";
        resource2.keywords = new ArrayList<>();
        resource2.status = ResourceStatus.AVAILABLE;
        resource2.archived = false;

        List<Resource> resources = Arrays.asList(testResource, resource2);
        when(resourceRepository.findAllActive()).thenReturn(resources);

        // When: Get all resources
        List<ResourceResponse> responses = resourceService.getAllResources();

        // Then: Should return all resources
        assertThat(responses).hasSize(2);
        assertThat(responses.get(0).title).isEqualTo("The Hobbit");
        assertThat(responses.get(1).title).isEqualTo("1984");

        // Verify: Repository was called once
        verify(resourceRepository, times(1)).findAllActive();
    }

    @Test
    @DisplayName("Should return empty list when no resources found")
    void shouldReturnEmptyListWhenNoResourcesFound() {
        // Given: Repository returns empty list
        when(resourceRepository.findAllActive()).thenReturn(new ArrayList<>());

        // When: Get all resources
        List<ResourceResponse> responses = resourceService.getAllResources();

        // Then: Should return empty list
        assertThat(responses).isEmpty();

        // Verify: Repository was called
        verify(resourceRepository, times(1)).findAllActive();
    }

    // ========== UPDATE TESTS ==========

    // NOTE: Update test disabled because it requires database context
    // The service calls resource.update() which calls persist() - a Panache method requiring database
    // This test is better suited for integration testing with @QuarkusTest
    // See ResourceServiceIntegrationTest for the full integration test
    /*
    @Test
    @DisplayName("Should update resource successfully")
    void shouldUpdateResourceSuccessfully() {
        // This test cannot resource with Mockito alone because:
        // 1. Service finds resource via repository (mockable)
        // 2. Service calls resource.update() on entity (not mockable without PowerMock)
        // 3. Entity's update() calls this.persist() which needs database
        // Solution: Use @QuarkusTest integration test instead
    }
    */

    @Test
    @DisplayName("Should throw exception when updating non-existent resource")
    void shouldThrowExceptionWhenUpdatingNonExistentResource() {
        // Given: Repository returns null
        when(resourceRepository.findActiveById(999L)).thenReturn(null);

        // When & Then: Should throw exception
        assertThatThrownBy(() -> resourceService.updateResource(999L, updateRequest, "admin"))
            .isInstanceOf(ResourceNotFoundException.class);

        // Verify: Only findActiveById was called
        verify(resourceRepository, times(1)).findActiveById(999L);
    }

    @Test
    @DisplayName("Should throw exception when updating archived resource")
    void shouldThrowExceptionWhenUpdatingArchivedResource() {
        // Given: Resource is archived
        testResource.archived = true;
        when(resourceRepository.findActiveById(1L)).thenReturn(testResource);

        // When & Then: Should throw exception from entity's update() method
        assertThatThrownBy(() -> resourceService.updateResource(1L, updateRequest, "admin"))
            .isInstanceOf(IllegalStateException.class)  // Entity throws IllegalStateException
            .hasMessageContaining("archived resource");

        // Verify: findActiveById was called
        verify(resourceRepository, times(1)).findActiveById(1L);
    }

    // ========== DELETE/ARCHIVE TESTS ==========

    // NOTE: Archive test disabled because it requires database context
    // The service calls repository.archiveResource() which calls entity.archive() and persist()
    // See ResourceServiceIntegrationTest for the full integration test
    /*
    @Test
    @DisplayName("Should archive resource successfully")
    void shouldArchiveResourceSuccessfully() {
        // This test requires database context for Panache persist() method
        // Use @QuarkusTest integration test instead
    }
    */

    @Test
    @DisplayName("Should throw exception when archiving non-existent resource")
    void shouldThrowExceptionWhenArchivingNonExistentResource() {
        // Given: Repository returns null
        when(resourceRepository.findActiveById(999L)).thenReturn(null);

        // When & Then: Should throw exception
        assertThatThrownBy(() -> resourceService.archiveResource(999L, "Reason", "admin"))
            .isInstanceOf(ResourceNotFoundException.class);

        // Verify: Only findActiveById was called
        verify(resourceRepository, times(1)).findActiveById(999L);
        verify(resourceRepository, never()).archiveResource(anyLong(), anyString(), anyString());
    }

    // ========== STATUS CHANGE TESTS ==========
    // Note: ResourceService doesn't have a changeStatus() method
    // Status changes are done through entity methods: markAvailable(), markReserved(), markUnavailable()
    // These are tested in ResourceUnitTest and integration tests

    // ========== MOCKITO VERIFICATION TESTS ==========

    @Test
    @DisplayName("Should verify no interactions when method not called")
    void shouldVerifyNoInteractions() {
        // Given: Fresh mock with no interactions

        // Then: Verify no methods were called
        verifyNoInteractions(resourceRepository);
    }

    @Test
    @DisplayName("Should verify exact number of invocations")
    void shouldVerifyExactNumberOfInvocations() {
        // Given: Repository configured
        when(resourceRepository.findActiveById(1L)).thenReturn(testResource);

        // When: Call method multiple times
        resourceService.getResourceById(1L);
        resourceService.getResourceById(1L);
        resourceService.getResourceById(1L);

        // Then: Verify exact number of calls
        verify(resourceRepository, times(3)).findActiveById(1L);
    }

    @Test
    @DisplayName("Should verify method was never called")
    void shouldVerifyMethodWasNeverCalled() {
        // Given: Repository configured for one method only
        when(resourceRepository.findActiveById(1L)).thenReturn(testResource);

        // When: Call only getResourceById
        resourceService.getResourceById(1L);

        // Then: Verify other methods were never called
        verify(resourceRepository, never()).findAllActive();
        verify(resourceRepository, never()).createResource(
            anyString(), any(), anyInt(), anyString(), any(), any(), anyString()
        );
    }

    @Test
    @DisplayName("Should verify argument matchers")
    void shouldVerifyArgumentMatchers() {
        // Given: Repository configured
        when(resourceRepository.createResource(
            anyString(), any(), anyInt(), anyString(), any(), any(), anyString()
        )).thenReturn(testResource);

        // When: Create resource
        resourceService.createResource(createRequest, "test-user");

        // Then: Verify with argument matchers
        verify(resourceRepository).createResource(
            contains("Hobbit"),      // Title contains "Hobbit"
            eq(ResourceType.BOOK),    // Exact type match
            gt(1900),                 // Year greater than 1900
            anyString(),              // Any creator
            anyList(),                // Any keywords list
            startsWith("http://"),    // URL starts with http://
            eq("test-user")           // Exact user match
        );
    }
}
