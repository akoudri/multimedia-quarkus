package com.akfc.catalog.services;

import com.akfc.catalog.data.Resource;
import com.akfc.catalog.data.ResourceRepository;
import com.akfc.catalog.data.ResourceStatus;
import com.akfc.catalog.data.ResourceType;
import com.akfc.catalog.dto.CreateResourceRequest;
import com.akfc.catalog.dto.ResourceResponse;
import com.akfc.catalog.dto.UpdateResourceRequest;
import com.akfc.catalog.errors.*;
import com.akfc.catalog.messaging.ResourceEventPublisher;
import io.quarkus.cache.CacheInvalidate;
import io.quarkus.cache.CacheInvalidateAll;
import io.quarkus.cache.CacheResult;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;
import jakarta.validation.Valid;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Business service for Resource management.
 *
 * Handles all business logic, validation, and orchestration for resource operations.
 * Implements complete CRUD lifecycle with business rules enforcement.
 */
@ApplicationScoped
public class ResourceService {

    @Inject
    ResourceRepository resourceRepository;

    @Inject
    ResourceEventPublisher eventPublisher;

    // ========== CREATE ==========

    /**
     * Create a new resource.
     *
     * Business Rules:
     * - All required fields must be provided
     * - Year must be valid (1000-9999)
     * - Title and creator cannot be blank
     *
     * @param request Create request DTO
     * @param createdBy User creating the resource
     * @return Created resource response
     * @throws ResourceValidationException if validation fails
     */
    @Transactional
    @CacheInvalidateAll(cacheName = "all-resources")
    @CacheInvalidateAll(cacheName = "resources-by-type")
    @CacheInvalidateAll(cacheName = "resources-by-keyword")
    public ResourceResponse createResource(@Valid CreateResourceRequest request, String createdBy) {
        // Validate request
        validateCreateRequest(request);

        // Create resource using repository
        Resource resource = resourceRepository.createResource(
            request.title,
            request.type,
            request.year,
            request.creator,
            request.keywords,
            request.illustrationUrl,
            createdBy
        );

        // Publish resource created event to RabbitMQ
        eventPublisher.publishResourceCreated(resource);

        return ResourceResponse.from(resource);
    }

    // ========== READ ==========

    /**
     * Get resource by ID.
     *
     * @param id Resource ID
     * @return Resource response
     * @throws ResourceNotFoundException if resource not found
     */
    @CacheResult(cacheName = "resource-by-id")
    public ResourceResponse getResourceById(Long id) {
        Resource resource = resourceRepository.findActiveById(id);
        if (resource == null) {
            throw new ResourceNotFoundException(id);
        }
        return ResourceResponse.from(resource);
    }

    /**
     * Find a resource entity by ID (for internal service use).
     * Returns the entity itself rather than a DTO.
     *
     * @param id Resource ID
     * @return Resource entity
     * @throws ResourceNotFoundException if resource not found
     */
    @CacheResult(cacheName = "resource-entity-by-id")
    public Resource findResourceEntityById(Long id) {
        Resource resource = resourceRepository.findActiveById(id);
        if (resource == null) {
            throw new ResourceNotFoundException(id);
        }
        return resource;
    }

    /**
     * Get all active resources.
     *
     * @return List of resource responses
     */
    @CacheResult(cacheName = "all-resources")
    public List<ResourceResponse> getAllResources() {
        return resourceRepository.findAllActive()
            .stream()
            .map(ResourceResponse::from)
            .collect(Collectors.toList());
    }

    /**
     * Get resources by type.
     *
     * @param type Resource type
     * @return List of resources of that type
     */
    @CacheResult(cacheName = "resources-by-type")
    public List<ResourceResponse> getResourcesByType(ResourceType type) {
        return Resource.findByType(type)
            .stream()
            .map(ResourceResponse::from)
            .collect(Collectors.toList());
    }

    /**
     * Search resources by keyword.
     *
     * @param keyword Keyword to search for
     * @return List of matching resources
     */
    @CacheResult(cacheName = "resources-by-keyword")
    public List<ResourceResponse> searchByKeyword(String keyword) {
        return Resource.findByKeyword(keyword)
            .stream()
            .map(ResourceResponse::from)
            .collect(Collectors.toList());
    }

    // ========== UPDATE ==========

    /**
     * Update an existing resource.
     *
     * Business Rules:
     * - Resource must exist and not be archived
     * - Cannot change resource type
     * - All validation rules still apply
     *
     * @param id Resource ID
     * @param request Update request DTO
     * @param modifiedBy User making the update
     * @return Updated resource response
     * @throws ResourceNotFoundException if resource not found
     * @throws ResourceAlreadyArchivedException if resource is archived
     */
    @Transactional
    @CacheInvalidate(cacheName = "resource-by-id")
    @CacheInvalidateAll(cacheName = "all-resources")
    @CacheInvalidateAll(cacheName = "resources-by-type")
    @CacheInvalidateAll(cacheName = "resources-by-keyword")
    public ResourceResponse updateResource(Long id, @Valid UpdateResourceRequest request, String modifiedBy) {
        // Find resource
        Resource resource = resourceRepository.findActiveById(id);
        if (resource == null) {
            throw new ResourceNotFoundException(id);
        }

        // Validate request
        validateUpdateRequest(request);

        // Update resource
        resource.update(
            request.title,
            request.year,
            request.creator,
            request.keywords,
            request.illustrationUrl,
            modifiedBy
        );

        return ResourceResponse.from(resource);
    }

    /**
     * Add a keyword to a resource.
     *
     * @param id Resource ID
     * @param keyword Keyword to add
     * @param modifiedBy User making the change
     * @return Updated resource response
     */
    @Transactional
    @CacheInvalidate(cacheName = "resource-by-id")
    @CacheInvalidateAll(cacheName = "all-resources")
    @CacheInvalidateAll(cacheName = "resources-by-keyword")
    public ResourceResponse addKeyword(Long id, String keyword, String modifiedBy) {
        Resource resource = getResourceEntity(id);

        if (keyword == null || keyword.trim().isEmpty()) {
            throw new ResourceValidationException("keyword", "Keyword cannot be empty");
        }

        resource.addKeyword(keyword, modifiedBy);
        return ResourceResponse.from(resource);
    }

    /**
     * Remove a keyword from a resource.
     *
     * @param id Resource ID
     * @param keyword Keyword to remove
     * @param modifiedBy User making the change
     * @return Updated resource response
     */
    @Transactional
    @CacheInvalidate(cacheName = "resource-by-id")
    @CacheInvalidateAll(cacheName = "all-resources")
    @CacheInvalidateAll(cacheName = "resources-by-keyword")
    public ResourceResponse removeKeyword(Long id, String keyword, String modifiedBy) {
        Resource resource = getResourceEntity(id);
        resource.removeKeyword(keyword, modifiedBy);
        return ResourceResponse.from(resource);
    }

    // ========== STATUS MANAGEMENT ==========

    /**
     * Mark resource as available.
     *
     * Business Rule: Cannot mark archived resources as available
     *
     * @param id Resource ID
     * @param modifiedBy User making the change
     * @return Updated resource response
     */
    @Transactional
    @CacheInvalidate(cacheName = "resource-by-id")
    @CacheInvalidateAll(cacheName = "all-resources")
    public ResourceResponse markAsAvailable(Long id, String modifiedBy) {
        Resource resource = getResourceEntity(id);
        resource.markAvailable(modifiedBy);
        return ResourceResponse.from(resource);
    }

    /**
     * Mark resource as unavailable.
     *
     * @param id Resource ID
     * @param modifiedBy User making the change
     * @return Updated resource response
     */
    @Transactional
    @CacheInvalidate(cacheName = "resource-by-id")
    @CacheInvalidateAll(cacheName = "all-resources")
    public ResourceResponse markAsUnavailable(Long id, String modifiedBy) {
        Resource resource = getResourceEntity(id);
        resource.markUnavailable(modifiedBy);
        return ResourceResponse.from(resource);
    }

    /**
     * Mark resource as reserved.
     *
     * Business Rule: Can only reserve available resources
     *
     * @param id Resource ID
     * @param modifiedBy User making the change
     * @return Updated resource response
     */
    @Transactional
    @CacheInvalidate(cacheName = "resource-by-id")
    @CacheInvalidateAll(cacheName = "all-resources")
    public ResourceResponse markAsReserved(Long id, String modifiedBy) {
        Resource resource = getResourceEntity(id);

        if (resource.status != ResourceStatus.AVAILABLE) {
            throw new ResourceBusinessException(
                "Cannot reserve resource that is not available",
                "RESOURCE_NOT_AVAILABLE"
            );
        }

        resource.markReserved(modifiedBy);
        return ResourceResponse.from(resource);
    }

    // ========== DELETE ==========

    /**
     * Archive a resource (soft delete).
     *
     * @param id Resource ID
     * @param reason Reason for archiving
     * @param archivedBy User archiving the resource
     * @return Archived resource response
     */
    @Transactional
    @CacheInvalidate(cacheName = "resource-by-id")
    @CacheInvalidateAll(cacheName = "all-resources")
    @CacheInvalidateAll(cacheName = "resources-by-type")
    @CacheInvalidateAll(cacheName = "resources-by-keyword")
    public ResourceResponse archiveResource(Long id, String reason, String archivedBy) {
        Resource resource = getResourceEntity(id);

        if (reason == null || reason.trim().isEmpty()) {
            throw new ResourceValidationException("reason", "Archive reason is required");
        }

        resource.archive(reason, archivedBy);
        return ResourceResponse.from(resource);
    }

    /**
     * Restore an archived resource.
     *
     * @param id Resource ID
     * @param restoredBy User restoring the resource
     * @return Restored resource response
     */
    @Transactional
    @CacheInvalidate(cacheName = "resource-by-id")
    @CacheInvalidateAll(cacheName = "all-resources")
    @CacheInvalidateAll(cacheName = "resources-by-type")
    @CacheInvalidateAll(cacheName = "resources-by-keyword")
    public ResourceResponse restoreResource(Long id, String restoredBy) {
        Resource resource = resourceRepository.findByIdIncludingArchived(id);
        if (resource == null) {
            throw new ResourceNotFoundException(id);
        }

        if (Boolean.FALSE.equals(resource.archived)) {
            throw new ResourceBusinessException(
                "Resource is not archived: " + id,
                "RESOURCE_NOT_ARCHIVED"
            );
        }

        resource.restore(restoredBy);
        return ResourceResponse.from(resource);
    }

    /**
     * Permanently delete a resource.
     *
     * Business Rule: Resource must be archived first
     *
     * @param id Resource ID
     * @throws ResourceNotFoundException if resource not found
     * @throws ResourceBusinessException if resource not archived
     */
    @Transactional
    @CacheInvalidate(cacheName = "resource-by-id")
    public void deleteResource(Long id) {
        Resource resource = resourceRepository.findByIdIncludingArchived(id);
        if (resource == null) {
            throw new ResourceNotFoundException(id);
        }

        if (Boolean.FALSE.equals(resource.archived)) {
            throw new ResourceBusinessException(
                "Cannot permanently delete non-archived resource. Archive it first.",
                "RESOURCE_NOT_ARCHIVED"
            );
        }

        resource.permanentlyDelete();
    }

    // ========== HELPER METHODS ==========

    /**
     * Get resource entity or throw exception.
     */
    private Resource getResourceEntity(Long id) {
        Resource resource = resourceRepository.findActiveById(id);
        if (resource == null) {
            throw new ResourceNotFoundException(id);
        }
        return resource;
    }

    /**
     * Validate create request.
     */
    private void validateCreateRequest(CreateResourceRequest request) {
        if (request.title == null || request.title.trim().isEmpty()) {
            throw new ResourceValidationException("title", "Title is required");
        }

        if (request.type == null) {
            throw new ResourceValidationException("type", "Type is required");
        }

        if (request.year == null) {
            throw new ResourceValidationException("year", "Year is required");
        }

        if (request.year < 1000 || request.year > 9999) {
            throw new ResourceValidationException("year", "Year must be between 1000 and 9999");
        }

        if (request.creator == null || request.creator.trim().isEmpty()) {
            throw new ResourceValidationException("creator", "Creator is required");
        }
    }

    /**
     * Validate update request.
     */
    private void validateUpdateRequest(UpdateResourceRequest request) {
        if (request.title == null || request.title.trim().isEmpty()) {
            throw new ResourceValidationException("title", "Title is required");
        }

        if (request.year == null) {
            throw new ResourceValidationException("year", "Year is required");
        }

        if (request.year < 1000 || request.year > 9999) {
            throw new ResourceValidationException("year", "Year must be between 1000 and 9999");
        }

        if (request.creator == null || request.creator.trim().isEmpty()) {
            throw new ResourceValidationException("creator", "Creator is required");
        }
    }

    /**
     * Check if resource exists.
     *
     * @param id Resource ID
     * @return True if exists
     */
    public boolean resourceExists(Long id) {
        return resourceRepository.existsById(id);
    }
}
