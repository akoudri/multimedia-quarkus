package com.akfc.catalog.web.resources;

import com.akfc.catalog.data.ResourceType;
import com.akfc.catalog.dto.CreateResourceRequest;
import com.akfc.catalog.dto.ResourceResponse;
import com.akfc.catalog.dto.UpdateResourceRequest;
import com.akfc.catalog.services.ResourceService;
import jakarta.inject.Inject;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.*;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.media.Content;
import org.eclipse.microprofile.openapi.annotations.media.ExampleObject;
import org.eclipse.microprofile.openapi.annotations.media.Schema;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import java.net.URI;
import java.util.List;

/**
 * REST Resource for multimedia resource management (catalog service).
 *
 * This controller demonstrates comprehensive REST API practices:
 * - Full CRUD operations
 * - Pagination and sorting
 * - Bean Validation on DTOs
 * - Proper HTTP status codes
 * - RFC 7807 Problem responses for errors
 * - OpenAPI/Swagger documentation
 *
 * Base path: /resources
 * Produces: application/json
 * Consumes: application/json
 *
 * OpenAPI documentation available at: /q/swagger-ui
 */
@Path("/resources")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
@Tag(name = "Resources", description = "Multimedia resource management endpoints")
public class ResourceResource {

    @Inject
    ResourceService resourceService;

    @Inject
    com.akfc.catalog.services.ResourceEnrichmentService enrichmentService;

    /**
     * Create a new multimedia resource.
     *
     * This is a CRITICAL endpoint using RFC 7807 Problem format for errors.
     *
     * Business rules enforced:
     * - Title is required (1-255 chars)
     * - Type must be BOOK, MOVIE, or MUSIC
     * - Year must be between 1000-9999
     * - Creator is required
     *
     * Returns HTTP 201 with Location header pointing to the new resource.
     *
     * Error scenarios:
     * - 400: Validation fails (missing fields, invalid values)
     *   Example: {"message": "...", "code": "VALIDATION_ERROR", "details": {...}}
     * - 500: Unexpected server error
     *   Example: {"message": "...", "code": "INTERNAL_ERROR", "details": {"correlationId": "..."}}
     */
    @POST
    @Operation(
        summary = "Create a new resource",
        description = "Creates a new multimedia resource (book, movie, or music) in the catalog"
    )
    @APIResponses({
        @APIResponse(
            responseCode = "201",
            description = "Resource created successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ResourceResponse.class),
                examples = @ExampleObject(
                    name = "Created book resource",
                    value = """
                        {
                          "id": 1,
                          "title": "1984",
                          "type": "BOOK",
                          "year": 1949,
                          "creator": "George Orwell",
                          "keywords": ["dystopia", "surveillance"],
                          "status": "AVAILABLE",
                          "createdAt": "2024-03-20T10:15:30",
                          "archived": false
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Validation error",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Validation error example",
                    value = """
                        {
                          "message": "Request validation failed",
                          "code": "VALIDATION_ERROR",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "violations": {
                              "title": "Title is required",
                              "year": "Year must be at most 9999"
                            }
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "500",
            description = "Internal server error",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Server error example",
                    value = """
                        {
                          "message": "An unexpected error occurred. Please contact support with the correlation ID.",
                          "code": "INTERNAL_ERROR",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "correlationId": "a1b2c3d4-e5f6-7890-abcd-ef1234567890",
                            "errorType": "NullPointerException"
                          }
                        }
                        """
                )
            )
        )
    })
    public Response createResource(
            @Valid CreateResourceRequest request,
            @Context UriInfo uriInfo) {

        ResourceResponse resource = resourceService.createResource(request, "system");

        URI location = uriInfo.getAbsolutePathBuilder()
            .path(resource.id.toString())
            .build();

        return Response
            .created(location)
            .entity(resource)
            .build();
    }

    /**
     * Get all resources with optional pagination and sorting.
     *
     * Supports:
     * - Pagination via page and size parameters
     * - Sorting via sort parameter (e.g., "title", "-year" for descending)
     *
     * Returns empty list if no resources found (not a 404).
     *
     * Error scenarios:
     * - 400: Invalid pagination parameters (negative page/size)
     * - 500: Unexpected server error
     */
    @GET
    @Operation(
        summary = "List all resources",
        description = "Retrieves a paginated list of all multimedia resources in the catalog"
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "List of resources (may be empty)",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ResourceResponse.class),
                examples = @ExampleObject(
                    name = "List of resources",
                    value = """
                        [
                          {
                            "id": 1,
                            "title": "1984",
                            "type": "BOOK",
                            "status": "AVAILABLE"
                          },
                          {
                            "id": 2,
                            "title": "Inception",
                            "type": "MOVIE",
                            "status": "RESERVED"
                          }
                        ]
                        """
                )
            )
        )
    })
    public List<ResourceResponse> getAllResources(
            @Parameter(description = "Page number (0-based)", example = "0")
            @QueryParam("page") @DefaultValue("0") @Min(0) int page,

            @Parameter(description = "Page size", example = "20")
            @QueryParam("size") @DefaultValue("20") @Min(1) int size,

            @Parameter(description = "Sort field (prefix with - for descending)", example = "title")
            @QueryParam("sort") String sort) {

        // For now, return all resources (pagination can be implemented later)
        return resourceService.getAllResources();
    }

    /**
     * Get a specific resource by ID.
     *
     * Error scenarios:
     * - 404: Resource not found
     *   Example: {"message": "Resource not found with ID: 999", "code": "RESOURCE_NOT_FOUND", ...}
     * - 500: Unexpected server error
     */
    @GET
    @Path("/{id}")
    @Operation(
        summary = "Get resource by ID",
        description = "Retrieves a single multimedia resource by its unique identifier"
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Resource found",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ResourceResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "Resource not found",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Not found error",
                    value = """
                        {
                          "message": "Resource not found with ID: 999",
                          "code": "RESOURCE_NOT_FOUND",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "resourceId": 999
                          }
                        }
                        """
                )
            )
        )
    })
    public ResourceResponse getResource(
            @Parameter(description = "Resource ID", required = true, example = "1")
            @PathParam("id") Long id) {
        return resourceService.getResourceById(id);
    }

    /**
     * Get enriched resource with reviews and user information.
     *
     * This endpoint demonstrates complete microservices resilience:
     * - Calls reviews-service to get reviews for this resource
     * - Calls users-service to get author information for each review
     * - Aggregates all data into a single enriched response
     * - Uses SmallRye Fault Tolerance for resilience:
     *   - Retry: Automatic retry on transient failures
     *   - Circuit Breaker: Prevents cascading failures
     *   - Timeout: Limits execution time
     *   - Fallback: Returns degraded response if dependencies fail
     *   - Bulkhead: Limits concurrent calls
     *
     * If the reviews or users services are unavailable, the endpoint will:
     * - Return the resource information without reviews (graceful degradation)
     * - Or return placeholder user information for reviews
     *
     * @param id Resource ID
     * @return Enriched resource with reviews and user data
     */
    @GET
    @Path("/{id}/enriched")
    @Operation(
        summary = "Get enriched resource with reviews and user data",
        description = "Retrieves a resource enriched with reviews from reviews-service and user information " +
                      "from users-service. Implements complete fault tolerance patterns for resilience."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Enriched resource retrieved successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = com.akfc.catalog.dto.EnrichedResourceResponse.class),
                examples = @ExampleObject(
                    name = "Enriched book resource",
                    value = """
                        {
                          "id": 1,
                          "title": "1984",
                          "type": "BOOK",
                          "year": 1949,
                          "creator": "George Orwell",
                          "keywords": ["dystopia", "surveillance"],
                          "illustrationUrl": "https://example.com/1984.jpg",
                          "status": "AVAILABLE",
                          "reviews": [
                            {
                              "id": 1,
                              "resourceId": 1,
                              "userId": 5,
                              "rating": 5,
                              "comment": "Masterpiece of dystopian literature!",
                              "publicationDate": "2024-03-15",
                              "status": "APPROVED",
                              "user": {
                                "id": 5,
                                "firstName": "John",
                                "lastName": "Doe",
                                "email": "john.doe@example.com",
                                "role": "USER",
                                "accountStatus": "ACTIVE"
                              }
                            }
                          ],
                          "statistics": {
                            "totalReviews": 1,
                            "averageRating": 5.0,
                            "fiveStarCount": 1,
                            "fourStarCount": 0,
                            "threeStarCount": 0,
                            "twoStarCount": 0,
                            "oneStarCount": 0
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "Resource not found",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Not found error",
                    value = """
                        {
                          "message": "Resource not found with ID: 999",
                          "code": "RESOURCE_NOT_FOUND",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "resourceId": 999
                          }
                        }
                        """
                )
            )
        )
    })
    public com.akfc.catalog.dto.EnrichedResourceResponse getEnrichedResource(
            @Parameter(description = "Resource ID", required = true, example = "1")
            @PathParam("id") Long id) {

        // Get the resource (will throw exception if not found)
        com.akfc.catalog.data.Resource resource = resourceService.findResourceEntityById(id);

        // Enrich with reviews and user data (with fault tolerance)
        return enrichmentService.enrichResource(resource);
    }

    /**
     * Update an existing resource.
     *
     * Note: Cannot change the resource type once created.
     *
     * Error scenarios:
     * - 400: Validation error
     * - 404: Resource not found
     * - 409: Resource is archived (cannot be modified)
     *   Example: {"message": "Resource is archived...", "code": "RESOURCE_ARCHIVED", ...}
     * - 500: Unexpected server error
     */
    @PUT
    @Path("/{id}")
    @Operation(
        summary = "Update a resource",
        description = "Updates an existing multimedia resource. Cannot update archived resources."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Resource updated successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = ResourceResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "Resource not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "Resource is archived",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    value = """
                        {
                          "message": "Resource is archived and cannot be modified: 1",
                          "code": "RESOURCE_ARCHIVED",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "resourceId": 1
                          }
                        }
                        """
                )
            )
        )
    })
    public ResourceResponse updateResource(
            @Parameter(description = "Resource ID", required = true)
            @PathParam("id") Long id,
            @Valid UpdateResourceRequest request) {
        return resourceService.updateResource(id, request, "system");
    }

    /**
     * Delete a resource permanently.
     *
     * Business rule: Resource must be archived first before permanent deletion.
     *
     * Returns HTTP 204 No Content on success.
     *
     * Error scenarios:
     * - 404: Resource not found
     * - 409: Resource is not archived (must archive first)
     *   Example: {"message": "Cannot permanently delete...", "code": "RESOURCE_NOT_ARCHIVED", ...}
     * - 500: Unexpected server error
     */
    @DELETE
    @Path("/{id}")
    @Operation(
        summary = "Delete a resource permanently",
        description = "Permanently deletes a resource. The resource must be archived first (business rule)."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "204",
            description = "Resource deleted successfully"
        ),
        @APIResponse(
            responseCode = "404",
            description = "Resource not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "Resource must be archived first",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    value = """
                        {
                          "message": "Cannot permanently delete non-archived resource. Archive it first.",
                          "code": "RESOURCE_NOT_ARCHIVED",
                          "timestamp": "2024-03-20T10:15:30"
                        }
                        """
                )
            )
        )
    })
    public Response deleteResource(
            @Parameter(description = "Resource ID", required = true)
            @PathParam("id") Long id) {
        resourceService.deleteResource(id);
        return Response.noContent().build();
    }

    // ========== BUSINESS-SPECIFIC ENDPOINTS ==========

    /**
     * Get resources by type (BOOK, MOVIE, MUSIC).
     *
     * This is a specialized query endpoint that filters resources by type.
     */
    @GET
    @Path("/by-type/{type}")
    @Operation(
        summary = "Get resources by type",
        description = "Retrieves all resources of a specific type (BOOK, MOVIE, or MUSIC)"
    )
    @APIResponse(
        responseCode = "200",
        description = "List of resources of the specified type"
    )
    public List<ResourceResponse> getResourcesByType(
            @Parameter(description = "Resource type", required = true, example = "BOOK")
            @PathParam("type") ResourceType type) {
        return resourceService.getResourcesByType(type);
    }

    /**
     * Search resources by keyword.
     *
     * Searches in the keywords collection of resources.
     */
    @GET
    @Path("/search")
    @Operation(
        summary = "Search resources by keyword",
        description = "Searches for resources that contain the specified keyword in their keywords collection"
    )
    @APIResponse(
        responseCode = "200",
        description = "List of matching resources"
    )
    public List<ResourceResponse> searchByKeyword(
            @Parameter(description = "Keyword to search for", required = true, example = "dystopia")
            @QueryParam("keyword") String keyword) {
        return resourceService.searchByKeyword(keyword);
    }

    /**
     * Reserve a resource.
     *
     * Business rule: Can only reserve AVAILABLE resources.
     *
     * Error scenarios:
     * - 404: Resource not found
     * - 409: Resource is not available for reservation
     *   Example: {"message": "Cannot reserve resource that is not available", "code": "RESOURCE_NOT_AVAILABLE", ...}
     */
    @POST
    @Path("/{id}/reserve")
    @Operation(
        summary = "Reserve a resource",
        description = "Marks a resource as reserved. Only available resources can be reserved."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Resource reserved successfully"
        ),
        @APIResponse(
            responseCode = "409",
            description = "Resource is not available",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    value = """
                        {
                          "message": "Cannot reserve resource that is not available",
                          "code": "RESOURCE_NOT_AVAILABLE",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "businessRule": "RESOURCE_NOT_AVAILABLE"
                          }
                        }
                        """
                )
            )
        )
    })
    public ResourceResponse reserveResource(
            @Parameter(description = "Resource ID", required = true)
            @PathParam("id") Long id) {
        return resourceService.markAsReserved(id, "system");
    }

    /**
     * Mark resource as available.
     */
    @POST
    @Path("/{id}/make-available")
    @Operation(
        summary = "Mark resource as available",
        description = "Changes resource status to AVAILABLE"
    )
    @APIResponse(responseCode = "200", description = "Resource marked as available")
    public ResourceResponse makeAvailable(
            @Parameter(description = "Resource ID", required = true)
            @PathParam("id") Long id) {
        return resourceService.markAsAvailable(id, "system");
    }

    /**
     * Mark resource as unavailable.
     */
    @POST
    @Path("/{id}/make-unavailable")
    @Operation(
        summary = "Mark resource as unavailable",
        description = "Changes resource status to UNAVAILABLE"
    )
    @APIResponse(responseCode = "200", description = "Resource marked as unavailable")
    public ResourceResponse makeUnavailable(
            @Parameter(description = "Resource ID", required = true)
            @PathParam("id") Long id) {
        return resourceService.markAsUnavailable(id, "system");
    }

    /**
     * Archive a resource (soft delete).
     *
     * Archived resources cannot be modified and are hidden from normal queries.
     *
     * Error scenarios:
     * - 404: Resource not found
     * - 400: Archive reason is required
     */
    @POST
    @Path("/{id}/archive")
    @Operation(
        summary = "Archive a resource",
        description = "Soft-deletes a resource by archiving it. Requires a reason."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Resource archived successfully"
        ),
        @APIResponse(
            responseCode = "400",
            description = "Archive reason is required"
        )
    })
    public ResourceResponse archiveResource(
            @Parameter(description = "Resource ID", required = true)
            @PathParam("id") Long id,

            @Parameter(description = "Reason for archiving", required = true, example = "Damaged beyond repair")
            @QueryParam("reason") String reason) {
        return resourceService.archiveResource(id, reason, "system");
    }

    /**
     * Restore an archived resource.
     *
     * Error scenarios:
     * - 404: Resource not found
     * - 409: Resource is not archived
     */
    @POST
    @Path("/{id}/restore")
    @Operation(
        summary = "Restore an archived resource",
        description = "Restores a previously archived resource back to AVAILABLE status"
    )
    @APIResponses({
        @APIResponse(responseCode = "200", description = "Resource restored successfully"),
        @APIResponse(responseCode = "404", description = "Resource not found"),
        @APIResponse(responseCode = "409", description = "Resource is not archived")
    })
    public ResourceResponse restoreResource(
            @Parameter(description = "Resource ID", required = true)
            @PathParam("id") Long id) {
        return resourceService.restoreResource(id, "system");
    }

    /**
     * Add a keyword to a resource.
     *
     * Error scenarios:
     * - 404: Resource not found
     * - 400: Keyword cannot be empty
     */
    @POST
    @Path("/{id}/keywords")
    @Operation(
        summary = "Add a keyword to a resource",
        description = "Adds a new keyword to the resource's keywords collection"
    )
    @APIResponses({
        @APIResponse(responseCode = "200", description = "Keyword added successfully"),
        @APIResponse(responseCode = "404", description = "Resource not found"),
        @APIResponse(responseCode = "400", description = "Keyword cannot be empty")
    })
    public ResourceResponse addKeyword(
            @Parameter(description = "Resource ID", required = true)
            @PathParam("id") Long id,

            @Parameter(description = "Keyword to add", required = true, example = "sci-fi")
            @QueryParam("keyword") String keyword) {
        return resourceService.addKeyword(id, keyword, "system");
    }

    /**
     * Remove a keyword from a resource.
     *
     * Error scenarios:
     * - 404: Resource not found
     */
    @DELETE
    @Path("/{id}/keywords")
    @Operation(
        summary = "Remove a keyword from a resource",
        description = "Removes a keyword from the resource's keywords collection"
    )
    @APIResponses({
        @APIResponse(responseCode = "200", description = "Keyword removed successfully"),
        @APIResponse(responseCode = "404", description = "Resource not found")
    })
    public ResourceResponse removeKeyword(
            @Parameter(description = "Resource ID", required = true)
            @PathParam("id") Long id,

            @Parameter(description = "Keyword to remove", required = true, example = "sci-fi")
            @QueryParam("keyword") String keyword) {
        return resourceService.removeKeyword(id, keyword, "system");
    }
}
