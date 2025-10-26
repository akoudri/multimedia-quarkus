package com.akfc.catalog.web.controllers;

import com.akfc.catalog.data.ResourceType;
import com.akfc.catalog.dto.CreateResourceRequest;
import com.akfc.catalog.dto.ResourceResponse;
import com.akfc.catalog.dto.UpdateResourceRequest;
import com.akfc.catalog.errors.ResourceNotFoundException;
import com.akfc.catalog.services.ResourceService;
import io.quarkus.qute.CheckedTemplate;
import io.quarkus.qute.TemplateInstance;
import jakarta.inject.Inject;
import jakarta.validation.Valid;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;

import java.net.URI;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Web Controller for Catalog Resource Management UI.
 *
 * Provides a complete web interface for managing multimedia resources using Qute templates.
 * This controller serves HTML pages with:
 * - Paginated resource listing with search and filter
 * - Resource detail view
 * - Create/Edit forms with validation
 * - Delete operations with confirmation
 * - Success/error message feedback
 *
 * Technology Stack:
 * - Quarkus Qute: Server-side template engine
 * - Bootstrap 5: Responsive CSS framework (via CDN)
 * - FontAwesome: Icon library (via CDN)
 *
 * Routes:
 * - GET  /web/catalog         : List resources with pagination and filters
 * - GET  /web/catalog/{id}    : View resource details
 * - GET  /web/catalog/new     : Show create form
 * - POST /web/catalog         : Create new resource
 * - GET  /web/catalog/{id}/edit : Show edit form
 * - POST /web/catalog/{id}    : Update existing resource
 * - POST /web/catalog/{id}/delete : Delete resource
 *
 * @see com.akfc.catalog.services.ResourceService
 */
@Path("/web/catalog")
@Produces(MediaType.TEXT_HTML)
public class CatalogWebController {

    @Inject
    ResourceService resourceService;

    /**
     * Type-safe Qute templates using @CheckedTemplate.
     *
     * This approach provides compile-time checking of template parameters
     * and better IDE support for template development.
     *
     * Note: requireTypeSafeExpressions is set to false to allow safe navigation operator (?.}
     * and more flexible template expressions.
     */
    @CheckedTemplate(basePath = "catalog", requireTypeSafeExpressions = false)
    public static class Templates {
        /**
         * Main list page template.
         *
         * @param resources List of resources to display
         * @param currentPage Current page number (0-based)
         * @param pageSize Items per page
         * @param totalPages Total number of pages
         * @param searchQuery Search query string (can be null)
         * @param filterType Selected resource type filter (can be null)
         * @param message Success/info message (can be null)
         * @param error Error message (can be null)
         */
        public static native TemplateInstance list(
            List<ResourceResponse> resources,
            int currentPage,
            int pageSize,
            int totalPages,
            int prevPage,
            int nextPage,
            boolean hasPrevPage,
            boolean hasNextPage,
            String searchQuery,
            String filterType,
            String message,
            String error
        );

        /**
         * Resource detail view template.
         *
         * @param resource Resource to display
         * @param message Success/info message (can be null)
         * @param error Error message (can be null)
         */
        public static native TemplateInstance view(
            ResourceResponse resource,
            String message,
            String error
        );

        /**
         * Create/Edit form template.
         *
         * @param resource Resource to edit (null for create)
         * @param isEdit True if editing, false if creating
         * @param resourceTypes Available resource types
         * @param errors Validation errors map (can be null)
         */
        public static native TemplateInstance form(
            ResourceResponse resource,
            boolean isEdit,
            ResourceType[] resourceTypes,
            java.util.Map<String, String> errors
        );
    }

    // ========================================
    // LIST AND SEARCH
    // ========================================

    /**
     * Display paginated list of resources with optional search and filter.
     *
     * Query Parameters:
     * - page: Page number (0-based, default: 0)
     * - size: Items per page (default: 10)
     * - search: Search query for title (optional)
     * - type: Filter by resource type (optional)
     * - message: Success message to display (optional)
     * - error: Error message to display (optional)
     *
     * @param page Current page number
     * @param size Items per page
     * @param searchQuery Search query
     * @param filterType Resource type filter
     * @param message Success message
     * @param error Error message
     * @return Rendered list page
     */
    @GET
    public TemplateInstance list(
            @QueryParam("page") @DefaultValue("0") int page,
            @QueryParam("size") @DefaultValue("10") int size,
            @QueryParam("search") String searchQuery,
            @QueryParam("type") String filterType,
            @QueryParam("message") String message,
            @QueryParam("error") String error
    ) {
        // Validate page and size
        if (page < 0) page = 0;
        if (size < 1) size = 10;
        if (size > 100) size = 100; // Max 100 items per page

        // Get all resources (filtering will be done in-memory for simplicity)
        List<ResourceResponse> allResources = resourceService.getAllResources();

        // Apply search filter
        if (searchQuery != null && !searchQuery.trim().isEmpty()) {
            String query = searchQuery.toLowerCase();
            allResources = allResources.stream()
                .filter(r -> r.title.toLowerCase().contains(query) ||
                            r.creator.toLowerCase().contains(query))
                .collect(Collectors.toList());
        }

        // Apply type filter
        if (filterType != null && !filterType.trim().isEmpty()) {
            try {
                ResourceType type = ResourceType.valueOf(filterType.toUpperCase());
                allResources = allResources.stream()
                    .filter(r -> r.type == type)
                    .collect(Collectors.toList());
            } catch (IllegalArgumentException e) {
                // Invalid type, ignore filter
            }
        }

        // Calculate pagination
        int totalItems = allResources.size();
        int totalPages = (int) Math.ceil((double) totalItems / size);

        // Get current page items
        int startIndex = page * size;
        int endIndex = Math.min(startIndex + size, totalItems);

        List<ResourceResponse> pageResources = startIndex < totalItems
            ? allResources.subList(startIndex, endIndex)
            : List.of();

        // Compute pagination values
        int prevPage = Math.max(0, page - 1);
        int nextPage = page + 1;
        boolean hasPrevPage = page > 0;
        boolean hasNextPage = nextPage < totalPages;

        // Render template
        return Templates.list(
            pageResources,
            page,
            size,
            totalPages,
            prevPage,
            nextPage,
            hasPrevPage,
            hasNextPage,
            searchQuery,
            filterType,
            message,
            error
        );
    }

    // ========================================
    // VIEW DETAILS
    // ========================================

    /**
     * Display detailed view of a single resource.
     *
     * @param id Resource ID
     * @param message Success message (optional)
     * @param error Error message (optional)
     * @return Rendered detail page or redirect
     */
    @GET
    @Path("/{id}")
    public Object view(
            @PathParam("id") Long id,
            @QueryParam("message") String message,
            @QueryParam("error") String error
    ) {
        try {
            ResourceResponse resource = resourceService.getResourceById(id);
            return Templates.view(resource, message, error);
        } catch (ResourceNotFoundException e) {
            // Redirect to list with error message
            return Response.seeOther(
                URI.create("/web/catalog?error=" + encodeMessage("Resource not found: " + id))
            ).build();
        }
    }

    // ========================================
    // CREATE
    // ========================================

    /**
     * Display create form for new resource.
     *
     * @return Rendered create form
     */
    @GET
    @Path("/new")
    public TemplateInstance newForm() {
        return Templates.form(
            null,
            false,
            ResourceType.values(),
            null
        );
    }

    /**
     * Handle resource creation form submission.
     *
     * Form Parameters:
     * - title: Resource title (required)
     * - type: Resource type (required)
     * - year: Publication year (required)
     * - creator: Author/artist name (required)
     * - illustrationUrl: Image URL (optional)
     *
     * @param title Resource title
     * @param type Resource type
     * @param year Publication year
     * @param creator Creator name
     * @param illustrationUrl Image URL
     * @return Redirect to list or re-render form with errors
     */
    @POST
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    public Response create(
            @FormParam("title") String title,
            @FormParam("type") String type,
            @FormParam("year") Integer year,
            @FormParam("creator") String creator,
            @FormParam("illustrationUrl") String illustrationUrl
    ) {
        try {
            // Build request DTO
            CreateResourceRequest request = new CreateResourceRequest();
            request.title = title;
            request.type = type != null ? ResourceType.valueOf(type) : null;
            request.year = year;
            request.creator = creator;
            request.illustrationUrl = illustrationUrl;

            // Create resource
            ResourceResponse resource = resourceService.createResource(request, "web-user");

            // Redirect to detail view with success message
            return Response.seeOther(
                URI.create("/web/catalog/" + resource.id + "?message=" + encodeMessage("Resource created successfully!"))
            ).build();

        } catch (Exception e) {
            // Handle validation or business errors
            // In production, you'd extract field-level errors from the exception
            // For simplicity, redirect back to list with error
            return Response.seeOther(
                URI.create("/web/catalog?error=" + encodeMessage("Failed to create resource: " + e.getMessage()))
            ).build();
        }
    }

    // ========================================
    // EDIT
    // ========================================

    /**
     * Display edit form for existing resource.
     *
     * @param id Resource ID
     * @return Rendered edit form or redirect
     */
    @GET
    @Path("/{id}/edit")
    public Object editForm(@PathParam("id") Long id) {
        try {
            ResourceResponse resource = resourceService.getResourceById(id);
            return Templates.form(
                resource,
                true,
                ResourceType.values(),
                null
            );
        } catch (ResourceNotFoundException e) {
            // Redirect to list with error
            return Response.seeOther(
                URI.create("/web/catalog?error=" + encodeMessage("Resource not found: " + id))
            ).build();
        }
    }

    /**
     * Handle resource update form submission.
     *
     * @param id Resource ID
     * @param title Resource title
     * @param year Publication year
     * @param creator Creator name
     * @param illustrationUrl Image URL
     * @return Redirect to detail view or re-render form with errors
     */
    @POST
    @Path("/{id}")
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    public Response update(
            @PathParam("id") Long id,
            @FormParam("title") String title,
            @FormParam("year") Integer year,
            @FormParam("creator") String creator,
            @FormParam("illustrationUrl") String illustrationUrl
    ) {
        try {
            // Build request DTO
            UpdateResourceRequest request = new UpdateResourceRequest();
            request.title = title;
            request.year = year;
            request.creator = creator;
            request.illustrationUrl = illustrationUrl;

            // Update resource
            ResourceResponse resource = resourceService.updateResource(id, request, "web-user");

            // Redirect to detail view with success message
            return Response.seeOther(
                URI.create("/web/catalog/" + resource.id + "?message=" + encodeMessage("Resource updated successfully!"))
            ).build();

        } catch (ResourceNotFoundException e) {
            return Response.seeOther(
                URI.create("/web/catalog?error=" + encodeMessage("Resource not found: " + id))
            ).build();
        } catch (Exception e) {
            // Redirect back to edit form with error
            return Response.seeOther(
                URI.create("/web/catalog/" + id + "/edit?error=" + encodeMessage("Failed to update: " + e.getMessage()))
            ).build();
        }
    }

    // ========================================
    // DELETE
    // ========================================

    /**
     * Handle resource deletion.
     *
     * Note: Resource must be archived first (business rule).
     * For simplicity in web UI, we'll archive and then delete in one operation.
     *
     * @param id Resource ID
     * @return Redirect to list with success or error message
     */
    @POST
    @Path("/{id}/delete")
    public Response delete(@PathParam("id") Long id) {
        try {
            // Archive first (required by business rules)
            resourceService.archiveResource(id, "Deleted from web UI", "web-user");

            // Then permanently delete
            resourceService.deleteResource(id);

            // Redirect to list with success message
            return Response.seeOther(
                URI.create("/web/catalog?message=" + encodeMessage("Resource deleted successfully!"))
            ).build();

        } catch (ResourceNotFoundException e) {
            return Response.seeOther(
                URI.create("/web/catalog?error=" + encodeMessage("Resource not found: " + id))
            ).build();
        } catch (Exception e) {
            return Response.seeOther(
                URI.create("/web/catalog?error=" + encodeMessage("Failed to delete: " + e.getMessage()))
            ).build();
        }
    }

    // ========================================
    // HELPER METHODS
    // ========================================

    /**
     * URL encode a message for query parameters.
     *
     * @param message Message to encode
     * @return URL-encoded message
     */
    private String encodeMessage(String message) {
        if (message == null) return "";
        try {
            return java.net.URLEncoder.encode(message, "UTF-8");
        } catch (Exception e) {
            return message;
        }
    }
}
