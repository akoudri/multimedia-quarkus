package com.akfc.catalog.errors;

/**
 * Exception thrown when a requested resource cannot be found.
 *
 * This typically results in HTTP 404 responses.
 */
public class ResourceNotFoundException extends ResourceException {

    private final Long resourceId;

    public ResourceNotFoundException(Long id) {
        super("Resource not found with ID: " + id);
        this.resourceId = id;
    }

    public ResourceNotFoundException(String message) {
        super(message);
        this.resourceId = null;
    }

    public Long getResourceId() {
        return resourceId;
    }
}
