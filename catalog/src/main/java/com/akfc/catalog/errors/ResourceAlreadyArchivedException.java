package com.akfc.catalog.errors;

/**
 * Exception thrown when attempting to perform operations on an archived resource.
 *
 * This typically results in HTTP 409 (Conflict) responses.
 */
public class ResourceAlreadyArchivedException extends ResourceException {

    private final Long resourceId;

    public ResourceAlreadyArchivedException(Long id) {
        super("Resource is archived and cannot be modified: " + id);
        this.resourceId = id;
    }

    public ResourceAlreadyArchivedException(String message) {
        super(message);
        this.resourceId = null;
    }

    public Long getResourceId() {
        return resourceId;
    }
}
