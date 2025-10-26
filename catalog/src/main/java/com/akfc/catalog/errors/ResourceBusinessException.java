package com.akfc.catalog.errors;

/**
 * Exception thrown when a business rule is violated.
 *
 * Examples:
 * - Attempting to reserve an unavailable resource
 * - Trying to restore a resource that isn't archived
 * - Deleting a resource that's currently reserved
 *
 * This typically results in HTTP 409 (Conflict) or 422 (Unprocessable Entity) responses.
 */
public class ResourceBusinessException extends ResourceException {

    private final String businessRule;

    public ResourceBusinessException(String message) {
        super(message);
        this.businessRule = null;
    }

    public ResourceBusinessException(String message, String businessRule) {
        super(message);
        this.businessRule = businessRule;
    }

    public String getBusinessRule() {
        return businessRule;
    }
}
