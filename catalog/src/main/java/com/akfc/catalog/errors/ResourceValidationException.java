package com.akfc.catalog.errors;

import java.util.HashMap;
import java.util.Map;

/**
 * Exception thrown when resource data fails validation.
 *
 * This typically results in HTTP 400 (Bad Request) responses.
 * Can contain multiple validation errors for different fields.
 */
public class ResourceValidationException extends ResourceException {

    private final Map<String, String> fieldErrors;

    public ResourceValidationException(String message) {
        super(message);
        this.fieldErrors = new HashMap<>();
    }

    public ResourceValidationException(String message, Map<String, String> fieldErrors) {
        super(message);
        this.fieldErrors = new HashMap<>(fieldErrors);
    }

    public ResourceValidationException(String field, String error) {
        super("Validation failed for field: " + field);
        this.fieldErrors = new HashMap<>();
        this.fieldErrors.put(field, error);
    }

    public Map<String, String> getFieldErrors() {
        return new HashMap<>(fieldErrors);
    }

    public void addFieldError(String field, String error) {
        this.fieldErrors.put(field, error);
    }
}
