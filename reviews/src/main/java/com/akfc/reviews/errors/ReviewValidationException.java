package com.akfc.reviews.errors;

import java.util.HashMap;
import java.util.Map;

/**
 * Exception thrown when review data fails validation.
 */
public class ReviewValidationException extends ReviewException {

    private final Map<String, String> fieldErrors;

    public ReviewValidationException(String message) {
        super(message);
        this.fieldErrors = new HashMap<>();
    }

    public ReviewValidationException(String message, Map<String, String> fieldErrors) {
        super(message);
        this.fieldErrors = new HashMap<>(fieldErrors);
    }

    public ReviewValidationException(String field, String error) {
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
