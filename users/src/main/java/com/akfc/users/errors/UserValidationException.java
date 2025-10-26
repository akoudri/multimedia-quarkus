package com.akfc.users.errors;

import java.util.HashMap;
import java.util.Map;

/**
 * Exception thrown when user data fails validation.
 */
public class UserValidationException extends UserException {

    private final Map<String, String> fieldErrors;

    public UserValidationException(String message) {
        super(message);
        this.fieldErrors = new HashMap<>();
    }

    public UserValidationException(String message, Map<String, String> fieldErrors) {
        super(message);
        this.fieldErrors = new HashMap<>(fieldErrors);
    }

    public UserValidationException(String field, String error) {
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
