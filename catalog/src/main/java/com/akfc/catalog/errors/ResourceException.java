package com.akfc.catalog.errors;

/**
 * Base exception for all resource-related errors.
 *
 * This is a runtime exception that serves as the parent class
 * for all custom exceptions in the catalog service.
 */
public class ResourceException extends RuntimeException {

    public ResourceException(String message) {
        super(message);
    }

    public ResourceException(String message, Throwable cause) {
        super(message, cause);
    }
}
