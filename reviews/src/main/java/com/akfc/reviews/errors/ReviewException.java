package com.akfc.reviews.errors;

/**
 * Base exception for all review-related errors.
 */
public class ReviewException extends RuntimeException {

    public ReviewException(String message) {
        super(message);
    }

    public ReviewException(String message, Throwable cause) {
        super(message, cause);
    }
}
