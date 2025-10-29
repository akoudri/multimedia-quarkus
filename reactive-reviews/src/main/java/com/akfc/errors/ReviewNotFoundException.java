package com.akfc.errors;

/**
 * Exception thrown when a requested review cannot be found.
 */
public class ReviewNotFoundException extends ReviewException {

    private final Long reviewId;

    public ReviewNotFoundException(Long id) {
        super("Review not found with ID: " + id);
        this.reviewId = id;
    }

    public ReviewNotFoundException(String message) {
        super(message);
        this.reviewId = null;
    }

    public Long getReviewId() {
        return reviewId;
    }
}
