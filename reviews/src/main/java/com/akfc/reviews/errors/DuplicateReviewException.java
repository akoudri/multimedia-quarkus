package com.akfc.reviews.errors;

/**
 * Exception thrown when a user attempts to create multiple reviews for the same resource.
 */
public class DuplicateReviewException extends ReviewException {

    private final Long userId;
    private final Long resourceId;

    public DuplicateReviewException(Long userId, Long resourceId) {
        super(String.format("User %d has already reviewed resource %d", userId, resourceId));
        this.userId = userId;
        this.resourceId = resourceId;
    }

    public Long getUserId() {
        return userId;
    }

    public Long getWorkId() {
        return resourceId;
    }
}
