package com.akfc.errors;

/**
 * Exception thrown when a review-related business rule is violated.
 *
 * Examples:
 * - Attempting to approve an already approved review
 * - Trying to modify a review by someone other than the author
 * - Flagging a rejected review
 * - Moderation operations by non-moderators
 */
public class ReviewBusinessException extends ReviewException {

    private final String businessRule;

    public ReviewBusinessException(String message) {
        super(message);
        this.businessRule = null;
    }

    public ReviewBusinessException(String message, String businessRule) {
        super(message);
        this.businessRule = businessRule;
    }

    public String getBusinessRule() {
        return businessRule;
    }
}
