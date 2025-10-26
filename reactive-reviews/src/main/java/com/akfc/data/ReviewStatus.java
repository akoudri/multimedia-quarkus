package com.akfc.data;

/**
 * Enumeration representing the moderation status of a review.
 *
 * Review Lifecycle:
 * 1. PENDING → Initial state when review is created
 * 2. APPROVED → Moderator approves the review (visible to all users)
 * 3. REJECTED → Moderator rejects the review (hidden from public)
 * 4. FLAGGED → Review flagged by user or moderator for attention (spam, inappropriate content, etc.)
 *
 * Status Transitions:
 * - PENDING → APPROVED (normal approval flow)
 * - PENDING → REJECTED (review doesn't meet guidelines)
 * - PENDING → FLAGGED (needs further review)
 * - APPROVED → FLAGGED (user reports issue)
 * - FLAGGED → APPROVED (reviewed and cleared)
 * - FLAGGED → REJECTED (confirmed as inappropriate)
 */
public enum ReviewStatus {
    /**
     * Review is awaiting moderation.
     * Default state for newly created reviews.
     */
    PENDING,

    /**
     * Review has been approved by a moderator.
     * Visible to all users.
     */
    APPROVED,

    /**
     * Review has been rejected by a moderator.
     * Not visible to users (except the author).
     */
    REJECTED,

    /**
     * Review has been flagged for attention.
     * Possible reasons: spam, inappropriate language, off-topic, etc.
     * Requires moderator review.
     */
    FLAGGED
}
