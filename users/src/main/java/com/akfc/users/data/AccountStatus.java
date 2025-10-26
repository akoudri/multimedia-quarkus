package com.akfc.users.data;

/**
 * Enumeration representing the status of a user account.
 *
 * Account status determines whether a user can access the system
 * and under what conditions.
 */
public enum AccountStatus {
    /**
     * Account is active and user can access the system normally.
     */
    ACTIVE,

    /**
     * Account is suspended (temporarily disabled).
     * User cannot access the system until reactivated.
     * Reason for suspension should be recorded.
     */
    SUSPENDED,

    /**
     * Account is locked due to security concerns.
     * Requires administrator intervention to unlock.
     */
    LOCKED,

    /**
     * Account is awaiting email verification or administrator approval.
     * User cannot fully access the system until verified.
     */
    PENDING_VERIFICATION
}
