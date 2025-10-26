package com.akfc.users.errors;

/**
 * Exception thrown when a user-related business rule is violated.
 *
 * Examples:
 * - Attempting to activate an already active account
 * - Trying to suspend a locked account
 * - Promoting a user who is already a moderator
 * - Account operations on suspended/locked accounts
 */
public class UserBusinessException extends UserException {

    private final String businessRule;

    public UserBusinessException(String message) {
        super(message);
        this.businessRule = null;
    }

    public UserBusinessException(String message, String businessRule) {
        super(message);
        this.businessRule = businessRule;
    }

    public String getBusinessRule() {
        return businessRule;
    }
}
