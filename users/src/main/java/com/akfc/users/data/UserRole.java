package com.akfc.users.data;

/**
 * Enumeration representing the role/permission level of a user.
 *
 * Roles determine what actions a user can perform in the system.
 */
public enum UserRole {
    /**
     * Regular user with basic permissions.
     * Can browse catalog, write reviews, etc.
     */
    USER,

    /**
     * Moderator with elevated permissions.
     * Can manage resources, moderate reviews, manage users, etc.
     */
    MODERATOR
}
