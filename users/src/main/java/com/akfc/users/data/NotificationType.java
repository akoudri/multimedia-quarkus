package com.akfc.users.data;

/**
 * Enumeration of notification types in the system.
 *
 * Defines the different types of notifications that users can receive.
 */
public enum NotificationType {
    /**
     * Notification about a new resource created in the catalog.
     */
    RESOURCE_CREATED,

    /**
     * Notification about a resource being updated.
     */
    RESOURCE_UPDATED,

    /**
     * Notification about a resource being deleted.
     */
    RESOURCE_DELETED,

    /**
     * System notification (maintenance, announcements, etc.).
     */
    SYSTEM,

    /**
     * Account-related notification (password reset, verification, etc.).
     */
    ACCOUNT
}
