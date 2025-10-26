package com.akfc.catalog.data;

/**
 * Enumeration representing the availability status of a resource.
 *
 * This enum tracks whether a resource is currently available for use/borrowing.
 */
public enum ResourceStatus {
    /**
     * Resource is available for borrowing/viewing.
     */
    AVAILABLE,

    /**
     * Resource is currently unavailable (e.g., borrowed, under maintenance).
     */
    UNAVAILABLE,

    /**
     * Resource is reserved for a specific user.
     */
    RESERVED
}
