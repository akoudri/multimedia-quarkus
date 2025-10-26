package com.akfc.users.web.mappers;

import com.fasterxml.jackson.annotation.JsonInclude;

import java.time.LocalDateTime;
import java.util.Map;

/**
 * Standard error response structure for all business exceptions.
 *
 * This provides a consistent error format across all API endpoints.
 *
 * Example JSON response:
 * {
 *   "message": "Resource not found with ID: 123",
 *   "code": "RESOURCE_NOT_FOUND",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "resourceId": "123"
 *   }
 * }
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ErrorResponse {

    /**
     * Human-readable error message describing what went wrong.
     */
    public String message;

    /**
     * Machine-readable error code for programmatic handling.
     * Examples: RESOURCE_NOT_FOUND, VALIDATION_ERROR, RESOURCE_ARCHIVED
     */
    public String code;

    /**
     * When the error occurred (ISO 8601 format).
     */
    public LocalDateTime timestamp;

    /**
     * Additional contextual information about the error.
     * Only included when relevant.
     */
    public Map<String, Object> details;

    public ErrorResponse() {
        this.timestamp = LocalDateTime.now();
    }

    public ErrorResponse(String message, String code) {
        this.message = message;
        this.code = code;
        this.timestamp = LocalDateTime.now();
    }

    public ErrorResponse(String message, String code, Map<String, Object> details) {
        this.message = message;
        this.code = code;
        this.timestamp = LocalDateTime.now();
        this.details = details;
    }

    public static ErrorResponse of(String message, String code) {
        return new ErrorResponse(message, code);
    }

    public static ErrorResponse of(String message, String code, Map<String, Object> details) {
        return new ErrorResponse(message, code, details);
    }
}
