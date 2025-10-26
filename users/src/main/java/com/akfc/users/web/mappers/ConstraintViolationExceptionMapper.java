package com.akfc.users.web.mappers;

import jakarta.validation.ConstraintViolation;
import jakarta.validation.ConstraintViolationException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

import java.util.HashMap;
import java.util.Map;

/**
 * Custom mapper for Bean Validation (Jakarta Validation) errors.
 *
 * This customizes the default Quarkus behavior for constraint violations,
 * providing a consistent error format across all validation types.
 *
 * By default, Quarkus returns a plain list of violations. This mapper
 * wraps them in our standard ErrorResponse format.
 *
 * Triggered when:
 * - @Valid annotation triggers validation on request DTOs
 * - @NotNull, @NotBlank, @Size, @Min, @Max, etc. constraints are violated
 * - Method parameters annotated with constraints fail validation
 *
 * Example scenarios:
 * - POST /resources with missing required field (title = null)
 * - POST /resources with invalid size (@Size violated)
 * - POST /resources with year < 1000 (@Min violated)
 *
 * Example JSON response (400):
 * {
 *   "message": "Request validation failed",
 *   "code": "VALIDATION_ERROR",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "violations": {
 *       "title": "Title is required",
 *       "year": "Year must be at least 1000"
 *     }
 *   }
 * }
 *
 * Note: This provides better consistency with our custom validation exceptions.
 */
@Provider
public class ConstraintViolationExceptionMapper implements ExceptionMapper<ConstraintViolationException> {

    @Override
    public Response toResponse(ConstraintViolationException exception) {
        // Extract violations into a field -> message map
        Map<String, String> violations = new HashMap<>();

        for (ConstraintViolation<?> violation : exception.getConstraintViolations()) {
            // Extract field name from property path (e.g., "title" from "createResource.request.title")
            String fieldName = getFieldName(violation.getPropertyPath().toString());
            violations.put(fieldName, violation.getMessage());
        }

        // Create error response with violations
        ErrorResponse error = ErrorResponse.of(
            "Request validation failed",
            "VALIDATION_ERROR",
            Map.of("violations", violations)
        );

        return Response
            .status(Response.Status.BAD_REQUEST)  // HTTP 400
            .entity(error)
            .build();
    }

    /**
     * Extract the field name from a property path.
     *
     * Property path format: "methodName.paramName.fieldName"
     * Examples:
     * - "createResource.request.title" → "title"
     * - "year" → "year"
     */
    private String getFieldName(String propertyPath) {
        if (propertyPath == null || propertyPath.isEmpty()) {
            return "unknown";
        }

        // Get the last segment after the last dot
        int lastDotIndex = propertyPath.lastIndexOf('.');
        if (lastDotIndex >= 0) {
            return propertyPath.substring(lastDotIndex + 1);
        }

        return propertyPath;
    }
}
