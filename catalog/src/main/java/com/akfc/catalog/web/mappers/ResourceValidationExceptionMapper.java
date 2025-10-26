package com.akfc.catalog.web.mappers;

import com.akfc.catalog.errors.ResourceValidationException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

import java.util.HashMap;
import java.util.Map;

/**
 * Maps ResourceValidationException to HTTP 400 Bad Request response.
 *
 * This handles validation errors specific to business rules (not Bean Validation).
 * Used when request data is syntactically valid but semantically incorrect.
 *
 * Example scenarios:
 * - Year is 10000 (out of valid range 1000-9999)
 * - Title is empty string after trimming
 * - Required field is missing
 *
 * Example JSON response:
 * {
 *   "message": "Year must be between 1000 and 9999",
 *   "code": "VALIDATION_ERROR",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "field": "year",
 *     "rejectedValue": "10000",
 *     "violations": {
 *       "year": "Year must be between 1000 and 9999"
 *     }
 *   }
 * }
 */
@Provider
public class ResourceValidationExceptionMapper implements ExceptionMapper<ResourceValidationException> {

    @Override
    public Response toResponse(ResourceValidationException exception) {
        Map<String, Object> details = new HashMap<>();

        // Include validation errors map if available
        if (exception.getFieldErrors() != null && !exception.getFieldErrors().isEmpty()) {
            details.put("violations", exception.getFieldErrors());
        }

        ErrorResponse error = ErrorResponse.of(
            exception.getMessage(),
            "VALIDATION_ERROR",
            details.isEmpty() ? null : details
        );

        return Response
            .status(Response.Status.BAD_REQUEST)  // HTTP 400
            .entity(error)
            .build();
    }
}
