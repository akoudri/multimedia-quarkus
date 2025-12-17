package com.akfc.reviews.web.mappers;

import com.akfc.reviews.errors.ReviewValidationException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

import java.util.HashMap;
import java.util.Map;

/**
 * Maps ReviewValidationException to HTTP 400 Bad Request response.
 *
 * This handles validation errors specific to business rules (not Bean Validation).
 * Used when request data is syntactically valid but semantically incorrect.
 *
 * Example scenarios:
 * - Rating is outside valid range (1-5)
 * - Comment is too short (minimum 10 characters)
 * - Required field is missing
 * - Invalid status transition
 *
 * Example JSON response:
 * {
 *   "message": "Comment must be at least 10 characters",
 *   "code": "VALIDATION_ERROR",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "violations": {
 *       "comment": "Comment must be at least 10 characters"
 *     }
 *   }
 * }
 */
@Provider
public class ReviewValidationExceptionMapper implements ExceptionMapper<ReviewValidationException> {

    @Override
    public Response toResponse(ReviewValidationException exception) {
        Map<String, Object> details = new HashMap<>();

        // Include field errors map if available
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
