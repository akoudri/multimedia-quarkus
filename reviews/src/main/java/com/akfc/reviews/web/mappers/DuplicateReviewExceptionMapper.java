package com.akfc.reviews.web.mappers;

import com.akfc.reviews.errors.DuplicateReviewException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

import java.util.HashMap;
import java.util.Map;

/**
 * Maps DuplicateReviewException to HTTP 409 Conflict response.
 *
 * This handles the business rule that a user can only create one review per resource.
 * Attempting to create a second review for the same resource will trigger this exception.
 *
 * Example scenario:
 * - User 42 attempts to create a second review for resource 123
 * - Service detects existing review by same user for same resource
 * - Service throws DuplicateReviewException(42, 123)
 * - This mapper converts it to 409 with JSON body
 *
 * Example JSON response:
 * {
 *   "message": "User 42 has already reviewed resource 123",
 *   "code": "DUPLICATE_REVIEW",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "userId": 42,
 *     "resourceId": 123
 *   }
 * }
 */
@Provider
public class DuplicateReviewExceptionMapper implements ExceptionMapper<DuplicateReviewException> {

    @Override
    public Response toResponse(DuplicateReviewException exception) {
        Map<String, Object> details = new HashMap<>();

        // Include userId if available
        if (exception.getUserId() != null) {
            details.put("userId", exception.getUserId());
        }

        // Include resourceId if available
        if (exception.getWorkId() != null) {
            details.put("resourceId", exception.getWorkId());
        }

        ErrorResponse error = ErrorResponse.of(
            exception.getMessage(),
            "DUPLICATE_REVIEW",
            details.isEmpty() ? null : details
        );

        return Response
            .status(Response.Status.CONFLICT)  // HTTP 409
            .entity(error)
            .build();
    }
}
