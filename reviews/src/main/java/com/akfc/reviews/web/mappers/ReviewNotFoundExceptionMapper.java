package com.akfc.reviews.web.mappers;

import com.akfc.reviews.errors.ReviewNotFoundException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

import java.util.Map;

/**
 * Maps ReviewNotFoundException to HTTP 404 Not Found response.
 *
 * This is a LOCAL exception mapper that handles a specific business exception.
 * It converts the exception into a structured JSON response with appropriate HTTP status.
 *
 * Example scenario:
 * - Client requests GET /reviews/999
 * - Review ID 999 does not exist
 * - Service throws ReviewNotFoundException(999)
 * - This mapper converts it to 404 with JSON body
 *
 * Example JSON response:
 * {
 *   "message": "Review not found with ID: 999",
 *   "code": "REVIEW_NOT_FOUND",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "reviewId": 999
 *   }
 * }
 */
@Provider
public class ReviewNotFoundExceptionMapper implements ExceptionMapper<ReviewNotFoundException> {

    @Override
    public Response toResponse(ReviewNotFoundException exception) {
        // Extract review ID from exception for additional context
        Long reviewId = exception.getReviewId();

        // Only include reviewId in details if it's available
        Map<String, Object> details = reviewId != null
            ? Map.of("reviewId", reviewId)
            : null;

        ErrorResponse error = ErrorResponse.of(
            exception.getMessage(),
            "REVIEW_NOT_FOUND",
            details
        );

        return Response
            .status(Response.Status.NOT_FOUND)  // HTTP 404
            .entity(error)
            .build();
    }
}
