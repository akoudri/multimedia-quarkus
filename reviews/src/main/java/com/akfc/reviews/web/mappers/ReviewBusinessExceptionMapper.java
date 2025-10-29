package com.akfc.reviews.web.mappers;

import com.akfc.reviews.errors.ReviewBusinessException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

import java.util.Map;

/**
 * Maps ReviewBusinessException to HTTP 409 Conflict response.
 *
 * This handles business rule violations that prevent an operation from completing.
 * The request is syntactically valid and validated, but business logic rejects it.
 *
 * Example scenarios:
 * - Attempting to approve an already approved review
 * - Trying to modify a review by someone other than the author
 * - Flagging a rejected review
 * - Moderation operations by non-moderators
 * - Attempting to update a review that is already APPROVED or REJECTED
 *
 * Example JSON response:
 * {
 *   "message": "Cannot approve an already approved review",
 *   "code": "REVIEW_ALREADY_APPROVED",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "businessRule": "REVIEW_ALREADY_APPROVED"
 *   }
 * }
 */
@Provider
public class ReviewBusinessExceptionMapper implements ExceptionMapper<ReviewBusinessException> {

    @Override
    public Response toResponse(ReviewBusinessException exception) {
        Map<String, Object> details = null;

        // Include business rule code if available
        if (exception.getBusinessRule() != null) {
            details = Map.of("businessRule", exception.getBusinessRule());
        }

        ErrorResponse error = ErrorResponse.of(
            exception.getMessage(),
            exception.getBusinessRule() != null ? exception.getBusinessRule() : "BUSINESS_RULE_VIOLATION",
            details
        );

        return Response
            .status(Response.Status.CONFLICT)  // HTTP 409
            .entity(error)
            .build();
    }
}
