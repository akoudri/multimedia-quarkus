package com.akfc.users.web.mappers;

import com.akfc.users.errors.UserBusinessException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

import java.util.Map;

/**
 * Maps UserBusinessException to HTTP 409 Conflict response.
 *
 * This handles business rule violations that prevent a user operation from completing.
 * The request is syntactically valid and validated, but business logic rejects it.
 *
 * Example scenarios:
 * - Attempting to activate an already active user account
 * - Trying to suspend a user who is already suspended
 * - Attempting to lock a user who is already locked
 * - Promoting a user who already has the target role
 * - Status transitions that violate the user lifecycle state machine
 *   (e.g., PENDING_VERIFICATION → SUSPENDED without going through ACTIVE)
 * - Operations on archived users without restoration
 * - Attempting to delete a user account that hasn't been archived first
 *
 * Example JSON response:
 * {
 *   "message": "Cannot activate user: account is already active",
 *   "code": "USER_ALREADY_ACTIVE",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "businessRule": "USER_ALREADY_ACTIVE"
 *   }
 * }
 *
 * Example JSON response (without business rule code):
 * {
 *   "message": "Cannot suspend locked user account",
 *   "code": "BUSINESS_RULE_VIOLATION",
 *   "timestamp": "2024-03-20T10:15:30"
 * }
 */
@Provider
public class UserBusinessExceptionMapper implements ExceptionMapper<UserBusinessException> {

    @Override
    public Response toResponse(UserBusinessException exception) {
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
