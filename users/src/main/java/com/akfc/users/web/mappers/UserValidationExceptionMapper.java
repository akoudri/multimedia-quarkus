package com.akfc.users.web.mappers;

import com.akfc.users.errors.UserValidationException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

import java.util.HashMap;
import java.util.Map;

/**
 * Maps UserValidationException to HTTP 400 Bad Request response.
 *
 * This handles validation errors specific to business rules (not Bean Validation).
 * Used when request data is syntactically valid but semantically incorrect for user operations.
 *
 * Example scenarios:
 * - Email format is invalid (not a proper email address)
 * - Password doesn't meet complexity requirements
 * - First name or last name contains invalid characters
 * - Phone number format is incorrect
 * - Multiple validation errors occur simultaneously
 *
 * Example JSON response (single field error):
 * {
 *   "message": "Validation failed for field: email",
 *   "code": "VALIDATION_ERROR",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "violations": {
 *       "email": "Email format is invalid"
 *     }
 *   }
 * }
 *
 * Example JSON response (multiple field errors):
 * {
 *   "message": "User validation failed",
 *   "code": "VALIDATION_ERROR",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "violations": {
 *       "email": "Email format is invalid",
 *       "password": "Password must contain at least one uppercase letter",
 *       "phoneNumber": "Phone number format is invalid"
 *     }
 *   }
 * }
 */
@Provider
public class UserValidationExceptionMapper implements ExceptionMapper<UserValidationException> {

    @Override
    public Response toResponse(UserValidationException exception) {
        Map<String, Object> details = new HashMap<>();

        // Include field-level validation errors map if available
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
