package com.akfc.users.web.mappers;

import com.akfc.users.errors.UserAlreadyExistsException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

import java.util.Map;

/**
 * Maps UserAlreadyExistsException to HTTP 409 Conflict response.
 *
 * This handles attempts to create a user with an email that already exists in the system.
 * Email addresses must be unique across all users to serve as a login identifier.
 *
 * Example scenarios:
 * - Client attempts to register with email "john@example.com"
 * - A user with email "john@example.com" already exists
 * - Service throws UserAlreadyExistsException("john@example.com")
 * - This mapper converts it to 409 with JSON body
 *
 * Example JSON response:
 * {
 *   "message": "User already exists with email: john@example.com",
 *   "code": "USER_ALREADY_EXISTS",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "email": "john@example.com"
 *   }
 * }
 */
@Provider
public class UserAlreadyExistsExceptionMapper implements ExceptionMapper<UserAlreadyExistsException> {

    @Override
    public Response toResponse(UserAlreadyExistsException exception) {
        // Include the conflicting email in details for client reference
        String email = exception.getEmail();

        Map<String, Object> details = email != null
            ? Map.of("email", email)
            : null;

        ErrorResponse error = ErrorResponse.of(
            exception.getMessage(),
            "USER_ALREADY_EXISTS",
            details
        );

        return Response
            .status(Response.Status.CONFLICT)  // HTTP 409
            .entity(error)
            .build();
    }
}
