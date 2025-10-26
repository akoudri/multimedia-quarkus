package com.akfc.users.web.mappers;

import com.akfc.users.errors.UserNotFoundException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

import java.util.HashMap;
import java.util.Map;

/**
 * Maps UserNotFoundException to HTTP 404 Not Found response.
 *
 * This is a LOCAL exception mapper that handles a specific business exception.
 * It converts the exception into a structured JSON response with appropriate HTTP status.
 *
 * Example scenarios:
 * - Client requests GET /users/999
 * - User ID 999 does not exist
 * - Service throws UserNotFoundException(999)
 * - This mapper converts it to 404 with JSON body
 *
 * - Client requests GET /users/email/unknown@example.com
 * - No user with that email exists
 * - Service throws UserNotFoundException("unknown@example.com")
 * - This mapper converts it to 404 with JSON body
 *
 * Example JSON response (by ID):
 * {
 *   "message": "User not found with ID: 999",
 *   "code": "USER_NOT_FOUND",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "userId": 999
 *   }
 * }
 *
 * Example JSON response (by email):
 * {
 *   "message": "User not found with email: unknown@example.com",
 *   "code": "USER_NOT_FOUND",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "email": "unknown@example.com"
 *   }
 * }
 */
@Provider
public class UserNotFoundExceptionMapper implements ExceptionMapper<UserNotFoundException> {

    @Override
    public Response toResponse(UserNotFoundException exception) {
        Map<String, Object> details = new HashMap<>();

        // Include userId or email in details depending on which is available
        if (exception.getUserId() != null) {
            details.put("userId", exception.getUserId());
        }
        if (exception.getEmail() != null) {
            details.put("email", exception.getEmail());
        }

        ErrorResponse error = ErrorResponse.of(
            exception.getMessage(),
            "USER_NOT_FOUND",
            details.isEmpty() ? null : details
        );

        return Response
            .status(Response.Status.NOT_FOUND)  // HTTP 404
            .entity(error)
            .build();
    }
}
