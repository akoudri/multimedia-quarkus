package com.akfc.users.web.mappers;

import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;
import org.jboss.logging.Logger;

import java.util.Map;
import java.util.UUID;

/**
 * Global exception mapper that catches all unhandled exceptions.
 *
 * This is a CATCH-ALL mapper for technical/unexpected errors that don't have
 * a specific mapper. It prevents leaking sensitive stack traces to clients.
 *
 * Key principles:
 * - Returns HTTP 500 for all unhandled errors
 * - Logs full error details server-side for debugging
 * - Returns minimal client-safe information to the user
 * - Includes correlation ID for tracing errors in logs
 *
 * Example scenarios caught:
 * - NullPointerException
 * - Database connection failures
 * - Unexpected RuntimeException
 * - Infrastructure errors
 *
 * Example JSON response:
 * {
 *   "message": "An unexpected error occurred. Please contact support with the correlation ID.",
 *   "code": "INTERNAL_ERROR",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "correlationId": "a1b2c3d4-e5f6-7890-abcd-ef1234567890",
 *     "errorType": "NullPointerException"
 *   }
 * }
 *
 * Note: This mapper has LOW priority (will only catch exceptions not handled by other mappers).
 */
@Provider
public class GlobalExceptionMapper implements ExceptionMapper<Throwable> {

    private static final Logger LOG = Logger.getLogger(GlobalExceptionMapper.class);

    @Override
    public Response toResponse(Throwable throwable) {
        // Let JAX-RS handle WebApplicationException (includes redirects, 404s, etc.)
        // These are intentional HTTP responses, not errors
        if (throwable instanceof WebApplicationException) {
            WebApplicationException wae = (WebApplicationException) throwable;
            // Don't log redirects (3xx) and client errors (4xx) as server errors
            if (wae.getResponse().getStatus() < 500) {
                return wae.getResponse();
            }
        }

        // Generate correlation ID for tracking this specific error
        String correlationId = UUID.randomUUID().toString();

        // Log full error details server-side for debugging
        LOG.errorf(throwable, "Unhandled exception [correlationId=%s]: %s",
            correlationId, throwable.getMessage());

        // Create minimal error response for client
        ErrorResponse error = ErrorResponse.of(
            "An unexpected error occurred. Please contact support with the correlation ID.",
            "INTERNAL_ERROR",
            Map.of(
                "correlationId", correlationId,
                "errorType", throwable.getClass().getSimpleName()
            )
        );

        return Response
            .status(Response.Status.INTERNAL_SERVER_ERROR)  // HTTP 500
            .entity(error)
            .build();
    }
}
