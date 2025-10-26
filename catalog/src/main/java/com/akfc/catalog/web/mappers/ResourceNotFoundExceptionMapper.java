package com.akfc.catalog.web.mappers;

import com.akfc.catalog.errors.ResourceNotFoundException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

import java.util.Map;

/**
 * Maps ResourceNotFoundException to HTTP 404 Not Found response.
 *
 * This is a LOCAL exception mapper that handles a specific business exception.
 * It converts the exception into a structured JSON response with appropriate HTTP status.
 *
 * Example scenario:
 * - Client requests GET /resources/999
 * - Resource ID 999 does not exist
 * - Service throws ResourceNotFoundException(999)
 * - This mapper converts it to 404 with JSON body
 *
 * Example JSON response:
 * {
 *   "message": "Resource not found with ID: 999",
 *   "code": "RESOURCE_NOT_FOUND",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "resourceId": 999
 *   }
 * }
 */
@Provider
public class ResourceNotFoundExceptionMapper implements ExceptionMapper<ResourceNotFoundException> {

    @Override
    public Response toResponse(ResourceNotFoundException exception) {
        // Extract resource ID from exception for additional context
        Long resourceId = exception.getResourceId();

        ErrorResponse error = ErrorResponse.of(
            exception.getMessage(),
            "RESOURCE_NOT_FOUND",
            Map.of("resourceId", resourceId)
        );

        return Response
            .status(Response.Status.NOT_FOUND)  // HTTP 404
            .entity(error)
            .build();
    }
}
