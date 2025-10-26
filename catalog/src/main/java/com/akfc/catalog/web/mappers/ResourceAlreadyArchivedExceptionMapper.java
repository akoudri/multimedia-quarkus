package com.akfc.catalog.web.mappers;

import com.akfc.catalog.errors.ResourceAlreadyArchivedException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

import java.util.Map;

/**
 * Maps ResourceAlreadyArchivedException to HTTP 409 Conflict.
 *
 * This handles attempts to operate on archived resources.
 * Archived resources are soft-deleted and cannot be modified without restoration.
 *
 * Example scenarios:
 * - Attempting to update an archived resource
 * - Trying to change status of an archived resource
 * - Reserving an archived resource
 *
 * Example JSON response:
 * {
 *   "message": "Resource is already archived: 123",
 *   "code": "RESOURCE_ARCHIVED",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "resourceId": 123
 *   }
 * }
 */
@Provider
public class ResourceAlreadyArchivedExceptionMapper implements ExceptionMapper<ResourceAlreadyArchivedException> {

    @Override
    public Response toResponse(ResourceAlreadyArchivedException exception) {
        Long resourceId = exception.getResourceId();

        Map<String, Object> details = resourceId != null
            ? Map.of("resourceId", resourceId)
            : null;

        ErrorResponse error = ErrorResponse.of(
            exception.getMessage(),
            "RESOURCE_ARCHIVED",
            details
        );

        return Response
            .status(Response.Status.CONFLICT)  // HTTP 409
            .entity(error)
            .build();
    }
}
