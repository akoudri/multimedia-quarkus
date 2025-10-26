package com.akfc.catalog.web.mappers;

import com.akfc.catalog.errors.ResourceBusinessException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.ext.ExceptionMapper;
import jakarta.ws.rs.ext.Provider;

import java.util.Map;

/**
 * Maps ResourceBusinessException to HTTP 409 Conflict or 422 Unprocessable Entity.
 *
 * This handles business rule violations that prevent an operation from completing.
 * The request is syntactically valid and validated, but business logic rejects it.
 *
 * Example scenarios:
 * - Cannot reserve a resource that is not AVAILABLE
 * - Cannot delete non-archived resource
 * - Cannot modify archived resource
 *
 * Example JSON response:
 * {
 *   "message": "Cannot reserve resource that is not available",
 *   "code": "RESOURCE_NOT_AVAILABLE",
 *   "timestamp": "2024-03-20T10:15:30",
 *   "details": {
 *     "businessRule": "RESOURCE_NOT_AVAILABLE",
 *     "currentStatus": "RESERVED"
 *   }
 * }
 */
@Provider
public class ResourceBusinessExceptionMapper implements ExceptionMapper<ResourceBusinessException> {

    @Override
    public Response toResponse(ResourceBusinessException exception) {
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
