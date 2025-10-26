package com.akfc.users.web.resources;

import com.akfc.users.dto.CreateUserRequest;
import com.akfc.users.dto.UpdateUserRequest;
import com.akfc.users.dto.UserResponse;
import com.akfc.users.services.UserService;
import jakarta.inject.Inject;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.*;
import org.eclipse.microprofile.openapi.annotations.Operation;
import org.eclipse.microprofile.openapi.annotations.media.Content;
import org.eclipse.microprofile.openapi.annotations.media.ExampleObject;
import org.eclipse.microprofile.openapi.annotations.media.Schema;
import org.eclipse.microprofile.openapi.annotations.parameters.Parameter;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import java.net.URI;
import java.util.List;

/**
 * REST API for User Management.
 *
 * Provides comprehensive user lifecycle management including:
 * - Full CRUD operations (Create, Read, Update, Delete)
 * - Account status management (activate, suspend, lock)
 * - Role management (promote, demote)
 * - Email updates
 * - User search and filtering
 *
 * All endpoints use DTOs for input/output and leverage ExceptionMappers
 * for consistent error handling across the API.
 *
 * Error Response Format:
 * All error responses follow the standard ErrorResponse structure with:
 * - message: Human-readable error description
 * - code: Machine-readable error code
 * - timestamp: When the error occurred
 * - details: Additional context (field violations, IDs, etc.)
 *
 * Pagination Support:
 * Collection endpoints accept pagination parameters:
 * - page: Zero-based page number (default: 0)
 * - size: Items per page (default: 20, max: 100)
 * - sort: Sort field and direction (e.g., "email:asc", "createdAt:desc")
 *
 * @see com.akfc.users.services.UserService
 * @see com.akfc.users.web.mappers - Exception mappers for error handling
 */
@Path("/users")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
@Tag(name = "Users", description = "User account management and lifecycle operations")
public class UserResource {

    @Inject
    UserService userService;

    // ========================================
    // CRUD OPERATIONS
    // ========================================

    /**
     * Create a new user.
     *
     * Business Rules:
     * - Email must be unique (409 if exists)
     * - Email format must be valid (400 if invalid)
     * - First name and last name are required
     * - New users start with PENDING_VERIFICATION status
     * - New users have USER role by default
     *
     * @param request User creation request with email, names, phone
     * @param uriInfo Context for building Location header
     * @return HTTP 201 with created user and Location header
     */
    @POST
    @Operation(
        summary = "Create a new user",
        description = "Creates a new user account with PENDING_VERIFICATION status and USER role. " +
                      "Email must be unique. Phone number is optional."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "201",
            description = "User created successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class),
                examples = @ExampleObject(
                    name = "Created user",
                    value = """
                        {
                          "id": 1,
                          "email": "john.doe@example.com",
                          "firstName": "John",
                          "lastName": "Doe",
                          "phoneNumber": "+1234567890",
                          "accountStatus": "PENDING_VERIFICATION",
                          "role": "USER",
                          "lastLoginAt": null,
                          "loginCount": 0,
                          "archived": false,
                          "createdAt": "2024-03-20T10:15:30",
                          "createdBy": "system",
                          "modifiedAt": "2024-03-20T10:15:30",
                          "modifiedBy": "system"
                        }
                        """
                )
            ),
            headers = {
                @org.eclipse.microprofile.openapi.annotations.headers.Header(
                    name = "Location",
                    description = "URI of the created user",
                    schema = @Schema(type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.STRING)
                )
            }
        ),
        @APIResponse(
            responseCode = "400",
            description = "Validation error - Invalid input data",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Validation error",
                    value = """
                        {
                          "message": "Request validation failed",
                          "code": "VALIDATION_ERROR",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "violations": {
                              "email": "Email format is invalid",
                              "firstName": "First name is required"
                            }
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "409",
            description = "User with email already exists",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Duplicate email",
                    value = """
                        {
                          "message": "User with email john.doe@example.com already exists",
                          "code": "USER_ALREADY_EXISTS",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "email": "john.doe@example.com"
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "500",
            description = "Internal server error",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Internal error",
                    value = """
                        {
                          "message": "An unexpected error occurred. Please contact support with the correlation ID.",
                          "code": "INTERNAL_ERROR",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "correlationId": "a1b2c3d4-e5f6-7890-abcd-ef1234567890",
                            "errorType": "NullPointerException"
                          }
                        }
                        """
                )
            )
        )
    })
    public Response createUser(@Valid CreateUserRequest request, @Context UriInfo uriInfo) {
        // NOTE: In production, createdBy would come from SecurityIdentity (JWT/OIDC)
        // For demo purposes, using "system" as the creator
        UserResponse user = userService.createUser(request, "system");

        // Build Location header: /users/{id}
        URI location = uriInfo.getAbsolutePathBuilder()
            .path(user.id.toString())
            .build();

        // Return 201 Created with Location header and created resource
        return Response.created(location)
            .entity(user)
            .build();
    }

    /**
     * Get all users.
     *
     * Returns all active (non-archived) users.
     * Pagination parameters are accepted but not yet implemented.
     *
     * @param page Page number (zero-based)
     * @param size Items per page
     * @param sort Sort field and direction
     * @return List of user responses
     */
    @GET
    @Operation(
        summary = "List all users",
        description = "Returns all active users. Archived users are excluded. " +
                      "Pagination parameters are accepted for future use."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "List of users retrieved successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class, type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.ARRAY),
                examples = @ExampleObject(
                    name = "User list",
                    value = """
                        [
                          {
                            "id": 1,
                            "email": "john.doe@example.com",
                            "firstName": "John",
                            "lastName": "Doe",
                            "accountStatus": "ACTIVE",
                            "role": "USER",
                            "loginCount": 5
                          },
                          {
                            "id": 2,
                            "email": "jane.smith@example.com",
                            "firstName": "Jane",
                            "lastName": "Smith",
                            "accountStatus": "ACTIVE",
                            "role": "MODERATOR",
                            "loginCount": 12
                          }
                        ]
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "500",
            description = "Internal server error"
        )
    })
    public List<UserResponse> getAllUsers(
            @QueryParam("page")
            @DefaultValue("0")
            @Min(value = 0, message = "Page must be >= 0")
            @Parameter(description = "Page number (zero-based)", example = "0")
            int page,

            @QueryParam("size")
            @DefaultValue("20")
            @Min(value = 1, message = "Size must be >= 1")
            @Parameter(description = "Items per page", example = "20")
            int size,

            @QueryParam("sort")
            @Parameter(description = "Sort field and direction (e.g., 'email:asc', 'createdAt:desc')", example = "email:asc")
            String sort
    ) {
        // TODO: Implement pagination with PanacheQuery
        // For now, returning all users
        return userService.getAllUsers();
    }

    /**
     * Get user by ID.
     *
     * @param id User ID
     * @return User response
     */
    @GET
    @Path("/{id}")
    @Operation(
        summary = "Get user by ID",
        description = "Retrieves a specific user by their ID. Only returns active users."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "User found",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class),
                examples = @ExampleObject(
                    name = "User details",
                    value = """
                        {
                          "id": 1,
                          "email": "john.doe@example.com",
                          "firstName": "John",
                          "lastName": "Doe",
                          "phoneNumber": "+1234567890",
                          "accountStatus": "ACTIVE",
                          "role": "USER",
                          "lastLoginAt": "2024-03-20T08:30:00",
                          "loginCount": 5,
                          "archived": false,
                          "createdAt": "2024-01-15T10:00:00",
                          "modifiedAt": "2024-03-20T08:30:00"
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "User not found",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Not found",
                    value = """
                        {
                          "message": "User not found with ID: 999",
                          "code": "USER_NOT_FOUND",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "userId": 999
                          }
                        }
                        """
                )
            )
        )
    })
    public UserResponse getUser(
            @PathParam("id")
            @Parameter(description = "User ID", required = true, example = "1")
            Long id
    ) {
        return userService.getUserById(id);
    }

    /**
     * Update an existing user.
     *
     * Business Rules:
     * - User must exist and not be archived
     * - Cannot update email through this endpoint (use PUT /users/{id}/email)
     * - First name and last name are required
     *
     * @param id User ID
     * @param request Update request with new values
     * @return Updated user response
     */
    @PUT
    @Path("/{id}")
    @Operation(
        summary = "Update a user",
        description = "Updates user profile information (names, phone). " +
                      "Email updates must use the dedicated email update endpoint. " +
                      "Cannot update archived users."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "User updated successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Validation error - Invalid input data"
        ),
        @APIResponse(
            responseCode = "404",
            description = "User not found",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Not found",
                    value = """
                        {
                          "message": "User not found with ID: 999",
                          "code": "USER_NOT_FOUND",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "userId": 999
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "500",
            description = "Internal server error"
        )
    })
    public UserResponse updateUser(
            @PathParam("id")
            @Parameter(description = "User ID", required = true, example = "1")
            Long id,

            @Valid UpdateUserRequest request
    ) {
        // NOTE: In production, modifiedBy would come from SecurityIdentity
        return userService.updateUser(id, request, "system");
    }

    /**
     * Delete a user permanently.
     *
     * Business Rule: User must be archived first before permanent deletion.
     * This is a GDPR compliance operation for permanent data removal.
     *
     * @param id User ID
     * @return HTTP 204 No Content on success
     */
    @DELETE
    @Path("/{id}")
    @Operation(
        summary = "Delete a user permanently",
        description = "Permanently deletes a user. User must be archived first. " +
                      "This operation is irreversible and should be used for GDPR compliance. " +
                      "Use POST /users/{id}/archive for soft delete instead."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "204",
            description = "User deleted successfully"
        ),
        @APIResponse(
            responseCode = "404",
            description = "User not found",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Not found",
                    value = """
                        {
                          "message": "User not found with ID: 999",
                          "code": "USER_NOT_FOUND",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "userId": 999
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "409",
            description = "User must be archived first",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Not archived",
                    value = """
                        {
                          "message": "Cannot permanently delete non-archived user. Archive it first.",
                          "code": "USER_NOT_ARCHIVED",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "businessRule": "USER_NOT_ARCHIVED"
                          }
                        }
                        """
                )
            )
        )
    })
    public Response deleteUser(
            @PathParam("id")
            @Parameter(description = "User ID", required = true, example = "1")
            Long id
    ) {
        userService.deleteUser(id);
        return Response.noContent().build();
    }

    // ========================================
    // BUSINESS OPERATIONS - USER SEARCH
    // ========================================

    /**
     * Find user by email address.
     *
     * @param email Email address to search for
     * @return User response
     */
    @GET
    @Path("/by-email")
    @Operation(
        summary = "Find user by email",
        description = "Searches for a user by their email address"
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "User found",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Email parameter required or invalid format"
        ),
        @APIResponse(
            responseCode = "404",
            description = "User not found with this email",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Not found",
                    value = """
                        {
                          "message": "User not found with email: unknown@example.com",
                          "code": "USER_NOT_FOUND",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "email": "unknown@example.com"
                          }
                        }
                        """
                )
            )
        )
    })
    public UserResponse getUserByEmail(
            @QueryParam("email")
            @NotBlank(message = "Email parameter is required")
            @Email(message = "Email format is invalid")
            @Parameter(description = "Email address to search for", required = true, example = "john.doe@example.com")
            String email
    ) {
        return userService.getUserByEmail(email);
    }

    /**
     * Get all moderators.
     *
     * Returns all users with MODERATOR role.
     *
     * @return List of moderator users
     */
    @GET
    @Path("/moderators")
    @Operation(
        summary = "List all moderators",
        description = "Returns all users with MODERATOR role"
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "List of moderators",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class, type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.ARRAY),
                examples = @ExampleObject(
                    name = "Moderator list",
                    value = """
                        [
                          {
                            "id": 2,
                            "email": "jane.smith@example.com",
                            "firstName": "Jane",
                            "lastName": "Smith",
                            "accountStatus": "ACTIVE",
                            "role": "MODERATOR",
                            "loginCount": 12
                          }
                        ]
                        """
                )
            )
        )
    })
    public List<UserResponse> getAllModerators() {
        return userService.getAllModerators();
    }

    // ========================================
    // BUSINESS OPERATIONS - ACCOUNT STATUS
    // ========================================

    /**
     * Activate a user account.
     *
     * Business Rules:
     * - Can activate PENDING_VERIFICATION or SUSPENDED accounts
     * - Cannot activate LOCKED accounts (must unlock first)
     * - Already ACTIVE accounts return error
     *
     * @param id User ID
     * @return Activated user response
     */
    @POST
    @Path("/{id}/activate")
    @Operation(
        summary = "Activate a user account",
        description = "Activates a user account from PENDING_VERIFICATION or SUSPENDED status. " +
                      "Locked accounts must be unlocked separately."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "User activated successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "User not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "Cannot activate - user is already active or locked",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = {
                    @ExampleObject(
                        name = "Already active",
                        value = """
                            {
                              "message": "User account is already active",
                              "code": "ALREADY_ACTIVE",
                              "timestamp": "2024-03-20T10:15:30",
                              "details": {
                                "businessRule": "ALREADY_ACTIVE"
                              }
                            }
                            """
                    ),
                    @ExampleObject(
                        name = "Account locked",
                        value = """
                            {
                              "message": "Cannot activate locked account. Unlock it first.",
                              "code": "ACCOUNT_LOCKED",
                              "timestamp": "2024-03-20T10:15:30",
                              "details": {
                                "businessRule": "ACCOUNT_LOCKED"
                              }
                            }
                            """
                    )
                }
            )
        )
    })
    public UserResponse activateUser(
            @PathParam("id")
            @Parameter(description = "User ID", required = true, example = "1")
            Long id
    ) {
        // NOTE: In production, activatedBy would come from SecurityIdentity
        return userService.activateUser(id, "system");
    }

    /**
     * Suspend a user account.
     *
     * Suspends a user account with a required reason.
     * Suspended users cannot log in but can be reactivated.
     *
     * @param id User ID
     * @param reason Reason for suspension (required)
     * @return Suspended user response
     */
    @POST
    @Path("/{id}/suspend")
    @Operation(
        summary = "Suspend a user account",
        description = "Suspends a user account with a mandatory reason. " +
                      "Suspended users cannot log in but can be reactivated later."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "User suspended successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Suspension reason is required",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Missing reason",
                    value = """
                        {
                          "message": "Validation failed for field: reason",
                          "code": "VALIDATION_ERROR",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "violations": {
                              "reason": "Suspension reason is required"
                            }
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "User not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "User is already suspended"
        )
    })
    public UserResponse suspendUser(
            @PathParam("id")
            @Parameter(description = "User ID", required = true, example = "1")
            Long id,

            @QueryParam("reason")
            @NotBlank(message = "Suspension reason is required")
            @Parameter(description = "Reason for suspension", required = true, example = "Terms of service violation")
            String reason
    ) {
        // NOTE: In production, suspendedBy would come from SecurityIdentity
        return userService.suspendUser(id, reason, "system");
    }

    /**
     * Lock a user account.
     *
     * Locks a user account with a required reason.
     * Locked accounts cannot log in and require explicit unlocking.
     * More severe than suspension.
     *
     * @param id User ID
     * @param reason Reason for locking (required)
     * @return Locked user response
     */
    @POST
    @Path("/{id}/lock")
    @Operation(
        summary = "Lock a user account",
        description = "Locks a user account with a mandatory reason. " +
                      "Locked accounts require explicit unlocking before they can be activated. " +
                      "This is more severe than suspension."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "User locked successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Lock reason is required",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Missing reason",
                    value = """
                        {
                          "message": "Validation failed for field: reason",
                          "code": "VALIDATION_ERROR",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "violations": {
                              "reason": "Lock reason is required"
                            }
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "User not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "User is already locked"
        )
    })
    public UserResponse lockUser(
            @PathParam("id")
            @Parameter(description = "User ID", required = true, example = "1")
            Long id,

            @QueryParam("reason")
            @NotBlank(message = "Lock reason is required")
            @Parameter(description = "Reason for locking", required = true, example = "Security breach detected")
            String reason
    ) {
        // NOTE: In production, lockedBy would come from SecurityIdentity
        return userService.lockUser(id, reason, "system");
    }

    // ========================================
    // BUSINESS OPERATIONS - EMAIL UPDATE
    // ========================================

    /**
     * Update user's email address.
     *
     * Business Rules:
     * - New email must be unique
     * - Email format must be valid
     * - In production, may require email re-verification
     *
     * @param id User ID
     * @param newEmail New email address
     * @return Updated user response
     */
    @PUT
    @Path("/{id}/email")
    @Operation(
        summary = "Update user's email address",
        description = "Updates a user's email address. The new email must be unique. " +
                      "In production, this would trigger email verification."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Email updated successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Invalid email format",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Invalid email",
                    value = """
                        {
                          "message": "Request validation failed",
                          "code": "VALIDATION_ERROR",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "violations": {
                              "email": "Email format is invalid"
                            }
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "User not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "Email already in use by another user",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Duplicate email",
                    value = """
                        {
                          "message": "User with email new.email@example.com already exists",
                          "code": "USER_ALREADY_EXISTS",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "email": "new.email@example.com"
                          }
                        }
                        """
                )
            )
        )
    })
    public UserResponse updateEmail(
            @PathParam("id")
            @Parameter(description = "User ID", required = true, example = "1")
            Long id,

            @QueryParam("email")
            @NotBlank(message = "Email is required")
            @Email(message = "Email format is invalid")
            @Parameter(description = "New email address", required = true, example = "new.email@example.com")
            String newEmail
    ) {
        // NOTE: In production, modifiedBy would come from SecurityIdentity
        return userService.updateEmail(id, newEmail, "system");
    }

    // ========================================
    // BUSINESS OPERATIONS - LOGIN TRACKING
    // ========================================

    /**
     * Record user login.
     *
     * Updates lastLoginAt and increments loginCount.
     * Only ACTIVE users can log in.
     *
     * @param id User ID
     * @return Updated user response
     */
    @POST
    @Path("/{id}/login")
    @Operation(
        summary = "Record user login",
        description = "Records a user login event, updating lastLoginAt timestamp and incrementing login count. " +
                      "Only ACTIVE users can log in."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "Login recorded successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "User not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "User cannot login - account is not active",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Cannot login",
                    value = """
                        {
                          "message": "User cannot login - account is SUSPENDED",
                          "code": "CANNOT_LOGIN",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "businessRule": "CANNOT_LOGIN"
                          }
                        }
                        """
                )
            )
        )
    })
    public UserResponse recordLogin(
            @PathParam("id")
            @Parameter(description = "User ID", required = true, example = "1")
            Long id
    ) {
        return userService.recordLogin(id);
    }

    // ========================================
    // BUSINESS OPERATIONS - ROLE MANAGEMENT
    // ========================================

    /**
     * Promote user to moderator.
     *
     * Business Rule: Can only promote regular USER to MODERATOR.
     * Already moderators return error.
     *
     * @param id User ID
     * @return Updated user with MODERATOR role
     */
    @POST
    @Path("/{id}/promote")
    @Operation(
        summary = "Promote user to moderator",
        description = "Promotes a regular user to moderator role. " +
                      "Only users with USER role can be promoted."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "User promoted successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "User not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "User is already a moderator",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Already moderator",
                    value = """
                        {
                          "message": "User is already a moderator",
                          "code": "ALREADY_MODERATOR",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "businessRule": "ALREADY_MODERATOR"
                          }
                        }
                        """
                )
            )
        )
    })
    public UserResponse promoteToModerator(
            @PathParam("id")
            @Parameter(description = "User ID", required = true, example = "1")
            Long id
    ) {
        // NOTE: In production, promotedBy would come from SecurityIdentity
        return userService.promoteToModerator(id, "system");
    }

    /**
     * Demote moderator to regular user.
     *
     * @param id User ID
     * @return Updated user with USER role
     */
    @POST
    @Path("/{id}/demote")
    @Operation(
        summary = "Demote moderator to user",
        description = "Demotes a moderator to regular user role. " +
                      "Only users with MODERATOR role can be demoted."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "User demoted successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "User not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "User is already a regular user",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Already user",
                    value = """
                        {
                          "message": "User is already a regular user",
                          "code": "ALREADY_USER",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "businessRule": "ALREADY_USER"
                          }
                        }
                        """
                )
            )
        )
    })
    public UserResponse demoteToUser(
            @PathParam("id")
            @Parameter(description = "User ID", required = true, example = "1")
            Long id
    ) {
        // NOTE: In production, demotedBy would come from SecurityIdentity
        return userService.demoteToUser(id, "system");
    }

    // ========================================
    // BUSINESS OPERATIONS - ARCHIVING
    // ========================================

    /**
     * Archive a user (soft delete).
     *
     * Business Rule: Archive reason is required.
     * Archived users can be restored later.
     *
     * @param id User ID
     * @param reason Reason for archiving
     * @return Archived user response
     */
    @POST
    @Path("/{id}/archive")
    @Operation(
        summary = "Archive a user",
        description = "Soft deletes a user by archiving them. Archived users can be restored later. " +
                      "A reason must be provided for audit purposes. " +
                      "Use DELETE /users/{id} for permanent deletion."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "User archived successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "400",
            description = "Archive reason is required",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Missing reason",
                    value = """
                        {
                          "message": "Validation failed for field: reason",
                          "code": "VALIDATION_ERROR",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "violations": {
                              "reason": "Archive reason is required"
                            }
                          }
                        }
                        """
                )
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "User not found"
        )
    })
    public UserResponse archiveUser(
            @PathParam("id")
            @Parameter(description = "User ID", required = true, example = "1")
            Long id,

            @QueryParam("reason")
            @NotBlank(message = "Archive reason is required")
            @Parameter(description = "Reason for archiving", required = true, example = "User requested account deletion")
            String reason
    ) {
        // NOTE: In production, archivedBy would come from SecurityIdentity
        return userService.archiveUser(id, reason, "system");
    }

    /**
     * Restore an archived user.
     *
     * Restores a previously archived user, making them active again.
     *
     * @param id User ID
     * @return Restored user response
     */
    @POST
    @Path("/{id}/restore")
    @Operation(
        summary = "Restore an archived user",
        description = "Restores a previously archived user, making them active again."
    )
    @APIResponses({
        @APIResponse(
            responseCode = "200",
            description = "User restored successfully",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                schema = @Schema(implementation = UserResponse.class)
            )
        ),
        @APIResponse(
            responseCode = "404",
            description = "User not found"
        ),
        @APIResponse(
            responseCode = "409",
            description = "User is not archived",
            content = @Content(
                mediaType = MediaType.APPLICATION_JSON,
                examples = @ExampleObject(
                    name = "Not archived",
                    value = """
                        {
                          "message": "User is not archived: 1",
                          "code": "USER_NOT_ARCHIVED",
                          "timestamp": "2024-03-20T10:15:30",
                          "details": {
                            "businessRule": "USER_NOT_ARCHIVED"
                          }
                        }
                        """
                )
            )
        )
    })
    public UserResponse restoreUser(
            @PathParam("id")
            @Parameter(description = "User ID", required = true, example = "1")
            Long id
    ) {
        // NOTE: In production, restoredBy would come from SecurityIdentity
        return userService.restoreUser(id, "system");
    }
}
