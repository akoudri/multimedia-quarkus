package com.akfc.users.services;

import com.akfc.users.data.*;
import com.akfc.users.dto.CreateUserRequest;
import com.akfc.users.dto.UpdateUserRequest;
import com.akfc.users.dto.UserResponse;
import com.akfc.users.errors.*;
import io.quarkus.cache.CacheInvalidate;
import io.quarkus.cache.CacheInvalidateAll;
import io.quarkus.cache.CacheResult;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;
import jakarta.validation.Valid;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Business service for User management.
 *
 * Handles all business logic, validation, and orchestration for user operations.
 * Implements complete CRUD lifecycle with business rules enforcement.
 */
@ApplicationScoped
public class UserService {

    @Inject
    UserRepository userRepository;

    @Inject
    UserSubscriptionRepository subscriptionRepository;

    // ========== CREATE ==========

    /**
     * Create a new user.
     *
     * Business Rules:
     * - Email must be unique
     * - Email format must be valid
     * - Phone number format (if provided) must be valid
     * - New users start with PENDING_VERIFICATION status
     *
     * @param request Create request DTO
     * @param createdBy User or system creating the account
     * @return Created user response
     * @throws UserAlreadyExistsException if email already exists
     * @throws UserValidationException if validation fails
     */
    @Transactional
    @CacheInvalidateAll(cacheName = "all-users")
    @CacheInvalidateAll(cacheName = "user-by-email")
    public UserResponse createUser(@Valid CreateUserRequest request, String createdBy) {
        // Validate request
        validateCreateRequest(request);

        // Check if email already exists
        if (userRepository.existsByEmail(request.email)) {
            throw new UserAlreadyExistsException(request.email);
        }

        // Create user using repository
        User user = userRepository.createUser(
            request.email,
            request.firstName,
            request.lastName,
            request.phoneNumber,
            createdBy
        );

        return UserResponse.from(user);
    }

    /**
     * Create a moderator user.
     *
     * Business Rule: Only administrators should create moderators
     *
     * @param request Create request DTO
     * @param createdBy Administrator creating the account
     * @return Created moderator response
     */
    @Transactional
    @CacheInvalidateAll(cacheName = "all-users")
    @CacheInvalidateAll(cacheName = "all-moderators")
    @CacheInvalidateAll(cacheName = "user-by-email")
    public UserResponse createModerator(@Valid CreateUserRequest request, String createdBy) {
        // Validate request
        validateCreateRequest(request);

        // Check if email already exists
        if (userRepository.existsByEmail(request.email)) {
            throw new UserAlreadyExistsException(request.email);
        }

        // Create moderator using repository
        User user = userRepository.createModerator(
            request.email,
            request.firstName,
            request.lastName,
            createdBy
        );

        return UserResponse.from(user);
    }

    // ========== READ ==========

    /**
     * Get user by ID.
     *
     * @param id User ID
     * @return User response
     * @throws UserNotFoundException if user not found
     */
    @CacheResult(cacheName = "user-by-id")
    public UserResponse getUserById(Long id) {
        User user = userRepository.findActiveById(id);
        if (user == null) {
            throw new UserNotFoundException(id);
        }
        return UserResponse.from(user);
    }

    /**
     * Get user by email.
     *
     * @param email Email address
     * @return User response
     * @throws UserNotFoundException if user not found
     */
    @CacheResult(cacheName = "user-by-email")
    public UserResponse getUserByEmail(String email) {
        User user = userRepository.findByEmail(email);
        if (user == null) {
            throw new UserNotFoundException(email);
        }
        return UserResponse.from(user);
    }

    /**
     * Get all active users.
     *
     * @return List of user responses
     */
    @CacheResult(cacheName = "all-users")
    public List<UserResponse> getAllUsers() {
        return userRepository.findAllActive()
            .stream()
            .map(UserResponse::from)
            .collect(Collectors.toList());
    }

    /**
     * Get all moderators.
     *
     * @return List of moderator users
     */
    @CacheResult(cacheName = "all-moderators")
    public List<UserResponse> getAllModerators() {
        return User.findActiveModerators()
            .stream()
            .map(UserResponse::from)
            .collect(Collectors.toList());
    }

    // ========== UPDATE ==========

    /**
     * Update an existing user.
     *
     * Business Rules:
     * - User must exist and not be archived
     * - Cannot update email through this method (use updateEmail)
     * - All validation rules still apply
     *
     * @param id User ID
     * @param request Update request DTO
     * @param modifiedBy User making the update
     * @return Updated user response
     * @throws UserNotFoundException if user not found
     */
    @Transactional
    @CacheInvalidate(cacheName = "user-by-id")
    @CacheInvalidateAll(cacheName = "all-users")
    public UserResponse updateUser(Long id, @Valid UpdateUserRequest request, String modifiedBy) {
        // Find user
        User user = getUserEntity(id);

        // Validate request
        validateUpdateRequest(request);

        // Update user
        user.update(
            request.firstName,
            request.lastName,
            request.phoneNumber,
            modifiedBy
        );

        return UserResponse.from(user);
    }

    /**
     * Update user's email.
     *
     * Business Rules:
     * - New email must be unique
     * - May require re-verification in production
     *
     * @param id User ID
     * @param newEmail New email address
     * @param modifiedBy User making the change
     * @return Updated user response
     */
    @Transactional
    @CacheInvalidate(cacheName = "user-by-id")
    @CacheInvalidateAll(cacheName = "user-by-email")
    @CacheInvalidateAll(cacheName = "all-users")
    public UserResponse updateEmail(Long id, String newEmail, String modifiedBy) {
        User user = getUserEntity(id);

        // Check if new email already exists
        if (userRepository.existsByEmail(newEmail)) {
            throw new UserAlreadyExistsException(newEmail);
        }

        user.updateEmail(newEmail, modifiedBy);
        return UserResponse.from(user);
    }

    // ========== ACCOUNT STATUS MANAGEMENT ==========

    /**
     * Activate a user account.
     *
     * Business Rule: Can only activate PENDING_VERIFICATION or SUSPENDED accounts
     *
     * @param id User ID
     * @param activatedBy Administrator activating the account
     * @return Updated user response
     */
    @Transactional
    @CacheInvalidate(cacheName = "user-by-id")
    @CacheInvalidateAll(cacheName = "all-users")
    public UserResponse activateUser(Long id, String activatedBy) {
        User user = getUserEntity(id);

        if (user.accountStatus == AccountStatus.ACTIVE) {
            throw new UserBusinessException(
                "User account is already active",
                "ALREADY_ACTIVE"
            );
        }

        if (user.accountStatus == AccountStatus.LOCKED) {
            throw new UserBusinessException(
                "Cannot activate locked account. Unlock it first.",
                "ACCOUNT_LOCKED"
            );
        }

        user.activate(activatedBy);
        return UserResponse.from(user);
    }

    /**
     * Suspend a user account.
     *
     * @param id User ID
     * @param reason Reason for suspension
     * @param suspendedBy Administrator suspending the account
     * @return Updated user response
     */
    @Transactional
    @CacheInvalidate(cacheName = "user-by-id")
    @CacheInvalidateAll(cacheName = "all-users")
    public UserResponse suspendUser(Long id, String reason, String suspendedBy) {
        User user = getUserEntity(id);

        if (user.accountStatus == AccountStatus.SUSPENDED) {
            throw new UserBusinessException(
                "User account is already suspended",
                "ALREADY_SUSPENDED"
            );
        }

        if (reason == null || reason.trim().isEmpty()) {
            throw new UserValidationException("reason", "Suspension reason is required");
        }

        user.suspend(reason, suspendedBy);
        return UserResponse.from(user);
    }

    /**
     * Lock a user account.
     *
     * @param id User ID
     * @param reason Reason for locking
     * @param lockedBy Administrator locking the account
     * @return Updated user response
     */
    @Transactional
    @CacheInvalidate(cacheName = "user-by-id")
    @CacheInvalidateAll(cacheName = "all-users")
    public UserResponse lockUser(Long id, String reason, String lockedBy) {
        User user = getUserEntity(id);

        if (user.accountStatus == AccountStatus.LOCKED) {
            throw new UserBusinessException(
                "User account is already locked",
                "ALREADY_LOCKED"
            );
        }

        if (reason == null || reason.trim().isEmpty()) {
            throw new UserValidationException("reason", "Lock reason is required");
        }

        user.lock(reason, lockedBy);
        return UserResponse.from(user);
    }

    /**
     * Record user login.
     *
     * @param id User ID
     * @return Updated user response
     */
    @Transactional
    @CacheInvalidate(cacheName = "user-by-id")
    public UserResponse recordLogin(Long id) {
        User user = getUserEntity(id);

        if (!user.canLogin()) {
            throw new UserBusinessException(
                "User cannot login - account is " + user.accountStatus,
                "CANNOT_LOGIN"
            );
        }

        user.recordLogin();
        return UserResponse.from(user);
    }

    // ========== ROLE MANAGEMENT ==========

    /**
     * Promote user to moderator.
     *
     * Business Rule: Can only promote regular users
     *
     * @param id User ID
     * @param promotedBy Administrator promoting the user
     * @return Updated user response
     */
    @Transactional
    @CacheInvalidate(cacheName = "user-by-id")
    @CacheInvalidateAll(cacheName = "all-users")
    @CacheInvalidateAll(cacheName = "all-moderators")
    public UserResponse promoteToModerator(Long id, String promotedBy) {
        User user = getUserEntity(id);

        if (user.role == UserRole.MODERATOR) {
            throw new UserBusinessException(
                "User is already a moderator",
                "ALREADY_MODERATOR"
            );
        }

        user.promoteToModerator(promotedBy);
        return UserResponse.from(user);
    }

    /**
     * Demote moderator to regular user.
     *
     * @param id User ID
     * @param demotedBy Administrator demoting the user
     * @return Updated user response
     */
    @Transactional
    @CacheInvalidate(cacheName = "user-by-id")
    @CacheInvalidateAll(cacheName = "all-users")
    @CacheInvalidateAll(cacheName = "all-moderators")
    public UserResponse demoteToUser(Long id, String demotedBy) {
        User user = getUserEntity(id);

        if (user.role == UserRole.USER) {
            throw new UserBusinessException(
                "User is already a regular user",
                "ALREADY_USER"
            );
        }

        user.demoteToUser(demotedBy);
        return UserResponse.from(user);
    }

    // ========== DELETE ==========

    /**
     * Archive a user (soft delete).
     *
     * @param id User ID
     * @param reason Reason for archiving
     * @param archivedBy Administrator archiving the user
     * @return Archived user response
     */
    @Transactional
    @CacheInvalidate(cacheName = "user-by-id")
    @CacheInvalidateAll(cacheName = "user-by-email")
    @CacheInvalidateAll(cacheName = "all-users")
    @CacheInvalidateAll(cacheName = "all-moderators")
    public UserResponse archiveUser(Long id, String reason, String archivedBy) {
        User user = getUserEntity(id);

        if (reason == null || reason.trim().isEmpty()) {
            throw new UserValidationException("reason", "Archive reason is required");
        }

        user.archive(reason, archivedBy);
        return UserResponse.from(user);
    }

    /**
     * Restore an archived user.
     *
     * @param id User ID
     * @param restoredBy Administrator restoring the user
     * @return Restored user response
     */
    @Transactional
    @CacheInvalidate(cacheName = "user-by-id")
    @CacheInvalidateAll(cacheName = "user-by-email")
    @CacheInvalidateAll(cacheName = "all-users")
    @CacheInvalidateAll(cacheName = "all-moderators")
    public UserResponse restoreUser(Long id, String restoredBy) {
        User user = userRepository.findByIdIncludingArchived(id);
        if (user == null) {
            throw new UserNotFoundException(id);
        }

        if (Boolean.FALSE.equals(user.archived)) {
            throw new UserBusinessException(
                "User is not archived: " + id,
                "USER_NOT_ARCHIVED"
            );
        }

        user.restore(restoredBy);
        return UserResponse.from(user);
    }

    /**
     * Permanently delete a user (GDPR compliance).
     *
     * Business Rule: User must be archived first
     *
     * @param id User ID
     * @throws UserNotFoundException if user not found
     * @throws UserBusinessException if user not archived
     */
    @Transactional
    @CacheInvalidate(cacheName = "user-by-id")
    public void deleteUser(Long id) {
        User user = userRepository.findByIdIncludingArchived(id);
        if (user == null) {
            throw new UserNotFoundException(id);
        }

        if (Boolean.FALSE.equals(user.archived)) {
            throw new UserBusinessException(
                "Cannot permanently delete non-archived user. Archive it first.",
                "USER_NOT_ARCHIVED"
            );
        }

        user.permanentlyDelete();
    }

    // ========== HELPER METHODS ==========

    /**
     * Get user entity or throw exception.
     */
    private User getUserEntity(Long id) {
        User user = userRepository.findActiveById(id);
        if (user == null) {
            throw new UserNotFoundException(id);
        }
        return user;
    }

    /**
     * Validate create request.
     */
    private void validateCreateRequest(CreateUserRequest request) {
        if (request.email == null || request.email.trim().isEmpty()) {
            throw new UserValidationException("email", "Email is required");
        }

        if (!request.email.matches("^[A-Za-z0-9+_.-]+@(.+)$")) {
            throw new UserValidationException("email", "Email format is invalid");
        }

        if (request.firstName == null || request.firstName.trim().isEmpty()) {
            throw new UserValidationException("firstName", "First name is required");
        }

        if (request.lastName == null || request.lastName.trim().isEmpty()) {
            throw new UserValidationException("lastName", "Last name is required");
        }
    }

    /**
     * Validate update request.
     */
    private void validateUpdateRequest(UpdateUserRequest request) {
        if (request.firstName == null || request.firstName.trim().isEmpty()) {
            throw new UserValidationException("firstName", "First name is required");
        }

        if (request.lastName == null || request.lastName.trim().isEmpty()) {
            throw new UserValidationException("lastName", "Last name is required");
        }
    }

    /**
     * Check if user exists by email.
     *
     * @param email Email address
     * @return True if exists
     */
    public boolean userExistsByEmail(String email) {
        return userRepository.existsByEmail(email);
    }

    // ========== SUBSCRIPTION MANAGEMENT ==========

    /**
     * Subscribe a user to a resource type.
     *
     * @param userId User ID
     * @param resourceType Resource type to subscribe to
     * @return Created or reactivated subscription
     * @throws UserNotFoundException if user not found
     */
    @Transactional
    public UserSubscription subscribeToResourceType(Long userId, ResourceType resourceType) {
        // Verify user exists
        if (!userRepository.existsById(userId)) {
            throw new UserNotFoundException(userId);
        }

        return subscriptionRepository.subscribe(userId, resourceType);
    }

    /**
     * Unsubscribe a user from a resource type.
     *
     * @param userId User ID
     * @param resourceType Resource type to unsubscribe from
     * @return True if subscription was removed
     */
    @Transactional
    public boolean unsubscribeFromResourceType(Long userId, ResourceType resourceType) {
        return subscriptionRepository.unsubscribe(userId, resourceType);
    }

    /**
     * Get all subscriptions for a user.
     *
     * @param userId User ID
     * @return List of subscriptions
     */
    public List<UserSubscription> getUserSubscriptions(Long userId) {
        return subscriptionRepository.findByUserId(userId);
    }

    /**
     * Get active subscriptions for a user.
     *
     * @param userId User ID
     * @return List of active subscriptions
     */
    public List<UserSubscription> getActiveUserSubscriptions(Long userId) {
        return subscriptionRepository.findActiveByUserId(userId);
    }

    /**
     * Get all user IDs subscribed to a specific resource type.
     *
     * @param resourceType Resource type
     * @return List of user IDs
     */
    public List<Long> getUsersSubscribedTo(ResourceType resourceType) {
        return subscriptionRepository.findUserIdsSubscribedTo(resourceType);
    }

    /**
     * Check if a user is subscribed to a resource type.
     *
     * @param userId User ID
     * @param resourceType Resource type
     * @return True if subscribed
     */
    public boolean isUserSubscribedTo(Long userId, ResourceType resourceType) {
        return subscriptionRepository.isSubscribed(userId, resourceType);
    }

    /**
     * Toggle subscription active status.
     *
     * @param userId User ID
     * @param resourceType Resource type
     * @return Updated subscription
     */
    @Transactional
    public UserSubscription toggleSubscription(Long userId, ResourceType resourceType) {
        return subscriptionRepository.toggleSubscription(userId, resourceType);
    }
}
