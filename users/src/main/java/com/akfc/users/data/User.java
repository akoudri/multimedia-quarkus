package com.akfc.users.data;

import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import jakarta.persistence.*;
import jakarta.validation.constraints.*;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.UpdateTimestamp;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

/**
 * Entity representing a user in the multimedia library system.
 *
 * This entity uses the ACTIVE RECORD pattern via PanacheEntityBase.
 * Demonstrates full lifecycle management with:
 * - Automatic timestamps
 * - Email validation
 * - Account status tracking
 * - Soft delete (archived flag)
 * - Audit trail (created/modified by)
 *
 * Examples:
 * 1. Regular user: email="john.doe@example.com", lastName="Doe", firstName="John",
 *    status=ACTIVE, role=USER
 *
 * 2. Moderator: email="jane.admin@example.com", lastName="Smith", firstName="Jane",
 *    status=ACTIVE, role=MODERATOR
 */
@Entity
@Table(name = "users", indexes = {
    @Index(name = "idx_user_email", columnList = "email", unique = true),
    @Index(name = "idx_user_status", columnList = "account_status"),
    @Index(name = "idx_user_role", columnList = "role"),
    @Index(name = "idx_user_archived", columnList = "archived")
})
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class User extends PanacheEntityBase {

    // ========== PRIMARY KEY ==========

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    public Long id;

    // ========== BUSINESS FIELDS ==========

    /**
     * User email address (unique identifier).
     * Must be valid email format and unique in the system.
     */
    @NotBlank(message = "Email is required")
    @Email(message = "Email must be valid")
    @Size(max = 255, message = "Email must not exceed 255 characters")
    @Column(nullable = false, unique = true, length = 255)
    public String email;

    /**
     * User's last name.
     */
    @NotBlank(message = "Last name is required")
    @Size(min = 1, max = 100, message = "Last name must be between 1 and 100 characters")
    @Column(name = "last_name", nullable = false, length = 100)
    public String lastName;

    /**
     * User's first name.
     */
    @NotBlank(message = "First name is required")
    @Size(min = 1, max = 100, message = "First name must be between 1 and 100 characters")
    @Column(name = "first_name", nullable = false, length = 100)
    public String firstName;

    /**
     * Phone number (optional).
     * Basic pattern validation for international format.
     */
    @Pattern(regexp = "^\\+?[1-9]\\d{1,14}$",
             message = "Phone number must be valid international format")
    @Size(max = 20, message = "Phone number must not exceed 20 characters")
    @Column(name = "phone_number", length = 20)
    public String phoneNumber;

    /**
     * Date when user registered in the system.
     * Automatically set on first persist if not provided.
     */
    @NotNull(message = "Registration date is required")
    @Column(name = "registration_date", nullable = false)
    public LocalDate registrationDate;

    // ========== ROLE & STATUS FIELDS ==========

    /**
     * User's role in the system (USER or MODERATOR).
     * Determines permissions and access levels.
     * Defaults to USER on creation.
     */
    @NotNull(message = "Role is required")
    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 20)
    public UserRole role;

    /**
     * Account status tracking the user's account state.
     * Can be: ACTIVE, SUSPENDED, PENDING_VERIFICATION, LOCKED
     * Defaults to PENDING_VERIFICATION on creation.
     */
    @NotNull(message = "Account status is required")
    @Enumerated(EnumType.STRING)
    @Column(name = "account_status", nullable = false, length = 30)
    public AccountStatus accountStatus;

    /**
     * Reason for suspension or lock (if applicable).
     */
    @Size(max = 500, message = "Status reason must not exceed 500 characters")
    @Column(name = "status_reason", length = 500)
    public String statusReason;

    /**
     * Date when account status was last changed.
     */
    @Column(name = "status_changed_at")
    public LocalDateTime statusChangedAt;

    /**
     * Soft delete flag.
     * When true, user is hidden from normal queries but retained for audit.
     */
    @Column(nullable = false)
    public Boolean archived = false;

    /**
     * Reason for archiving the user account.
     */
    @Size(max = 500, message = "Archive reason must not exceed 500 characters")
    @Column(name = "archive_reason", length = 500)
    public String archiveReason;

    // ========== AUDIT/TIMESTAMP FIELDS ==========

    /**
     * Automatic creation timestamp using Hibernate annotation.
     * Set once when entity is first persisted.
     */
    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    public LocalDateTime createdAt;

    /**
     * Automatic update timestamp using Hibernate annotation.
     * Updated every time entity is modified.
     */
    @UpdateTimestamp
    @Column(name = "updated_at", nullable = false)
    public LocalDateTime updatedAt;

    /**
     * Administrator who created this user account.
     */
    @Size(max = 100, message = "Created by must not exceed 100 characters")
    @Column(name = "created_by", length = 100)
    public String createdBy;

    /**
     * Administrator who last modified this user account.
     */
    @Size(max = 100, message = "Modified by must not exceed 100 characters")
    @Column(name = "modified_by", length = 100)
    public String modifiedBy;

    /**
     * Last login timestamp for activity tracking.
     */
    @Column(name = "last_login_at")
    public LocalDateTime lastLoginAt;

    // ========== JPA LIFECYCLE CALLBACKS ==========

    /**
     * Called automatically before entity is persisted.
     * Initializes default values and validates business rules.
     */
    @PrePersist
    protected void onCreate() {
        // Set registration date if not provided
        if (registrationDate == null) {
            registrationDate = LocalDate.now();
        }

        // Set default role if not specified
        if (role == null) {
            role = UserRole.USER;
        }

        // Set default account status
        if (accountStatus == null) {
            accountStatus = AccountStatus.PENDING_VERIFICATION;
            statusChangedAt = LocalDateTime.now();
        }

        // Ensure archived flag is set
        if (archived == null) {
            archived = false;
        }

        // Normalize email to lowercase for consistency
        if (email != null) {
            email = email.toLowerCase().trim();
        }
    }

    /**
     * Called automatically before entity is updated.
     * Validates business rules and tracks status changes.
     */
    @PreUpdate
    protected void onUpdate() {
        // Validate that archived users cannot be modified without proper authorization
        if (Boolean.TRUE.equals(archived) && modifiedBy == null) {
            throw new IllegalStateException("Cannot update archived user without specifying modifier");
        }

        // Normalize email if changed
        if (email != null) {
            email = email.toLowerCase().trim();
        }
    }

    // ========== ACTIVE RECORD PATTERN - BUSINESS METHODS ==========

    /**
     * Find a user by email address.
     *
     * @param email Email address (case-insensitive)
     * @return Optional containing user if found
     */
    public static Optional<User> findByEmail(String email) {
        return find("LOWER(email) = LOWER(?1) AND archived = false",
                   email).firstResultOptional();
    }

    /**
     * Find all users by role.
     *
     * @param role User role filter
     * @return List of users with specified role
     */
    public static List<User> findByRole(UserRole role) {
        return list("role = ?1 AND archived = false", role);
    }

    /**
     * Find all users by account status.
     *
     * @param status Account status filter
     * @return List of users with specified status
     */
    public static List<User> findByStatus(AccountStatus status) {
        return list("accountStatus = ?1 AND archived = false", status);
    }

    /**
     * Find all active moderators.
     *
     * @return List of active moderator users
     */
    public static List<User> findActiveModerators() {
        return list("role = ?1 AND accountStatus = ?2 AND archived = false",
                   UserRole.MODERATOR, AccountStatus.ACTIVE);
    }

    /**
     * Find all active regular users.
     *
     * @return List of active regular users
     */
    public static List<User> findActiveUsers() {
        return list("role = ?1 AND accountStatus = ?2 AND archived = false",
                   UserRole.USER, AccountStatus.ACTIVE);
    }

    /**
     * Search users by name (first or last name, case-insensitive).
     *
     * @param name Name to search for (partial match)
     * @return List of users matching the name
     */
    public static List<User> searchByName(String name) {
        return list("(LOWER(firstName) LIKE LOWER(?1) OR LOWER(lastName) LIKE LOWER(?1)) " +
                   "AND archived = false",
                   "%" + name + "%");
    }

    /**
     * Check if an email is already registered.
     *
     * @param email Email to check
     * @return true if email exists, false otherwise
     */
    public static boolean emailExists(String email) {
        return count("LOWER(email) = LOWER(?1)", email) > 0;
    }

    /**
     * BUSINESS METHOD: Find users registered after a specific date.
     *
     * Useful for reporting and analytics.
     *
     * @param date Cutoff date
     * @return List of users registered after the date
     */
    public static List<User> findRegisteredAfter(LocalDate date) {
        return list("registrationDate >= ?1 AND archived = false ORDER BY registrationDate DESC",
                   date);
    }

    /**
     * BUSINESS METHOD: Find users who haven't logged in for a specified number of days.
     *
     * Useful for inactive user cleanup or re-engagement campaigns.
     *
     * @param days Number of days of inactivity
     * @return List of inactive users
     */
    public static List<User> findInactiveUsers(int days) {
        LocalDateTime cutoffDate = LocalDateTime.now().minusDays(days);
        return list("(lastLoginAt IS NULL OR lastLoginAt < ?1) " +
                   "AND accountStatus = ?2 AND archived = false " +
                   "ORDER BY lastLoginAt ASC NULLS FIRST",
                   cutoffDate, AccountStatus.ACTIVE);
    }

    /**
     * BUSINESS METHOD: Count active users by role.
     *
     * @param role User role
     * @return Number of active users with the role
     */
    public static long countActiveByRole(UserRole role) {
        return count("role = ?1 AND accountStatus = ?2 AND archived = false",
                    role, AccountStatus.ACTIVE);
    }

    // ========== INSTANCE METHODS FOR LIFECYCLE MANAGEMENT ==========

    /**
     * Activate a user account.
     *
     * @param activatedBy Administrator performing activation
     */
    public void activate(String activatedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot activate archived user");
        }
        this.accountStatus = AccountStatus.ACTIVE;
        this.statusChangedAt = LocalDateTime.now();
        this.statusReason = null;
        this.modifiedBy = activatedBy;
    }

    /**
     * Suspend a user account.
     *
     * @param reason Reason for suspension
     * @param suspendedBy Administrator performing suspension
     */
    public void suspend(String reason, String suspendedBy) {
        this.accountStatus = AccountStatus.SUSPENDED;
        this.statusChangedAt = LocalDateTime.now();
        this.statusReason = reason;
        this.modifiedBy = suspendedBy;
    }

    /**
     * Lock a user account (usually due to security concerns).
     *
     * @param reason Reason for locking
     * @param lockedBy Administrator performing lock
     */
    public void lock(String reason, String lockedBy) {
        this.accountStatus = AccountStatus.LOCKED;
        this.statusChangedAt = LocalDateTime.now();
        this.statusReason = reason;
        this.modifiedBy = lockedBy;
    }

    /**
     * Archive this user (soft delete).
     *
     * @param reason Reason for archiving
     * @param archivedBy Administrator performing archive
     */
    public void archive(String reason, String archivedBy) {
        this.archived = true;
        this.archiveReason = reason;
        this.modifiedBy = archivedBy;
        this.accountStatus = AccountStatus.SUSPENDED;
        this.statusChangedAt = LocalDateTime.now();
    }

    /**
     * Restore an archived user.
     *
     * @param restoredBy Administrator performing restoration
     */
    public void restore(String restoredBy) {
        this.archived = false;
        this.archiveReason = null;
        this.modifiedBy = restoredBy;
        this.accountStatus = AccountStatus.ACTIVE;
        this.statusChangedAt = LocalDateTime.now();
        this.persist();
    }

    // ========== CREATE ==========

    /**
     * Create a new user and persist it to the database.
     *
     * This factory method creates a user with PENDING_VERIFICATION status
     * and USER role by default (as set by @PrePersist).
     *
     * @param email User's email address (will be normalized to lowercase)
     * @param firstName User's first name
     * @param lastName User's last name
     * @param phoneNumber Optional phone number (international format)
     * @param createdBy User or system creating the account
     * @return The created and persisted user
     */
    public static User create(String email, String firstName, String lastName,
                             String phoneNumber, String createdBy) {
        User user = new User();
        user.email = email; // Will be normalized by @PrePersist
        user.firstName = firstName;
        user.lastName = lastName;
        user.registrationDate = LocalDate.now();
        user.phoneNumber = phoneNumber;
        user.createdBy = createdBy;
        // role and accountStatus will be set by @PrePersist

        user.persist();
        return user;
    }

    /**
     * Simplified create method with required fields only.
     *
     * @param email User's email
     * @param firstName First name
     * @param lastName Last name
     * @param createdBy User creating the account
     * @return The created and persisted user
     */
    public static User create(String email, String firstName, String lastName, String createdBy) {
        return create(email, firstName, lastName, null, createdBy);
    }

    /**
     * Create a moderator account directly.
     *
     * This should only be called by administrators.
     *
     * @param email User's email
     * @param firstName First name
     * @param lastName Last name
     * @param createdBy Administrator creating the account
     * @return The created moderator user
     */
    public static User createModerator(String email, String firstName, String lastName,
                                      String createdBy) {
        User user = create(email, firstName, lastName, createdBy);
        user.role = UserRole.MODERATOR;
        user.accountStatus = AccountStatus.ACTIVE; // Moderators are active by default
        user.statusChangedAt = LocalDateTime.now();
        user.persist();
        return user;
    }

    // ========== UPDATE ==========

    /**
     * Update user's personal information.
     *
     * This method allows updating mutable user fields.
     *
     * @param firstName New first name
     * @param lastName New last name
     * @param phoneNumber New phone number
     * @param modifiedBy User making the update
     */
    public void update(String firstName, String lastName, String phoneNumber, String modifiedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot update archived user. Restore it first.");
        }

        this.firstName = firstName;
        this.lastName = lastName;
        this.phoneNumber = phoneNumber;
        this.modifiedBy = modifiedBy;

        // updatedAt will be set automatically by @UpdateTimestamp
        this.persist();
    }

    /**
     * Update user's email address.
     *
     * IMPORTANT: This may require re-verification in a real system.
     *
     * @param newEmail New email address
     * @param modifiedBy User making the change
     */
    public void updateEmail(String newEmail, String modifiedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot update archived user");
        }
        this.email = newEmail; // Will be normalized by @PreUpdate
        this.modifiedBy = modifiedBy;
        // In a real system, might want to set accountStatus to PENDING_VERIFICATION
        this.persist();
    }

    /**
     * Update user's phone number.
     *
     * @param newPhoneNumber New phone number (international format)
     * @param modifiedBy User making the change
     */
    public void updatePhoneNumber(String newPhoneNumber, String modifiedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot update archived user");
        }
        this.phoneNumber = newPhoneNumber;
        this.modifiedBy = modifiedBy;
        this.persist();
    }

    // ========== ROLE MANAGEMENT ==========

    /**
     * Record a user login.
     */
    public void recordLogin() {
        this.lastLoginAt = LocalDateTime.now();
        this.persist();
    }

    /**
     * Promote user to moderator.
     *
     * @param promotedBy Administrator performing promotion
     */
    public void promoteToModerator(String promotedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot promote archived user");
        }
        if (this.role == UserRole.MODERATOR) {
            throw new IllegalStateException("User is already a moderator");
        }
        this.role = UserRole.MODERATOR;
        this.modifiedBy = promotedBy;
        this.persist();
    }

    /**
     * Demote moderator to regular user.
     *
     * @param demotedBy Administrator performing demotion
     */
    public void demoteToUser(String demotedBy) {
        if (Boolean.TRUE.equals(archived)) {
            throw new IllegalStateException("Cannot demote archived user");
        }
        if (this.role == UserRole.USER) {
            throw new IllegalStateException("User is already a regular user");
        }
        this.role = UserRole.USER;
        this.modifiedBy = demotedBy;
        this.persist();
    }

    // ========== HARD DELETE ==========

    /**
     * Permanently delete this user from the database.
     *
     * WARNING: This is a hard delete and cannot be undone!
     * Use archive() for soft delete instead in most cases.
     *
     * This should only be used when:
     * - User exercises "right to be forgotten" (GDPR)
     * - Data retention policies require permanent deletion
     * - The account was created by mistake
     *
     * @throws IllegalStateException if user is not archived first
     */
    public void permanentlyDelete() {
        if (Boolean.FALSE.equals(archived)) {
            throw new IllegalStateException(
                "Cannot permanently delete a non-archived user. Archive it first for safety.");
        }
        this.delete();
    }

    /**
     * Force delete without archiving first.
     *
     * DANGEROUS: Use with extreme caution!
     * Only for administrative or legal compliance purposes.
     *
     * @param confirmedBy Administrator confirming the deletion
     */
    public void forceDelete(String confirmedBy) {
        // Log or audit this dangerous operation
        this.modifiedBy = confirmedBy;
        this.delete();
    }

    // ========== UTILITY METHODS ==========

    /**
     * Get full name of the user.
     *
     * @return Full name (firstName + lastName)
     */
    public String getFullName() {
        return firstName + " " + lastName;
    }

    /**
     * Check if user is a moderator.
     *
     * @return True if user has MODERATOR role
     */
    public boolean isModerator() {
        return this.role == UserRole.MODERATOR;
    }

    /**
     * Check if user account is currently active.
     *
     * @return True if account status is ACTIVE and not archived
     */
    public boolean isActive() {
        return this.accountStatus == AccountStatus.ACTIVE && Boolean.FALSE.equals(archived);
    }

    /**
     * Check if user can login.
     *
     * @return True if account is active or pending verification (not suspended/locked)
     */
    public boolean canLogin() {
        return !Boolean.TRUE.equals(archived) &&
               (this.accountStatus == AccountStatus.ACTIVE ||
                this.accountStatus == AccountStatus.PENDING_VERIFICATION);
    }
}
