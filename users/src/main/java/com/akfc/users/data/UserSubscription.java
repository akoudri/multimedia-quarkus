package com.akfc.users.data;

import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.CreationTimestamp;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Entity representing a user's subscription to a resource type.
 *
 * Users can subscribe to BOOK, MOVIE, or MUSIC to receive notifications
 * when new resources of those types are created or updated.
 */
@Entity
@Table(name = "user_subscriptions",
    uniqueConstraints = {
        @UniqueConstraint(name = "uk_user_resource_type",
                         columnNames = {"user_id", "resource_type"})
    },
    indexes = {
        @Index(name = "idx_subscription_user", columnList = "user_id"),
        @Index(name = "idx_subscription_type", columnList = "resource_type"),
        @Index(name = "idx_subscription_active", columnList = "active")
    }
)
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class UserSubscription extends PanacheEntityBase {

    // ========== PRIMARY KEY ==========

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    public Long id;

    // ========== BUSINESS FIELDS ==========

    /**
     * User ID who is subscribing.
     * References User.id in the users table.
     */
    @NotNull(message = "User ID is required")
    @Column(name = "user_id", nullable = false)
    public Long userId;

    /**
     * Resource type the user is subscribing to.
     */
    @NotNull(message = "Resource type is required")
    @Enumerated(EnumType.STRING)
    @Column(name = "resource_type", nullable = false, length = 20)
    public ResourceType resourceType;

    /**
     * Whether the subscription is active.
     * Users can temporarily disable subscriptions without deleting them.
     */
    @NotNull
    @Column(nullable = false)
    public Boolean active = true;

    // ========== AUDIT/TIMESTAMP FIELDS ==========

    /**
     * When the subscription was created.
     */
    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    public LocalDateTime createdAt;

    /**
     * When the subscription was last modified.
     */
    @Column(name = "modified_at")
    public LocalDateTime modifiedAt;

    // ========== JPA LIFECYCLE CALLBACKS ==========

    /**
     * Called automatically before entity is persisted.
     */
    @PrePersist
    protected void onCreate() {
        if (active == null) {
            active = true;
        }
    }

    /**
     * Called automatically before entity is updated.
     */
    @PreUpdate
    protected void onUpdate() {
        modifiedAt = LocalDateTime.now();
    }

    // ========== ACTIVE RECORD PATTERN - BUSINESS METHODS ==========

    /**
     * Find all subscriptions for a user.
     *
     * @param userId User ID
     * @return List of subscriptions
     */
    public static List<UserSubscription> findByUserId(Long userId) {
        return list("userId = ?1 ORDER BY resourceType", userId);
    }

    /**
     * Find active subscriptions for a user.
     *
     * @param userId User ID
     * @return List of active subscriptions
     */
    public static List<UserSubscription> findActiveByUserId(Long userId) {
        return list("userId = ?1 AND active = true ORDER BY resourceType", userId);
    }

    /**
     * Find all users subscribed to a specific resource type.
     *
     * @param resourceType Resource type
     * @return List of user IDs subscribed to this type
     */
    public static List<Long> findUserIdsSubscribedTo(ResourceType resourceType) {
        return find("resourceType = ?1 AND active = true", resourceType)
            .project(Long.class)
            .singleResult();
    }

    /**
     * Find all users subscribed to a specific resource type (full subscription objects).
     *
     * @param resourceType Resource type
     * @return List of subscriptions for this type
     */
    public static List<UserSubscription> findByResourceType(ResourceType resourceType) {
        return list("resourceType = ?1 AND active = true ORDER BY createdAt", resourceType);
    }

    /**
     * Check if a user is subscribed to a resource type.
     *
     * @param userId User ID
     * @param resourceType Resource type
     * @return True if user has an active subscription
     */
    public static boolean isSubscribed(Long userId, ResourceType resourceType) {
        return count("userId = ?1 AND resourceType = ?2 AND active = true",
                    userId, resourceType) > 0;
    }

    /**
     * Find a subscription by user and resource type.
     *
     * @param userId User ID
     * @param resourceType Resource type
     * @return Subscription or null if not found
     */
    public static UserSubscription findByUserAndType(Long userId, ResourceType resourceType) {
        return find("userId = ?1 AND resourceType = ?2", userId, resourceType).firstResult();
    }

    /**
     * Count active subscriptions for a user.
     *
     * @param userId User ID
     * @return Number of active subscriptions
     */
    public static long countActiveByUserId(Long userId) {
        return count("userId = ?1 AND active = true", userId);
    }

    /**
     * Count total subscriptions for a resource type.
     *
     * @param resourceType Resource type
     * @return Number of users subscribed
     */
    public static long countByResourceType(ResourceType resourceType) {
        return count("resourceType = ?1 AND active = true", resourceType);
    }

    // ========== INSTANCE METHODS FOR LIFECYCLE MANAGEMENT ==========

    /**
     * Activate this subscription.
     */
    public void activate() {
        this.active = true;
        this.modifiedAt = LocalDateTime.now();
        this.persist();
    }

    /**
     * Deactivate this subscription (soft disable).
     */
    public void deactivate() {
        this.active = false;
        this.modifiedAt = LocalDateTime.now();
        this.persist();
    }

    /**
     * Toggle subscription active status.
     */
    public void toggle() {
        this.active = !this.active;
        this.modifiedAt = LocalDateTime.now();
        this.persist();
    }

    // ========== CREATE ==========

    /**
     * Create a new subscription for a user.
     *
     * @param userId User ID
     * @param resourceType Resource type to subscribe to
     * @return Created subscription
     * @throws IllegalStateException if subscription already exists
     */
    public static UserSubscription subscribe(Long userId, ResourceType resourceType) {
        // Check if subscription already exists
        UserSubscription existing = findByUserAndType(userId, resourceType);
        if (existing != null) {
            // If exists but inactive, reactivate it
            if (Boolean.FALSE.equals(existing.active)) {
                existing.activate();
                return existing;
            }
            throw new IllegalStateException(
                String.format("User %d is already subscribed to %s", userId, resourceType)
            );
        }

        // Create new subscription
        UserSubscription subscription = new UserSubscription();
        subscription.userId = userId;
        subscription.resourceType = resourceType;
        subscription.active = true;

        subscription.persist();
        return subscription;
    }

    /**
     * Remove a subscription (unsubscribe).
     *
     * @param userId User ID
     * @param resourceType Resource type to unsubscribe from
     * @return True if subscription was removed
     */
    public static boolean unsubscribe(Long userId, ResourceType resourceType) {
        UserSubscription subscription = findByUserAndType(userId, resourceType);
        if (subscription != null) {
            subscription.delete();
            return true;
        }
        return false;
    }

    // ========== UTILITY METHODS ==========

    /**
     * Check if this subscription is currently active.
     *
     * @return True if active
     */
    public boolean isActive() {
        return Boolean.TRUE.equals(this.active);
    }

    @Override
    public String toString() {
        return "UserSubscription{" +
                "id=" + id +
                ", userId=" + userId +
                ", resourceType=" + resourceType +
                ", active=" + active +
                ", createdAt=" + createdAt +
                '}';
    }
}
