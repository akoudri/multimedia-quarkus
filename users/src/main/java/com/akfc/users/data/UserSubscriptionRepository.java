package com.akfc.users.data;

import io.quarkus.hibernate.orm.panache.PanacheRepository;
import jakarta.enterprise.context.ApplicationScoped;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Repository for UserSubscription entity using the REPOSITORY PATTERN.
 *
 * Handles data access logic for user subscriptions to resource types.
 */
@ApplicationScoped
public class UserSubscriptionRepository implements PanacheRepository<UserSubscription> {

    // ========== CREATE ==========

    /**
     * Subscribe a user to a resource type.
     *
     * @param userId User ID
     * @param resourceType Resource type
     * @return Created or reactivated subscription
     */
    public UserSubscription subscribe(Long userId, ResourceType resourceType) {
        return UserSubscription.subscribe(userId, resourceType);
    }

    /**
     * Unsubscribe a user from a resource type.
     *
     * @param userId User ID
     * @param resourceType Resource type
     * @return True if subscription was removed
     */
    public boolean unsubscribe(Long userId, ResourceType resourceType) {
        return UserSubscription.unsubscribe(userId, resourceType);
    }

    // ========== READ ==========

    /**
     * Find all subscriptions for a user.
     *
     * @param userId User ID
     * @return List of subscriptions
     */
    public List<UserSubscription> findByUserId(Long userId) {
        return list("userId = ?1 ORDER BY resourceType", userId);
    }

    /**
     * Find active subscriptions for a user.
     *
     * @param userId User ID
     * @return List of active subscriptions
     */
    public List<UserSubscription> findActiveByUserId(Long userId) {
        return list("userId = ?1 AND active = true ORDER BY resourceType", userId);
    }

    /**
     * Find all users subscribed to a specific resource type.
     *
     * @param resourceType Resource type
     * @return List of user IDs subscribed to this type
     */
    public List<Long> findUserIdsSubscribedTo(ResourceType resourceType) {
        return find("resourceType = ?1 AND active = true", resourceType)
            .stream()
            .map(sub -> ((UserSubscription) sub).userId)
            .collect(Collectors.toList());
    }

    /**
     * Find all subscriptions for a specific resource type.
     *
     * @param resourceType Resource type
     * @return List of subscriptions
     */
    public List<UserSubscription> findByResourceType(ResourceType resourceType) {
        return list("resourceType = ?1 AND active = true ORDER BY createdAt", resourceType);
    }

    /**
     * Find a subscription by user and resource type.
     *
     * @param userId User ID
     * @param resourceType Resource type
     * @return Subscription or null if not found
     */
    public UserSubscription findByUserAndType(Long userId, ResourceType resourceType) {
        return find("userId = ?1 AND resourceType = ?2", userId, resourceType).firstResult();
    }

    // ========== UPDATE ==========

    /**
     * Activate a subscription.
     *
     * @param userId User ID
     * @param resourceType Resource type
     * @return Updated subscription or null if not found
     */
    public UserSubscription activateSubscription(Long userId, ResourceType resourceType) {
        UserSubscription subscription = findByUserAndType(userId, resourceType);
        if (subscription != null) {
            subscription.activate();
        }
        return subscription;
    }

    /**
     * Deactivate a subscription.
     *
     * @param userId User ID
     * @param resourceType Resource type
     * @return Updated subscription or null if not found
     */
    public UserSubscription deactivateSubscription(Long userId, ResourceType resourceType) {
        UserSubscription subscription = findByUserAndType(userId, resourceType);
        if (subscription != null) {
            subscription.deactivate();
        }
        return subscription;
    }

    /**
     * Toggle subscription active status.
     *
     * @param userId User ID
     * @param resourceType Resource type
     * @return Updated subscription or null if not found
     */
    public UserSubscription toggleSubscription(Long userId, ResourceType resourceType) {
        UserSubscription subscription = findByUserAndType(userId, resourceType);
        if (subscription != null) {
            subscription.toggle();
        }
        return subscription;
    }

    // ========== DELETE ==========

    /**
     * Delete all subscriptions for a user.
     *
     * @param userId User ID
     * @return Number of subscriptions deleted
     */
    public long deleteByUserId(Long userId) {
        return delete("userId", userId);
    }

    /**
     * Delete all inactive subscriptions (cleanup).
     *
     * @return Number of subscriptions deleted
     */
    public long deleteInactive() {
        return delete("active", false);
    }

    // ========== CHECKS ==========

    /**
     * Check if a user is subscribed to a resource type.
     *
     * @param userId User ID
     * @param resourceType Resource type
     * @return True if user has an active subscription
     */
    public boolean isSubscribed(Long userId, ResourceType resourceType) {
        return count("userId = ?1 AND resourceType = ?2 AND active = true",
                    userId, resourceType) > 0;
    }

    /**
     * Check if subscription exists (active or inactive).
     *
     * @param userId User ID
     * @param resourceType Resource type
     * @return True if subscription exists
     */
    public boolean exists(Long userId, ResourceType resourceType) {
        return count("userId = ?1 AND resourceType = ?2", userId, resourceType) > 0;
    }

    // ========== COUNTS ==========

    /**
     * Count active subscriptions for a user.
     *
     * @param userId User ID
     * @return Number of active subscriptions
     */
    public long countActiveByUserId(Long userId) {
        return count("userId = ?1 AND active = true", userId);
    }

    /**
     * Count total subscriptions for a user.
     *
     * @param userId User ID
     * @return Total number of subscriptions
     */
    public long countByUserId(Long userId) {
        return count("userId", userId);
    }

    /**
     * Count total subscribers for a resource type.
     *
     * @param resourceType Resource type
     * @return Number of users subscribed
     */
    public long countByResourceType(ResourceType resourceType) {
        return count("resourceType = ?1 AND active = true", resourceType);
    }

    /**
     * Get subscription statistics.
     *
     * @return List of [resourceType, count] for each type
     */
    public List<Object[]> getSubscriptionStats() {
        return getEntityManager().createQuery(
            "SELECT s.resourceType, COUNT(s) " +
            "FROM UserSubscription s " +
            "WHERE s.active = true " +
            "GROUP BY s.resourceType " +
            "ORDER BY s.resourceType",
            Object[].class
        ).getResultList();
    }
}
