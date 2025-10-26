package com.akfc.users.data;

import io.quarkus.hibernate.orm.panache.PanacheRepository;
import jakarta.enterprise.context.ApplicationScoped;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Repository for Notification entity using the REPOSITORY PATTERN.
 *
 * Handles data access logic for notifications, including queries for
 * user notifications, read/unread status, and resource-related notifications.
 */
@ApplicationScoped
public class NotificationRepository implements PanacheRepository<Notification> {

    // ========== CREATE ==========

    /**
     * Create a new notification and persist it.
     *
     * @param userId User ID (can be null for broadcast)
     * @param type Notification type
     * @param title Notification title
     * @param message Notification message
     * @param resourceId Related resource ID (optional)
     * @param resourceType Resource type (optional)
     * @param createdBy System/service creating the notification
     * @return The created and persisted notification
     */
    public Notification createNotification(Long userId, NotificationType type, String title,
                                          String message, Long resourceId, String resourceType,
                                          String createdBy) {
        Notification notification = new Notification();
        notification.userId = userId;
        notification.type = type;
        notification.title = title;
        notification.message = message;
        notification.resourceId = resourceId;
        notification.resourceType = resourceType;
        notification.createdBy = createdBy;

        persist(notification);
        return notification;
    }

    /**
     * Create a notification without resource reference.
     *
     * @param userId User ID (can be null for broadcast)
     * @param type Notification type
     * @param title Notification title
     * @param message Notification message
     * @param createdBy System/service creating the notification
     * @return The created and persisted notification
     */
    public Notification createNotification(Long userId, NotificationType type, String title,
                                          String message, String createdBy) {
        return createNotification(userId, type, title, message, null, null, createdBy);
    }

    /**
     * Create a broadcast notification for all users.
     *
     * @param type Notification type
     * @param title Notification title
     * @param message Notification message
     * @param createdBy System/service creating the notification
     * @return The created and persisted notification
     */
    public Notification createBroadcastNotification(NotificationType type, String title,
                                                   String message, String createdBy) {
        return createNotification(null, type, title, message, createdBy);
    }

    // ========== READ ==========

    /**
     * Find all active notifications (not archived).
     *
     * @return List of all active notifications
     */
    public List<Notification> findAllActive() {
        return list("archived = false ORDER BY createdAt DESC");
    }

    /**
     * Find notification by ID, only if not archived.
     *
     * @param id Notification ID
     * @return Notification or null if not found
     */
    public Notification findActiveById(Long id) {
        return find("id = ?1 AND archived = false", id).firstResult();
    }

    /**
     * Find notification by ID, including archived ones.
     *
     * @param id Notification ID
     * @return Notification or null if not found
     */
    public Notification findByIdIncludingArchived(Long id) {
        return findById(id);
    }

    /**
     * Find all notifications for a specific user.
     *
     * @param userId User ID
     * @return List of notifications for the user
     */
    public List<Notification> findByUserId(Long userId) {
        return list("userId = ?1 AND archived = false ORDER BY createdAt DESC", userId);
    }

    /**
     * Find unread notifications for a specific user.
     *
     * @param userId User ID
     * @return List of unread notifications
     */
    public List<Notification> findUnreadByUserId(Long userId) {
        return list("userId = ?1 AND isRead = false AND archived = false ORDER BY createdAt DESC",
                   userId);
    }

    /**
     * Find read notifications for a specific user.
     *
     * @param userId User ID
     * @return List of read notifications
     */
    public List<Notification> findReadByUserId(Long userId) {
        return list("userId = ?1 AND isRead = true AND archived = false ORDER BY createdAt DESC",
                   userId);
    }

    /**
     * Find notifications by type for a specific user.
     *
     * @param userId User ID
     * @param type Notification type
     * @return List of notifications of the specified type
     */
    public List<Notification> findByUserIdAndType(Long userId, NotificationType type) {
        return list("userId = ?1 AND type = ?2 AND archived = false ORDER BY createdAt DESC",
                   userId, type);
    }

    /**
     * Find notifications related to a specific resource.
     *
     * @param resourceId Resource ID
     * @return List of notifications about this resource
     */
    public List<Notification> findByResourceId(Long resourceId) {
        return list("resourceId = ?1 AND archived = false ORDER BY createdAt DESC", resourceId);
    }

    /**
     * Find all broadcast notifications (userId is null).
     *
     * @return List of broadcast notifications
     */
    public List<Notification> findBroadcastNotifications() {
        return list("userId IS NULL AND archived = false ORDER BY createdAt DESC");
    }

    /**
     * Find recent notifications for a user (last N days).
     *
     * @param userId User ID
     * @param days Number of days to look back
     * @return List of recent notifications
     */
    public List<Notification> findRecentByUserId(Long userId, int days) {
        LocalDateTime cutoffDate = LocalDateTime.now().minusDays(days);
        return list("userId = ?1 AND createdAt >= ?2 AND archived = false ORDER BY createdAt DESC",
                   userId, cutoffDate);
    }

    /**
     * Find notifications created within a time range.
     *
     * @param startDate Start of the range
     * @param endDate End of the range
     * @return List of notifications in the range
     */
    public List<Notification> findByDateRange(LocalDateTime startDate, LocalDateTime endDate) {
        return list("createdAt >= ?1 AND createdAt <= ?2 AND archived = false ORDER BY createdAt DESC",
                   startDate, endDate);
    }

    // ========== UPDATE ==========

    /**
     * Mark a notification as read.
     *
     * @param id Notification ID
     * @return Updated notification
     */
    public Notification markAsRead(Long id) {
        Notification notification = findActiveById(id);
        if (notification != null && Boolean.FALSE.equals(notification.isRead)) {
            notification.markAsRead();
        }
        return notification;
    }

    /**
     * Mark a notification as unread.
     *
     * @param id Notification ID
     * @return Updated notification
     */
    public Notification markAsUnread(Long id) {
        Notification notification = findActiveById(id);
        if (notification != null && Boolean.TRUE.equals(notification.isRead)) {
            notification.markAsUnread();
        }
        return notification;
    }

    /**
     * Mark all notifications for a user as read.
     *
     * @param userId User ID
     * @return Number of notifications marked as read
     */
    public int markAllAsReadForUser(Long userId) {
        return update("isRead = true, readAt = ?1 WHERE userId = ?2 AND isRead = false AND archived = false",
                     LocalDateTime.now(), userId);
    }

    // ========== DELETE ==========

    /**
     * Archive a notification (soft delete).
     *
     * @param id Notification ID
     * @return Archived notification
     */
    public Notification archiveNotification(Long id) {
        Notification notification = findActiveById(id);
        if (notification != null) {
            notification.archive();
        }
        return notification;
    }

    /**
     * Restore an archived notification.
     *
     * @param id Notification ID
     * @return Restored notification
     */
    public Notification restoreNotification(Long id) {
        Notification notification = findById(id);
        if (notification != null && Boolean.TRUE.equals(notification.archived)) {
            notification.restore();
        }
        return notification;
    }

    /**
     * Permanently delete a notification.
     *
     * @param id Notification ID
     * @throws IllegalStateException if notification not archived first
     */
    public void permanentlyDeleteNotification(Long id) {
        Notification notification = findById(id);
        if (notification == null) {
            throw new IllegalArgumentException("Notification not found: " + id);
        }
        notification.permanentlyDelete();
    }

    /**
     * Delete all archived notifications older than specified days.
     *
     * @param days Number of days
     * @return Number of notifications deleted
     */
    public long deleteArchivedOlderThan(int days) {
        LocalDateTime cutoffDate = LocalDateTime.now().minusDays(days);
        return delete("archived = true AND createdAt < ?1", cutoffDate);
    }

    // ========== COUNTS ==========

    /**
     * Count unread notifications for a user.
     *
     * @param userId User ID
     * @return Number of unread notifications
     */
    public long countUnreadByUserId(Long userId) {
        return count("userId = ?1 AND isRead = false AND archived = false", userId);
    }

    /**
     * Count all notifications for a user.
     *
     * @param userId User ID
     * @return Total number of notifications
     */
    public long countByUserId(Long userId) {
        return count("userId = ?1 AND archived = false", userId);
    }

    /**
     * Count notifications by type.
     *
     * @param type Notification type
     * @return Number of notifications of that type
     */
    public long countByType(NotificationType type) {
        return count("type = ?1 AND archived = false", type);
    }

    // ========== EXISTS ==========

    /**
     * Check if notification exists by ID.
     *
     * @param id Notification ID
     * @return True if exists and not archived
     */
    public boolean existsById(Long id) {
        return count("id = ?1 AND archived = false", id) > 0;
    }

    /**
     * Check if user has any unread notifications.
     *
     * @param userId User ID
     * @return True if user has unread notifications
     */
    public boolean hasUnreadNotifications(Long userId) {
        return count("userId = ?1 AND isRead = false AND archived = false", userId) > 0;
    }
}
