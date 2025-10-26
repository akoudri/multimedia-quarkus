package com.akfc.users.data;

import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import org.hibernate.annotations.Cache;
import org.hibernate.annotations.CacheConcurrencyStrategy;
import org.hibernate.annotations.CreationTimestamp;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Entity representing a notification for a user.
 *
 * Notifications are created when events occur in the system that users should be aware of,
 * such as new resources being created, updates to resources they follow, or system announcements.
 *
 * This entity uses the ACTIVE RECORD pattern via PanacheEntityBase.
 */
@Entity
@Table(name = "notifications", indexes = {
    @Index(name = "idx_notification_user", columnList = "user_id"),
    @Index(name = "idx_notification_type", columnList = "type"),
    @Index(name = "idx_notification_read", columnList = "is_read"),
    @Index(name = "idx_notification_created", columnList = "created_at"),
    @Index(name = "idx_notification_resource", columnList = "resource_id")
})
@Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class Notification extends PanacheEntityBase {

    // ========== PRIMARY KEY ==========

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    public Long id;

    // ========== BUSINESS FIELDS ==========

    /**
     * User ID this notification is for.
     * Can be null for system-wide broadcast notifications.
     */
    @Column(name = "user_id")
    public Long userId;

    /**
     * Type of notification.
     */
    @NotNull(message = "Notification type is required")
    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 30)
    public NotificationType type;

    /**
     * Notification title/subject.
     */
    @NotBlank(message = "Title is required")
    @Size(max = 255, message = "Title must not exceed 255 characters")
    @Column(nullable = false, length = 255)
    public String title;

    /**
     * Notification message/body.
     */
    @NotBlank(message = "Message is required")
    @Size(max = 1000, message = "Message must not exceed 1000 characters")
    @Column(nullable = false, length = 1000)
    public String message;

    /**
     * Related resource ID (if applicable).
     * References a resource in the catalog service.
     */
    @Column(name = "resource_id")
    public Long resourceId;

    /**
     * Resource type (BOOK, MOVIE, MUSIC) for reference.
     */
    @Size(max = 50, message = "Resource type must not exceed 50 characters")
    @Column(name = "resource_type", length = 50)
    public String resourceType;

    /**
     * Whether the notification has been read by the user.
     */
    @NotNull
    @Column(name = "is_read", nullable = false)
    public Boolean isRead = false;

    /**
     * When the notification was read.
     */
    @Column(name = "read_at")
    public LocalDateTime readAt;

    /**
     * Soft delete flag.
     * When true, notification is hidden from normal queries but retained for audit.
     */
    @Column(nullable = false)
    public Boolean archived = false;

    // ========== AUDIT/TIMESTAMP FIELDS ==========

    /**
     * Automatic creation timestamp using Hibernate annotation.
     * Set once when entity is first persisted.
     */
    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    public LocalDateTime createdAt;

    /**
     * System or service that created this notification.
     */
    @Size(max = 100, message = "Created by must not exceed 100 characters")
    @Column(name = "created_by", length = 100)
    public String createdBy;

    // ========== JPA LIFECYCLE CALLBACKS ==========

    /**
     * Called automatically before entity is persisted.
     * Initializes default values.
     */
    @PrePersist
    protected void onCreate() {
        // Ensure read flag is set
        if (isRead == null) {
            isRead = false;
        }

        // Ensure archived flag is set
        if (archived == null) {
            archived = false;
        }
    }

    // ========== ACTIVE RECORD PATTERN - BUSINESS METHODS ==========

    /**
     * Find all notifications for a specific user.
     *
     * @param userId User ID
     * @return List of notifications for the user
     */
    public static List<Notification> findByUserId(Long userId) {
        return list("userId = ?1 AND archived = false ORDER BY createdAt DESC", userId);
    }

    /**
     * Find unread notifications for a specific user.
     *
     * @param userId User ID
     * @return List of unread notifications
     */
    public static List<Notification> findUnreadByUserId(Long userId) {
        return list("userId = ?1 AND isRead = false AND archived = false ORDER BY createdAt DESC", userId);
    }

    /**
     * Find notifications by type for a specific user.
     *
     * @param userId User ID
     * @param type Notification type
     * @return List of notifications of the specified type
     */
    public static List<Notification> findByUserIdAndType(Long userId, NotificationType type) {
        return list("userId = ?1 AND type = ?2 AND archived = false ORDER BY createdAt DESC", userId, type);
    }

    /**
     * Find notifications related to a specific resource.
     *
     * @param resourceId Resource ID
     * @return List of notifications about this resource
     */
    public static List<Notification> findByResourceId(Long resourceId) {
        return list("resourceId = ?1 AND archived = false ORDER BY createdAt DESC", resourceId);
    }

    /**
     * Find all broadcast notifications (userId is null).
     *
     * @return List of broadcast notifications
     */
    public static List<Notification> findBroadcastNotifications() {
        return list("userId IS NULL AND archived = false ORDER BY createdAt DESC");
    }

    /**
     * Count unread notifications for a user.
     *
     * @param userId User ID
     * @return Number of unread notifications
     */
    public static long countUnreadByUserId(Long userId) {
        return count("userId = ?1 AND isRead = false AND archived = false", userId);
    }

    /**
     * Find recent notifications for a user (last N days).
     *
     * @param userId User ID
     * @param days Number of days to look back
     * @return List of recent notifications
     */
    public static List<Notification> findRecentByUserId(Long userId, int days) {
        LocalDateTime cutoffDate = LocalDateTime.now().minusDays(days);
        return list("userId = ?1 AND createdAt >= ?2 AND archived = false ORDER BY createdAt DESC",
                   userId, cutoffDate);
    }

    // ========== INSTANCE METHODS FOR LIFECYCLE MANAGEMENT ==========

    /**
     * Mark this notification as read.
     */
    public void markAsRead() {
        if (Boolean.FALSE.equals(isRead)) {
            this.isRead = true;
            this.readAt = LocalDateTime.now();
            this.persist();
        }
    }

    /**
     * Mark this notification as unread.
     */
    public void markAsUnread() {
        if (Boolean.TRUE.equals(isRead)) {
            this.isRead = false;
            this.readAt = null;
            this.persist();
        }
    }

    /**
     * Archive this notification (soft delete).
     */
    public void archive() {
        this.archived = true;
        this.persist();
    }

    /**
     * Restore an archived notification.
     */
    public void restore() {
        this.archived = false;
        this.persist();
    }

    // ========== CREATE ==========

    /**
     * Create a new notification and persist it to the database.
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
    public static Notification create(Long userId, NotificationType type, String title,
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

        notification.persist();
        return notification;
    }

    /**
     * Simplified create method without resource reference.
     *
     * @param userId User ID (can be null for broadcast)
     * @param type Notification type
     * @param title Notification title
     * @param message Notification message
     * @param createdBy System/service creating the notification
     * @return The created and persisted notification
     */
    public static Notification create(Long userId, NotificationType type, String title,
                                     String message, String createdBy) {
        return create(userId, type, title, message, null, null, createdBy);
    }

    /**
     * Create a broadcast notification (for all users).
     *
     * @param type Notification type
     * @param title Notification title
     * @param message Notification message
     * @param createdBy System/service creating the notification
     * @return The created and persisted notification
     */
    public static Notification createBroadcast(NotificationType type, String title,
                                              String message, String createdBy) {
        return create(null, type, title, message, createdBy);
    }

    // ========== HARD DELETE ==========

    /**
     * Permanently delete this notification from the database.
     *
     * @throws IllegalStateException if notification is not archived first
     */
    public void permanentlyDelete() {
        if (Boolean.FALSE.equals(archived)) {
            throw new IllegalStateException(
                "Cannot permanently delete a non-archived notification. Archive it first.");
        }
        this.delete();
    }

    // ========== UTILITY METHODS ==========

    /**
     * Check if this is a broadcast notification.
     *
     * @return True if userId is null
     */
    public boolean isBroadcast() {
        return this.userId == null;
    }

    /**
     * Check if this notification has been read.
     *
     * @return True if notification has been read
     */
    public boolean hasBeenRead() {
        return Boolean.TRUE.equals(this.isRead);
    }

    /**
     * Check if this notification is about a resource.
     *
     * @return True if resourceId is not null
     */
    public boolean hasResource() {
        return this.resourceId != null;
    }
}
