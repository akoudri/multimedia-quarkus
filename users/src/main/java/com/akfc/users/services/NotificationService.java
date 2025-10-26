package com.akfc.users.services;

import com.akfc.users.data.Notification;
import com.akfc.users.data.NotificationRepository;
import com.akfc.users.data.NotificationType;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import jakarta.transaction.Transactional;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Business service for Notification management.
 *
 * Handles all business logic, validation, and orchestration for notification operations.
 * Provides methods to create, read, update, and delete notifications.
 */
@ApplicationScoped
public class NotificationService {

    @Inject
    NotificationRepository notificationRepository;

    // ========== CREATE ==========

    /**
     * Create a new notification for a user.
     *
     * @param userId User ID (can be null for broadcast)
     * @param type Notification type
     * @param title Notification title
     * @param message Notification message
     * @param resourceId Related resource ID (optional)
     * @param resourceType Resource type (optional)
     * @param createdBy System/service creating the notification
     * @return Created notification
     */
    @Transactional
    public Notification createNotification(Long userId, NotificationType type, String title,
                                          String message, Long resourceId, String resourceType,
                                          String createdBy) {
        return notificationRepository.createNotification(
            userId, type, title, message, resourceId, resourceType, createdBy
        );
    }

    /**
     * Create a notification without resource reference.
     *
     * @param userId User ID (can be null for broadcast)
     * @param type Notification type
     * @param title Notification title
     * @param message Notification message
     * @param createdBy System/service creating the notification
     * @return Created notification
     */
    @Transactional
    public Notification createNotification(Long userId, NotificationType type, String title,
                                          String message, String createdBy) {
        return notificationRepository.createNotification(userId, type, title, message, createdBy);
    }

    /**
     * Create a broadcast notification for all users.
     *
     * @param type Notification type
     * @param title Notification title
     * @param message Notification message
     * @param createdBy System/service creating the notification
     * @return Created notification
     */
    @Transactional
    public Notification createBroadcastNotification(NotificationType type, String title,
                                                   String message, String createdBy) {
        return notificationRepository.createBroadcastNotification(type, title, message, createdBy);
    }

    // ========== READ ==========

    /**
     * Get notification by ID.
     *
     * @param id Notification ID
     * @return Notification or null if not found
     */
    public Notification getNotificationById(Long id) {
        return notificationRepository.findActiveById(id);
    }

    /**
     * Get all notifications for a user.
     *
     * @param userId User ID
     * @return List of notifications
     */
    public List<Notification> getNotificationsByUserId(Long userId) {
        return notificationRepository.findByUserId(userId);
    }

    /**
     * Get unread notifications for a user.
     *
     * @param userId User ID
     * @return List of unread notifications
     */
    public List<Notification> getUnreadNotificationsByUserId(Long userId) {
        return notificationRepository.findUnreadByUserId(userId);
    }

    /**
     * Get read notifications for a user.
     *
     * @param userId User ID
     * @return List of read notifications
     */
    public List<Notification> getReadNotificationsByUserId(Long userId) {
        return notificationRepository.findReadByUserId(userId);
    }

    /**
     * Get notifications by type for a user.
     *
     * @param userId User ID
     * @param type Notification type
     * @return List of notifications
     */
    public List<Notification> getNotificationsByUserIdAndType(Long userId, NotificationType type) {
        return notificationRepository.findByUserIdAndType(userId, type);
    }

    /**
     * Get recent notifications for a user.
     *
     * @param userId User ID
     * @param days Number of days to look back
     * @return List of recent notifications
     */
    public List<Notification> getRecentNotifications(Long userId, int days) {
        return notificationRepository.findRecentByUserId(userId, days);
    }

    /**
     * Get all broadcast notifications.
     *
     * @return List of broadcast notifications
     */
    public List<Notification> getBroadcastNotifications() {
        return notificationRepository.findBroadcastNotifications();
    }

    /**
     * Get all notifications.
     *
     * @return List of all active notifications
     */
    public List<Notification> getAllNotifications() {
        return notificationRepository.findAllActive();
    }

    // ========== UPDATE ==========

    /**
     * Mark a notification as read.
     *
     * @param id Notification ID
     * @return Updated notification
     */
    @Transactional
    public Notification markAsRead(Long id) {
        return notificationRepository.markAsRead(id);
    }

    /**
     * Mark a notification as unread.
     *
     * @param id Notification ID
     * @return Updated notification
     */
    @Transactional
    public Notification markAsUnread(Long id) {
        return notificationRepository.markAsUnread(id);
    }

    /**
     * Mark all notifications for a user as read.
     *
     * @param userId User ID
     * @return Number of notifications marked as read
     */
    @Transactional
    public int markAllAsReadForUser(Long userId) {
        return notificationRepository.markAllAsReadForUser(userId);
    }

    // ========== DELETE ==========

    /**
     * Archive a notification (soft delete).
     *
     * @param id Notification ID
     * @return Archived notification
     */
    @Transactional
    public Notification archiveNotification(Long id) {
        return notificationRepository.archiveNotification(id);
    }

    /**
     * Restore an archived notification.
     *
     * @param id Notification ID
     * @return Restored notification
     */
    @Transactional
    public Notification restoreNotification(Long id) {
        return notificationRepository.restoreNotification(id);
    }

    /**
     * Permanently delete a notification.
     *
     * @param id Notification ID
     */
    @Transactional
    public void deleteNotification(Long id) {
        notificationRepository.permanentlyDeleteNotification(id);
    }

    /**
     * Delete old archived notifications.
     *
     * @param days Number of days - delete notifications older than this
     * @return Number of notifications deleted
     */
    @Transactional
    public long deleteOldArchivedNotifications(int days) {
        return notificationRepository.deleteArchivedOlderThan(days);
    }

    // ========== UTILITY METHODS ==========

    /**
     * Count unread notifications for a user.
     *
     * @param userId User ID
     * @return Number of unread notifications
     */
    public long countUnreadNotifications(Long userId) {
        return notificationRepository.countUnreadByUserId(userId);
    }

    /**
     * Count all notifications for a user.
     *
     * @param userId User ID
     * @return Total number of notifications
     */
    public long countNotifications(Long userId) {
        return notificationRepository.countByUserId(userId);
    }

    /**
     * Check if user has unread notifications.
     *
     * @param userId User ID
     * @return True if user has unread notifications
     */
    public boolean hasUnreadNotifications(Long userId) {
        return notificationRepository.hasUnreadNotifications(userId);
    }

    /**
     * Check if notification exists.
     *
     * @param id Notification ID
     * @return True if exists and not archived
     */
    public boolean notificationExists(Long id) {
        return notificationRepository.existsById(id);
    }
}
