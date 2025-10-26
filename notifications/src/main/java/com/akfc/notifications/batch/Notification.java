package com.akfc.notifications.batch;

import jakarta.persistence.*;
import java.time.LocalDateTime;

/**
 * Minimal entity mapping for Notification table in users database.
 *
 * This is a read-only entity used only for batch deletion operations.
 * The full entity definition exists in the users service.
 */
@Entity
@Table(name = "notifications")
public class Notification {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    public Long id;

    @Column(name = "user_id")
    public Long userId;

    @Column(name = "is_read", nullable = false)
    public Boolean isRead;

    @Column(name = "read_at")
    public LocalDateTime readAt;

    @Column(name = "created_at", nullable = false)
    public LocalDateTime createdAt;
}
