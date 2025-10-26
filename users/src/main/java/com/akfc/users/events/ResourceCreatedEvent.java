package com.akfc.users.events;

import java.time.LocalDateTime;

/**
 * Event received when a new resource is created in the catalog.
 *
 * This event is consumed from RabbitMQ to notify users about new resources
 * that might interest them based on their preferences.
 */
public class ResourceCreatedEvent {

    /**
     * Unique identifier of the created resource.
     */
    public Long resourceId;

    /**
     * Title of the resource.
     */
    public String title;

    /**
     * Type of resource (BOOK, MOVIE, MUSIC).
     */
    public String type;

    /**
     * Publication/Release year.
     */
    public Integer year;

    /**
     * Timestamp when the resource was created.
     */
    public LocalDateTime createdAt;

    /**
     * User who created the resource (for audit trail).
     */
    public String createdBy;

    /**
     * Default constructor for JSON deserialization.
     */
    public ResourceCreatedEvent() {
    }

    /**
     * Constructor with all fields.
     */
    public ResourceCreatedEvent(Long resourceId, String title, String type,
                                Integer year, LocalDateTime createdAt, String createdBy) {
        this.resourceId = resourceId;
        this.title = title;
        this.type = type;
        this.year = year;
        this.createdAt = createdAt;
        this.createdBy = createdBy;
    }

    @Override
    public String toString() {
        return "ResourceCreatedEvent{" +
                "resourceId=" + resourceId +
                ", title='" + title + '\'' +
                ", type='" + type + '\'' +
                ", year=" + year +
                ", createdAt=" + createdAt +
                ", createdBy='" + createdBy + '\'' +
                '}';
    }
}
