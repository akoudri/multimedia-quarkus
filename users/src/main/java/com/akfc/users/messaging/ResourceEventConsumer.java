package com.akfc.users.messaging;

import com.akfc.users.data.NotificationType;
import com.akfc.users.data.ResourceType;
import com.akfc.users.events.ResourceCreatedEvent;
import com.akfc.users.services.NotificationService;
import com.akfc.users.services.UserService;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import org.eclipse.microprofile.reactive.messaging.Incoming;
import org.jboss.logging.Logger;

import java.util.List;

/**
 * Consumer service for resource-related events from the catalog service.
 *
 * Consumes events from RabbitMQ to notify users about new resources
 * that might interest them based on their preferences (subscriptions).
 */
@ApplicationScoped
public class ResourceEventConsumer {

    private static final Logger LOG = Logger.getLogger(ResourceEventConsumer.class);

    @Inject
    NotificationService notificationService;

    @Inject
    UserService userService;

    /**
     * Handle resource created events.
     *
     * This method is called automatically when a message arrives on the resource-events queue.
     * The queue is bound to the library.resources exchange with routing keys: resource.created, resource.updated
     *
     * Creates notifications only for users subscribed to the resource type.
     *
     * @param event The resource created event
     */
    @Incoming("resource-events")
    public void onResourceCreated(ResourceCreatedEvent event) {
        LOG.infof("Received resource created event: %s", event);

        try {
            // Parse the resource type from the event
            if (event.type == null) {
                LOG.warnf("Resource event has no type, skipping notification: %s", event);
                return;
            }

            ResourceType resourceType;
            try {
                resourceType = ResourceType.valueOf(event.type.toUpperCase());
            } catch (IllegalArgumentException e) {
                LOG.warnf("Unknown resource type '%s' in event, skipping notification", event.type);
                return;
            }

            // Find all users subscribed to this resource type
            List<Long> subscribedUserIds = userService.getUsersSubscribedTo(resourceType);

            if (subscribedUserIds.isEmpty()) {
                LOG.infof("No users subscribed to %s, no notifications created", resourceType);
                return;
            }

            LOG.infof("Found %d users subscribed to %s", subscribedUserIds.size(), resourceType);

            // Prepare notification content
            String title = String.format("New %s Available", resourceType.toString());
            String message = String.format(
                "A new %s has been added to the catalog: '%s' (%d)",
                resourceType.toString().toLowerCase(),
                event.title,
                event.year != null ? event.year : 0
            );

            // Create individual notifications for each subscribed user
            int notificationCount = 0;
            for (Long userId : subscribedUserIds) {
                try {
                    notificationService.createNotification(
                        userId,
                        NotificationType.RESOURCE_CREATED,
                        title,
                        message,
                        event.resourceId,
                        event.type,
                        "catalog-service"
                    );
                    notificationCount++;
                } catch (Exception e) {
                    LOG.errorf(e, "Failed to create notification for user %d", userId);
                    // Continue with other users even if one fails
                }
            }

            LOG.infof("Created %d notifications for resource ID: %d, Title: '%s', Type: %s",
                    notificationCount, event.resourceId, event.title, event.type);

        } catch (Exception e) {
            LOG.errorf(e, "Error processing resource created event for resource ID: %d", event.resourceId);
            // Note: Depending on error handling strategy, you might want to:
            // - Acknowledge the message (consume it even if processing failed)
            // - Reject the message (send to dead letter queue)
            // - Retry processing (with exponential backoff)
            throw e; // Re-throw to trigger message rejection/retry based on RabbitMQ configuration
        }
    }

    /**
     * Handle resource updated events.
     *
     * Note: The current configuration subscribes to both resource.created and resource.updated
     * routing keys, so this method would be called for updates as well.
     *
     * Creates notifications only for users subscribed to the resource type.
     */
    public void onResourceUpdated(ResourceCreatedEvent event) {
        //TODO
    }
}
