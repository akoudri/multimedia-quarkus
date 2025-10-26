package com.akfc.catalog.messaging;

import com.akfc.catalog.data.Resource;
import com.akfc.catalog.events.ResourceCreatedEvent;
import io.smallrye.reactive.messaging.rabbitmq.OutgoingRabbitMQMetadata;
import jakarta.enterprise.context.ApplicationScoped;
import jakarta.inject.Inject;
import org.eclipse.microprofile.reactive.messaging.Channel;
import org.eclipse.microprofile.reactive.messaging.Emitter;
import org.eclipse.microprofile.reactive.messaging.Message;
import org.jboss.logging.Logger;

/**
 * Publisher service for resource-related events.
 *
 * Publishes events to RabbitMQ exchange when resources are created, updated, or deleted.
 * Other services (like users service) can subscribe to these events for notifications.
 */
@ApplicationScoped
public class ResourceEventPublisher {

    private static final Logger LOG = Logger.getLogger(ResourceEventPublisher.class);

    /**
     * Emitter for the resource-events channel configured in application.properties.
     *
     * Messages sent to this emitter are published to the RabbitMQ exchange:
     * - Exchange: library.resources
     * - Type: topic
     * - Routing key: determined per message
     */
    @Inject
    @Channel("resource-events")
    Emitter<ResourceCreatedEvent> resourceEventsEmitter;

    /**
     * Publish a resource created event.
     *
     * @param resource The created resource
     */
    public void publishResourceCreated(Resource resource) {
        ResourceCreatedEvent event = new ResourceCreatedEvent(
            resource.id,
            resource.title,
            resource.type.name(),
            resource.year,
            resource.createdAt,
            resource.createdBy
        );

        LOG.infof("Publishing resource created event: %s", event);

        // Create metadata with routing key for topic exchange
        OutgoingRabbitMQMetadata metadata = new OutgoingRabbitMQMetadata.Builder()
            .withRoutingKey("resource.created")
            .build();

        // Send message with metadata
        Message<ResourceCreatedEvent> message = Message.of(event)
            .addMetadata(metadata);

        resourceEventsEmitter.send(message);

        LOG.infof("Resource created event published successfully for resource ID: %d", resource.id);
    }

    /**
     * Publish a resource updated event.
     *
     * @param resource The updated resource
     */
    public void publishResourceUpdated(Resource resource) {
        ResourceCreatedEvent event = new ResourceCreatedEvent(
            resource.id,
            resource.title,
            resource.type.name(),
            resource.year,
            resource.createdAt,
            resource.modifiedBy
        );

        LOG.infof("Publishing resource updated event: %s", event);

        OutgoingRabbitMQMetadata metadata = new OutgoingRabbitMQMetadata.Builder()
            .withRoutingKey("resource.updated")
            .build();

        Message<ResourceCreatedEvent> message = Message.of(event)
            .addMetadata(metadata);

        resourceEventsEmitter.send(message);

        LOG.infof("Resource updated event published successfully for resource ID: %d", resource.id);
    }

    /**
     * Publish a resource deleted event.
     *
     * @param resourceId ID of the deleted resource
     * @param title Title of the deleted resource
     */
    public void publishResourceDeleted(Long resourceId, String title) {
        ResourceCreatedEvent event = new ResourceCreatedEvent(
            resourceId,
            title,
            null,
            null,
            null,
            null
        );

        LOG.infof("Publishing resource deleted event: %s", event);

        OutgoingRabbitMQMetadata metadata = new OutgoingRabbitMQMetadata.Builder()
            .withRoutingKey("resource.deleted")
            .build();

        Message<ResourceCreatedEvent> message = Message.of(event)
            .addMetadata(metadata);

        resourceEventsEmitter.send(message);

        LOG.infof("Resource deleted event published successfully for resource ID: %d", resourceId);
    }
}
