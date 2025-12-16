package com.akfc.users.events;

import io.quarkus.kafka.client.serialization.ObjectMapperDeserializer;

/**
 * Kafka deserializer for ResourceCreatedEvent.
 *
 * This class extends ObjectMapperDeserializer to provide type information
 * required for JSON deserialization of Kafka messages.
 */
public class ResourceCreatedEventDeserializer extends ObjectMapperDeserializer<ResourceCreatedEvent> {

    public ResourceCreatedEventDeserializer() {
        super(ResourceCreatedEvent.class);
    }
}
