# Shared Configuration Module

This module centralizes common configuration properties across all microservices in the multimedia-library project.

## Purpose

Instead of duplicating configuration across each service's `application.properties`, shared properties are defined once in this module and automatically inherited by all services that depend on it.

## How It Works

1. **Packaged as JAR**: This module is built as a simple JAR containing only the `application.properties` file
2. **Dependency Injection**: Each service declares `implementation project(':shared-config')` in its `build.gradle`
3. **Property Merging**: Quarkus automatically merges properties from all JARs on the classpath
4. **Override Support**: Service-specific properties in individual `application.properties` files override shared defaults

## Shared Properties

The following categories of properties are centralized:

### Infrastructure
- **Database**: PostgreSQL connection defaults (username, password, db-kind)
- **Redis**: Caching configuration
- **RabbitMQ**: Message broker connection settings
- **Consul**: Service discovery and registration

### Development Settings
- **Hibernate**: Database generation strategy, SQL logging
- **HTTP**: Default host binding (0.0.0.0)

### Observability (Full Stack)
- **OpenTelemetry**: Distributed tracing configuration
- **Micrometer/Prometheus**: Metrics collection and export
- **Logging**: Structured format with trace ID correlation

## Service-Specific Properties

Each service maintains its own `application.properties` for:
- HTTP port (8081, 8082, 8083, etc.)
- Database connection URL (different ports/databases per service)
- Application/service name
- OpenTelemetry service name
- Entity-specific cache configuration
- RabbitMQ messaging channels (publishers/subscribers)
- REST client endpoints
- Service-specific features (e.g., Google Books API in catalog)

## Configuration Size Reduction

### Before (Per Service)
- **catalog**: 129 lines
- **users**: 111 lines
- **reviews**: 115 lines
- **reactive-reviews**: 31 lines
- **notifications**: 29 lines

**Total: 415 lines** (with significant duplication)

### After
- **shared-config**: 81 lines (common properties)
- **catalog**: 59 lines (service-specific only)
- **users**: 42 lines
- **reviews**: 45 lines
- **reactive-reviews**: 20 lines
- **notifications**: 29 lines

**Total: 276 lines** (33% reduction, zero duplication)

## Benefits

1. **Single Source of Truth**: Change observability config once, applies everywhere
2. **Consistency**: All services use identical infrastructure settings
3. **Maintainability**: Easier to update shared settings (e.g., switch OTLP endpoint)
4. **Reduced Errors**: No risk of inconsistent configuration across services
5. **Cleaner Service Config**: Service properties files now focus on service-specific concerns

## Usage

### Adding a New Service

1. Add dependency in your service's `build.gradle`:
   ```gradle
   dependencies {
       implementation project(':shared-config')
       // ... other dependencies
   }
   ```

2. Create `application.properties` with only service-specific properties:
   ```properties
   # HTTP port (required - different per service)
   quarkus.http.port=8086

   # Database URL (required - different per service)
   quarkus.datasource.jdbc.url=jdbc:postgresql://localhost:5433/myservice

   # Service name (required)
   quarkus.application.name=my-service
   quarkus.otel.service.name=my-service

   # Service-specific configuration...
   ```

3. All shared properties (database credentials, Redis, RabbitMQ, observability) are automatically inherited

### Overriding Shared Properties

Simply redefine the property in your service's `application.properties`. Service-specific values always take precedence:

```properties
# Override shared database generation strategy
quarkus.hibernate-orm.database.generation=none
```

## Notes

- **Warnings**: Services without certain extensions (e.g., no OpenTelemetry dependency) will show "unrecognized configuration key" warnings at build time. These are harmless - Quarkus ignores properties for missing extensions.
- **No Code**: This module contains no Java code, only configuration resources
- **Build Order**: Gradle automatically builds `shared-config` before dependent services
