package com.akfc.catalog.data;

import io.quarkus.test.common.QuarkusTestResourceLifecycleManager;
import org.testcontainers.containers.PostgreSQLContainer;

import java.util.Map;

/**
 * Testcontainers resource for PostgreSQL database.
 *
 * This class manages the lifecycle of a PostgreSQL Docker container for testing:
 * - Starts a PostgreSQL container before tests
 * - Configures Quarkus to use the container
 * - Stops the container after tests
 *
 * Benefits:
 * - Real PostgreSQL database for testing
 * - Isolated test environment
 * - Automatic cleanup
 * - Production-like testing without mocking
 *
 * Usage:
 * Add @QuarkusTestResource(PostgresTestResource.class) to test classes
 */
public class PostgresTestResource implements QuarkusTestResourceLifecycleManager {

    private static final String POSTGRES_IMAGE = "postgres:15-alpine";
    private static final String DATABASE_NAME = "catalog_test";
    private static final String USERNAME = "test";
    private static final String PASSWORD = "test";

    private PostgreSQLContainer<?> postgresContainer;

    /**
     * Start PostgreSQL container before tests.
     *
     * This method is called once before any tests in the test class run.
     * It configures and starts the PostgreSQL container.
     *
     * @return Map of properties to override in application.properties
     */
    @Override
    public Map<String, String> start() {
        // Create and configure PostgreSQL container
        postgresContainer = new PostgreSQLContainer<>(POSTGRES_IMAGE)
            .withDatabaseName(DATABASE_NAME)
            .withUsername(USERNAME)
            .withPassword(PASSWORD)
            .withReuse(false);  // Don't reuse containers between test runs

        // Start the container
        postgresContainer.start();

        // Return configuration properties for Quarkus
        // These override the values in application.properties
        return Map.of(
            "quarkus.datasource.jdbc.url", postgresContainer.getJdbcUrl(),
            "quarkus.datasource.username", postgresContainer.getUsername(),
            "quarkus.datasource.password", postgresContainer.getPassword(),
            "quarkus.datasource.db-kind", "postgresql",
            "quarkus.hibernate-orm.database.generation", "drop-and-create",
            "quarkus.hibernate-orm.log.sql", "false"
        );
    }

    /**
     * Stop PostgreSQL container after tests.
     *
     * This method is called once after all tests in the test class complete.
     * It stops and removes the container.
     */
    @Override
    public void stop() {
        if (postgresContainer != null) {
            postgresContainer.stop();
            postgresContainer.close();
        }
    }

    /**
     * Get the JDBC URL of the running container.
     * Useful for debugging or logging.
     *
     * @return JDBC URL
     */
    public String getJdbcUrl() {
        return postgresContainer != null ? postgresContainer.getJdbcUrl() : null;
    }
}
