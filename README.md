# Bibliothèque Multimédia - Quarkus Mono-Repo

A Gradle mono-repository project containing three independent Quarkus microservices for managing a multimedia library system.

## Project Structure

```
multimedia-library/
├── catalogue/          # Catalog management service
│   ├── src/
│   │   ├── main/
│   │   │   ├── java/
│   │   │   └── resources/
│   │   └── test/
│   └── build.gradle
├── users/             # User management service
│   ├── src/
│   │   ├── main/
│   │   │   ├── java/
│   │   │   └── resources/
│   │   └── test/
│   └── build.gradle
├── reviews/           # Reviews and ratings service
│   ├── src/
│   │   ├── main/
│   │   │   ├── java/
│   │   │   └── resources/
│   │   └── test/
│   └── build.gradle
├── build.gradle       # Root build configuration
├── settings.gradle    # Multi-module project settings
└── gradle.properties  # Shared Gradle properties
```

## Philosophy

This project follows a **mono-repo multi-module architecture** where:

- **Independent Services**: Each module (`catalogue`, `users`, `reviews`) is an autonomous microservice with its own domain logic and can be developed, tested, and deployed independently.
- **Centralized Configuration**: Shared dependencies and build configurations are managed at the root level to ensure consistency across modules.
- **Quarkus Framework**: All modules leverage Quarkus for fast startup times, low memory footprint, and developer productivity with live coding.
- **Gradle Multi-Project Build**: Uses Gradle's multi-project capabilities to manage all modules from a single repository while maintaining module independence.

### Benefits of This Approach:

1. **Single Source of Truth**: All related services in one repository
2. **Shared Build Configuration**: Dependency management centralized in root `build.gradle`
3. **Easy Refactoring**: Changes across multiple services can be committed atomically
4. **Consistent Tooling**: Same build tools, testing frameworks, and development practices
5. **Simplified CI/CD**: Build and test all services in a single pipeline

## Prerequisites

- Java 17 or higher
- Gradle 8.x or higher (wrapper included)
- Quarkus CLI (optional, for generating modules)

## Getting Started

### Building All Modules

From the root directory:

```bash
./gradlew build
```

### Building a Specific Module

```bash
./gradlew :catalogue:build
./gradlew :users:build
./gradlew :reviews:build
```

## Running Services in Development Mode

Quarkus provides a powerful development mode with live reload capabilities.

### Start the Catalogue Service

```bash
cd catalogue
../gradlew quarkusDev
```

Or from root:

```bash
./gradlew :catalogue:quarkusDev
```

Default URL: http://localhost:8080

### Start the Users Service

```bash
cd users
../gradlew quarkusDev
```

Or from root:

```bash
./gradlew :users:quarkusDev
```

To avoid port conflicts, configure a different port in `users/src/main/resources/application.properties`:
```properties
quarkus.http.port=8081
```

### Start the Reviews Service

```bash
cd reviews
../gradlew quarkusDev
```

Or from root:

```bash
./gradlew :reviews:quarkusDev
```

To avoid port conflicts, configure a different port in `reviews/src/main/resources/application.properties`:
```properties
quarkus.http.port=8082
```

### Running Multiple Services Simultaneously

Open separate terminal windows for each service or use a process manager like `tmux` or `screen`:

```bash
# Terminal 1
./gradlew :catalogue:quarkusDev

# Terminal 2
./gradlew :users:quarkusDev

# Terminal 3
./gradlew :reviews:quarkusDev
```

## Testing

### Run All Tests

```bash
./gradlew test
```

### Run Tests for a Specific Module

```bash
./gradlew :catalogue:test
./gradlew :users:test
./gradlew :reviews:test
```

## Dependencies

Each module includes the following core dependencies:

- **Quarkus REST (RESTEasy Reactive)**: For building REST APIs
- **Quarkus Arc**: CDI dependency injection container
- **Quarkus JUnit5**: Testing framework integration
- **REST Assured**: REST API testing

Additional dependencies can be added per module in their respective `build.gradle` files.

## Adding New Dependencies

### To All Modules

Edit the root `build.gradle` file in the `subprojects` section:

```gradle
subprojects {
    dependencies {
        implementation 'io.quarkus:quarkus-some-extension'
    }
}
```

### To a Specific Module

Edit the module's `build.gradle` file:

```gradle
dependencies {
    implementation 'io.quarkus:quarkus-hibernate-orm-panache'
    implementation 'io.quarkus:quarkus-jdbc-postgresql'
}
```

## Creating New Modules (Optional)

To add a new module using Quarkus CLI:

```bash
cd multimedia-library
quarkus create app com.library:module-name --gradle --extension=resteasy-reactive,junit5 --no-code
```

Then update `settings.gradle` to include the new module:

```gradle
include 'module-name'
```

## Project Configuration Files

- **`settings.gradle`**: Defines the multi-module project structure
- **`build.gradle`** (root): Centralized dependency and plugin management
- **`gradle.properties`**: Quarkus version and shared properties
- **Module `build.gradle`**: Module-specific dependencies and configuration

## Useful Quarkus Dev Mode Features

- **Live Reload**: Changes to Java code are automatically recompiled
- **Dev UI**: Access at http://localhost:8080/q/dev
- **Continuous Testing**: Press `r` in the terminal to run tests continuously

## Packaging

### Package All Modules

```bash
./gradlew build
```

### Package a Specific Module

```bash
./gradlew :catalogue:build
```

The packaged application will be available in `<module>/build/quarkus-app/`.

### Create Uber JAR

To create a standalone uber-jar:

```bash
./gradlew build -Dquarkus.package.jar.type=uber-jar
```

## Running in Production

```bash
java -jar catalogue/build/quarkus-app/quarkus-run.jar
```

## Docker Support

Each module includes Dockerfiles generated by Quarkus in `src/main/docker/`.

Build Docker image:

```bash
cd catalogue
docker build -f src/main/docker/Dockerfile.jvm -t bibliotheque/catalogue .
```

## Resources

- [Quarkus Documentation](https://quarkus.io/guides/)
- [Gradle Multi-Project Builds](https://docs.gradle.org/current/userguide/multi_project_builds.html)
- [Quarkus REST Guide](https://quarkus.io/guides/rest)

## License

This project is provided as-is for educational and development purposes.
