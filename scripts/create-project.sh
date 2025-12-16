#!/bin/bash
# Script to automate the creation of a Quarkus Gradle mono-repo project
# Usage: ./create-project.sh [project-name]

# Configuration
PROJECT_NAME="${1:-multimedia-library}"
GROUP_ID="com.library"
MODULES=("catalogue" "users" "reviews")
QUARKUS_VERSION="3.28.4"

echo "Creating Quarkus mono-repo project: $PROJECT_NAME"
echo "================================================"

# Create root directory
mkdir -p "$PROJECT_NAME"
cd "$PROJECT_NAME" || exit 1

# Generate each module
for module in "${MODULES[@]}"; do
    echo ""
    echo "Creating module: $module"
    quarkus create app "${GROUP_ID}:${module}" \
        --gradle \
        --extension=resteasy-reactive,junit5 \
        --no-code
done

# Copy Gradle wrapper to root
echo ""
echo "Setting up Gradle wrapper at root level..."
cp -r "${MODULES[0]}/gradle" .
cp "${MODULES[0]}/gradlew" .
cp "${MODULES[0]}/gradlew.bat" .

# Remove individual settings.gradle files
echo "Cleaning up individual settings.gradle files..."
for module in "${MODULES[@]}"; do
    rm -f "${module}/settings.gradle"
done

# Create root gradle.properties
echo "Creating root gradle.properties..."
cat > gradle.properties << 'EOF'
# Gradle properties
quarkusPluginId=io.quarkus
quarkusPluginVersion=3.28.4
quarkusPlatformGroupId=io.quarkus.platform
quarkusPlatformArtifactId=quarkus-bom
quarkusPlatformVersion=3.28.4
EOF

# Create root settings.gradle
echo "Creating root settings.gradle..."
cat > settings.gradle << EOF
pluginManagement {
    repositories {
        mavenCentral()
        gradlePluginPortal()
        mavenLocal()
    }
    plugins {
        id "\${quarkusPluginId}" version "\${quarkusPluginVersion}"
    }
}

rootProject.name = '$PROJECT_NAME'

include 'catalogue'
include 'users'
include 'reviews'
EOF

# Create root build.gradle
echo "Creating root build.gradle..."
cat > build.gradle << 'EOF'
allprojects {
    group 'com.library'
    version '1.0.0-SNAPSHOT'

    repositories {
        mavenCentral()
        mavenLocal()
    }
}

subprojects {
    apply plugin: 'java'

    java {
        sourceCompatibility = JavaVersion.VERSION_21
        targetCompatibility = JavaVersion.VERSION_21
    }

    dependencies {
        implementation enforcedPlatform("${quarkusPlatformGroupId}:${quarkusPlatformArtifactId}:${quarkusPlatformVersion}")
        implementation 'io.quarkus:quarkus-rest'
        implementation 'io.quarkus:quarkus-arc'

        testImplementation 'io.quarkus:quarkus-junit5'
        testImplementation 'io.rest-assured:rest-assured'
    }

    test {
        systemProperty "java.util.logging.manager", "org.jboss.logmanager.LogManager"
    }
}
EOF

# Update each module's build.gradle
echo "Updating module build.gradle files..."
for module in "${MODULES[@]}"; do
    cat > "${module}/build.gradle" << 'EOF'
plugins {
    id 'java'
    id 'io.quarkus'
}

// Module-specific dependencies can be added here
dependencies {
    // Add module-specific dependencies here if needed
}

compileJava {
    options.encoding = 'UTF-8'
    options.compilerArgs << '-parameters'
}

compileTestJava {
    options.encoding = 'UTF-8'
}
EOF
done

# Configure different ports for each service
echo "Configuring unique ports for each service..."
echo "quarkus.http.port=8080" >> catalogue/src/main/resources/application.properties
echo "quarkus.http.port=8081" >> users/src/main/resources/application.properties
echo "quarkus.http.port=8082" >> reviews/src/main/resources/application.properties

echo ""
echo "================================================"
echo "Project setup complete!"
echo ""
echo "Verifying project structure..."
./gradlew projects

echo ""
echo "Building all modules..."
./gradlew build

echo ""
echo "================================================"
echo "SUCCESS! Your Quarkus mono-repo is ready."
echo ""
echo "Next steps:"
echo "  1. cd $PROJECT_NAME"
echo "  2. Start catalogue service: ./gradlew :catalogue:quarkusDev"
echo "  3. Start users service: ./gradlew :users:quarkusDev (in new terminal)"
echo "  4. Start reviews service: ./gradlew :reviews:quarkusDev (in new terminal)"
echo ""
echo "Service URLs:"
echo "  - Catalogue: http://localhost:8080"
echo "  - Users: http://localhost:8081"
echo "  - Reviews: http://localhost:8082"
echo ""
