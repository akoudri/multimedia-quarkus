#!/usr/bin/env bash
# ============================================================================
# Build - Build microservices
# ============================================================================
# 12-Factor App: Factor V - Build, Release, Run
# Strictly separate build and run stages
# ============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/config.sh"

# ============================================================================
# Usage
# ============================================================================
usage() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS] [SERVICE...]

Build microservices using Gradle.

Options:
    -h, --help          Show this help message
    -c, --clean         Clean before building
    -t, --test          Run tests after building
    -s, --skip-tests    Skip tests (default)
    -p, --parallel      Build in parallel
    -n, --native        Build native executables (GraalVM required)
    --docker            Build Docker images
    -v, --verbose       Verbose output

Services:
    If no services specified, all services will be built.
    Available: catalog, users, reviews, reactive-reviews, notifications

Examples:
    $(basename "$0")                    # Build all services
    $(basename "$0") -c                 # Clean and build all
    $(basename "$0") catalog users      # Build specific services
    $(basename "$0") -t                 # Build and run tests
    $(basename "$0") --native catalog   # Build native image

EOF
    exit 0
}

# ============================================================================
# Parse Arguments
# ============================================================================
CLEAN=false
RUN_TESTS=false
PARALLEL=false
NATIVE=false
BUILD_DOCKER=false
VERBOSE=false
SERVICES_TO_BUILD=()

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -c|--clean)
            CLEAN=true
            shift
            ;;
        -t|--test)
            RUN_TESTS=true
            shift
            ;;
        -s|--skip-tests)
            RUN_TESTS=false
            shift
            ;;
        -p|--parallel)
            PARALLEL=true
            shift
            ;;
        -n|--native)
            NATIVE=true
            shift
            ;;
        --docker)
            BUILD_DOCKER=true
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        *)
            SERVICES_TO_BUILD+=("$1")
            shift
            ;;
    esac
done

# If no services specified, build all
if [[ ${#SERVICES_TO_BUILD[@]} -eq 0 ]]; then
    SERVICES_TO_BUILD=("${SERVICE_NAMES[@]}")
fi

# ============================================================================
# Pre-flight Checks
# ============================================================================
log_step "Pre-flight checks..."

# Check Java
if ! command_exists java; then
    log_error "Java is not installed"
    exit 1
fi

java_version=$(java -version 2>&1 | head -1 | awk -F '"' '{print $2}' | cut -d'.' -f1)
if [[ "$java_version" -lt 21 ]]; then
    log_error "Java 21+ is required. Found: Java ${java_version}"
    exit 1
fi
log_success "Java ${java_version} found"

# Check for native build requirements
if [[ "$NATIVE" == true ]]; then
    if ! command_exists native-image; then
        log_error "GraalVM native-image is required for native builds"
        log_info "Install GraalVM and run: gu install native-image"
        exit 1
    fi
    log_success "GraalVM native-image found"
fi

# ============================================================================
# Build
# ============================================================================
cd "${PROJECT_ROOT}"

log_step "Building services: ${SERVICES_TO_BUILD[*]}"
echo ""

# Build Gradle tasks
GRADLE_TASKS=""

for service in "${SERVICES_TO_BUILD[@]}"; do
    if [[ "$CLEAN" == true ]]; then
        GRADLE_TASKS="${GRADLE_TASKS} :${service}:clean"
    fi

    if [[ "$NATIVE" == true ]]; then
        GRADLE_TASKS="${GRADLE_TASKS} :${service}:build -Dquarkus.package.type=native"
    else
        GRADLE_TASKS="${GRADLE_TASKS} :${service}:quarkusBuild"
    fi
done

# Build Gradle options
GRADLE_OPTS_ARRAY=()

if [[ "$RUN_TESTS" != true ]]; then
    GRADLE_OPTS_ARRAY+=("-x" "test")
fi

if [[ "$PARALLEL" == true ]]; then
    GRADLE_OPTS_ARRAY+=("--parallel")
fi

if [[ "$VERBOSE" == true ]]; then
    GRADLE_OPTS_ARRAY+=("--info")
fi

# Run build
BUILD_CMD="./gradlew ${GRADLE_TASKS} ${GRADLE_OPTS_ARRAY[*]:-}"
log_info "Running: ${BUILD_CMD}"
echo ""

start_time=$(date +%s)

if eval "${BUILD_CMD}"; then
    end_time=$(date +%s)
    duration=$((end_time - start_time))

    echo ""
    log_success "Build completed in ${duration} seconds"
else
    echo ""
    log_error "Build failed"
    exit 1
fi

# ============================================================================
# Docker Images (Optional)
# ============================================================================
if [[ "$BUILD_DOCKER" == true ]]; then
    log_step "Building Docker images..."

    for service in "${SERVICES_TO_BUILD[@]}"; do
        log_info "Building Docker image for ${service}..."

        if [[ "$NATIVE" == true ]]; then
            docker build -f "${PROJECT_ROOT}/${service}/src/main/docker/Dockerfile.native" \
                -t "library/${service}:latest" \
                "${PROJECT_ROOT}/${service}"
        else
            docker build -f "${PROJECT_ROOT}/${service}/src/main/docker/Dockerfile.jvm" \
                -t "library/${service}:latest" \
                "${PROJECT_ROOT}/${service}"
        fi

        log_success "Docker image built: library/${service}:latest"
    done
fi

# ============================================================================
# Summary
# ============================================================================
echo ""
log_info "Build artifacts:"
for service in "${SERVICES_TO_BUILD[@]}"; do
    jar_path="${PROJECT_ROOT}/${service}/build/quarkus-app/quarkus-run.jar"
    if [[ -f "$jar_path" ]]; then
        size=$(du -h "$jar_path" | cut -f1)
        echo "  - ${service}: ${jar_path} (${size})"
    fi
done
