#!/usr/bin/env bash
# ============================================================================
# Dev - Start a microservice in development mode
# ============================================================================
# 12-Factor App: Factor X - Dev/Prod Parity
# Keep development, staging, and production as similar as possible
# ============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/config.sh"

# ============================================================================
# Usage
# ============================================================================
usage() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS] SERVICE

Start a microservice in Quarkus dev mode with live reload.

Options:
    -h, --help          Show this help message
    -p, --port PORT     Override default port
    --debug             Enable remote debugging (port 5005)
    --suspend           Suspend until debugger attaches
    --clean             Clean before starting

Services:
    catalog             Catalog service (port 8081)
    users               Users service (port 8082)
    reviews             Reviews service (port 8083)
    reactive-reviews    Reactive Reviews service (port 8084)
    notifications       Notifications batch service (port 8085)

Examples:
    $(basename "$0") catalog              # Start catalog in dev mode
    $(basename "$0") users --debug        # Start users with debugging
    $(basename "$0") reviews -p 9083      # Start reviews on custom port

EOF
    exit 0
}

# ============================================================================
# Parse Arguments
# ============================================================================
SERVICE=""
CUSTOM_PORT=""
DEBUG_MODE=false
SUSPEND_MODE=false
CLEAN_FIRST=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -p|--port)
            CUSTOM_PORT="$2"
            shift 2
            ;;
        --debug)
            DEBUG_MODE=true
            shift
            ;;
        --suspend)
            SUSPEND_MODE=true
            shift
            ;;
        --clean)
            CLEAN_FIRST=true
            shift
            ;;
        *)
            if [[ -z "$SERVICE" ]]; then
                SERVICE="$1"
            else
                log_error "Unknown argument: $1"
                usage
            fi
            shift
            ;;
    esac
done

# ============================================================================
# Validate Service
# ============================================================================
if [[ -z "$SERVICE" ]]; then
    log_error "Service name is required"
    usage
fi

# Check if service exists
SERVICE_DIR="${PROJECT_ROOT}/${SERVICE}"
if [[ ! -d "$SERVICE_DIR" ]]; then
    log_error "Service directory not found: ${SERVICE_DIR}"
    log_info "Available services: ${SERVICE_NAMES[*]}"
    exit 1
fi

# Get default port
DEFAULT_PORT="${SERVICES[$SERVICE]:-}"
if [[ -z "$DEFAULT_PORT" ]]; then
    log_error "Unknown service: ${SERVICE}"
    log_info "Available services: ${SERVICE_NAMES[*]}"
    exit 1
fi

PORT="${CUSTOM_PORT:-$DEFAULT_PORT}"

# ============================================================================
# Pre-flight Checks
# ============================================================================
log_step "Starting ${SERVICE} in development mode..."

# Check if port is already in use
if port_in_use "$PORT"; then
    log_error "Port ${PORT} is already in use"
    log_info "Use --port to specify a different port, or stop the existing service"
    exit 1
fi

# Check if infrastructure is running
log_info "Checking infrastructure status..."

# Check PostgreSQL for the service
case "$SERVICE" in
    catalog)
        if ! port_in_use 5432; then
            log_warn "Catalog PostgreSQL (port 5432) is not running"
            log_info "Run: ./scripts/infra-up.sh --only-db"
        fi
        ;;
    users|notifications)
        if ! port_in_use 7654; then
            log_warn "Users PostgreSQL (port 7654) is not running"
            log_info "Run: ./scripts/infra-up.sh --only-db"
        fi
        ;;
    reviews|reactive-reviews)
        if ! port_in_use 6543; then
            log_warn "Reviews PostgreSQL (port 6543) is not running"
            log_info "Run: ./scripts/infra-up.sh --only-db"
        fi
        ;;
esac

# Check Redis
if ! port_in_use 6379; then
    log_warn "Redis (port 6379) is not running"
fi

# Check Kafka
if ! port_in_use 29092; then
    log_warn "Kafka (port 29092) is not running"
fi

# ============================================================================
# Build Gradle Command
# ============================================================================
cd "${PROJECT_ROOT}"

GRADLE_CMD="./gradlew :${SERVICE}:quarkusDev"

# Add debug options
if [[ "$DEBUG_MODE" == true ]]; then
    GRADLE_CMD="${GRADLE_CMD} -Ddebug=true"
    if [[ "$SUSPEND_MODE" == true ]]; then
        GRADLE_CMD="${GRADLE_CMD} -Dsuspend=true"
    fi
    log_info "Debug mode enabled on port 5005"
fi

# Add custom port
if [[ -n "$CUSTOM_PORT" ]]; then
    GRADLE_CMD="${GRADLE_CMD} -Dquarkus.http.port=${PORT}"
fi

# Clean first if requested
if [[ "$CLEAN_FIRST" == true ]]; then
    log_info "Cleaning build..."
    ./gradlew :${SERVICE}:clean
fi

# ============================================================================
# Start Service
# ============================================================================
echo ""
log_info "Service: ${SERVICE}"
log_info "Port: ${PORT}"
log_info "Command: ${GRADLE_CMD}"
echo ""

# Set environment variables for dev mode
export QUARKUS_PROFILE=dev

# Run the service
exec ${GRADLE_CMD}
