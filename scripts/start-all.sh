#!/usr/bin/env bash
# ============================================================================
# Start All - Start all microservices
# ============================================================================
# 12-Factor App: Factor VIII - Concurrency
# Scale out via the process model
# ============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/config.sh"

# ============================================================================
# Usage
# ============================================================================
usage() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS]

Start all microservices in production mode (as JARs).

Options:
    -h, --help          Show this help message
    -d, --dev           Start in development mode (quarkusDev)
    --parallel          Start services in parallel (default: sequential)
    --skip SERVICE      Skip a specific service (can be repeated)
    --only SERVICE      Start only specific services (can be repeated)

Examples:
    $(basename "$0")                    # Start all services
    $(basename "$0") -d                 # Start all in dev mode
    $(basename "$0") --skip notifications  # Start all except notifications
    $(basename "$0") --only catalog --only users  # Start only catalog and users

EOF
    exit 0
}

# ============================================================================
# Parse Arguments
# ============================================================================
DEV_MODE=false
PARALLEL=false
SKIP_SERVICES=()
ONLY_SERVICES=()

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -d|--dev)
            DEV_MODE=true
            shift
            ;;
        --parallel)
            PARALLEL=true
            shift
            ;;
        --skip)
            SKIP_SERVICES+=("$2")
            shift 2
            ;;
        --only)
            ONLY_SERVICES+=("$2")
            shift 2
            ;;
        *)
            log_error "Unknown argument: $1"
            usage
            ;;
    esac
done

# ============================================================================
# Determine Services to Start
# ============================================================================
SERVICES_TO_START=()

if [[ ${#ONLY_SERVICES[@]} -gt 0 ]]; then
    SERVICES_TO_START=("${ONLY_SERVICES[@]}")
else
    for service in "${SERVICE_NAMES[@]}"; do
        skip=false
        for skip_service in "${SKIP_SERVICES[@]}"; do
            if [[ "$service" == "$skip_service" ]]; then
                skip=true
                break
            fi
        done
        if [[ "$skip" == false ]]; then
            SERVICES_TO_START+=("$service")
        fi
    done
fi

if [[ ${#SERVICES_TO_START[@]} -eq 0 ]]; then
    log_error "No services to start"
    exit 1
fi

# ============================================================================
# Pre-flight Checks
# ============================================================================
log_step "Pre-flight checks..."

# Check if infrastructure is running
if ! port_in_use 5432; then
    log_error "Infrastructure is not running. Start it first:"
    log_info "  ./scripts/infra-up.sh"
    exit 1
fi

log_success "Infrastructure is running"

# Check for port conflicts
for service in "${SERVICES_TO_START[@]}"; do
    port="${SERVICES[$service]}"
    if port_in_use "$port"; then
        log_error "Port ${port} (${service}) is already in use"
        exit 1
    fi
done

log_success "All ports are available"

# ============================================================================
# Build Services (Production Mode)
# ============================================================================
if [[ "$DEV_MODE" == false ]]; then
    log_step "Building services..."
    cd "${PROJECT_ROOT}"

    # Build all required services
    BUILD_TASKS=""
    for service in "${SERVICES_TO_START[@]}"; do
        BUILD_TASKS="${BUILD_TASKS} :${service}:quarkusBuild"
    done

    ./gradlew ${BUILD_TASKS} -x test

    log_success "Build completed"
fi

# ============================================================================
# Start Services
# ============================================================================
log_step "Starting services..."

# Create logs directory
mkdir -p "${LOG_DIR}"

# PID file for tracking running services
PID_FILE="${PROJECT_ROOT}/.services.pid"
> "${PID_FILE}"

start_service() {
    local service=$1
    local port=${SERVICES[$service]}

    if [[ "$DEV_MODE" == true ]]; then
        log_info "Starting ${service} in dev mode on port ${port}..."
        cd "${PROJECT_ROOT}"
        nohup ./gradlew :${service}:quarkusDev > "${LOG_DIR}/${service}.log" 2>&1 &
        echo "$! ${service}" >> "${PID_FILE}"
    else
        local jar_file="${PROJECT_ROOT}/${service}/build/quarkus-app/quarkus-run.jar"
        if [[ ! -f "$jar_file" ]]; then
            log_error "JAR not found: ${jar_file}"
            return 1
        fi

        log_info "Starting ${service} on port ${port}..."
        nohup java -jar "$jar_file" > "${LOG_DIR}/${service}.log" 2>&1 &
        echo "$! ${service}" >> "${PID_FILE}"
    fi
}

if [[ "$PARALLEL" == true ]]; then
    # Start all services in parallel
    for service in "${SERVICES_TO_START[@]}"; do
        start_service "$service" &
    done
    wait
else
    # Start services sequentially
    for service in "${SERVICES_TO_START[@]}"; do
        start_service "$service"
        sleep 2  # Give each service time to claim its port
    done
fi

# ============================================================================
# Wait for Services to be Ready
# ============================================================================
log_step "Waiting for services to be ready..."

all_ready=true
for service in "${SERVICES_TO_START[@]}"; do
    port=${SERVICES[$service]}
    if ! wait_for_service "$service" "http://localhost:${port}/q/health/ready" 60 2; then
        all_ready=false
    fi
done

echo ""
if [[ "$all_ready" == true ]]; then
    log_success "All services are running!"
else
    log_warn "Some services may not be ready. Check logs in: ${LOG_DIR}"
fi

# ============================================================================
# Summary
# ============================================================================
echo ""
log_info "Running services:"
for service in "${SERVICES_TO_START[@]}"; do
    port=${SERVICES[$service]}
    echo "  - ${service}: http://localhost:${port}"
done

echo ""
log_info "Useful commands:"
echo "  - View logs:    ./scripts/logs.sh ${SERVICES_TO_START[0]}"
echo "  - Health check: ./scripts/health-check.sh"
echo "  - Stop all:     ./scripts/stop-all.sh"
