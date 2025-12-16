#!/usr/bin/env bash
# ============================================================================
# Infrastructure Up - Start backing services
# ============================================================================
# 12-Factor App: Factor IV - Backing Services
# Treat backing services as attached resources
# ============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/config.sh"

# ============================================================================
# Usage
# ============================================================================
usage() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS] [SERVICES...]

Start infrastructure services using Docker Compose.

Options:
    -h, --help          Show this help message
    -d, --detach        Run in detached mode (default)
    -f, --foreground    Run in foreground
    --no-wait           Don't wait for services to be healthy
    --only-db           Start only database services
    --only-messaging    Start only messaging services (Kafka)
    --only-observability Start only observability stack

Services:
    If no services specified, all infrastructure services will be started.
    Available: postgres, redis, consul, kafka, prometheus, grafana, zipkin

Examples:
    $(basename "$0")                    # Start all infrastructure
    $(basename "$0") --only-db          # Start only databases
    $(basename "$0") postgres redis     # Start specific services

EOF
    exit 0
}

# ============================================================================
# Parse Arguments
# ============================================================================
DETACHED=true
WAIT_FOR_HEALTHY=true
SERVICES_TO_START=""

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -d|--detach)
            DETACHED=true
            shift
            ;;
        -f|--foreground)
            DETACHED=false
            shift
            ;;
        --no-wait)
            WAIT_FOR_HEALTHY=false
            shift
            ;;
        --only-db)
            SERVICES_TO_START="catalog-postgres users-postgres reviews-postgres pgadmin4"
            shift
            ;;
        --only-messaging)
            SERVICES_TO_START="kafka kafka-ui"
            shift
            ;;
        --only-observability)
            SERVICES_TO_START="prometheus grafana zipkin otel-collector"
            shift
            ;;
        *)
            SERVICES_TO_START="${SERVICES_TO_START} $1"
            shift
            ;;
    esac
done

# ============================================================================
# Main
# ============================================================================
log_step "Starting infrastructure services..."

cd "${DOCKER_DIR}"

# Check if docker-compose file exists
if [[ ! -f "${DOCKER_COMPOSE_FILE}" ]]; then
    log_error "Docker compose file not found: ${DOCKER_COMPOSE_FILE}"
    exit 1
fi

# Check if Docker is running
if ! docker info >/dev/null 2>&1; then
    log_error "Docker is not running. Please start Docker first."
    exit 1
fi

# Build docker-compose command
COMPOSE_CMD="docker compose -f ${DOCKER_COMPOSE_FILE}"

if [[ "$DETACHED" == true ]]; then
    COMPOSE_CMD="${COMPOSE_CMD} up -d"
else
    COMPOSE_CMD="${COMPOSE_CMD} up"
fi

# Add specific services if requested
if [[ -n "${SERVICES_TO_START}" ]]; then
    COMPOSE_CMD="${COMPOSE_CMD} ${SERVICES_TO_START}"
fi

log_info "Running: ${COMPOSE_CMD}"
eval "${COMPOSE_CMD}"

# Wait for services to be healthy
if [[ "$WAIT_FOR_HEALTHY" == true && "$DETACHED" == true ]]; then
    echo ""
    log_step "Waiting for services to be healthy..."

    # Wait for PostgreSQL databases
    wait_for_service "Catalog PostgreSQL" "http://localhost:5432" 15 2 || true
    wait_for_service "Users PostgreSQL" "http://localhost:7654" 15 2 || true
    wait_for_service "Reviews PostgreSQL" "http://localhost:6543" 15 2 || true

    # Wait for Redis
    if command_exists redis-cli; then
        for ((i=1; i<=15; i++)); do
            if redis-cli -p 6379 ping >/dev/null 2>&1; then
                log_success "Redis is ready"
                break
            fi
            sleep 1
        done
    fi

    # Wait for Consul
    wait_for_service "Consul" "http://localhost:8500/v1/status/leader" 15 2 || true

    # Wait for Kafka (via Kafka UI)
    wait_for_service "Kafka UI" "http://localhost:8080" 30 2 || true

    # Wait for observability stack
    wait_for_service "Prometheus" "http://localhost:9090/-/healthy" 15 2 || true
    wait_for_service "Grafana" "http://localhost:3000/api/health" 15 2 || true
    wait_for_service "Zipkin" "http://localhost:9411/health" 15 2 || true
fi

echo ""
log_success "Infrastructure services started!"
echo ""
log_info "Available services:"
echo "  - PostgreSQL (Catalog):  localhost:5432"
echo "  - PostgreSQL (Users):    localhost:7654"
echo "  - PostgreSQL (Reviews):  localhost:6543"
echo "  - Redis:                 localhost:6379"
echo "  - Consul UI:             http://localhost:8500"
echo "  - Kafka:                 localhost:29092"
echo "  - Kafka UI:              http://localhost:8080"
echo "  - Prometheus:            http://localhost:9090"
echo "  - Grafana:               http://localhost:3000"
echo "  - Zipkin:                http://localhost:9411"
