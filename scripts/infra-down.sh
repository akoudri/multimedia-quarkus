#!/usr/bin/env bash
# ============================================================================
# Infrastructure Down - Stop backing services
# ============================================================================
# 12-Factor App: Factor IX - Disposability
# Maximize robustness with fast startup and graceful shutdown
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

Stop infrastructure services using Docker Compose.

Options:
    -h, --help          Show this help message
    -v, --volumes       Remove volumes (WARNING: destroys data)
    --remove-orphans    Remove orphan containers
    --timeout SECONDS   Timeout for graceful shutdown (default: 30)

Services:
    If no services specified, all infrastructure services will be stopped.

Examples:
    $(basename "$0")                    # Stop all infrastructure
    $(basename "$0") -v                 # Stop and remove volumes
    $(basename "$0") postgres redis     # Stop specific services

EOF
    exit 0
}

# ============================================================================
# Parse Arguments
# ============================================================================
REMOVE_VOLUMES=false
REMOVE_ORPHANS=false
TIMEOUT=30
SERVICES_TO_STOP=""

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -v|--volumes)
            REMOVE_VOLUMES=true
            shift
            ;;
        --remove-orphans)
            REMOVE_ORPHANS=true
            shift
            ;;
        --timeout)
            TIMEOUT="$2"
            shift 2
            ;;
        *)
            SERVICES_TO_STOP="${SERVICES_TO_STOP} $1"
            shift
            ;;
    esac
done

# ============================================================================
# Main
# ============================================================================
log_step "Stopping infrastructure services..."

cd "${DOCKER_DIR}"

# Check if docker-compose file exists
if [[ ! -f "${DOCKER_COMPOSE_FILE}" ]]; then
    log_error "Docker compose file not found: ${DOCKER_COMPOSE_FILE}"
    exit 1
fi

# Build docker-compose command
COMPOSE_CMD="docker compose -f ${DOCKER_COMPOSE_FILE}"

if [[ -n "${SERVICES_TO_STOP}" ]]; then
    # Stop specific services
    log_info "Stopping services: ${SERVICES_TO_STOP}"
    ${COMPOSE_CMD} stop -t "${TIMEOUT}" ${SERVICES_TO_STOP}
    ${COMPOSE_CMD} rm -f ${SERVICES_TO_STOP}
else
    # Stop all services
    DOWN_OPTS="-t ${TIMEOUT}"

    if [[ "$REMOVE_VOLUMES" == true ]]; then
        log_warn "Volumes will be removed. All data will be lost!"
        read -p "Are you sure? (y/N) " -n 1 -r
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            DOWN_OPTS="${DOWN_OPTS} -v"
        else
            log_info "Keeping volumes."
        fi
    fi

    if [[ "$REMOVE_ORPHANS" == true ]]; then
        DOWN_OPTS="${DOWN_OPTS} --remove-orphans"
    fi

    log_info "Running: docker compose down ${DOWN_OPTS}"
    ${COMPOSE_CMD} down ${DOWN_OPTS}
fi

echo ""
log_success "Infrastructure services stopped!"
