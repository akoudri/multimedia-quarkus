#!/usr/bin/env bash
# ============================================================================
# Logs - View and manage service logs
# ============================================================================
# 12-Factor App: Factor XI - Logs
# Treat logs as event streams
# ============================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/config.sh"

# ============================================================================
# Usage
# ============================================================================
usage() {
    cat << EOF
Usage: $(basename "$0") [OPTIONS] [SERVICE]

View and manage service logs.

Options:
    -h, --help          Show this help message
    -f, --follow        Follow log output (tail -f)
    -n, --lines N       Number of lines to show (default: 100)
    --all               Show logs from all services
    --docker SERVICE    Show Docker container logs
    --since TIME        Show logs since timestamp (e.g., "1h", "30m")
    --grep PATTERN      Filter logs by pattern
    --errors            Show only error logs

Services:
    catalog, users, reviews, reactive-reviews, notifications

Docker services:
    postgres, redis, consul, kafka, prometheus, grafana, zipkin

Examples:
    $(basename "$0") catalog             # View catalog logs
    $(basename "$0") -f catalog          # Follow catalog logs
    $(basename "$0") --all               # View all service logs
    $(basename "$0") --docker kafka      # View Kafka container logs
    $(basename "$0") catalog --errors    # View only errors

EOF
    exit 0
}

# ============================================================================
# Parse Arguments
# ============================================================================
FOLLOW=false
LINES=100
ALL_SERVICES=false
DOCKER_SERVICE=""
SINCE=""
GREP_PATTERN=""
ERRORS_ONLY=false
SERVICE=""

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -f|--follow)
            FOLLOW=true
            shift
            ;;
        -n|--lines)
            LINES="$2"
            shift 2
            ;;
        --all)
            ALL_SERVICES=true
            shift
            ;;
        --docker)
            DOCKER_SERVICE="$2"
            shift 2
            ;;
        --since)
            SINCE="$2"
            shift 2
            ;;
        --grep)
            GREP_PATTERN="$2"
            shift 2
            ;;
        --errors)
            ERRORS_ONLY=true
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
# Docker Logs
# ============================================================================
if [[ -n "$DOCKER_SERVICE" ]]; then
    log_step "Docker logs for: ${DOCKER_SERVICE}"

    DOCKER_CMD="docker logs"

    if [[ "$FOLLOW" == true ]]; then
        DOCKER_CMD="${DOCKER_CMD} -f"
    fi

    DOCKER_CMD="${DOCKER_CMD} --tail ${LINES}"

    if [[ -n "$SINCE" ]]; then
        DOCKER_CMD="${DOCKER_CMD} --since ${SINCE}"
    fi

    DOCKER_CMD="${DOCKER_CMD} ${DOCKER_SERVICE}"

    if [[ -n "$GREP_PATTERN" ]]; then
        eval "${DOCKER_CMD}" 2>&1 | grep -i "${GREP_PATTERN}"
    elif [[ "$ERRORS_ONLY" == true ]]; then
        eval "${DOCKER_CMD}" 2>&1 | grep -iE "(error|exception|fatal|failed)"
    else
        eval "${DOCKER_CMD}"
    fi

    exit 0
fi

# ============================================================================
# All Services Logs
# ============================================================================
if [[ "$ALL_SERVICES" == true ]]; then
    log_step "Aggregated logs from all services"

    # Create temporary directory for log aggregation
    temp_dir=$(mktemp -d)
    trap "rm -rf $temp_dir" EXIT

    # Collect logs from all services
    for svc in "${SERVICE_NAMES[@]}"; do
        log_file="${LOG_DIR}/${svc}.log"
        if [[ -f "$log_file" ]]; then
            # Add service name prefix to each line
            tail -n "$LINES" "$log_file" | while read -r line; do
                echo "[${svc}] $line"
            done >> "${temp_dir}/all.log"
        fi
    done

    # Sort by timestamp and display
    if [[ -f "${temp_dir}/all.log" ]]; then
        if [[ -n "$GREP_PATTERN" ]]; then
            sort "${temp_dir}/all.log" | grep -i "${GREP_PATTERN}"
        elif [[ "$ERRORS_ONLY" == true ]]; then
            sort "${temp_dir}/all.log" | grep -iE "(error|exception|fatal|failed)"
        else
            sort "${temp_dir}/all.log"
        fi

        if [[ "$FOLLOW" == true ]]; then
            log_info "Following all logs (Ctrl+C to stop)..."
            tail -f "${LOG_DIR}"/*.log 2>/dev/null | while read -r line; do
                echo "$line"
            done
        fi
    else
        log_warn "No log files found in ${LOG_DIR}"
    fi

    exit 0
fi

# ============================================================================
# Single Service Logs
# ============================================================================
if [[ -z "$SERVICE" ]]; then
    log_error "Service name is required"
    log_info "Available services: ${SERVICE_NAMES[*]}"
    usage
fi

# Validate service
valid_service=false
for svc in "${SERVICE_NAMES[@]}"; do
    if [[ "$svc" == "$SERVICE" ]]; then
        valid_service=true
        break
    fi
done

if [[ "$valid_service" != true ]]; then
    log_error "Unknown service: ${SERVICE}"
    log_info "Available services: ${SERVICE_NAMES[*]}"
    exit 1
fi

log_file="${LOG_DIR}/${SERVICE}.log"

# Check if log file exists
if [[ ! -f "$log_file" ]]; then
    log_warn "Log file not found: ${log_file}"
    log_info "Service might not have been started yet, or logs are in console."

    # Try to get logs from running process
    port="${SERVICES[$SERVICE]}"
    pid=$(get_service_pid "$port")

    if [[ -n "$pid" ]]; then
        log_info "Service is running (PID: ${pid}). Try starting with: ./scripts/start-all.sh"
    fi

    exit 1
fi

log_step "Logs for: ${SERVICE}"
log_info "Log file: ${log_file}"
echo ""

# Build tail command
TAIL_CMD="tail"

if [[ "$FOLLOW" == true ]]; then
    TAIL_CMD="${TAIL_CMD} -f"
else
    TAIL_CMD="${TAIL_CMD} -n ${LINES}"
fi

TAIL_CMD="${TAIL_CMD} ${log_file}"

# Apply filters
if [[ -n "$GREP_PATTERN" ]]; then
    eval "${TAIL_CMD}" | grep -i "${GREP_PATTERN}"
elif [[ "$ERRORS_ONLY" == true ]]; then
    eval "${TAIL_CMD}" | grep -iE "(error|exception|fatal|failed|warn)"
else
    eval "${TAIL_CMD}"
fi
