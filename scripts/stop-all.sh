#!/usr/bin/env bash
# ============================================================================
# Stop All - Stop all microservices gracefully
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
Usage: $(basename "$0") [OPTIONS] [SERVICE...]

Stop microservices gracefully.

Options:
    -h, --help          Show this help message
    -f, --force         Force kill (SIGKILL instead of SIGTERM)
    --timeout SECONDS   Graceful shutdown timeout (default: 30)

Services:
    If no services specified, all running services will be stopped.
    Available: catalog, users, reviews, reactive-reviews, notifications

Examples:
    $(basename "$0")                    # Stop all services
    $(basename "$0") catalog users      # Stop specific services
    $(basename "$0") -f                 # Force stop all

EOF
    exit 0
}

# ============================================================================
# Parse Arguments
# ============================================================================
FORCE_KILL=false
TIMEOUT=30
SERVICES_TO_STOP=()

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -f|--force)
            FORCE_KILL=true
            shift
            ;;
        --timeout)
            TIMEOUT="$2"
            shift 2
            ;;
        *)
            SERVICES_TO_STOP+=("$1")
            shift
            ;;
    esac
done

# If no services specified, stop all
if [[ ${#SERVICES_TO_STOP[@]} -eq 0 ]]; then
    SERVICES_TO_STOP=("${SERVICE_NAMES[@]}")
fi

# ============================================================================
# Stop Services
# ============================================================================
log_step "Stopping microservices..."

stopped_count=0
failed_count=0

for service in "${SERVICES_TO_STOP[@]}"; do
    port="${SERVICES[$service]:-}"
    if [[ -z "$port" ]]; then
        log_warn "Unknown service: ${service}"
        continue
    fi

    pid=$(get_service_pid "$port")

    if [[ -z "$pid" ]]; then
        log_info "${service} is not running (port ${port})"
        continue
    fi

    if [[ "$FORCE_KILL" == true ]]; then
        log_info "Force stopping ${service} (PID: ${pid})..."
        kill -9 "$pid" 2>/dev/null || true
        sleep 1
        if ! kill -0 "$pid" 2>/dev/null; then
            log_success "${service} stopped"
            ((stopped_count++))
        else
            log_error "Failed to stop ${service}"
            ((failed_count++))
        fi
    else
        if graceful_shutdown "$pid" "$service" "$TIMEOUT"; then
            ((stopped_count++))
        else
            ((failed_count++))
        fi
    fi
done

# ============================================================================
# Cleanup PID File
# ============================================================================
PID_FILE="${PROJECT_ROOT}/.services.pid"
if [[ -f "$PID_FILE" ]]; then
    # Remove entries for stopped services
    temp_file=$(mktemp)
    while read -r line; do
        pid=$(echo "$line" | awk '{print $1}')
        if kill -0 "$pid" 2>/dev/null; then
            echo "$line" >> "$temp_file"
        fi
    done < "$PID_FILE"
    mv "$temp_file" "$PID_FILE"
fi

# ============================================================================
# Summary
# ============================================================================
echo ""
if [[ $failed_count -eq 0 ]]; then
    log_success "All services stopped successfully (${stopped_count} stopped)"
else
    log_warn "Stopped: ${stopped_count}, Failed: ${failed_count}"
    exit 1
fi
