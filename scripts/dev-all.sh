#!/usr/bin/env bash
# ============================================================================
# Dev All - Start multiple microservices in development mode
# ============================================================================
# 12-Factor App: Factor X - Dev/Prod Parity
# Uses dev.sh for each service to ensure consistent behavior
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

Start multiple microservices in Quarkus dev mode simultaneously.
Each service runs in its own terminal window.

Options:
    -h, --help          Show this help message
    -l, --list          List available services
    --no-terminal       Run in background (no terminal windows)
    --check             Only check infrastructure, don't start services

Services (default: catalog users reviews):
    catalog             Catalog service (port 8081)
    users               Users service (port 8082)
    reviews             Reviews service (port 8083)
    reactive-reviews    Reactive Reviews service (port 8084)
    notifications       Notifications batch service (port 8085)
    all                 Start all 5 services

Examples:
    $(basename "$0")                          # Start default services (catalog, users, reviews)
    $(basename "$0") catalog users            # Start only catalog and users
    $(basename "$0") all                      # Start all 5 services
    $(basename "$0") --no-terminal catalog    # Start catalog in background

EOF
    exit 0
}

# ============================================================================
# Detect Terminal Emulator
# ============================================================================
detect_terminal() {
    if command_exists gnome-terminal; then
        echo "gnome-terminal"
    elif command_exists konsole; then
        echo "konsole"
    elif command_exists xfce4-terminal; then
        echo "xfce4-terminal"
    elif command_exists xterm; then
        echo "xterm"
    elif command_exists kitty; then
        echo "kitty"
    elif command_exists alacritty; then
        echo "alacritty"
    else
        echo "none"
    fi
}

# ============================================================================
# Open Terminal with Command
# ============================================================================
open_terminal() {
    local title="$1"
    local cmd="$2"
    local terminal="$3"

    case "$terminal" in
        gnome-terminal)
            gnome-terminal --title="$title" -- bash -c "$cmd; echo ''; echo 'Press Enter to close...'; read"
            ;;
        konsole)
            konsole --new-tab -p tabtitle="$title" -e bash -c "$cmd; echo ''; echo 'Press Enter to close...'; read"
            ;;
        xfce4-terminal)
            xfce4-terminal --title="$title" -e "bash -c \"$cmd; echo ''; echo 'Press Enter to close...'; read\""
            ;;
        xterm)
            xterm -title "$title" -e "bash -c \"$cmd; echo ''; echo 'Press Enter to close...'; read\"" &
            ;;
        kitty)
            kitty --title "$title" bash -c "$cmd; echo ''; echo 'Press Enter to close...'; read" &
            ;;
        alacritty)
            alacritty --title "$title" -e bash -c "$cmd; echo ''; echo 'Press Enter to close...'; read" &
            ;;
        *)
            log_error "No supported terminal emulator found"
            return 1
            ;;
    esac
}

# ============================================================================
# Parse Arguments
# ============================================================================
SELECTED_SERVICES=()
NO_TERMINAL=false
CHECK_ONLY=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -l|--list)
            log_info "Available services:"
            for svc in "${SERVICE_NAMES[@]}"; do
                echo "  - $svc (port ${SERVICES[$svc]})"
            done
            exit 0
            ;;
        --no-terminal)
            NO_TERMINAL=true
            shift
            ;;
        --check)
            CHECK_ONLY=true
            shift
            ;;
        all)
            SELECTED_SERVICES=("${SERVICE_NAMES[@]}")
            shift
            ;;
        *)
            # Check if it's a valid service name
            if [[ -v "SERVICES[$1]" ]]; then
                SELECTED_SERVICES+=("$1")
            else
                log_error "Unknown service or option: $1"
                log_info "Use --list to see available services"
                exit 1
            fi
            shift
            ;;
    esac
done

# Default services if none specified
if [[ ${#SELECTED_SERVICES[@]} -eq 0 ]]; then
    SELECTED_SERVICES=("catalog" "users" "reviews")
fi

# ============================================================================
# Pre-flight Checks
# ============================================================================
log_step "Starting ${#SELECTED_SERVICES[@]} microservice(s) in dev mode..."
echo ""

# Check infrastructure
log_info "Checking infrastructure status..."

INFRA_OK=true

# Check PostgreSQL instances
if ! port_in_use 5432; then
    log_warn "Catalog PostgreSQL (port 5432) is not running"
    INFRA_OK=false
fi

if ! port_in_use 6543; then
    log_warn "Reviews PostgreSQL (port 6543) is not running"
    INFRA_OK=false
fi

if ! port_in_use 7654; then
    log_warn "Users PostgreSQL (port 7654) is not running"
    INFRA_OK=false
fi

# Check Redis
if ! port_in_use 6379; then
    log_warn "Redis (port 6379) is not running"
    INFRA_OK=false
fi

# Check Kafka
if ! port_in_use 29092; then
    log_warn "Kafka (port 29092) is not running"
    INFRA_OK=false
fi

# Check Keycloak (optional)
if port_in_use 8180; then
    log_success "Keycloak (port 8180) is running"
else
    log_info "Keycloak (port 8180) is not running (optional for auth)"
fi

if [[ "$INFRA_OK" == false ]]; then
    echo ""
    log_warn "Some infrastructure services are not running"
    log_info "Run: cd docker && docker compose up -d"
    echo ""
fi

if [[ "$CHECK_ONLY" == true ]]; then
    exit 0
fi

# ============================================================================
# Check Port Availability
# ============================================================================
log_info "Checking port availability..."

PORTS_OK=true
for svc in "${SELECTED_SERVICES[@]}"; do
    port="${SERVICES[$svc]}"
    if port_in_use "$port"; then
        log_error "Port $port ($svc) is already in use"
        PORTS_OK=false
    else
        log_success "Port $port ($svc) is available"
    fi
done

if [[ "$PORTS_OK" == false ]]; then
    log_error "Some ports are already in use. Stop existing services first."
    exit 1
fi

echo ""

# ============================================================================
# Start Services
# ============================================================================
TERMINAL=$(detect_terminal)

if [[ "$NO_TERMINAL" == true ]] || [[ "$TERMINAL" == "none" ]]; then
    if [[ "$TERMINAL" == "none" ]] && [[ "$NO_TERMINAL" == false ]]; then
        log_warn "No terminal emulator found, running in background mode"
    fi

    # Background mode - use nohup and log files
    mkdir -p "${LOG_DIR}"

    for svc in "${SELECTED_SERVICES[@]}"; do
        port="${SERVICES[$svc]}"
        log_file="${LOG_DIR}/${svc}.log"

        log_info "Starting $svc (port $port) in background..."
        log_info "  Log file: $log_file"

        nohup "${SCRIPT_DIR}/dev.sh" "$svc" > "$log_file" 2>&1 &
        echo $! > "${LOG_DIR}/${svc}.pid"

        sleep 2
    done

    echo ""
    log_success "Services started in background"
    log_info "View logs with: tail -f ${LOG_DIR}/<service>.log"
    log_info "Stop with: kill \$(cat ${LOG_DIR}/<service>.pid)"

else
    # Terminal mode - open each service in a new terminal window
    log_info "Using terminal: $TERMINAL"
    echo ""

    for svc in "${SELECTED_SERVICES[@]}"; do
        port="${SERVICES[$svc]}"
        log_info "Starting $svc (port $port) in new terminal..."

        open_terminal \
            "$svc (port $port)" \
            "${SCRIPT_DIR}/dev.sh $svc" \
            "$TERMINAL"

        # Wait between service starts to avoid resource contention
        sleep 3
    done
fi

# ============================================================================
# Summary
# ============================================================================
echo ""
echo "======================================"
log_success "All services started!"
echo ""
echo "Services running on:"
for svc in "${SELECTED_SERVICES[@]}"; do
    port="${SERVICES[$svc]}"
    echo "  - $svc: http://localhost:$port"
done
echo ""
echo "Web UI: http://localhost:8081/web/catalog"
echo "======================================"
