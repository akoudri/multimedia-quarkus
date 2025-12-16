#!/usr/bin/env bash
# ============================================================================
# Health Check - Check health of all services
# ============================================================================
# 12-Factor App: Factor IX - Disposability
# Fast startup and graceful shutdown require proper health checks
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

Check health status of microservices and infrastructure.

Options:
    -h, --help          Show this help message
    -v, --verbose       Show detailed health information
    -w, --watch         Continuously monitor (refresh every 5s)
    --json              Output in JSON format
    --infra             Check infrastructure services only
    --services          Check microservices only

Services:
    If no services specified, all services will be checked.

Examples:
    $(basename "$0")                    # Check all services
    $(basename "$0") -v catalog         # Detailed check for catalog
    $(basename "$0") -w                 # Watch mode
    $(basename "$0") --infra            # Check infrastructure only

EOF
    exit 0
}

# ============================================================================
# Parse Arguments
# ============================================================================
VERBOSE=false
WATCH_MODE=false
JSON_OUTPUT=false
CHECK_INFRA=true
CHECK_SERVICES=true
SERVICES_TO_CHECK=()

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -w|--watch)
            WATCH_MODE=true
            shift
            ;;
        --json)
            JSON_OUTPUT=true
            shift
            ;;
        --infra)
            CHECK_SERVICES=false
            shift
            ;;
        --services)
            CHECK_INFRA=false
            shift
            ;;
        *)
            SERVICES_TO_CHECK+=("$1")
            shift
            ;;
    esac
done

# If specific services specified, only check those
if [[ ${#SERVICES_TO_CHECK[@]} -gt 0 ]]; then
    CHECK_INFRA=false
fi

# ============================================================================
# Health Check Functions
# ============================================================================

check_http_health() {
    local name=$1
    local url=$2
    local timeout=${3:-$HEALTH_CHECK_TIMEOUT}

    local response
    local http_code

    response=$(curl -sf -o /dev/null -w "%{http_code}" --max-time "$timeout" "$url" 2>/dev/null) || response="000"

    if [[ "$response" == "200" ]]; then
        return 0
    else
        return 1
    fi
}

check_tcp_health() {
    local name=$1
    local host=$2
    local port=$3
    local timeout=${4:-$HEALTH_CHECK_TIMEOUT}

    if command_exists nc; then
        nc -z -w "$timeout" "$host" "$port" 2>/dev/null
    elif command_exists timeout; then
        timeout "$timeout" bash -c "echo > /dev/tcp/$host/$port" 2>/dev/null
    else
        return 1
    fi
}

print_status() {
    local name=$1
    local status=$2
    local details=${3:-""}

    if [[ "$JSON_OUTPUT" == true ]]; then
        echo "{\"service\": \"$name\", \"status\": \"$status\", \"details\": \"$details\"}"
    else
        if [[ "$status" == "UP" ]]; then
            printf "  ${GREEN}%-25s${NC} %s" "$name" "UP"
        elif [[ "$status" == "DOWN" ]]; then
            printf "  ${RED}%-25s${NC} %s" "$name" "DOWN"
        else
            printf "  ${YELLOW}%-25s${NC} %s" "$name" "$status"
        fi

        if [[ -n "$details" && "$VERBOSE" == true ]]; then
            echo " ($details)"
        else
            echo ""
        fi
    fi
}

get_service_health_details() {
    local port=$1
    local response

    response=$(curl -sf --max-time 5 "http://localhost:${port}/q/health" 2>/dev/null) || return 1

    if command_exists jq; then
        echo "$response" | jq -r '.checks[] | "\(.name): \(.status)"' 2>/dev/null | tr '\n' ', ' | sed 's/,$//'
    else
        echo "$response"
    fi
}

# ============================================================================
# Check Infrastructure
# ============================================================================
check_infrastructure() {
    echo ""
    log_step "Infrastructure Services"
    echo ""

    # PostgreSQL databases
    if check_tcp_health "Catalog PostgreSQL" "localhost" 5432; then
        print_status "Catalog PostgreSQL (5432)" "UP"
    else
        print_status "Catalog PostgreSQL (5432)" "DOWN"
    fi

    if check_tcp_health "Users PostgreSQL" "localhost" 7654; then
        print_status "Users PostgreSQL (7654)" "UP"
    else
        print_status "Users PostgreSQL (7654)" "DOWN"
    fi

    if check_tcp_health "Reviews PostgreSQL" "localhost" 6543; then
        print_status "Reviews PostgreSQL (6543)" "UP"
    else
        print_status "Reviews PostgreSQL (6543)" "DOWN"
    fi

    # Redis
    if check_tcp_health "Redis" "localhost" 6379; then
        print_status "Redis (6379)" "UP"
    else
        print_status "Redis (6379)" "DOWN"
    fi

    # Consul
    if check_http_health "Consul" "http://localhost:8500/v1/status/leader"; then
        print_status "Consul (8500)" "UP"
    else
        print_status "Consul (8500)" "DOWN"
    fi

    # Kafka
    if check_tcp_health "Kafka" "localhost" 29092; then
        print_status "Kafka (29092)" "UP"
    else
        print_status "Kafka (29092)" "DOWN"
    fi

    # Kafka UI
    if check_http_health "Kafka UI" "http://localhost:8080"; then
        print_status "Kafka UI (8080)" "UP"
    else
        print_status "Kafka UI (8080)" "DOWN"
    fi

    echo ""
    log_step "Observability Stack"
    echo ""

    # Prometheus
    if check_http_health "Prometheus" "http://localhost:9090/-/healthy"; then
        print_status "Prometheus (9090)" "UP"
    else
        print_status "Prometheus (9090)" "DOWN"
    fi

    # Grafana
    if check_http_health "Grafana" "http://localhost:3000/api/health"; then
        print_status "Grafana (3000)" "UP"
    else
        print_status "Grafana (3000)" "DOWN"
    fi

    # Zipkin
    if check_http_health "Zipkin" "http://localhost:9411/health"; then
        print_status "Zipkin (9411)" "UP"
    else
        print_status "Zipkin (9411)" "DOWN"
    fi

    # OTel Collector
    if check_http_health "OTel Collector" "http://localhost:8888/metrics"; then
        print_status "OTel Collector (4317)" "UP"
    else
        print_status "OTel Collector (4317)" "DOWN"
    fi
}

# ============================================================================
# Check Microservices
# ============================================================================
check_microservices() {
    echo ""
    log_step "Microservices"
    echo ""

    local services_to_check=("${SERVICE_NAMES[@]}")
    if [[ ${#SERVICES_TO_CHECK[@]} -gt 0 ]]; then
        services_to_check=("${SERVICES_TO_CHECK[@]}")
    fi

    for service in "${services_to_check[@]}"; do
        local port="${SERVICES[$service]:-}"
        if [[ -z "$port" ]]; then
            print_status "$service" "UNKNOWN" "Invalid service name"
            continue
        fi

        local health_url="http://localhost:${port}/q/health/ready"

        if check_http_health "$service" "$health_url"; then
            local details=""
            if [[ "$VERBOSE" == true ]]; then
                details=$(get_service_health_details "$port" || echo "")
            fi
            print_status "${service} (${port})" "UP" "$details"
        else
            # Check if process is running but not ready
            if port_in_use "$port"; then
                print_status "${service} (${port})" "STARTING" "Port in use but health check failed"
            else
                print_status "${service} (${port})" "DOWN"
            fi
        fi
    done
}

# ============================================================================
# Main
# ============================================================================
do_health_check() {
    if [[ "$JSON_OUTPUT" != true ]]; then
        clear 2>/dev/null || true
        echo ""
        echo "=============================================="
        echo "  Health Check - $(date '+%Y-%m-%d %H:%M:%S')"
        echo "=============================================="
    fi

    if [[ "$CHECK_INFRA" == true ]]; then
        check_infrastructure
    fi

    if [[ "$CHECK_SERVICES" == true ]]; then
        check_microservices
    fi

    echo ""
}

if [[ "$WATCH_MODE" == true ]]; then
    while true; do
        do_health_check
        sleep 5
    done
else
    do_health_check
fi
