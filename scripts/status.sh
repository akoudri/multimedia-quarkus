#!/usr/bin/env bash
# ============================================================================
# Status - Show comprehensive system status
# ============================================================================
# 12-Factor App: Factor XII - Admin Processes
# Run admin/management tasks as one-off processes
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

Show comprehensive status of the system including services, resources, and metrics.

Options:
    -h, --help          Show this help message
    -s, --short         Short output (services only)
    --json              Output in JSON format

Examples:
    $(basename "$0")                    # Full status
    $(basename "$0") -s                 # Short status

EOF
    exit 0
}

# ============================================================================
# Parse Arguments
# ============================================================================
SHORT_OUTPUT=false
JSON_OUTPUT=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -s|--short)
            SHORT_OUTPUT=true
            shift
            ;;
        --json)
            JSON_OUTPUT=true
            shift
            ;;
        *)
            log_error "Unknown argument: $1"
            usage
            ;;
    esac
done

# ============================================================================
# Status Functions
# ============================================================================

print_header() {
    local title=$1
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "  $title"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
}

print_service_row() {
    local name=$1
    local port=$2
    local status=$3
    local pid=${4:-"-"}
    local memory=${5:-"-"}

    if [[ "$status" == "UP" ]]; then
        printf "  ${GREEN}●${NC} %-22s %6s   %8s   %10s\n" "$name" "$port" "$pid" "$memory"
    elif [[ "$status" == "DOWN" ]]; then
        printf "  ${RED}●${NC} %-22s %6s   %8s   %10s\n" "$name" "$port" "-" "-"
    else
        printf "  ${YELLOW}●${NC} %-22s %6s   %8s   %10s\n" "$name" "$port" "$pid" "$memory"
    fi
}

get_process_memory() {
    local pid=$1
    if [[ -n "$pid" ]] && command_exists ps; then
        ps -o rss= -p "$pid" 2>/dev/null | awk '{printf "%.1f MB", $1/1024}'
    else
        echo "-"
    fi
}

# ============================================================================
# System Information
# ============================================================================
show_system_info() {
    print_header "System Information"
    echo ""
    echo "  Hostname:     $(hostname)"
    echo "  OS:           $(uname -s) $(uname -r)"
    echo "  Date:         $(date '+%Y-%m-%d %H:%M:%S %Z')"
    echo "  Environment:  ${APP_ENV}"
    echo "  Project:      ${PROJECT_ROOT}"

    if command_exists java; then
        java_version=$(java -version 2>&1 | head -1 | awk -F '"' '{print $2}')
        echo "  Java:         ${java_version}"
    fi

    if command_exists docker; then
        docker_version=$(docker --version 2>/dev/null | awk '{print $3}' | tr -d ',')
        echo "  Docker:       ${docker_version}"
    fi

    echo ""
}

# ============================================================================
# Docker Status
# ============================================================================
show_docker_status() {
    print_header "Docker Containers"
    echo ""

    if ! command_exists docker; then
        log_warn "Docker is not installed"
        return
    fi

    if ! docker info >/dev/null 2>&1; then
        log_warn "Docker is not running"
        return
    fi

    # Show running containers
    printf "  %-30s %-15s %-20s\n" "CONTAINER" "STATUS" "PORTS"
    echo "  ──────────────────────────────────────────────────────────────────"

    docker ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}" 2>/dev/null | tail -n +2 | while read -r line; do
        echo "  $line"
    done

    echo ""

    # Container count
    running=$(docker ps -q 2>/dev/null | wc -l)
    total=$(docker ps -aq 2>/dev/null | wc -l)
    echo "  Running: ${running}/${total} containers"
}

# ============================================================================
# Microservices Status
# ============================================================================
show_microservices_status() {
    print_header "Microservices"
    echo ""
    printf "  %-3s %-22s %6s   %8s   %10s\n" "" "SERVICE" "PORT" "PID" "MEMORY"
    echo "  ──────────────────────────────────────────────────────────────────"

    for service in "${SERVICE_NAMES[@]}"; do
        local port="${SERVICES[$service]}"
        local pid=$(get_service_pid "$port")
        local status="DOWN"
        local memory="-"

        if [[ -n "$pid" ]]; then
            # Check if it responds to health check
            if curl -sf --max-time 2 "http://localhost:${port}/q/health/ready" >/dev/null 2>&1; then
                status="UP"
            else
                status="STARTING"
            fi
            memory=$(get_process_memory "$pid")
        fi

        print_service_row "$service" "$port" "$status" "$pid" "$memory"
    done

    echo ""
}

# ============================================================================
# Infrastructure Status
# ============================================================================
show_infrastructure_status() {
    print_header "Infrastructure Services"
    echo ""
    printf "  %-3s %-22s %6s   %-10s\n" "" "SERVICE" "PORT" "STATUS"
    echo "  ──────────────────────────────────────────────────────────────────"

    # Define infrastructure checks
    declare -A INFRA_CHECKS=(
        ["Catalog PostgreSQL"]="5432|tcp"
        ["Users PostgreSQL"]="7654|tcp"
        ["Reviews PostgreSQL"]="6543|tcp"
        ["Redis"]="6379|tcp"
        ["Consul"]="8500|http://localhost:8500/v1/status/leader"
        ["Kafka"]="29092|tcp"
        ["Kafka UI"]="8080|http://localhost:8080"
        ["Prometheus"]="9090|http://localhost:9090/-/healthy"
        ["Grafana"]="3000|http://localhost:3000/api/health"
        ["Zipkin"]="9411|http://localhost:9411/health"
        ["OTel Collector"]="4317|http://localhost:8888/metrics"
    )

    for service in "Catalog PostgreSQL" "Users PostgreSQL" "Reviews PostgreSQL" "Redis" "Consul" "Kafka" "Kafka UI" "Prometheus" "Grafana" "Zipkin" "OTel Collector"; do
        local check="${INFRA_CHECKS[$service]}"
        local port="${check%%|*}"
        local check_type="${check#*|}"
        local status="DOWN"

        if [[ "$check_type" == "tcp" ]]; then
            if port_in_use "$port"; then
                status="UP"
            fi
        else
            if curl -sf --max-time 2 "$check_type" >/dev/null 2>&1; then
                status="UP"
            elif port_in_use "$port"; then
                status="STARTING"
            fi
        fi

        if [[ "$status" == "UP" ]]; then
            printf "  ${GREEN}●${NC} %-22s %6s   %-10s\n" "$service" "$port" "UP"
        elif [[ "$status" == "STARTING" ]]; then
            printf "  ${YELLOW}●${NC} %-22s %6s   %-10s\n" "$service" "$port" "STARTING"
        else
            printf "  ${RED}●${NC} %-22s %6s   %-10s\n" "$service" "$port" "DOWN"
        fi
    done

    echo ""
}

# ============================================================================
# Resource Usage
# ============================================================================
show_resource_usage() {
    print_header "Resource Usage"
    echo ""

    # Memory usage
    if command_exists free; then
        echo "  Memory:"
        free -h | awk 'NR==2{printf "    Total: %s | Used: %s | Free: %s | Available: %s\n", $2, $3, $4, $7}'
    fi

    # Disk usage
    if command_exists df; then
        echo ""
        echo "  Disk (Project):"
        df -h "${PROJECT_ROOT}" | awk 'NR==2{printf "    Total: %s | Used: %s (%s) | Available: %s\n", $2, $3, $5, $4}'
    fi

    # Docker disk usage
    if command_exists docker && docker info >/dev/null 2>&1; then
        echo ""
        echo "  Docker:"
        docker system df 2>/dev/null | awk 'NR>1{printf "    %s: %s (Reclaimable: %s)\n", $1, $3, $5}'
    fi

    echo ""
}

# ============================================================================
# Quick URLs
# ============================================================================
show_urls() {
    print_header "Quick Access URLs"
    echo ""
    echo "  Microservices:"
    for service in "${SERVICE_NAMES[@]}"; do
        local port="${SERVICES[$service]}"
        echo "    - ${service}: http://localhost:${port}"
    done

    echo ""
    echo "  Infrastructure:"
    echo "    - Consul UI:    http://localhost:8500"
    echo "    - Kafka UI:     http://localhost:8080"

    echo ""
    echo "  Observability:"
    echo "    - Prometheus:   http://localhost:9090"
    echo "    - Grafana:      http://localhost:3000 (admin/admin)"
    echo "    - Zipkin:       http://localhost:9411"

    echo ""
}

# ============================================================================
# Main
# ============================================================================
echo ""
echo "╔══════════════════════════════════════════════════════════════════╗"
echo "║          Bibliothèque Multimédia - System Status                 ║"
echo "╚══════════════════════════════════════════════════════════════════╝"

if [[ "$SHORT_OUTPUT" == true ]]; then
    show_microservices_status
    show_infrastructure_status
else
    show_system_info
    show_microservices_status
    show_infrastructure_status
    show_docker_status
    show_resource_usage
    show_urls
fi
