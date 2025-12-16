#!/usr/bin/env bash
# ============================================================================
# Environment Check - Verify development environment setup
# ============================================================================
# 12-Factor App: Factor III - Config
# Verify that all required configuration is in place
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

Verify that the development environment is properly configured.

Options:
    -h, --help          Show this help message
    -v, --verbose       Show detailed information
    --fix               Attempt to fix common issues

Examples:
    $(basename "$0")                    # Basic environment check
    $(basename "$0") -v                 # Verbose check
    $(basename "$0") --fix              # Check and fix issues

EOF
    exit 0
}

# ============================================================================
# Parse Arguments
# ============================================================================
VERBOSE=false
AUTO_FIX=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        --fix)
            AUTO_FIX=true
            shift
            ;;
        *)
            log_error "Unknown argument: $1"
            usage
            ;;
    esac
done

# ============================================================================
# Check Functions
# ============================================================================
errors=0
warnings=0

check_pass() {
    local name=$1
    local details=${2:-""}
    printf "  ${GREEN}✓${NC} %-35s" "$name"
    if [[ -n "$details" && "$VERBOSE" == true ]]; then
        echo " ($details)"
    else
        echo ""
    fi
}

check_fail() {
    local name=$1
    local details=${2:-""}
    printf "  ${RED}✗${NC} %-35s" "$name"
    if [[ -n "$details" ]]; then
        echo " - $details"
    else
        echo ""
    fi
    ((errors++))
}

check_warn() {
    local name=$1
    local details=${2:-""}
    printf "  ${YELLOW}!${NC} %-35s" "$name"
    if [[ -n "$details" ]]; then
        echo " - $details"
    else
        echo ""
    fi
    ((warnings++))
}

# ============================================================================
# System Requirements
# ============================================================================
echo ""
log_step "System Requirements"
echo ""

# Check Java
if command_exists java; then
    java_version=$(java -version 2>&1 | head -1 | awk -F '"' '{print $2}')
    java_major=$(echo "$java_version" | cut -d'.' -f1)

    if [[ "$java_major" -ge 21 ]]; then
        check_pass "Java 21+" "$java_version"
    else
        check_fail "Java 21+" "Found $java_version, need 21+"
    fi

    if [[ "$VERBOSE" == true ]]; then
        echo "      JAVA_HOME: ${JAVA_HOME:-not set}"
    fi
else
    check_fail "Java" "Not installed"
fi

# Check Gradle wrapper
if [[ -f "${PROJECT_ROOT}/gradlew" ]]; then
    check_pass "Gradle wrapper" "gradlew present"
else
    check_fail "Gradle wrapper" "gradlew not found"
fi

# Check Docker
if command_exists docker; then
    if docker info >/dev/null 2>&1; then
        docker_version=$(docker --version | awk '{print $3}' | tr -d ',')
        check_pass "Docker" "$docker_version"
    else
        check_fail "Docker" "Not running"
    fi
else
    check_fail "Docker" "Not installed"
fi

# Check Docker Compose
if command_exists docker && docker compose version >/dev/null 2>&1; then
    compose_version=$(docker compose version --short 2>/dev/null)
    check_pass "Docker Compose" "$compose_version"
else
    check_warn "Docker Compose" "Not available (docker compose)"
fi

# Check Git
if command_exists git; then
    git_version=$(git --version | awk '{print $3}')
    check_pass "Git" "$git_version"
else
    check_warn "Git" "Not installed"
fi

# Check curl
if command_exists curl; then
    check_pass "curl" "Available"
else
    check_warn "curl" "Not installed (needed for health checks)"
fi

# ============================================================================
# Project Structure
# ============================================================================
echo ""
log_step "Project Structure"
echo ""

# Check service directories
for service in "${SERVICE_NAMES[@]}"; do
    service_dir="${PROJECT_ROOT}/${service}"
    if [[ -d "$service_dir" ]]; then
        if [[ -f "${service_dir}/build.gradle" ]]; then
            check_pass "$service" "build.gradle present"
        else
            check_fail "$service" "build.gradle missing"
        fi
    else
        check_fail "$service" "Directory not found"
    fi
done

# Check shared-config
if [[ -d "${PROJECT_ROOT}/shared-config" ]]; then
    check_pass "shared-config" "Present"
else
    check_fail "shared-config" "Directory not found"
fi

# Check docker directory
if [[ -d "${DOCKER_DIR}" ]]; then
    if [[ -f "${DOCKER_COMPOSE_FILE}" ]]; then
        check_pass "docker-compose.yml" "Present"
    else
        check_fail "docker-compose.yml" "Not found"
    fi
else
    check_fail "docker directory" "Not found"
fi

# ============================================================================
# Configuration Files
# ============================================================================
echo ""
log_step "Configuration Files"
echo ""

# Check environment file
env_file="${DOCKER_DIR}/.env"
if [[ -f "$env_file" ]]; then
    check_pass ".env file" "Present"

    if [[ "$VERBOSE" == true ]]; then
        echo "      Variables defined:"
        grep -v '^#' "$env_file" | grep '=' | cut -d'=' -f1 | while read -r var; do
            echo "        - $var"
        done
    fi
else
    check_warn ".env file" "Not found in docker/"

    if [[ "$AUTO_FIX" == true ]]; then
        log_info "Creating default .env file..."
        cat > "$env_file" << 'ENVEOF'
# Database Configuration
APP_DB_USER=training
APP_DB_PASSWORD=training
CAT_DB_NAME=catalog
USERS_DB_NAME=users
REVIEWS_DB_NAME=reviews

# PgAdmin Configuration
PGADMIN_EMAIL=admin@library.local
PGADMIN_PASSWORD=admin

# Grafana Configuration
GRAFANA_USER=admin
GRAFANA_PASSWORD=admin

# Domain Configuration
DOMAIN=localhost
EMAIL=admin@localhost
ENVEOF
        check_pass ".env file" "Created with defaults"
    fi
fi

# Check Prometheus config
prometheus_config="${DOCKER_DIR}/prometheus/prometheus.yml"
if [[ -f "$prometheus_config" ]]; then
    check_pass "prometheus.yml" "Present"
else
    check_warn "prometheus.yml" "Not found"
fi

# Check OTel Collector config
otel_config="${DOCKER_DIR}/otel-collector-config.yaml"
if [[ -f "$otel_config" ]]; then
    check_pass "otel-collector-config.yaml" "Present"
else
    check_warn "otel-collector-config.yaml" "Not found"
fi

# ============================================================================
# Network & Ports
# ============================================================================
echo ""
log_step "Network & Ports"
echo ""

# Check if default ports are available
declare -A PORT_CHECKS=(
    ["5432"]="Catalog PostgreSQL"
    ["6543"]="Reviews PostgreSQL"
    ["7654"]="Users PostgreSQL"
    ["6379"]="Redis"
    ["8500"]="Consul"
    ["29092"]="Kafka"
    ["9090"]="Prometheus"
    ["3000"]="Grafana"
    ["9411"]="Zipkin"
)

for port in "${!PORT_CHECKS[@]}"; do
    service="${PORT_CHECKS[$port]}"
    if port_in_use "$port"; then
        check_pass "$service (${port})" "In use"
    else
        if [[ "$VERBOSE" == true ]]; then
            check_warn "$service (${port})" "Available (not running)"
        fi
    fi
done

# Check microservice ports
echo ""
log_info "Microservice ports:"
for service in "${SERVICE_NAMES[@]}"; do
    port="${SERVICES[$service]}"
    if port_in_use "$port"; then
        check_pass "$service (${port})" "In use"
    else
        if [[ "$VERBOSE" == true ]]; then
            echo "    $service: Port ${port} available"
        fi
    fi
done

# ============================================================================
# Disk Space
# ============================================================================
echo ""
log_step "Disk Space"
echo ""

if command_exists df; then
    available=$(df -h "${PROJECT_ROOT}" | awk 'NR==2{print $4}')
    used_percent=$(df -h "${PROJECT_ROOT}" | awk 'NR==2{print $5}' | tr -d '%')

    if [[ "$used_percent" -lt 90 ]]; then
        check_pass "Disk space" "${available} available"
    elif [[ "$used_percent" -lt 95 ]]; then
        check_warn "Disk space" "${available} available (${used_percent}% used)"
    else
        check_fail "Disk space" "Low disk space (${used_percent}% used)"
    fi
fi

# Docker disk usage
if command_exists docker && docker info >/dev/null 2>&1; then
    docker_usage=$(docker system df --format '{{.Size}}' 2>/dev/null | head -1)
    if [[ -n "$docker_usage" ]]; then
        check_pass "Docker disk usage" "$docker_usage"
    fi
fi

# ============================================================================
# Memory
# ============================================================================
echo ""
log_step "Memory"
echo ""

if command_exists free; then
    total_mem=$(free -g | awk 'NR==2{print $2}')
    available_mem=$(free -g | awk 'NR==2{print $7}')

    if [[ "$available_mem" -ge 4 ]]; then
        check_pass "Available memory" "${available_mem}GB available of ${total_mem}GB"
    elif [[ "$available_mem" -ge 2 ]]; then
        check_warn "Available memory" "${available_mem}GB available (recommend 4GB+)"
    else
        check_fail "Available memory" "${available_mem}GB available (need at least 2GB)"
    fi
fi

# ============================================================================
# Summary
# ============================================================================
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

if [[ $errors -eq 0 && $warnings -eq 0 ]]; then
    log_success "Environment check passed! All requirements met."
elif [[ $errors -eq 0 ]]; then
    log_warn "Environment check completed with ${warnings} warning(s)"
else
    log_error "Environment check failed with ${errors} error(s) and ${warnings} warning(s)"
fi

echo ""
echo "Next steps:"
echo "  1. Start infrastructure:  ./scripts/infra-up.sh"
echo "  2. Start a service:       ./scripts/dev.sh catalog"
echo "  3. Check system status:   ./scripts/status.sh"
echo ""

exit $errors
