#!/usr/bin/env bash
# ============================================================================
# Configuration - Shared environment variables for all scripts
# ============================================================================
# 12-Factor App: Factor III - Config
# Store config in the environment, not in code
# ============================================================================

# Project root directory (relative to scripts/)
export PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Docker compose directory
export DOCKER_DIR="${PROJECT_ROOT}/docker"

# ============================================================================
# Service Configuration
# ============================================================================
# Define all microservices with their ports
declare -A SERVICES=(
    ["catalog"]=8081
    ["users"]=8082
    ["reviews"]=8083
    ["reactive-reviews"]=8084
    ["notifications"]=8085
)
export SERVICES

# Service names array for iteration
export SERVICE_NAMES=("catalog" "users" "reviews" "reactive-reviews" "notifications")

# ============================================================================
# Infrastructure Configuration
# ============================================================================
export DOCKER_COMPOSE_FILE="${DOCKER_DIR}/docker-compose.yml"

# Infrastructure services (from docker-compose)
export INFRA_SERVICES="catalog-postgres users-postgres reviews-postgres redis consul kafka zookeeper"
export OBSERVABILITY_SERVICES="prometheus grafana zipkin otel-collector"

# ============================================================================
# Health Check Configuration
# ============================================================================
export HEALTH_CHECK_TIMEOUT=5
export HEALTH_CHECK_RETRIES=30
export HEALTH_CHECK_INTERVAL=2

# ============================================================================
# Logging Configuration
# ============================================================================
# 12-Factor App: Factor XI - Logs
# Treat logs as event streams
export LOG_DIR="${PROJECT_ROOT}/logs"
export LOG_FORMAT="json"  # json or plain

# ============================================================================
# Build Configuration
# ============================================================================
export GRADLE_OPTS="-Xmx2g -XX:+HeapDumpOnOutOfMemoryError"
export JAVA_HOME="${JAVA_HOME:-/usr/lib/jvm/java-21-openjdk-amd64}"

# ============================================================================
# Environment Detection
# ============================================================================
# Detect current environment (dev, staging, prod)
export APP_ENV="${APP_ENV:-dev}"

# ============================================================================
# Color codes for output
# ============================================================================
export RED='\033[0;31m'
export GREEN='\033[0;32m'
export YELLOW='\033[1;33m'
export BLUE='\033[0;34m'
export PURPLE='\033[0;35m'
export CYAN='\033[0;36m'
export NC='\033[0m' # No Color

# ============================================================================
# Utility Functions
# ============================================================================

# Print colored message
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[OK]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_step() {
    echo -e "${PURPLE}[STEP]${NC} $1"
}

# Check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check if a port is in use
# Note: ss is preferred over lsof because lsof may require elevated permissions
# to detect Docker container ports
port_in_use() {
    local port=$1
    if command_exists ss; then
        ss -tuln | grep -q ":$port "
    elif command_exists netstat; then
        netstat -tuln | grep -q ":$port "
    elif command_exists lsof; then
        lsof -i :"$port" >/dev/null 2>&1
    else
        return 1
    fi
}

# Wait for a service to be ready
wait_for_service() {
    local name=$1
    local url=$2
    local max_retries=${3:-$HEALTH_CHECK_RETRIES}
    local interval=${4:-$HEALTH_CHECK_INTERVAL}

    log_info "Waiting for $name to be ready..."

    for ((i=1; i<=max_retries; i++)); do
        if curl -sf "$url" >/dev/null 2>&1; then
            log_success "$name is ready"
            return 0
        fi
        echo -n "."
        sleep "$interval"
    done

    echo ""
    log_error "$name failed to start after $((max_retries * interval)) seconds"
    return 1
}

# Get process ID for a service
get_service_pid() {
    local port=$1
    if command_exists lsof; then
        lsof -t -i :"$port" 2>/dev/null | head -1
    elif command_exists fuser; then
        fuser "$port/tcp" 2>/dev/null | awk '{print $1}'
    fi
}

# Graceful shutdown of a process
graceful_shutdown() {
    local pid=$1
    local name=$2
    local timeout=${3:-30}

    if [ -z "$pid" ]; then
        return 0
    fi

    log_info "Stopping $name (PID: $pid)..."

    # Send SIGTERM for graceful shutdown
    kill -TERM "$pid" 2>/dev/null

    # Wait for process to terminate
    for ((i=0; i<timeout; i++)); do
        if ! kill -0 "$pid" 2>/dev/null; then
            log_success "$name stopped gracefully"
            return 0
        fi
        sleep 1
    done

    # Force kill if still running
    log_warn "$name did not stop gracefully, forcing..."
    kill -9 "$pid" 2>/dev/null
    sleep 1

    if ! kill -0 "$pid" 2>/dev/null; then
        log_success "$name stopped (forced)"
        return 0
    fi

    log_error "Failed to stop $name"
    return 1
}
