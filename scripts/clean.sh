#!/usr/bin/env bash
# ============================================================================
# Clean - Clean build artifacts and temporary files
# ============================================================================
# 12-Factor App: Factor II - Dependencies
# Clean environment ensures reproducible builds
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

Clean build artifacts, logs, and temporary files.

Options:
    -h, --help          Show this help message
    -a, --all           Clean everything (build, logs, docker)
    --build             Clean build artifacts only (default)
    --logs              Clean log files only
    --docker            Clean Docker images and volumes
    --gradle            Clean Gradle caches
    -f, --force         Don't ask for confirmation

Examples:
    $(basename "$0")                    # Clean build artifacts
    $(basename "$0") -a                 # Clean everything
    $(basename "$0") --docker           # Clean Docker resources

EOF
    exit 0
}

# ============================================================================
# Parse Arguments
# ============================================================================
CLEAN_BUILD=false
CLEAN_LOGS=false
CLEAN_DOCKER=false
CLEAN_GRADLE=false
FORCE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -a|--all)
            CLEAN_BUILD=true
            CLEAN_LOGS=true
            CLEAN_DOCKER=true
            CLEAN_GRADLE=true
            shift
            ;;
        --build)
            CLEAN_BUILD=true
            shift
            ;;
        --logs)
            CLEAN_LOGS=true
            shift
            ;;
        --docker)
            CLEAN_DOCKER=true
            shift
            ;;
        --gradle)
            CLEAN_GRADLE=true
            shift
            ;;
        -f|--force)
            FORCE=true
            shift
            ;;
        *)
            log_error "Unknown argument: $1"
            usage
            ;;
    esac
done

# Default to build clean if nothing specified
if [[ "$CLEAN_BUILD" == false && "$CLEAN_LOGS" == false && "$CLEAN_DOCKER" == false && "$CLEAN_GRADLE" == false ]]; then
    CLEAN_BUILD=true
fi

# ============================================================================
# Confirmation
# ============================================================================
if [[ "$FORCE" != true ]]; then
    echo "The following will be cleaned:"
    [[ "$CLEAN_BUILD" == true ]] && echo "  - Build artifacts (build/ directories)"
    [[ "$CLEAN_LOGS" == true ]] && echo "  - Log files (${LOG_DIR})"
    [[ "$CLEAN_DOCKER" == true ]] && echo "  - Docker images and volumes (library/*)"
    [[ "$CLEAN_GRADLE" == true ]] && echo "  - Gradle caches (.gradle/)"
    echo ""

    read -p "Continue? (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        log_info "Cancelled"
        exit 0
    fi
fi

# ============================================================================
# Clean Build Artifacts
# ============================================================================
if [[ "$CLEAN_BUILD" == true ]]; then
    log_step "Cleaning build artifacts..."

    cd "${PROJECT_ROOT}"

    # Run Gradle clean
    ./gradlew clean 2>/dev/null || true

    # Remove additional build directories
    for service in "${SERVICE_NAMES[@]}"; do
        rm -rf "${PROJECT_ROOT}/${service}/build" 2>/dev/null || true
        rm -rf "${PROJECT_ROOT}/${service}/out" 2>/dev/null || true
    done

    # Remove shared-config build
    rm -rf "${PROJECT_ROOT}/shared-config/build" 2>/dev/null || true

    # Remove root build directory
    rm -rf "${PROJECT_ROOT}/build" 2>/dev/null || true

    # Remove PID file
    rm -f "${PROJECT_ROOT}/.services.pid" 2>/dev/null || true

    log_success "Build artifacts cleaned"
fi

# ============================================================================
# Clean Logs
# ============================================================================
if [[ "$CLEAN_LOGS" == true ]]; then
    log_step "Cleaning log files..."

    if [[ -d "${LOG_DIR}" ]]; then
        rm -rf "${LOG_DIR:?}"/*
        log_success "Log files cleaned"
    else
        log_info "Log directory does not exist: ${LOG_DIR}"
    fi
fi

# ============================================================================
# Clean Docker
# ============================================================================
if [[ "$CLEAN_DOCKER" == true ]]; then
    log_step "Cleaning Docker resources..."

    if ! command_exists docker; then
        log_warn "Docker is not installed"
    else
        # Remove project-specific images
        log_info "Removing library/* images..."
        docker images --format '{{.Repository}}:{{.Tag}}' | grep '^library/' | xargs -r docker rmi -f 2>/dev/null || true

        # Prune unused images
        log_info "Pruning unused images..."
        docker image prune -f 2>/dev/null || true

        # Optionally prune volumes (dangerous!)
        if [[ "$FORCE" == true ]]; then
            log_warn "Pruning unused volumes..."
            docker volume prune -f 2>/dev/null || true
        fi

        log_success "Docker resources cleaned"
    fi
fi

# ============================================================================
# Clean Gradle Caches
# ============================================================================
if [[ "$CLEAN_GRADLE" == true ]]; then
    log_step "Cleaning Gradle caches..."

    # Project-level Gradle cache
    rm -rf "${PROJECT_ROOT}/.gradle" 2>/dev/null || true

    # User-level Gradle cache for this project
    if [[ -d "${HOME}/.gradle/caches" ]]; then
        log_info "Note: User-level Gradle cache at ~/.gradle/caches is not cleaned"
        log_info "To clean it manually: rm -rf ~/.gradle/caches"
    fi

    log_success "Gradle caches cleaned"
fi

# ============================================================================
# Summary
# ============================================================================
echo ""
log_success "Cleanup completed!"

# Show disk space recovered
if command_exists du; then
    project_size=$(du -sh "${PROJECT_ROOT}" 2>/dev/null | cut -f1)
    log_info "Project size: ${project_size}"
fi
