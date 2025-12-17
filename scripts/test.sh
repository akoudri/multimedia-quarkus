#!/usr/bin/env bash
# ============================================================================
# Test - Run different types of tests (unit, integration, e2e)
# ============================================================================
# 12-Factor App: Factor X - Dev/Prod Parity
# Keep development, staging, and production as similar as possible
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

Run tests for microservices.

Options:
    -h, --help          Show this help message
    -u, --unit          Run unit tests only (*UnitTest, *MockitoTest)
    -i, --integration   Run integration tests only (*IntegrationTest, *IT, *TestcontainersTest, *ApiTest)
    -e, --e2e           Run end-to-end tests only (*E2ETest) - requires infrastructure
    -a, --all           Run all tests (default)
    -c, --coverage      Generate test coverage report (JaCoCo)
    -v, --verbose       Verbose output
    --fail-fast         Stop on first test failure
    --rerun-failed      Re-run only previously failed tests
    --check-infra       Check infrastructure before running integration/e2e tests

Services:
    If no services specified, all services will be tested.
    Available: catalog, users, reviews, reactive-reviews, notifications

Test Naming Conventions:
    Unit tests:        *UnitTest.java, *MockitoTest.java
    Integration tests: *IntegrationTest.java, *IT.java, *TestcontainersTest.java, *ApiTest.java
    E2E tests:         *E2ETest.java (Playwright)

Examples:
    $(basename "$0")                    # Run all tests for all services
    $(basename "$0") -u                 # Run only unit tests
    $(basename "$0") -i catalog         # Run integration tests for catalog
    $(basename "$0") -e --check-infra   # Run e2e tests after checking infrastructure
    $(basename "$0") -u -i              # Run unit and integration tests
    $(basename "$0") --coverage         # Run all tests with coverage report

EOF
    exit 0
}

# ============================================================================
# Parse Arguments
# ============================================================================
RUN_UNIT=false
RUN_INTEGRATION=false
RUN_E2E=false
RUN_ALL=true
COVERAGE=false
VERBOSE=false
FAIL_FAST=false
RERUN_FAILED=false
CHECK_INFRA=false
SERVICES_TO_TEST=()

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            ;;
        -u|--unit)
            RUN_UNIT=true
            RUN_ALL=false
            shift
            ;;
        -i|--integration)
            RUN_INTEGRATION=true
            RUN_ALL=false
            shift
            ;;
        -e|--e2e)
            RUN_E2E=true
            RUN_ALL=false
            shift
            ;;
        -a|--all)
            RUN_ALL=true
            shift
            ;;
        -c|--coverage)
            COVERAGE=true
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        --fail-fast)
            FAIL_FAST=true
            shift
            ;;
        --rerun-failed)
            RERUN_FAILED=true
            shift
            ;;
        --check-infra)
            CHECK_INFRA=true
            shift
            ;;
        *)
            SERVICES_TO_TEST+=("$1")
            shift
            ;;
    esac
done

# If no services specified, test all
if [[ ${#SERVICES_TO_TEST[@]} -eq 0 ]]; then
    SERVICES_TO_TEST=("${SERVICE_NAMES[@]}")
fi

# If --all or no specific type selected, run all
if [[ "$RUN_ALL" == true ]]; then
    RUN_UNIT=true
    RUN_INTEGRATION=true
    RUN_E2E=true
fi

# ============================================================================
# Pre-flight Checks
# ============================================================================
log_step "Pre-flight checks..."

# Check Java
if ! command_exists java; then
    log_error "Java is not installed"
    exit 1
fi

java_version=$(java -version 2>&1 | head -1 | awk -F '"' '{print $2}' | cut -d'.' -f1)
if [[ "$java_version" -lt 21 ]]; then
    log_error "Java 21+ is required. Found: Java ${java_version}"
    exit 1
fi
log_success "Java ${java_version} found"

# ============================================================================
# Infrastructure Check (for integration/e2e tests)
# ============================================================================
check_infrastructure() {
    log_step "Checking infrastructure..."

    local infra_ready=true

    # Check PostgreSQL databases
    for db_port in 5432 6543 7654; do
        if port_in_use "$db_port"; then
            log_success "PostgreSQL on port $db_port is available"
        else
            log_warn "PostgreSQL on port $db_port is not available"
            infra_ready=false
        fi
    done

    # Check Redis
    if port_in_use 6379; then
        log_success "Redis on port 6379 is available"
    else
        log_warn "Redis on port 6379 is not available"
        infra_ready=false
    fi

    # Check Kafka
    if port_in_use 29092; then
        log_success "Kafka on port 29092 is available"
    else
        log_warn "Kafka on port 29092 is not available"
        infra_ready=false
    fi

    if [[ "$infra_ready" == false ]]; then
        log_error "Infrastructure is not fully available"
        log_info "Run: ./scripts/infra-up.sh"
        return 1
    fi

    log_success "Infrastructure check passed"
    return 0
}

if [[ "$CHECK_INFRA" == true ]] && { [[ "$RUN_INTEGRATION" == true ]] || [[ "$RUN_E2E" == true ]]; }; then
    if ! check_infrastructure; then
        exit 1
    fi
fi

# ============================================================================
# Build Test Patterns
# ============================================================================
UNIT_PATTERNS="*UnitTest,*MockitoTest"
INTEGRATION_PATTERNS="*IntegrationTest,*IT,*TestcontainersTest,*ApiTest"
E2E_PATTERNS="*E2ETest"

build_test_filter() {
    local patterns=""

    if [[ "$RUN_UNIT" == true ]]; then
        patterns="${UNIT_PATTERNS}"
    fi

    if [[ "$RUN_INTEGRATION" == true ]]; then
        if [[ -n "$patterns" ]]; then
            patterns="${patterns},${INTEGRATION_PATTERNS}"
        else
            patterns="${INTEGRATION_PATTERNS}"
        fi
    fi

    if [[ "$RUN_E2E" == true ]]; then
        if [[ -n "$patterns" ]]; then
            patterns="${patterns},${E2E_PATTERNS}"
        else
            patterns="${E2E_PATTERNS}"
        fi
    fi

    echo "$patterns"
}

# ============================================================================
# Run Tests
# ============================================================================
cd "${PROJECT_ROOT}"

# Build test type description
TEST_TYPES=""
if [[ "$RUN_UNIT" == true ]]; then TEST_TYPES="${TEST_TYPES}unit "; fi
if [[ "$RUN_INTEGRATION" == true ]]; then TEST_TYPES="${TEST_TYPES}integration "; fi
if [[ "$RUN_E2E" == true ]]; then TEST_TYPES="${TEST_TYPES}e2e "; fi

log_step "Running ${TEST_TYPES}tests for: ${SERVICES_TO_TEST[*]}"
echo ""

# Build Gradle tasks
GRADLE_TASKS=""
TEST_FILTER=$(build_test_filter)

for service in "${SERVICES_TO_TEST[@]}"; do
    if [[ -n "$TEST_FILTER" ]]; then
        GRADLE_TASKS="${GRADLE_TASKS} :${service}:test --tests \"${TEST_FILTER}\""
    else
        GRADLE_TASKS="${GRADLE_TASKS} :${service}:test"
    fi
done

# Build Gradle options
GRADLE_OPTS_ARRAY=()

if [[ "$VERBOSE" == true ]]; then
    GRADLE_OPTS_ARRAY+=("--info")
fi

if [[ "$FAIL_FAST" == true ]]; then
    GRADLE_OPTS_ARRAY+=("--fail-fast")
fi

if [[ "$RERUN_FAILED" == true ]]; then
    GRADLE_OPTS_ARRAY+=("--rerun-tasks")
fi

if [[ "$COVERAGE" == true ]]; then
    GRADLE_TASKS="${GRADLE_TASKS} jacocoTestReport"
fi

# Run tests
# Note: Using eval to handle the --tests parameter with patterns
BUILD_CMD="./gradlew ${GRADLE_TASKS} ${GRADLE_OPTS_ARRAY[*]:-}"
log_info "Running: ${BUILD_CMD}"
echo ""

start_time=$(date +%s)
test_exit_code=0

if eval "${BUILD_CMD}"; then
    end_time=$(date +%s)
    duration=$((end_time - start_time))
    echo ""
    log_success "Tests completed in ${duration} seconds"
else
    test_exit_code=$?
    end_time=$(date +%s)
    duration=$((end_time - start_time))
    echo ""
    log_error "Tests failed after ${duration} seconds"
fi

# ============================================================================
# Summary
# ============================================================================
echo ""
log_info "Test reports available at:"
for service in "${SERVICES_TO_TEST[@]}"; do
    report_path="${PROJECT_ROOT}/${service}/build/reports/tests/test/index.html"
    if [[ -f "$report_path" ]]; then
        echo "  - ${service}: file://${report_path}"
    fi
done

if [[ "$COVERAGE" == true ]]; then
    echo ""
    log_info "Coverage reports available at:"
    for service in "${SERVICES_TO_TEST[@]}"; do
        coverage_path="${PROJECT_ROOT}/${service}/build/reports/jacoco/test/html/index.html"
        if [[ -f "$coverage_path" ]]; then
            echo "  - ${service}: file://${coverage_path}"
        fi
    done
fi

exit $test_exit_code
