#!/bin/bash
# Comprehensive test runner for pact-cek

echo "=== Running Comprehensive CEK Tests ==="
echo

# Color codes for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to run a test category
run_test_category() {
    local category=$1
    local test_file=$2

    echo -e "${YELLOW}Running $category tests...${NC}"

    if cargo test -p pact-cek --test $test_file -- --nocapture; then
        echo -e "${GREEN}✓ $category tests passed${NC}"
    else
        echo -e "${RED}✗ $category tests failed${NC}"
        exit 1
    fi
    echo
}

# Run existing tests in src/tests.rs
echo -e "${YELLOW}Running existing unit tests...${NC}"
if cargo test -p pact-cek --lib -- --nocapture; then
    echo -e "${GREEN}✓ Unit tests passed${NC}"
else
    echo -e "${RED}✗ Unit tests failed${NC}"
    exit 1
fi
echo

# Run comprehensive CEK tests
run_test_category "Comprehensive CEK" "comprehensive_cek_tests"

# Run database and capability tests
run_test_category "Database & Capability" "database_capability_tests"

# Run performance and stress tests (these might take longer)
echo -e "${YELLOW}Running performance tests (this may take a while)...${NC}"
if cargo test -p pact-cek --test performance_stress_tests --release -- --nocapture; then
    echo -e "${GREEN}✓ Performance tests passed${NC}"
else
    echo -e "${RED}✗ Performance tests failed${NC}"
    exit 1
fi
echo

# Run property-based tests
run_test_category "Property-based" "property_based_tests"

# Run with different feature flags if any
echo -e "${YELLOW}Running tests with all features...${NC}"
if cargo test -p pact-cek --all-features -- --nocapture; then
    echo -e "${GREEN}✓ All-features tests passed${NC}"
else
    echo -e "${RED}✗ All-features tests failed${NC}"
    exit 1
fi
echo

# Check for test coverage (optional, requires cargo-tarpaulin)
if command -v cargo-tarpaulin &> /dev/null; then
    echo -e "${YELLOW}Generating test coverage report...${NC}"
    cargo tarpaulin -p pact-cek --out Html --output-dir target/coverage
    echo -e "${GREEN}Coverage report generated at target/coverage/tarpaulin-report.html${NC}"
else
    echo -e "${YELLOW}Skipping coverage (install cargo-tarpaulin for coverage reports)${NC}"
fi
echo

# Summary
echo -e "${GREEN}=== All tests completed successfully! ===${NC}"
echo
echo "Test Summary:"
echo "- Unit tests: ✓"
echo "- Comprehensive CEK tests: ✓"
echo "- Database & Capability tests: ✓"
echo "- Performance tests: ✓"
echo "- Property-based tests: ✓"
echo
echo "The CEK architecture and builtins are working correctly!"
