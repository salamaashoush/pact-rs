#!/bin/bash

# PactZombies Test Runner
# Runs all test files across all lessons

echo "🧟 PactZombies Test Runner 🧟"
echo "============================"
echo ""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Find pact executable
PACT_BIN=$(which pact)
if [ -z "$PACT_BIN" ]; then
    # Try common build location
    PACT_BIN="/home/salama/Workspace/Kadena/pact-5/dist-newstyle/build/x86_64-linux/ghc-9.6.7/pact-tng-5.2/x/pact/opt/build/pact/pact"
    if [ ! -f "$PACT_BIN" ]; then
        echo -e "${RED}Error: pact executable not found!${NC}"
        echo "Please install pact or update the path in this script"
        exit 1
    fi
fi

echo "Using pact at: $PACT_BIN"
echo ""

# Track results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Function to run a test
run_test() {
    local lesson=$1
    local test_file=$2
    local full_path="lessons/$lesson/$test_file"
    
    if [ -f "$full_path" ]; then
        echo -e "${YELLOW}Running $lesson/$test_file...${NC}"
        TOTAL_TESTS=$((TOTAL_TESTS + 1))
        
        # Run test and capture output
        cd "lessons/$lesson" 2>/dev/null
        OUTPUT=$($PACT_BIN "$test_file" 2>&1)
        RESULT=$?
        cd - > /dev/null 2>&1
        
        if [ $RESULT -eq 0 ]; then
            echo -e "${GREEN}✓ PASSED${NC}"
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            echo -e "${RED}✗ FAILED${NC}"
            FAILED_TESTS=$((FAILED_TESTS + 1))
            echo "Error output:"
            echo "$OUTPUT" | tail -n 10
        fi
        echo ""
    else
        echo -e "${YELLOW}Warning: $full_path not found${NC}"
    fi
}

# Run tests for each lesson
echo "Lesson 1: Making Zombies"
echo "------------------------"
run_test "lesson-01-making-zombies" "test-simple.repl"

echo "Lesson 2: Zombie Attacks"
echo "------------------------"
run_test "lesson-02-zombie-attacks" "test-feeding.repl"

echo "Lesson 3: Advanced Zombies"
echo "--------------------------"
run_test "lesson-03-advanced-zombies" "test-ownership.repl"

echo "Lesson 4: Zombie Economics"
echo "--------------------------"
run_test "lesson-04-zombie-economics" "test-economy.repl"

echo "Lesson 5: Zombie Quests"
echo "------------------------"
run_test "lesson-05-zombie-quests" "test-quests.repl"

# Summary
echo ""
echo "Test Summary"
echo "============"
echo -e "Total Tests: $TOTAL_TESTS"
echo -e "${GREEN}Passed: $PASSED_TESTS${NC}"
echo -e "${RED}Failed: $FAILED_TESTS${NC}"

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}All tests passed! 🎉${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed. Please check the errors above.${NC}"
    exit 1
fi