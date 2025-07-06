#!/bin/bash

# PactZombies Lesson 1 - Running the examples
# ===========================================

echo "🧟 Welcome to PactZombies - Lesson 1: Making Your First Zombie"
echo "=============================================================="
echo ""

# Find the pact executable
PACT_BIN="/home/salama/Workspace/Kadena/pact-5/dist-newstyle/build/x86_64-linux/ghc-9.6.7/pact-tng-5.2/x/pact/opt/build/pact/pact"
if [ ! -f "$PACT_BIN" ]; then
    PACT_BIN=$(which pact)
    if [ -z "$PACT_BIN" ]; then
        echo "Error: pact executable not found!"
        echo "Please build with: cabal build exe:pact"
        exit 1
    fi
fi

echo "Using pact at: $PACT_BIN"
echo ""

# Change to lesson directory
cd "$(dirname "$0")"

echo "1. Testing simple zombie creation..."
echo "-----------------------------------"
$PACT_BIN test-simple.repl
echo ""

echo "2. To test the complete zombie factory with all features:"
echo "   $PACT_BIN test-complete.repl"
echo ""

echo "3. To experiment with your own zombies:"
echo "   - Edit zombie-simple.pact"
echo "   - Run: $PACT_BIN"
echo "   - In the REPL, type: (load \"zombie-simple.pact\")"
echo ""

echo "Happy zombie making! 🧟‍♂️"