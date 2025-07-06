# PactZombies Course Structure

## Overview
PactZombies is an interactive course to learn Pact smart contract development on Kadena, inspired by CryptoZombies.

## Directory Structure
```
pactzombies/
├── README.md                    # Course overview and getting started
├── COURSE_STRUCTURE.md         # This file
├── index.html                  # Web interface for the course
├── lessons/                    # All lessons
│   ├── lesson-01-making-zombies/
│   │   ├── README.md          # Lesson content and instructions
│   │   ├── zombie-simple.pact # Simple starter code
│   │   ├── zombie-factory.pact # Main exercise file
│   │   ├── zombie-factory-complete.pact # Complete solution
│   │   ├── test-simple.repl   # Basic test
│   │   ├── test-zombies.repl  # Main tests
│   │   ├── test-complete.repl # Tests for complete solution
│   │   └── run-lesson1.sh    # Helper script to run examples
│   ├── lesson-02-zombie-attacks/
│   ├── lesson-03-advanced-zombies/
│   ├── lesson-04-zombie-economics/
│   ├── lesson-05-zombie-quests/
│   └── lesson-06-zombie-apocalypse/
├── contracts/                  # Shared contracts
├── tests/                      # Integration tests
├── assets/                     # Images and resources
└── web/                        # Web app components

## Lesson Progress

### ✅ Lesson 1: Making Your First Zombie
- Basic Pact syntax
- Modules and functions
- Schemas and tables
- Random number generation
- **Status**: Complete

### 🚧 Lesson 2: Zombies Attack Their Victims
- Function parameters and guards
- Battle mechanics
- Leveling system
- **Status**: To be created

### 🚧 Lesson 3: Advanced Zombie Features
- Capabilities deep dive
- Ownership and transfers
- Complex game logic
- **Status**: To be created

### 🚧 Lesson 4: Zombie Economics
- Fungible tokens
- Marketplace mechanics
- Trading systems
- **Status**: To be created

### 🚧 Lesson 5: Multi-Step Zombie Quests
- Pacts (multi-step transactions)
- Quest chains
- Cross-chain transfers
- **Status**: To be created

### 🚧 Lesson 6: Zombie Apocalypse
- Complete DApp
- Gas optimization
- Production deployment
- **Status**: To be created

## Development Notes

### Running Tests
```bash
# From lesson directory:
cabal run exe:pact -- test-simple.repl

# Or use the helper script:
./run-lesson1.sh
```

### Key Concepts Covered
1. **Pact Basics**: Syntax, types, functions
2. **Data Management**: Schemas, tables, CRUD operations
3. **Security**: Guards, capabilities, ownership
4. **Game Mechanics**: Random generation, battle systems
5. **Economics**: Tokens, marketplaces, incentives
6. **Advanced Features**: Pacts, cross-chain, optimization

### Future Enhancements
- [ ] Interactive web REPL integration
- [ ] Visual zombie builder
- [ ] Achievement system
- [ ] Community challenges
- [ ] Deploy to testnet button
- [ ] Video tutorials

## Contributing
To add new lessons or improve existing ones:
1. Follow the existing lesson structure
2. Include complete working examples
3. Add comprehensive tests
4. Update this structure document