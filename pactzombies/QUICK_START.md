# PactZombies Quick Start Guide 🚀

Get started with PactZombies in 5 minutes!

## Prerequisites

1. **Install Pact**:
   ```bash
   # Option 1: Using Homebrew (macOS)
   brew install kadena-io/pact/pact

   # Option 2: Using Nix
   nix-env -i pact

   # Option 3: Build from source
   git clone https://github.com/kadena-io/pact.git
   cd pact
   cabal install exe:pact
   ```

2. **Verify Installation**:
   ```bash
   pact --version
   ```

## Starting the Course

### Step 1: Navigate to Lesson 1
```bash
cd pactzombies/lessons/lesson-01-making-zombies
```

### Step 2: Read the Tutorial
Open `README.md` in your favorite editor or markdown viewer.

### Step 3: Write Your First Zombie Contract
Create your own version by copying the template:
```bash
cp zombie-simple.pact my-zombie.pact
```

### Step 4: Test Your Code
```bash
pact test-simple.repl
```

## Course Navigation

### Lesson Structure
Each lesson contains:
- `README.md` - Tutorial content
- `*.pact` - Contract files to edit
- `*-complete.pact` - Solution files
- `test-*.repl` - Test scripts

### Recommended Path
1. Read the README
2. Complete exercises in order
3. Test your code frequently
4. Check solutions when stuck
5. Experiment with variations

## Testing Your Code

### Running REPL Tests
```bash
# Basic test
pact test-simple.repl

# Full test suite
pact test-complete.repl
```

### Interactive REPL
```bash
# Start REPL
pact

# Load your contract
pact> (load "my-zombie.pact")

# Create tables
pact> (create-table zombies-table)

# Test functions
pact> (create-zombie "My First Zombie")
```

## Common Commands

### Pact REPL Commands
```lisp
; Load a file
(load "filename.pact")

; Start a transaction
(begin-tx)

; Commit transaction
(commit-tx)

; Rollback transaction
(rollback-tx)

; Set environment data
(env-data {"sender": "alice"})

; Set chain data
(env-chain-data {"chain-id": "0", "block-time": (time "2024-01-01T00:00:00Z")})
```

### Useful Patterns
```lisp
; Check table contents
(select zombies-table (constantly true))

; Get specific zombie
(read zombies-table "zombie_12345")

; Count zombies
(length (select zombies-table (where "owner" (= "alice"))))
```

## Troubleshooting

### Common Issues

1. **"Table not found" error**:
   ```lisp
   (create-table zombies-table)
   ```

2. **"Module not found" error**:
   ```lisp
   (load "zombie-factory.pact")
   ```

3. **"Not in transaction" error**:
   ```lisp
   (begin-tx)
   ; your operations
   (commit-tx)
   ```

4. **Type errors**:
   - Check function signatures
   - Ensure correct parameter types
   - Use type annotations

## Tips for Success

### 1. Use the REPL
Test small pieces of code interactively before adding to your contract.

### 2. Read Error Messages
Pact provides detailed error messages - read them carefully!

### 3. Start Simple
Get basic functionality working before adding complexity.

### 4. Test Frequently
Run tests after each change to catch errors early.

### 5. Compare with Solutions
If stuck, peek at the `-complete.pact` files for hints.

## Quick Reference

### Module Structure
```pact
(module my-module GOV
  (defcap GOV () true)
  
  (defschema my-schema
    field1:string
    field2:integer)
    
  (deftable my-table:{my-schema})
  
  (defun my-function (param:string)
    ; function body
  )
)
```

### Common Functions
```pact
; Insert data
(insert table-name key-value {"field": value})

; Update data
(update table-name key-value {"field": new-value})

; Read data
(read table-name key-value)

; Select with filter
(select table-name (where "field" (= value)))
```

## Next Steps

1. **Complete All Lessons**: Work through lessons 1-6
2. **Build Your Own**: Modify the zombie game with your ideas
3. **Deploy to Testnet**: Test on Kadena's testnet
4. **Join Community**: Share your progress and get help

## Getting Help

- **Course Issues**: Check solution files and test scripts
- **Pact Questions**: [Pact Documentation](https://pact-language.readthedocs.io)
- **Kadena Community**: [Discord](https://discord.gg/kadena)

Happy zombie building! 🧟‍♂️