# Chapter 2: Setting Up Your Development Environment

## Introduction

Setting up an effective Pact development environment is crucial for productive smart contract development. This chapter covers installation, configuration, and tooling to get you started with Pact development using the official tools and REPL.

## Installing Pact

### Option 1: Using Pre-built Binaries

The easiest way to get started is using pre-built binaries from the official releases:

```bash
# Download latest Pact release
curl -L https://github.com/kadena-io/pact/releases/latest/download/pact-linux.tar.gz | tar -xz

# Add to PATH
export PATH=$PATH:$(pwd)/pact

# Verify installation
pact --version
```

### Option 2: Building from Source

For the latest features or development builds:

```bash
# Prerequisites: Haskell Stack
curl -sSL https://get.haskellstack.org/ | sh

# Clone repository
git clone https://github.com/kadena-io/pact-5.git
cd pact-5

# Build Pact
stack build

# Install to local bin
stack install

# Verify installation
pact --version
```

### Option 3: Using Nix (Recommended for Reproducible Builds)

```bash
# Install Nix
curl -L https://nixos.org/nix/install | sh

# Enter development environment
cd pact-5
nix develop

# Build and run
cabal build exe:pact
cabal run exe:pact
```

## Development Tools and IDE Setup

### VS Code with Pact Extension

1. **Install VS Code**: Download from https://code.visualstudio.com/
2. **Install Pact Extension**: Search for "Pact" in the Extensions marketplace
3. **Configure settings**:

```json
{
  "pact.pactPath": "/path/to/pact",
  "pact.trace": true,
  "pact.gasLimit": 150000,
  "pact.gasPrice": 0.00000001
}
```

### Emacs Configuration

Add to your `.emacs` configuration:

```elisp
;; Pact mode
(add-to-list 'load-path "~/.emacs.d/pact-mode")
(require 'pact-mode)
(add-to-list 'auto-mode-alist '("\\.pact\\'" . pact-mode))

;; Syntax highlighting
(add-hook 'pact-mode-hook
          (lambda ()
            (setq-local comment-start ";;")
            (setq-local comment-start-skip ";+\\s-*")
            (font-lock-mode 1)))
```

### Vim/Neovim Configuration

Create `~/.vim/syntax/pact.vim`:

```vim
" Pact syntax highlighting
if exists("b:current_syntax")
  finish
endif

" Keywords
syn keyword pactKeyword defun defcap defschema deftable defpact module interface
syn keyword pactKeyword let let* if cond and or not enforce enforce-one
syn keyword pactKeyword with-read with-default-read bind lambda map filter fold
syn keyword pactKeyword insert update write delete read select keys
syn keyword pactKeyword create-table describe-table

" Built-in functions
syn keyword pactBuiltin length + - * / < > <= >= = != abs ceiling floor round
syn keyword pactBuiltin format concat take drop reverse sort
syn keyword pactBuiltin hash time chain-data tx-sender tx-hash

" Types
syn keyword pactType string integer decimal bool time guard keyset object

" Operators
syn match pactOperator "[-+*/<>=!&|]"

" Comments
syn match pactComment ";;.*$"

" Strings
syn region pactString start='"' end='"' skip='\\"'

" Numbers
syn match pactNumber "\<\d\+\>"
syn match pactNumber "\<\d\+\.\d\+\>"

" Highlighting
hi def link pactKeyword     Keyword
hi def link pactBuiltin     Function
hi def link pactType        Type
hi def link pactOperator    Operator
hi def link pactComment     Comment
hi def link pactString      String
hi def link pactNumber      Number

let b:current_syntax = "pact"
```

## Using the Pact REPL

### Starting the REPL

```bash
# Start interactive REPL
pact

# Load and run a script
pact script.repl

# Run with specific database
pact --database=mydb.sqlite script.repl

# Enable trace mode for debugging
pact --trace script.repl
```

### Basic REPL Commands

```pact
;; Load a Pact file
(load "contract.pact")

;; Start a transaction
(begin-tx)

;; Commit transaction
(commit-tx)

;; Rollback transaction
(rollback-tx)

;; Set environment data
(env-data { "keyset": { "keys": ["alice"], "pred": "keys-all" } })

;; Set signing keys
(env-keys ["alice"])

;; Set chain data
(env-chain-data { "chain-id": "0", "block-height": 100 })

;; Clear REPL state
(env-reset)

;; Execute with gas limit
(env-gaslimit 150000)

;; Get help
(help)
```

### REPL Configuration

Create a `.pact-repl` configuration file:

```json
{
  "gasLimit": 150000,
  "gasPrice": 0.00000001,
  "traceMode": true,
  "database": {
    "type": "sqlite",
    "file": "dev.sqlite"
  },
  "logLevel": "info"
}
```

## Project Structure

### Recommended Directory Layout

```
my-pact-project/
├── contracts/
│   ├── token.pact
│   ├── exchange.pact
│   └── governance.pact
├── interfaces/
│   ├── fungible-v2.pact
│   └── marketplace-v1.pact
├── tests/
│   ├── token-tests.repl
│   ├── exchange-tests.repl
│   └── integration-tests.repl
├── data/
│   ├── keysets.json
│   └── initial-data.json
├── scripts/
│   ├── deploy.repl
│   └── setup.repl
├── docs/
│   └── README.md
└── pact.yaml
```

### Project Configuration (pact.yaml)

```yaml
# Pact project configuration
project:
  name: "my-pact-project"
  version: "1.0.0"
  description: "My awesome Pact smart contracts"

# Compilation settings
compile:
  output: "build/"
  optimize: true
  warnings: true

# Testing configuration
test:
  files: ["tests/*.repl"]
  database: "test.sqlite"
  gasLimit: 150000

# Deployment settings
deploy:
  network: "testnet"
  gasPrice: 0.00000001
  sender: "deployment-account"

# Dependencies
dependencies:
  - name: "pact-stdlib"
    version: "1.0.0"
    source: "github:kadena-io/pact-stdlib"
```

## Writing Your First Contract

### Hello World Contract

Create `contracts/hello-world.pact`:

```pact
(module hello-world GOVERNANCE
  @doc "A simple hello world contract"
  
  (defcap GOVERNANCE ()
    @doc "Only admin can upgrade this contract"
    (enforce-keyset 'admin-keyset))
  
  (defschema message
    @doc "Message schema"
    text:string
    author:string
    timestamp:time)
  
  (deftable messages:{message})
  
  (defun say-hello:string (name:string)
    @doc "Say hello to someone"
    (format "Hello, {}!" [name]))
  
  (defun store-message:string (msg:string author:string)
    @doc "Store a message on the blockchain"
    (let ((msg-id (hash [msg author (chain-data 'time)])))
      (insert messages msg-id {
        "text": msg,
        "author": author,
        "timestamp": (chain-data 'time)
      })
      (format "Message stored with ID: {}" [msg-id])))
  
  (defun get-message:object (msg-id:string)
    @doc "Retrieve a stored message"
    (read messages msg-id))
  
  (defun list-messages:[object] ()
    @doc "List all messages"
    (select messages (constantly true)))
)

;; Create the table
(create-table messages)
```

### Test File

Create `tests/hello-world-tests.repl`:

```pact
;; hello-world-tests.repl
(begin-tx "Load and test hello world contract")

;; Set up test environment
(env-data {
  "admin-keyset": ["admin-key"]
})

(env-keys ["admin-key"])

;; Load the contract
(load "../contracts/hello-world.pact")

;; Test basic functionality
(expect "Say hello works"
  "Hello, Alice!"
  (say-hello "Alice"))

;; Test message storage
(expect "Message storage works"
  true
  (!= "" (store-message "Hello blockchain!" "Alice")))

;; Test message retrieval
(let ((msg-id (store-message "Test message" "Bob")))
  (let ((stored-msg (get-message msg-id)))
    (expect "Message retrieved correctly"
      "Test message"
      (at 'text stored-msg))
    
    (expect "Author stored correctly"
      "Bob"
      (at 'author stored-msg))))

;; Test message listing
(expect "Message listing works"
  true
  (> (length (list-messages)) 0))

(commit-tx)

;; Test error conditions
(begin-tx "Test error conditions")

;; Test unauthorized access
(env-keys [])
(expect-failure "Unauthorized access fails"
  "keyset failure"
  (store-message "Unauthorized" "Hacker"))

(commit-tx)

(print "All tests passed!")
```

## Database Setup

### SQLite Database (Development)

```bash
# Create development database
sqlite3 dev.sqlite "CREATE TABLE IF NOT EXISTS pact_tables (table_name TEXT PRIMARY KEY);"

# Run REPL with SQLite
pact --database=dev.sqlite
```

### In-Memory Database (Testing)

```pact
;; Use in-memory database for testing
(env-database-type "memory")

;; Or in REPL startup
pact --database=:memory:
```

### Production Database Configuration

```yaml
# Production database config
database:
  type: "postgresql"
  host: "localhost"
  port: 5432
  database: "pact_production"
  username: "pact_user"
  password: "${DB_PASSWORD}"
  pool_size: 10
  timeout: 30
```

## Development Workflow

### Basic Workflow

1. **Write Contract**: Create `.pact` files in `contracts/`
2. **Write Tests**: Create `.repl` files in `tests/`
3. **Test Locally**: Run tests with `pact tests/my-test.repl`
4. **Deploy to Testnet**: Use deployment scripts
5. **Integration Testing**: Test on testnet
6. **Deploy to Mainnet**: Final deployment

### Automated Testing Script

Create `scripts/test.sh`:

```bash
#!/bin/bash
set -e

echo "Running Pact tests..."

# Run all test files
for test_file in tests/*.repl; do
  echo "Running $test_file..."
  pact --database=:memory: "$test_file"
done

echo "All tests passed!"
```

### Deployment Script

Create `scripts/deploy.repl`:

```pact
;; deploy.repl - Deployment script
(begin-tx "Deploy contracts")

;; Load deployment configuration
(env-data (read-msg "deploy-config"))

;; Set deployment keys
(env-keys (read-msg "deploy-keys"))

;; Deploy contracts in order
(load "../contracts/interfaces.pact")
(load "../contracts/token.pact")
(load "../contracts/exchange.pact")

;; Initialize with deployment data
(setup-initial-state (read-msg "initial-state"))

(commit-tx)

(print "Deployment completed successfully!")
```

## Debugging and Troubleshooting

### Common Issues

1. **Module Loading Errors**:
```pact
;; Check module loading
(describe-module 'my-module)

;; Verify table existence
(describe-table my-module.my-table)
```

2. **Keyset Issues**:
```pact
;; Check keyset definition
(describe-keyset 'my-keyset)

;; Test keyset enforcement
(env-keys ["test-key"])
(enforce-keyset 'my-keyset)
```

3. **Gas Limit Issues**:
```pact
;; Increase gas limit
(env-gaslimit 500000)

;; Check gas usage
(env-gaslog)
```

### Debugging Tools

```pact
;; Enable trace mode
(env-trace true)

;; Log intermediate values
(defun debug-transfer (from to amount)
  (let ((from-bal (get-balance from))
        (to-bal (get-balance to)))
    (trace (format "Transfer: {} -> {}, amount: {}" [from to amount]))
    (trace (format "Balances before: {} = {}, {} = {}" [from from-bal to to-bal]))
    ;; ... perform transfer
    (trace "Transfer completed")))

;; Use env-step for step-by-step execution
(env-step true)
```

## Performance Optimization

### REPL Performance

```pact
;; Use transactions for better performance
(begin-tx)
;; ... multiple operations
(commit-tx)

;; Batch database operations
(let ((updates [
  ["alice" 1000.0],
  ["bob" 2000.0],
  ["charlie" 1500.0]
]))
  (map (lambda (update)
         (bind update [account balance]
           (update accounts account { "balance": balance })))
       updates))
```

### Database Optimization

```pact
;; Use appropriate database for environment
;; Development: SQLite
;; Testing: In-memory
;; Production: PostgreSQL

;; Optimize queries
(with-read accounts account { "balance" := bal, "guard" := g }
  ;; Process data
  )

;; Instead of
(let ((bal (at 'balance (read accounts account)))
      (g (at 'guard (read accounts account))))
  ;; Process data
  )
```

## Security Considerations

### Development Security

1. **Never commit private keys**:
```bash
# Add to .gitignore
*.key
keysets.json
.env
```

2. **Use environment variables**:
```bash
export ADMIN_KEY="your-key-here"
export DB_PASSWORD="your-password"
```

3. **Validate all inputs**:
```pact
(defun safe-transfer (from to amount)
  ;; Validate inputs
  (enforce (> amount 0.0) "Amount must be positive")
  (enforce (!= from to) "Cannot transfer to self")
  (enforce (is-valid-account from) "Invalid from account")
  (enforce (is-valid-account to) "Invalid to account")
  ;; ... perform transfer
  )
```

## Summary

A proper Pact development environment includes:

1. **Pact Installation** - Binary, source, or Nix-based
2. **IDE Configuration** - VS Code, Emacs, or Vim with Pact support
3. **REPL Mastery** - Interactive development and testing
4. **Project Structure** - Organized contracts, tests, and scripts
5. **Database Setup** - Appropriate for each environment
6. **Development Workflow** - Automated testing and deployment
7. **Debugging Tools** - Trace mode, logging, and step execution
8. **Performance Optimization** - Efficient REPL and database usage
9. **Security Practices** - Key management and input validation

This foundation enables productive Pact development with confidence in security, performance, and maintainability.

## Exercises

1. Set up a complete Pact development environment with your preferred editor
2. Create a project structure for a DeFi application
3. Write and test a simple token contract with comprehensive tests
4. Set up automated testing and deployment scripts
5. Configure database environments for development, testing, and production

## References

- Pact installation: https://github.com/kadena-io/pact
- REPL documentation: https://pact-language.readthedocs.io/
- Development tools: https://github.com/kadena-io/pact-atom
- Database configuration: Pact server documentation