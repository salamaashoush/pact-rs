# Chapter 4: The Module System - Deep Dive

## Introduction

Modules are the foundation of code organization in Pact. They provide namespace isolation, reusable components, and controlled upgrade mechanisms. This chapter explores every aspect of the module system, from basic definitions to advanced governance patterns.

## Understanding Modules

### What is a Module?

A module in Pact is a collection of:
- **Functions** (`defun`) - Executable code
- **Capabilities** (`defcap`) - Permissions
- **Schemas** (`defschema`) - Data structures  
- **Tables** (`deftable`) - Database tables
- **Constants** (`defconst`) - Immutable values
- **Pacts** (`defpact`) - Multi-step transactions

### Basic Module Structure

```pact
(module my-token 'admin-keyset
  @doc "A token module with all the components"
  
  ;; Implement interfaces
  (implements fungible-v2)
  
  ;; Import other modules
  (use util)
  
  ;; Define a constant
  (defconst MINIMUM_BALANCE 0.0001
    "Minimum balance to keep account active")
  
  ;; Define a schema
  (defschema account
    @doc "Account schema"
    balance:decimal
    guard:guard
    active:bool)
  
  ;; Create a table
  (deftable accounts:{account})
  
  ;; Define capabilities
  (defcap GOVERNANCE ()
    "Module governance capability"
    (enforce-keyset 'admin-keyset))
  
  ;; Define functions
  (defun get-balance:decimal (account:string)
    @doc "Get account balance"
    (at 'balance (read accounts account)))
)
```

## Module Lifecycle

### 1. Definition and Parsing

When you define a module, Pact:

```
Source Code
    ↓ Lexer
AST (Abstract Syntax Tree)
    ↓ Parser
Parse Tree
    ↓ Desugar
IR (Intermediate Representation)
    ↓ Type Check
Typed IR
```

### 2. Governance Evaluation

Before installation, Pact checks governance:

```pact
;; Keyset governance
(module my-module 'admin-keyset ...)

;; Capability governance  
(module my-module (GOVERNANCE) ...)

;; Non-upgradeable module
(module my-module (enforce false) ...)
```

### 3. Module Hashing

Every module gets a unique hash based on its content:

```pact
;; The hash includes:
;; - Module name
;; - Governance
;; - All definitions
;; - Imports
;; - Blessed hashes

(at 'hash (describe-module 'my-module))
;; "5a7f3d2b8c9e1f4a6b3d8e2f5c7a9b4d"
```

### 4. Dependency Resolution

Pact calculates all transitive dependencies:

```pact
(module token-v2 'admin
  (use token-v1)    ;; Direct dependency
  ;; If token-v1 uses util, then util is a transitive dependency
)
```

### 5. Storage

Modules are stored with their dependencies:

```json
{
  "module": { /* module definition */ },
  "dependencies": {
    "util.helper": { /* function def */ },
    "math.calculate": { /* function def */ }
  }
}
```

## Module Governance Models

### 1. Keyset Governance

The simplest and most common:

```pact
;; Define the keyset
(define-keyset 'module-admin (read-keyset 'module-admin))

;; Use in module
(module my-module 'module-admin
  ;; Module can only be upgraded by 'module-admin keyset
)
```

**Example upgrade scenario:**
```pact
;; First deployment
(env-keys ["admin-key-1", "admin-key-2"])
(env-data { 'module-admin: { 
  "keys": ["admin-key-1", "admin-key-2"], 
  "pred": "keys-all" }})

(module my-module 'module-admin
  (defun version() "1.0.0"))

;; Later upgrade (must satisfy keyset)
(module my-module 'module-admin
  (defun version() "1.0.1"))
```

### 2. Capability Governance

More flexible, capability-based control:

```pact
(module my-module (GOV)
  
  (defcap GOV ()
    @doc "Governance capability"
    (enforce-one "Any admin can upgrade"
      [(enforce-keyset 'super-admin)
       (enforce-keyset 'module-admin)
       (check-time-window)]))
  
  (defun check-time-window ()
    "Only allow upgrades during maintenance"
    (let ((hour (at 'hour (time))))
      (enforce (and (>= hour 2) (<= hour 4))
        "Upgrades only between 2-4 AM")))
)
```

### 3. Non-Upgradeable Modules

For immutable contracts:

```pact
(module immutable-lottery (enforce false)
  @doc "This module can never be changed"
  ;; Perfect for trustless applications
)
```

## Interfaces

### Defining Interfaces

Interfaces define contracts that modules can implement:

```pact
(interface fungible-token
  @doc "Standard fungible token interface"
  
  ;; Define schemas that implementations must include
  (defschema account
    balance:decimal
    guard:guard)
  
  ;; Function signatures (no implementation)
  (defun transfer:string (from:string to:string amount:decimal)
    @doc "Transfer tokens between accounts")
  
  (defun get-balance:decimal (account:string)
    @doc "Get account balance")
  
  ;; Capability signatures
  (defcap TRANSFER (from:string to:string amount:decimal)
    @doc "Transfer capability")
  
  ;; Properties for formal verification
  @model
  [(property conservation
     (= (fold (+) 0 (map (get-balance) all-accounts))
        total-supply))]
)
```

### Implementing Interfaces

```pact
(module my-token 'admin
  (implements fungible-token)
  
  ;; Must provide all schemas
  (defschema account
    balance:decimal
    guard:guard
    last-updated:time)  ;; Can add fields
  
  ;; Must implement all functions
  (defun transfer:string (from:string to:string amount:decimal)
    ;; Actual implementation
    (with-capability (TRANSFER from to amount)
      (debit from amount)
      (credit to amount)
      "Transfer complete"))
  
  ;; Must implement all capabilities
  (defcap TRANSFER (from:string to:string amount:decimal)
    @managed amount TRANSFER-mgr
    (enforce-guard (at 'guard (read accounts from))))
)
```

## Module Imports and Dependencies

### Basic Imports

```pact
;; Import everything
(use coin)

;; Selective import
(use coin ['transfer 'get-balance])

;; Namespaced import
(use kadena.coin)
```

### Hash-Pinned Imports

For strict version control:

```pact
;; Pin to specific version
(use coin "5a7f3d2b8c9e1f4a6b3d8e2f5c7a9b4d")

;; Selective import with hash
(use coin "5a7f3d2b8c9e1f4a6b3d8e2f5c7a9b4d" ['transfer])
```

### Import Resolution

```pact
(module defi-app 'admin
  (use token)      ;; Direct dependency
  (use math)       ;; Direct dependency
  
  ;; If token uses util, and math uses util,
  ;; util is included only once in transitive deps
)
```

## Module Storage and Loading

### How Modules are Stored

In the Pact database, modules are stored in special domains:

1. **Module Definition** - The actual module code and metadata
2. **Module Source** - The original source code (for reference)
3. **Module Dependencies** - All transitive dependencies

### The Loaded State

During execution, Pact maintains a "loaded" state:

```haskell
-- From the implementation
data Loaded = Loaded
  { _loModules :: Map ModuleName (ModuleData, ModuleCode)
  , _loAllLoaded :: Map FullyQualifiedName EvalDef
  , _loNamespace :: Maybe Namespace
  }
```

### Module Caching

Modules are cached for performance:
1. First access loads from database
2. Subsequent accesses use cache
3. Cache cleared on transaction boundaries

## Advanced Module Patterns

### 1. Upgradeable Module with Migration

```pact
(module user-data 'admin
  
  (defschema user-v2
    name:string
    email:string
    premium:bool     ;; New field
    guard:guard)
    
  (deftable users:{user-v2})
  
  (defun migrate-user (user-id:string)
    "Migrate v1 user to v2"
    (with-read users-v1 user-id
      { 'name := name, 'email := email, 'guard := guard }
      (insert users user-id
        { 'name: name
        , 'email: email
        , 'premium: false  ;; Default value
        , 'guard: guard })))
)
```

### 2. Module Factory Pattern

```pact
(module token-factory 'factory-admin
  
  (defcap CREATE-TOKEN (token-name:string)
    (enforce-keyset 'factory-admin))
  
  (defun create-token-module (token-name:string supply:decimal)
    (with-capability (CREATE-TOKEN token-name)
      ;; Install a new module dynamically
      (install-module
        (format "(module {} 'token-admin ...)" [token-name]))))
)
```

### 3. Module Registry Pattern

```pact
(module registry 'registry-admin
  
  (defschema registration
    module-name:string
    module-hash:string
    version:string
    active:bool)
    
  (deftable registrations:{registration})
  
  (defun register-module (name:string)
    "Register a module in the registry"
    (let ((hash (at 'hash (describe-module name))))
      (insert registrations name
        { 'module-name: name
        , 'module-hash: hash
        , 'version: "1.0.0"
        , 'active: true })))
)
```

## Module Security Considerations

### 1. Governance Security

```pact
;; BAD: Weak governance
(module insecure 'any-keyset  ;; Too permissive
  ...)

;; GOOD: Strong governance
(module secure 'multi-sig-admin  ;; Multi-sig required
  ...)
```

### 2. Dependency Security

```pact
;; BAD: Unpinned dependencies
(use unknown-module)  ;; Could change

;; GOOD: Pinned dependencies
(use verified-module "hash123...")  ;; Immutable
```

### 3. Interface Compliance

```pact
;; Interfaces enforce contracts
(module my-token 'admin
  (implements fungible-v2)  ;; Must implement correctly
  
  ;; Compiler ensures all functions are implemented
  ;; with correct signatures
)
```

## Testing Modules

### Unit Testing

```pact
;; test/my-module.repl
(begin-tx)
(env-keys ["admin"])
(env-data { 'admin-keyset: ["admin"] })

;; Load module
(load "my-module.pact")

;; Test functions
(expect "Balance starts at 0" 
  0.0 
  (my-module.get-balance "alice"))

(commit-tx)
```

### Integration Testing

```pact
;; Test module interactions
(begin-tx)
(load "token.pact")
(load "dex.pact")

;; Test cross-module calls
(expect "DEX can transfer tokens"
  "success"
  (dex.swap "token" "coin" 100.0))
  
(commit-tx)
```

## Module Best Practices

### 1. Clear Naming

```pact
;; GOOD: Descriptive module names
(module kadena-token-v2 ...)
(module user-authentication ...)

;; BAD: Vague names
(module stuff ...)
(module test2 ...)
```

### 2. Documentation

```pact
(module well-documented 'admin
  @doc "Comprehensive module documentation"
  @model "Include formal properties"
  
  (defun critical-function ()
    @doc "Every function should be documented"
    ...)
)
```

### 3. Versioning Strategy

```pact
(module my-app 'admin
  (defconst VERSION "2.1.0")
  
  ;; Blessed hashes for explicit versioning
  (bless "hash-of-v1")
  (bless "hash-of-v2")
)
```

### 4. Minimal Interfaces

```pact
;; GOOD: Focused interface
(interface transfer-only
  (defun transfer ...))

;; BAD: Kitchen sink interface  
(interface everything
  ;; 50 functions...
)
```

## Real-World Module Example

Here's a complete, production-ready module:

```pact
(module payment-processor 'payment-admin
  @doc "Payment processing module with escrow"
  
  (implements payment-interface)
  
  ;; Dependencies
  (use coin)
  (use util)
  
  ;; Constants
  (defconst ESCROW_DURATION (days 7))
  (defconst MIN_PAYMENT 0.01)
  
  ;; Schemas
  (defschema payment
    @doc "Payment record"
    payer:string
    payee:string
    amount:decimal
    status:string
    escrow-release:time
    guard:guard)
  
  ;; Tables
  (deftable payments:{payment})
  
  ;; Capabilities
  (defcap GOVERNANCE ()
    (enforce-keyset 'payment-admin))
  
  (defcap PROCESS (payment-id:string)
    @doc "Process a specific payment"
    (with-read payments payment-id
      { 'guard := guard }
      (enforce-guard guard)))
  
  ;; Functions
  (defun create-payment:string 
    (payer:string 
     payee:string 
     amount:decimal 
     payer-guard:guard)
    @doc "Create an escrowed payment"
    
    (enforce (>= amount MIN_PAYMENT) "Below minimum")
    
    (let ((payment-id (hash { 'payer: payer, 'payee: payee, 'time: (time) }))
          (release-time (add-time (time) ESCROW_DURATION)))
      
      (coin.transfer payer ESCROW_ACCOUNT amount)
      
      (insert payments payment-id
        { 'payer: payer
        , 'payee: payee
        , 'amount: amount
        , 'status: "pending"
        , 'escrow-release: release-time
        , 'guard: payer-guard })
      
      payment-id))
  
  (defun release-payment (payment-id:string)
    @doc "Release escrowed payment"
    (with-capability (PROCESS payment-id)
      (with-read payments payment-id
        { 'payee := payee
        , 'amount := amount
        , 'status := status
        , 'escrow-release := release-time }
        
        (enforce (= status "pending") "Payment not pending")
        (enforce-one "Payment can be released"
          [(enforce (>= (time) release-time) "Escrow period active")
           (enforce-keyset 'payment-admin)])
        
        (coin.transfer ESCROW_ACCOUNT payee amount)
        (update payments payment-id { 'status: "completed" }))))
)

;; Create table after module
(create-table payments)
```

## Module Troubleshooting

### Common Errors

1. **"Module already exists"**
   - Solution: Ensure governance is satisfied for upgrade

2. **"Interface not implemented"**
   - Solution: Implement all required functions

3. **"Unknown module"**
   - Solution: Check module name and namespace

4. **"Transitive dependency error"**
   - Solution: Ensure all dependencies are installed

## Summary

The module system is the backbone of Pact development. It provides:

- **Organization**: Clean code structure
- **Security**: Controlled governance
- **Reusability**: Interfaces and imports
- **Integrity**: Content-addressed hashing
- **Evolution**: Managed upgrades

Key takeaways:
1. Choose appropriate governance for your use case
2. Use interfaces for standardization
3. Pin dependencies for production
4. Document thoroughly
5. Test module interactions

In the next chapter, we'll explore Pact's unique capability system in detail.

## Exercises

1. Create a module with both keyset and capability governance options
2. Implement a standard interface in your module
3. Write a module that imports and extends another module
4. Create a non-upgradeable module for a lottery system
5. Build a module registry that tracks installed modules

## References

- Module implementation: `/pact/Pact/Core/IR/Term.hs`
- Module compilation: `/pact/Pact/Core/Compile.hs`
- Module hashing: `/pact/Pact/Core/IR/ModuleHashing.hs`
- Dependency resolution: `/pact/Pact/Core/TransitiveDependencies.hs`