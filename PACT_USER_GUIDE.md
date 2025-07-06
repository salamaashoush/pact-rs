# Pact 5 Comprehensive User Guide

## Table of Contents
1. [Introduction](#introduction)
2. [Module System](#module-system)
3. [Capability System](#capability-system)
4. [Keyset Security](#keyset-security)
5. [Database Operations](#database-operations)
6. [Gas Metering](#gas-metering)
7. [Builtin Functions Reference](#builtin-functions-reference)
8. [Practical Examples](#practical-examples)
9. [Best Practices & Optimization](#best-practices--optimization)

## Introduction

Pact is a smart contract language designed for blockchain applications with a focus on safety, clarity, and formal verification. Key features include:

- **Turing-incomplete** by design to prevent unbounded computation
- **Human-readable** syntax inspired by Lisp
- **Built-in authorization** with keysets and capabilities
- **Database abstraction** with table/row semantics
- **Formal verification** support with properties and invariants

## Module System

### Overview

Modules are the primary code organization unit in Pact, providing namespace isolation, reusable code, and upgradeable logic.

### Module Definition

```pact
(module my-module 'my-admin-keyset
  "Module documentation string"
  
  ;; Optionally implement interfaces
  (implements my-interface)
  
  ;; Define schemas
  (defschema user
    name:string
    balance:decimal
    active:bool)
  
  ;; Create tables
  (deftable users:{user})
  
  ;; Define functions
  (defun get-user:object{user} (id:string)
    "Get user by ID"
    (read users id))
)
```

### Module Governance

Modules can be governed by:

1. **Keyset Governance** - Simple key-based control
```pact
(module my-module 'admin-keyset ...)
```

2. **Capability Governance** - More flexible capability-based control
```pact
(module my-module GOVERNANCE ...)
```

### Interfaces

Interfaces define contracts that modules can implement:

```pact
(interface fungible-token
  "Standard fungible token interface"
  
  (defschema account
    balance:decimal
    guard:guard)
  
  (defun transfer:string (from:string to:string amount:decimal)
    @doc "Transfer tokens between accounts")
  
  (defun get-balance:decimal (account:string)
    @doc "Get account balance")
)
```

### Module Loading and Dependencies

Modules are loaded and stored with their transitive dependencies:

```pact
;; Import specific functions
(use my-module ['get-user 'update-user])

;; Import all functions
(use my-module)

;; Namespaced imports
(use kadena.token)
```

### Module Hashing

Modules are content-addressed using hashes. Each module has a unique hash based on its code content, ensuring integrity and enabling dependency tracking.

## Capability System

### Overview

Capabilities provide fine-grained, composable authorization for smart contract operations. They offer more flexibility than simple keyset checks.

### Capability Definition

```pact
(defcap TRANSFER (from:string to:string amount:decimal)
  "Capability to transfer funds"
  @managed amount TRANSFER-mgr
  (enforce-guard (at 'guard (read accounts from)))
  (enforce (> amount 0.0) "Amount must be positive")
)

(defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
  "Manages TRANSFER capability"
  (enforce (<= requested managed) "Transfer amount exceeded")
  (- managed requested))
```

### Capability Types

1. **Basic Capabilities** - Simple authorization tokens
```pact
(defcap ADMIN ()
  (enforce-keyset 'admin-keyset))
```

2. **Managed Capabilities** - Track resource usage
```pact
(defcap TRANSFER (from:string to:string amount:decimal)
  @managed amount TRANSFER-mgr
  ...)
```

3. **Event Capabilities** - Emit blockchain events
```pact
(defcap TRANSFER_EVENT (from:string to:string amount:decimal)
  @event true)
```

### Using Capabilities

```pact
;; Acquire capability for a scope
(with-capability (TRANSFER "alice" "bob" 100.0)
  (transfer-internal "alice" "bob" 100.0))

;; Require capability is already granted
(defun transfer-internal (from:string to:string amount:decimal)
  (require-capability (TRANSFER from to amount))
  ;; Implementation
)

;; Compose capabilities
(defun multi-transfer (transfers:[object])
  (compose-capability (ADMIN))
  (map (transfer-one) transfers))

;; Install capability for later use
(install-capability (TRANSFER "alice" "bob" 100.0))
```

### Capability Guards

Create guards that require specific capabilities:

```pact
(create-capability-guard (TRANSFER "alice" "bob" 100.0))
```

## Keyset Security

### Overview

Keysets provide cryptographic signature-based authorization using public key cryptography.

### Keyset Definition

```pact
;; Define a keyset in transaction data
(env-data { 'my-keyset: {
  'keys: ["key1" "key2" "key3"],
  'pred: 'keys-2
}})

;; Define keyset in contract
(define-keyset 'my-keyset (read-keyset 'my-keyset))
```

### Keyset Predicates

1. **keys-all** - All keys must sign
2. **keys-any** - At least one key must sign
3. **keys-2** - At least two keys must sign
4. **Custom predicates** - User-defined functions

```pact
;; Custom predicate
(defun custom-pred (count:integer matched:integer)
  "Require 3 of 5 signatures"
  (>= matched 3))
```

### Keyset Enforcement

```pact
;; Direct enforcement
(enforce-keyset 'my-keyset)

;; Guard-based enforcement
(enforce-guard (keyset-ref-guard 'my-keyset))

;; Conditional enforcement
(if (= (tx-type) "exec")
  (enforce-keyset 'admin-keyset)
  "Keyset not required for local calls")
```

### Key Formats

Pact supports multiple key formats:
- **Ed25519** - 64 character hex strings
- **WebAuthn** - Prefixed with "WEBAUTHN-"

## Database Operations

### Overview

Pact provides a key-value database abstraction with schema validation and row-level guards.

### Table Definition

```pact
(defschema account
  balance:decimal
  guard:guard
  created:time)

(deftable accounts:{account})
```

### Basic Operations

```pact
;; Insert (fails if exists)
(insert accounts "alice" {
  'balance: 1000.0,
  'guard: (read-keyset 'alice-keyset),
  'created: (time)
})

;; Write (upsert)
(write accounts "alice" {
  'balance: 1000.0,
  'guard: (read-keyset 'alice-keyset),
  'created: (time)
})

;; Update (fails if not exists)
(update accounts "alice" {
  'balance: 900.0
})

;; Read
(read accounts "alice")
(read accounts "alice" ['balance])  ;; Select fields

;; Select with filter
(select accounts (where 'balance (< 100.0)))

;; Keys
(keys accounts)

;; Fold
(fold-db accounts
  (lambda (k v) (+ 1))
  0)
```

### Advanced Patterns

```pact
;; With-read pattern
(with-read accounts "alice"
  { 'balance := bal, 'guard := g }
  (enforce-guard g)
  (update accounts "alice" { 'balance: (- bal 100.0) }))

;; With-default-read
(with-default-read accounts "alice"
  { 'balance: 0.0, 'guard: false }
  { 'balance := bal }
  bal)
```

## Gas Metering

### Overview

Gas metering prevents resource exhaustion by charging for computational work.

### Gas Model

Gas costs are based on:
- **Computation** - Basic operations, function calls
- **Memory** - Data structure creation and manipulation
- **Storage** - Database reads and writes
- **Cryptography** - Hashing, signature verification

### Gas Costs (Examples)

| Operation | Gas Cost |
|-----------|----------|
| Basic arithmetic | 30+ milligas |
| Function call | 100 milligas base |
| Database read | 2,500 milligas + 100/byte |
| Database write | 25,000 milligas + 200/byte |
| List concatenation | Based on result size |
| Cryptographic hash | 124,000+ milligas |

### Gas Optimization Tips

1. **Minimize database operations** - Batch reads/writes when possible
2. **Use field projection** - Read only needed fields
3. **Avoid large data structures** - Gas scales with size
4. **Precompute when possible** - Do calculations off-chain
5. **Use appropriate data types** - Integers are cheaper than strings

## Builtin Functions Reference

### Arithmetic Operations
```pact
(+ 1 2)           ; => 3
(- 10 3)          ; => 7
(* 4 5)           ; => 20
(/ 20 4)          ; => 5
(^ 2 3)           ; => 8
(mod 10 3)        ; => 1
(abs -5)          ; => 5
(negate 5)        ; => -5
```

### String Operations
```pact
(+ "hello" " world")              ; => "hello world"
(length "hello")                  ; => 5
(take 3 "hello")                  ; => "hel"
(drop 3 "hello")                  ; => "lo"
(int-to-str 10)                   ; => "10"
(str-to-int "10")                 ; => 10
(format "{} + {} = {}" [1 2 3])   ; => "1 + 2 = 3"
```

### List Operations
```pact
(length [1 2 3])                  ; => 3
(reverse [1 2 3])                 ; => [3 2 1]
(sort [3 1 2])                    ; => [1 2 3]
(distinct [1 2 2 3])              ; => [1 2 3]
(contains 2 [1 2 3])              ; => true
(map (+ 1) [1 2 3])               ; => [2 3 4]
(filter (< 2) [1 2 3 4])          ; => [3 4]
(fold (+) 0 [1 2 3])              ; => 6
```

### Time Operations
```pact
(time)                            ; Current time
(parse-time "%F" "2023-01-01")    ; Parse time
(format-time "%F %T" (time))      ; Format time
(add-time (time) (days 7))        ; Add 7 days
(diff-time t1 t2)                 ; Time difference
```

### Control Flow
```pact
(if (> 5 3) "yes" "no")           ; => "yes"

(cond
  ((= x 1) "one")
  ((= x 2) "two")
  "other")

(enforce (> balance 0.0) "Insufficient balance")

(try
  (/ 1 0)
  "Division by zero!")
```

## Practical Examples

### Simple Token Contract

```pact
(module simple-token 'admin-keyset
  (defschema account
    balance:decimal
    guard:guard)
  
  (deftable accounts:{account})
  
  (defcap TRANSFER (from:string to:string amount:decimal)
    @managed amount TRANSFER-mgr
    (enforce-guard (at 'guard (read accounts from)))
    (enforce (> amount 0.0) "Amount must be positive"))
  
  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    (enforce (<= requested managed) "Transfer exceeds approved amount")
    (- managed requested))
  
  (defun transfer:string (from:string to:string amount:decimal)
    (with-capability (TRANSFER from to amount)
      (with-read accounts from { 'balance := from-bal }
        (enforce (>= from-bal amount) "Insufficient balance")
        (update accounts from { 'balance: (- from-bal amount) })
        (with-default-read accounts to
          { 'balance: 0.0, 'guard: (keyset-ref-guard 'user) }
          { 'balance := to-bal }
          (write accounts to { 'balance: (+ to-bal amount) })))
      (format "Transferred {} from {} to {}" [amount from to])))
)
```

### Multi-Signature Wallet

```pact
(module multisig-wallet 'admin-keyset
  (defschema proposal
    description:string
    amount:decimal
    recipient:string
    approvals:[string]
    executed:bool)
  
  (deftable proposals:{proposal})
  
  (defcap PROPOSE () 
    (enforce-keyset 'wallet-members))
  
  (defcap APPROVE (id:string)
    (enforce-keyset 'wallet-members)
    (with-read proposals id { 'executed := executed }
      (enforce (not executed) "Already executed")))
  
  (defcap EXECUTE (id:string)
    (with-read proposals id 
      { 'approvals := approvals, 'executed := executed }
      (enforce (not executed) "Already executed")
      (enforce (>= (length approvals) 2) "Needs 2 approvals")))
  
  (defun propose:string (description:string amount:decimal recipient:string)
    (with-capability (PROPOSE)
      (let ((id (hash { 'proposal: description, 'time: (time) })))
        (insert proposals id {
          'description: description,
          'amount: amount,
          'recipient: recipient,
          'approvals: [(tx-signer)],
          'executed: false
        })
        id)))
  
  (defun approve:string (id:string)
    (with-capability (APPROVE id)
      (let ((signer (tx-signer)))
        (with-read proposals id { 'approvals := approvals }
          (enforce (not (contains signer approvals)) "Already approved")
          (update proposals id { 
            'approvals: (+ approvals [signer]) 
          })))))
  
  (defun execute:string (id:string)
    (with-capability (EXECUTE id)
      (with-read proposals id 
        { 'amount := amount, 'recipient := recipient }
        (update proposals id { 'executed: true })
        ;; Transfer logic here
        (format "Executed transfer of {} to {}" [amount recipient]))))
)
```

## Best Practices & Optimization

### 1. Gas Optimization

**Minimize Database Operations**
```pact
;; Bad: Multiple reads
(let ((bal1 (at 'balance (read accounts "alice")))
      (bal2 (at 'balance (read accounts "bob"))))
  (+ bal1 bal2))

;; Good: Single read with destructuring
(with-read accounts "alice" { 'balance := bal1 }
  (with-read accounts "bob" { 'balance := bal2 }
    (+ bal1 bal2)))
```

**Use Field Projection**
```pact
;; Bad: Read entire object
(at 'balance (read accounts "alice"))

;; Good: Read only needed field
(read accounts "alice" ['balance])
```

### 2. Security Best Practices

**Always Validate Inputs**
```pact
(defun transfer (to:string amount:decimal)
  (enforce (!= to "") "Recipient cannot be empty")
  (enforce (> amount 0.0) "Amount must be positive")
  (enforce (<= amount MAX_TRANSFER) "Amount exceeds maximum")
  ;; Implementation
)
```

**Use Capabilities for Authorization**
```pact
;; Bad: Direct keyset check in business logic
(defun withdraw (amount:decimal)
  (enforce-keyset 'user-keyset)
  ;; Implementation
)

;; Good: Capability-based authorization
(defcap WITHDRAW (user:string amount:decimal)
  (enforce-guard (at 'guard (read accounts user))))

(defun withdraw (amount:decimal)
  (with-capability (WITHDRAW (tx-sender) amount)
    ;; Implementation
  ))
```

### 3. Code Organization

**Separate Concerns**
```pact
;; Validation functions
(defun validate-amount:bool (amount:decimal)
  (and (> amount 0.0) (<= amount MAX_AMOUNT)))

;; Business logic
(defun transfer-impl (from:string to:string amount:decimal)
  (enforce (validate-amount amount) "Invalid amount")
  ;; Core transfer logic
)

;; Public API
(defun transfer (to:string amount:decimal)
  (transfer-impl (tx-sender) to amount))
```

### 4. Error Handling

**Provide Clear Error Messages**
```pact
;; Bad
(enforce (> balance amount) "Error")

;; Good
(enforce (> balance amount) 
  (format "Insufficient balance: {} < {}" [balance amount]))
```

**Use Try-Catch for Expected Failures**
```pact
(defun safe-divide:decimal (a:decimal b:decimal)
  (try
    (/ a b)
    0.0))  ;; Return 0 on division by zero
```

### 5. Testing Patterns

**Use REPL for Comprehensive Testing**
```pact
;; test.repl
(begin-tx)
(env-data { 'alice-keyset: { 'keys: ["alice-key"], 'pred: 'keys-all }})
(env-keys ["alice-key"])

(load "my-contract.pact")

(expect "Balance starts at 0"
  0.0
  (get-balance "alice"))

(transfer "alice" "bob" 100.0)

(expect-failure "Cannot transfer more than balance"
  "Insufficient balance"
  (transfer "alice" "bob" 200.0))

(commit-tx)
```

### 6. Upgradability Considerations

**Design for Upgrades**
```pact
;; Use interfaces for standardization
(implements fungible-v2)

;; Version your schemas
(defschema account-v2
  balance:decimal
  guard:guard
  metadata:object)  ;; New field

;; Provide migration functions
(defun migrate-v1-to-v2 (account:string)
  (with-read accounts-v1 account { 'balance := b, 'guard := g }
    (write accounts-v2 account {
      'balance: b,
      'guard: g,
      'metadata: {}
    })))
```

### 7. Formal Verification

**Add Properties and Invariants**
```pact
(module verified-token 'admin-keyset
  (property conservation
    (= (fold (+) 0 (map (at 'balance) (select accounts)))
       TOTAL_SUPPLY))
  
  (property positive-balances
    (all (lambda (acc) (>= (at 'balance acc) 0.0))
         (select accounts)))
)
```

## Conclusion

Pact provides a robust, safe environment for smart contract development with built-in security features, formal verification support, and clear semantics. By following these patterns and best practices, developers can create secure, efficient, and maintainable blockchain applications.

For more information and updates, visit the [official Pact documentation](https://docs.kadena.io/pact) and [GitHub repository](https://github.com/kadena-io/pact).
