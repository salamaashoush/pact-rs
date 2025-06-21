# Chapter 7: Database and Tables - Persistent State Management

## Introduction

Pact provides a built-in database abstraction that makes blockchain state management straightforward and secure. Unlike traditional smart contract platforms that require manual storage slot management, Pact offers high-level database operations with automatic schema validation, row-level security, and gas-metered access.

## Understanding Pact's Database Model

### Key-Value with Schema

Pact's database is fundamentally key-value storage enhanced with:
- **Schemas**: Structured data validation
- **Tables**: Namespaced collections
- **Guards**: Row-level security
- **Types**: Compile-time and runtime validation

```pact
;; Define the structure
(defschema user
  name:string
  balance:decimal
  active:bool
  last-login:time)

;; Create the table
(deftable users:{user})
```

### Database Domains

Pact organizes data into separate domains:

1. **User Tables** - Your application data
2. **System Keysets** - Authorization data  
3. **Modules** - Smart contract code
4. **Namespaces** - Organizational data
5. **DefPacts** - Multi-step transaction state

## Schema Definition

### Basic Schemas

```pact
(defschema account
  @doc "Bank account schema"
  balance:decimal
  guard:guard
  created:time)

(defschema token-metadata
  @doc "Token information"
  name:string
  symbol:string
  decimals:integer
  total-supply:decimal)
```

### Complex Schemas

```pact
(defschema trade-order
  @doc "DEX order book entry"
  order-id:string
  trader:string
  side:string              ;; "buy" or "sell"
  token-pair:string        ;; "KDA/USDC"
  amount:decimal
  price:decimal
  filled:decimal
  status:string            ;; "open", "filled", "cancelled"
  created:time
  expires:time
  metadata:object{order-meta})  ;; Nested schema

(defschema order-meta
  @doc "Order metadata"
  source:string           ;; "web", "api", "bot"
  slippage:decimal
  referrer:string)
```

### Schema Validation

Pact validates schemas at multiple levels:

```pact
;; Compile-time validation
(defschema user
  name:string
  balance:decimal)  ;; Types checked against builtin types

;; Runtime validation  
(insert users "alice" {
  "name": "Alice Smith",     ;; Must be string
  "balance": 1000.0          ;; Must be decimal
  ;; "active": true         ;; ERROR: Missing required field
})
```

## Table Creation and Management

### Creating Tables

```pact
(module my-app 'admin-keyset
  (defschema user
    name:string
    balance:decimal)
  
  ;; Table definition with schema
  (deftable users:{user})
  
  ;; Capability for table operations
  (defcap USER_ADMIN ()
    (enforce-keyset 'admin-keyset))
)

;; Create the actual table (after module is loaded)
(create-table users)
```

### Table Ownership and Security

Tables belong to modules and are protected by module governance:

```pact
;; Only the owning module can create the table
(create-table users)  ;; Must be called from my-app module context

;; Table operations require module capabilities
(defun create-user (id:string name:string)
  (with-capability (USER_ADMIN)  ;; Module-controlled capability
    (insert users id {
      "name": name,
      "balance": 0.0
    })))
```

## Basic Database Operations

### Insert - Create New Records

```pact
(defun create-account (id:string initial-balance:decimal owner-guard:guard)
  @doc "Create a new account"
  (insert accounts id {
    "balance": initial-balance,
    "guard": owner-guard,
    "created": (time)
  }))

;; Usage
(create-account "alice" 1000.0 (read-keyset "alice-keyset"))
```

**Insert Requirements:**
- Key must not exist (fails if exists)
- All schema fields must be provided
- Types must match schema exactly

### Write - Upsert Records

```pact
(defun save-user-preferences (user-id:string preferences:object)
  @doc "Save or update user preferences"
  (write user-prefs user-id {
    "user-id": user-id,
    "theme": (at "theme" preferences),
    "language": (at "language" preferences),
    "notifications": (at "notifications" preferences),
    "updated": (time)
  }))
```

**Write Characteristics:**
- Creates if doesn't exist, updates if exists
- All schema fields must be provided
- Atomic operation

### Update - Modify Existing Records

```pact
(defun update-balance (account:string new-balance:decimal)
  @doc "Update account balance"
  (update accounts account {
    "balance": new-balance
  }))

;; Partial update with computed fields
(defun mark-login (user:string)
  (update users user {
    "last-login": (time),
    "login-count": (+ 1 (at 'login-count (read users user)))
  }))
```

**Update Features:**
- Key must exist (fails if missing)
- Partial updates allowed
- Only specified fields are changed

### Read - Retrieve Records

```pact
;; Full record read
(read accounts "alice")
;; Returns: {"balance": 1000.0, "guard": {...}, "created": "2023-01-01T..."}

;; Selective field read
(read accounts "alice" ["balance"])
;; Returns: {"balance": 1000.0}

;; Multiple fields
(read accounts "alice" ["balance", "created"])
;; Returns: {"balance": 1000.0, "created": "2023-01-01T..."}
```

## Advanced Query Operations

### Select - Filter Records

```pact
;; Simple filter
(select accounts (where 'balance (< 100.0)))
;; Returns all accounts with balance < 100

;; Multiple conditions
(select accounts 
  (and? (where 'balance (< 1000.0))
        (where 'active (= true))))

;; With field projection
(select accounts ["name", "balance"] 
  (where 'balance (> 500.0)))

;; Complex filters
(defun find-vip-users ()
  (select users
    (and? (where 'balance (> 10000.0))
          (where 'last-login (> (add-time (time) (days -30)))))))
```

### Keys - List All Keys

```pact
;; Get all account IDs
(keys accounts)
;; Returns: ["alice", "bob", "charlie"]

;; Use with other operations
(defun count-accounts ()
  (length (keys accounts)))

(defun get-all-balances ()
  (map (lambda (k) 
    { "account": k, "balance": (at 'balance (read accounts k)) })
       (keys accounts)))
```

### Fold-DB - Aggregate Operations

```pact
;; Sum all balances
(fold-db accounts
  (lambda (key record total)
    (+ total (at 'balance record)))
  0.0)

;; Find maximum balance
(fold-db accounts
  (lambda (key record max-bal)
    (let ((balance (at 'balance record)))
      (if (> balance max-bal) balance max-bal)))
  0.0)

;; Collect active users
(fold-db users
  (lambda (key record active-list)
    (if (at 'active record)
        (+ active-list [key])
        active-list))
  [])
```

## Advanced Patterns

### With-Read Pattern

The `with-read` pattern combines reading and processing atomically:

```pact
(defun transfer (from:string to:string amount:decimal)
  @doc "Transfer funds between accounts"
  
  ;; Atomic read and validation
  (with-read accounts from 
    { "balance" := from-balance, "guard" := from-guard }
    
    ;; Authorize the sender
    (enforce-guard from-guard)
    
    ;; Validate sufficient funds
    (enforce (>= from-balance amount) "Insufficient funds")
    
    ;; Update sender balance
    (update accounts from { "balance": (- from-balance amount) })
    
    ;; Update receiver balance (with default read)
    (with-default-read accounts to
      { "balance": 0.0, "guard": (keyset-ref-guard 'null) }
      { "balance" := to-balance }
      (write accounts to { 
        "balance": (+ to-balance amount),
        "guard": (keyset-ref-guard 'default-guard)
      }))))
```

### With-Default-Read Pattern

Gracefully handle missing records:

```pact
(defun get-user-score (user:string)
  @doc "Get user score with default"
  (with-default-read user-scores user
    { "score": 0, "level": 1 }    ;; Default values
    { "score" := score }
    score))

(defun increment-counter (counter-name:string)
  (with-default-read counters counter-name
    { "value": 0 }
    { "value" := current }
    (write counters counter-name { "value": (+ current 1) })))
```

## Row-Level Security

### Guard-Protected Tables

```pact
(defschema protected-account
  balance:decimal
  guard:guard        ;; Each row has its own security
  metadata:object)

(deftable protected-accounts:{protected-account})

(defun create-protected-account (id:string guard:guard)
  (insert protected-accounts id {
    "balance": 0.0,
    "guard": guard,
    "metadata": {}
  }))

(defun secure-update (account:string new-balance:decimal)
  @doc "Update can only be done by account owner"
  (with-read protected-accounts account 
    { "guard" := account-guard }
    ;; Enforce row-level security
    (enforce-guard account-guard)
    (update protected-accounts account { "balance": new-balance })))
```

### Multi-Level Security

```pact
(module secure-vault 'vault-admin
  
  (defcap VAULT_ADMIN ()
    (enforce-keyset 'vault-admin))
  
  (defcap ACCOUNT_ACCESS (account:string)
    "Access specific account"
    (with-read vault-accounts account 
      { "guard" := account-guard }
      (enforce-guard account-guard)))
  
  (defun admin-transfer (from:string to:string amount:decimal)
    @doc "Admin can transfer between any accounts"
    (with-capability (VAULT_ADMIN)
      (transfer-internal from to amount)))
  
  (defun user-transfer (from:string to:string amount:decimal)
    @doc "Users can only transfer from accounts they control"
    (with-capability (ACCOUNT_ACCESS from)
      (transfer-internal from to amount)))
)
```

## Transaction Patterns

### Atomic Operations

```pact
(defun atomic-swap (user1:string user2:string item1:string item2:string)
  @doc "Atomically swap items between users"
  
  ;; All operations succeed or all fail
  (with-read inventories user1 { "items" := items1 }
    (with-read inventories user2 { "items" := items2 }
      
      ;; Validate ownership
      (enforce (contains item1 items1) "User1 doesn't own item1")
      (enforce (contains item2 items2) "User2 doesn't own item2")
      
      ;; Perform swap
      (update inventories user1 { 
        "items": (+ (remove item1 items1) [item2]) 
      })
      (update inventories user2 { 
        "items": (+ (remove item2 items2) [item1]) 
      }))))
```

### Batch Operations

```pact
(defun batch-create-accounts (account-data:[object])
  @doc "Create multiple accounts in one transaction"
  (map (create-single-account) account-data))

(defun create-single-account (account-info:object)
  (let ((id (at "id" account-info))
        (balance (at "balance" account-info))
        (guard (at "guard" account-info)))
    (insert accounts id {
      "balance": balance,
      "guard": guard,
      "created": (time)
    })))
```

## Database Performance and Gas Optimization

### Gas Costs

Database operations have significant gas costs:

| Operation | Base Cost | Variable Cost |
|-----------|-----------|---------------|
| `create-table` | 250,000 | - |
| `read` | 2,500 | +100 per byte |
| `write`/`insert`/`update` | 25,000 | +200 per byte |
| `select` | 2,500 | +per row scanned |
| `keys` | 2,500 | +per key |

### Optimization Strategies

#### 1. Minimize Data Size

```pact
;; BAD: Large objects increase gas costs
(defschema inefficient
  huge-text:string         ;; Large strings are expensive
  redundant-data:object)   ;; Nested objects add overhead

;; GOOD: Lean schema design
(defschema efficient
  id:string               ;; Only essential data
  amount:decimal
  active:bool)
```

#### 2. Use Field Projection

```pact
;; BAD: Read entire record when only need balance
(let ((balance (at 'balance (read accounts "alice"))))
  balance)

;; GOOD: Read only needed fields
(at 'balance (read accounts "alice" ["balance"]))
```

#### 3. Batch Operations

```pact
;; BAD: Multiple separate reads
(defun sum-balances (accounts:[string])
  (fold (+) 0.0 
    (map (lambda (acc) (at 'balance (read accounts acc))) accounts)))

;; GOOD: Use fold-db for aggregation
(defun total-balance ()
  (fold-db accounts
    (lambda (k r total) (+ total (at 'balance r)))
    0.0))
```

## Database Testing

### REPL Testing

```pact
;; test-database.repl
(begin-tx)
(env-data { "admin-keyset": { "keys": ["admin"], "pred": "keys-all" }})
(env-keys ["admin"])

(load "my-module.pact")
(create-table users)

;; Test basic operations
(expect "Insert succeeds"
  "Write succeeded"
  (my-module.create-user "alice" "Alice Smith"))

(expect "Read returns correct data"
  { "name": "Alice Smith", "balance": 0.0 }
  (read users "alice"))

;; Test error cases
(expect-failure "Duplicate insert fails"
  "row found"
  (my-module.create-user "alice" "Alice Again"))

(commit-tx)
```

### Property Testing

```pact
;; Test invariants
(defun test-balance-conservation ()
  "Total balance should remain constant during transfers"
  (let ((total-before (total-balance)))
    (transfer "alice" "bob" 100.0)
    (let ((total-after (total-balance)))
      (enforce (= total-before total-after) "Balance not conserved"))))
```

## Advanced Database Patterns

### Audit Trail Pattern

```pact
(defschema audit-log
  table-name:string
  row-key:string
  operation:string
  old-value:object
  new-value:object
  timestamp:time
  user:string)

(deftable audit-logs:{audit-log})

(defun audit-update (table:string key:string old-val:object new-val:object op:string)
  (insert audit-logs (hash [table key (time)]) {
    "table-name": table,
    "row-key": key,
    "operation": op,
    "old-value": old-val,
    "new-value": new-val,
    "timestamp": (time),
    "user": (tx-sender)
  }))

(defun audited-update (account:string new-balance:decimal)
  (let ((old-record (read accounts account)))
    (update accounts account { "balance": new-balance })
    (audit-update "accounts" account old-record 
      { "balance": new-balance } "update")))
```

### Versioning Pattern

```pact
(defschema versioned-record
  data:object
  version:integer
  updated:time
  updated-by:string)

(defun versioned-write (table:string key:string new-data:object)
  (with-default-read table key
    { "version": 0 }
    { "version" := current-version }
    (write table key {
      "data": new-data,
      "version": (+ current-version 1),
      "updated": (time),
      "updated-by": (tx-sender)
    })))
```

### Soft Delete Pattern

```pact
(defschema soft-deletable
  data:object
  active:bool
  deleted-at:time)

(defun soft-delete (table:string key:string)
  (update table key {
    "active": false,
    "deleted-at": (time)
  }))

(defun active-records (table:string)
  (select table (where 'active (= true))))
```

## Database Backends

### SQLite Backend (Production)

```pact
;; Configuration for production
{
  "database": {
    "backend": "sqlite",
    "file": "/var/pact/db.sqlite",
    "pragmas": {
      "synchronous": "NORMAL",
      "journal_mode": "WAL"
    }
  }
}
```

### Mock Backend (Testing)

```pact
;; In-memory for tests
{
  "database": {
    "backend": "mock",
    "persist": false
  }
}
```

## Error Handling

### Common Database Errors

```pact
;; Handle missing records gracefully
(defun safe-read (table:string key:string)
  (try 
    (read table key)
    {}))  ;; Return empty object if not found

;; Validate before operations
(defun safe-update (account:string new-balance:decimal)
  (if (> (length (keys accounts)) 0)
    (if (contains account (keys accounts))
      (update accounts account { "balance": new-balance })
      (error "Account not found"))
    (error "No accounts exist")))
```

## Migration Strategies

### Schema Evolution

```pact
;; Version 1 schema
(defschema user-v1
  name:string
  balance:decimal)

;; Version 2 schema (with new field)
(defschema user-v2
  name:string
  balance:decimal
  email:string)      ;; New field

;; Migration function
(defun migrate-user-v1-to-v2 (user-id:string default-email:string)
  (with-read users-v1 user-id { "name" := name, "balance" := balance }
    (insert users-v2 user-id {
      "name": name,
      "balance": balance,
      "email": default-email
    })))
```

## Summary

Pact's database system provides:

- **High-level abstraction** - No manual storage management
- **Schema validation** - Type safety at runtime
- **Row-level security** - Guard-based access control
- **Gas metering** - Predictable costs
- **ACID transactions** - Reliable state changes
- **Rich query support** - Filter, aggregate, and project operations

Key principles:
1. **Design lean schemas** to minimize gas costs
2. **Use field projection** for efficient reads
3. **Implement row-level security** with guards
4. **Test thoroughly** with various data sizes
5. **Plan for schema evolution** from the start

The database system is one of Pact's strongest features, providing blockchain applications with familiar, reliable data management patterns while maintaining the security and determinism required for smart contracts.

## Exercises

1. Design a schema for a decentralized exchange order book
2. Implement an audit trail for all database modifications  
3. Create a soft-delete system with recovery functions
4. Build a migration system for schema evolution
5. Optimize a database-heavy function for gas usage

## References

- Database types: `/pact/Pact/Core/Persistence/Types.hs`
- Builtin operations: `/pact/Pact/Core/IR/Eval/CEK/CoreBuiltin.hs`
- SQLite backend: `/pact/Pact/Core/Persistence/SQLite.hs`
- Gas model: `/pact/Pact/Core/Gas/TableGasModel.hs`