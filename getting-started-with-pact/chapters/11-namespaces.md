# Chapter 11: Namespaces and Module Organization

## Introduction

Namespaces in Pact provide a hierarchical organization system for modules, similar to packages in other programming languages. They enable code organization, prevent naming conflicts, and provide governance mechanisms for managing large-scale smart contract ecosystems.

## Understanding Namespaces

### What is a Namespace?

A namespace is a named container that groups related modules together. It provides:
- **Hierarchical organization** - logical grouping of related functionality
- **Name collision prevention** - multiple modules can have the same name in different namespaces
- **Access control** - governance over who can use and modify the namespace
- **Versioning support** - multiple versions of modules can coexist

```pact
;; Define namespace with governance
(define-namespace 'my-defi-protocol 
  (read-keyset 'protocol-users)    ;; User guard: who can use namespace
  (read-keyset 'protocol-admin))   ;; Admin guard: who can modify namespace

;; Set working namespace
(namespace 'my-defi-protocol)

;; Define module in namespace
(module lending-pool GOVERNANCE
  ;; Module implementation
  )

;; Reference: my-defi-protocol.lending-pool
```

### Namespace Architecture

Based on the Haskell implementation in `/pact/Pact/Core/Namespace.hs`, namespaces are structured as:

```haskell
data Namespace = Namespace
  { _nsName :: NamespaceName           -- Namespace identifier
  , _nsUser :: Guard QualifiedName PactValue    -- Usage control
  , _nsAdmin :: Guard QualifiedName PactValue   -- Administration control
  }
```

## Creating and Managing Namespaces

### Basic Namespace Definition

```pact
;; setup-namespace.pact
;; Define keyset data
(env-data {
  "namespace-admin": ["admin-key-1", "admin-key-2"],
  "namespace-users": ["user-key-1", "user-key-2", "admin-key-1"]
})

;; Create keysets
(define-keyset 'namespace-admin (read-keyset "namespace-admin"))
(define-keyset 'namespace-users (read-keyset "namespace-users"))

;; Define namespace
(define-namespace 'my-protocol
  (keyset-ref-guard 'namespace-users)   ;; Users can deploy/call modules
  (keyset-ref-guard 'namespace-admin))  ;; Admins can modify namespace

;; Set working namespace for subsequent modules
(namespace 'my-protocol)
```

### Namespace with Capability Guards

```pact
(module namespace-governance GOVERNANCE
  @doc "Advanced namespace governance using capabilities"
  
  (defcap GOVERNANCE () 
    (enforce-keyset 'protocol-admin))
  
  (defcap NAMESPACE_USER:bool (user:string operation:string)
    @doc "Control namespace usage"
    (with-read user-permissions user { "level" := level }
      (enforce (>= level 1) "Insufficient permission level")))
  
  (defcap NAMESPACE_ADMIN:bool (admin:string operation:string)
    @doc "Control namespace administration"
    (with-read admin-permissions admin { "is-admin" := is-admin }
      (enforce is-admin "Not an administrator")))
  
  ;; Define namespace with capability guards
  (define-namespace 'advanced-protocol
    (create-capability-guard (NAMESPACE_USER (tx-sender) "use"))
    (create-capability-guard (NAMESPACE_ADMIN (tx-sender) "admin")))
)
```

### Multi-Level Namespace Hierarchy

```pact
;; Root protocol namespace
(define-namespace 'kadena-defi
  (keyset-ref-guard 'defi-users)
  (keyset-ref-guard 'defi-admin))

;; Sub-protocol namespaces (conceptual - Pact uses flat namespace structure)
(define-namespace 'kadena-defi-lending
  (keyset-ref-guard 'lending-users)
  (keyset-ref-guard 'lending-admin))

(define-namespace 'kadena-defi-dex
  (keyset-ref-guard 'dex-users)
  (keyset-ref-guard 'dex-admin))
```

## Module Organization Patterns

### 1. Domain-Driven Organization

```pact
;; Financial Protocol Namespace
(namespace 'financial-protocol)

;; Core infrastructure modules
(module utils GOVERNANCE
  @doc "Common utilities and helpers"
  
  (defun format-decimal:string (amount:decimal precision:integer)
    @doc "Format decimal with specified precision"
    (format "{}" [(round amount precision)]))
  
  (defun validate-amount:bool (amount:decimal)
    @doc "Validate amount is positive and within bounds"
    (and (> amount 0.0) (< amount 1000000000.0)))
)

;; Token management
(module token-registry GOVERNANCE
  @doc "Registry for protocol tokens"
  
  (defschema token-info
    symbol:string
    name:string
    decimals:integer
    contract:module{fungible-v2})
    
  (deftable tokens:{token-info})
  
  (defun register-token:string (symbol:string name:string decimals:integer contract:module{fungible-v2})
    @doc "Register a new token in the protocol"
    (with-capability (GOVERNANCE)
      (insert tokens symbol {
        "symbol": symbol,
        "name": name,
        "decimals": decimals,
        "contract": contract
      })))
)

;; Core protocol logic
(module lending GOVERNANCE
  @doc "Lending protocol implementation"
  
  (use utils)         ;; Use utilities from same namespace
  (use token-registry) ;; Use token registry
  
  ;; Implementation using imported modules
  (defun calculate-interest:decimal (principal:decimal rate:decimal time:decimal)
    @doc "Calculate compound interest"
    (let ((formatted (format-decimal (* principal (^ (+ 1.0 rate) time)) 6)))
      (str-to-decimal formatted)))
)
```

### 2. Layer-Based Organization

```pact
;; Infrastructure layer
(namespace 'protocol-infra)

(module math GOVERNANCE
  @doc "Mathematical operations"
  
  (defun safe-divide:decimal (a:decimal b:decimal)
    @doc "Safe division with zero check"
    (enforce (!= b 0.0) "Division by zero")
    (/ a b))
  
  (defun calculate-percentage:decimal (value:decimal percentage:decimal)
    @doc "Calculate percentage of value"
    (* value (/ percentage 100.0)))
)

(module events GOVERNANCE
  @doc "Event logging and monitoring"
  
  (defschema event-log
    event-type:string
    data:object
    timestamp:time
    block-height:integer)
    
  (deftable event-logs:{event-log})
  
  (defun log-event:string (event-type:string data:object)
    @doc "Log protocol event"
    (insert event-logs (hash [event-type (at 'block-time (chain-data))]) {
      "event-type": event-type,
      "data": data,
      "timestamp": (at 'block-time (chain-data)),
      "block-height": (chain-data 'block-height)
    }))
)

;; Business logic layer
(namespace 'protocol-core)

(module lending-engine GOVERNANCE
  @doc "Core lending functionality"
  
  ;; Import from infrastructure layer
  (use protocol-infra.math)
  (use protocol-infra.events)
  
  ;; Business logic using infrastructure
  (defun process-loan:string (borrower:string amount:decimal rate:decimal)
    @doc "Process loan application"
    (let ((interest (calculate-percentage amount rate)))
      (log-event "LOAN_PROCESSED" {
        "borrower": borrower,
        "amount": amount,
        "interest": interest
      })
      (format "Loan processed: {} at {}% interest" [amount rate])))
)
```

### 3. Feature-Based Organization

```pact
;; Trading feature namespace
(namespace 'trading-features)

(module order-book GOVERNANCE
  @doc "Order book management"
  
  (defschema order
    id:string
    trader:string
    type:string
    amount:decimal
    price:decimal
    created:time)
    
  (deftable orders:{order})
  
  (defun place-order:string (id:string trader:string type:string amount:decimal price:decimal)
    @doc "Place trading order"
    (insert orders id {
      "id": id,
      "trader": trader,
      "type": type,
      "amount": amount,
      "price": price,
      "created": (at 'block-time (chain-data))
    }))
)

(module matching-engine GOVERNANCE
  @doc "Order matching logic"
  
  (use order-book)  ;; Use order book from same namespace
  
  (defun match-orders:string (buy-order-id:string sell-order-id:string)
    @doc "Match buy and sell orders"
    ;; Implementation using order-book functions
    "Orders matched successfully")
)

;; Analytics feature namespace  
(namespace 'analytics-features)

(module price-oracle GOVERNANCE
  @doc "Price data aggregation"
  
  (defschema price-data
    asset:string
    price:decimal
    timestamp:time
    source:string)
    
  (deftable prices:{price-data})
  
  (defun update-price:string (asset:string price:decimal source:string)
    @doc "Update asset price"
    (write prices asset {
      "asset": asset,
      "price": price,
      "timestamp": (at 'block-time (chain-data)),
      "source": source
    }))
)
```

## Cross-Namespace References

### Direct Module References

```pact
;; Reference modules from other namespaces
(namespace 'my-application)

(module trading-app GOVERNANCE
  @doc "Trading application using cross-namespace modules"
  
  ;; Import from different namespaces
  (use protocol-infra.math)              ;; Utility functions
  (use trading-features.order-book)      ;; Order management
  (use analytics-features.price-oracle)  ;; Price data
  
  (defun create-market-order:string (trader:string asset:string amount:decimal)
    @doc "Create market order at current price"
    (let ((current-price (at 'price (read prices asset))))
      ;; Use cross-namespace functions
      (place-order (hash [trader asset amount]) trader "MARKET" amount current-price)))
)
```

### Qualified Function Calls

```pact
;; Explicit qualified calls
(defun complex-calculation:decimal (base:decimal rate:decimal)
  @doc "Complex calculation using multiple namespace functions"
  
  ;; Fully qualified function calls
  (let ((safe-rate (protocol-infra.math.safe-divide rate 100.0))
        (percentage (protocol-infra.math.calculate-percentage base safe-rate)))
    
    ;; Log the operation
    (protocol-infra.events.log-event "CALCULATION" {
      "base": base,
      "rate": rate,
      "result": percentage
    })
    
    percentage))
```

### Selective Imports

```pact
(module optimized-module GOVERNANCE
  @doc "Module with selective imports for gas efficiency"
  
  ;; Import only specific functions
  (use protocol-infra.math [safe-divide calculate-percentage])
  (use protocol-infra.events [log-event])
  
  ;; Functions available without qualification
  (defun calculate:decimal (a:decimal b:decimal)
    (safe-divide (calculate-percentage a 50.0) b))
)
```

## Namespace Governance Patterns

### 1. Hierarchical Governance

```pact
(module namespace-hierarchy GOVERNANCE
  @doc "Hierarchical namespace governance"
  
  (defschema namespace-role
    namespace:string
    role:string
    permissions:[string])
    
  (deftable namespace-roles:{namespace-role})
  
  (defcap ROOT_ADMIN:bool ()
    @doc "Root administrator capability"
    (enforce-keyset 'root-admin))
  
  (defcap NAMESPACE_OWNER:bool (namespace:string)
    @doc "Namespace owner capability"
    (with-read namespace-roles (format "{}:owner" [namespace])
      { "permissions" := perms }
      (enforce (contains "ADMIN" perms) "Not namespace owner")))
  
  (defun create-child-namespace:string (parent:string child:string)
    @doc "Create child namespace under parent governance"
    (with-capability (NAMESPACE_OWNER parent)
      ;; Create child namespace with parent's governance
      (define-namespace (format "{}.{}" [parent child])
        (create-capability-guard (NAMESPACE_OWNER parent))
        (create-capability-guard (NAMESPACE_OWNER parent)))))
)
```

### 2. Democratic Governance

```pact
(module democratic-namespace GOVERNANCE
  @doc "Democratic namespace governance"
  
  (defschema proposal
    id:string
    description:string
    proposer:string
    votes-for:integer
    votes-against:integer
    deadline:time
    executed:bool)
    
  (deftable proposals:{proposal})
  
  (defschema voter
    address:string
    voting-power:integer)
    
  (deftable voters:{voter})
  
  (defcap VOTE:bool (voter:string proposal-id:string)
    @doc "Voting capability"
    (with-read voters voter { "voting-power" := power }
      (enforce (> power 0) "No voting power")))
  
  (defun propose-namespace-change:string (proposal-id:string description:string)
    @doc "Propose namespace governance change"
    (insert proposals proposal-id {
      "id": proposal-id,
      "description": description,
      "proposer": (tx-sender),
      "votes-for": 0,
      "votes-against": 0,
      "deadline": (add-time (at 'block-time (chain-data)) (days 7)),
      "executed": false
    }))
  
  (defun vote:string (proposal-id:string support:bool)
    @doc "Vote on proposal"
    (with-capability (VOTE (tx-sender) proposal-id)
      (with-read voters (tx-sender) { "voting-power" := power }
        (with-read proposals proposal-id {
          "votes-for" := for,
          "votes-against" := against
        }
          (if support
              (update proposals proposal-id { "votes-for": (+ for power) })
              (update proposals proposal-id { "votes-against": (+ against power) }))))))
)
```

### 3. Time-Locked Governance

```pact
(module timelock-governance GOVERNANCE
  @doc "Time-locked governance for namespace changes"
  
  (defschema timelock-proposal
    id:string
    target-namespace:string
    action:string
    params:object
    proposer:string
    created:time
    execution-time:time
    executed:bool)
    
  (deftable timelock-proposals:{timelock-proposal})
  
  (defconst TIMELOCK_DELAY:integer 172800)  ;; 48 hours
  
  (defun propose-with-timelock:string (proposal-id:string target-ns:string action:string params:object)
    @doc "Propose namespace change with timelock"
    (let ((execution-time (add-time (at 'block-time (chain-data)) (seconds TIMELOCK_DELAY))))
      (insert timelock-proposals proposal-id {
        "id": proposal-id,
        "target-namespace": target-ns,
        "action": action,
        "params": params,
        "proposer": (tx-sender),
        "created": (at 'block-time (chain-data)),
        "execution-time": execution-time,
        "executed": false
      })
      (format "Proposal {} scheduled for execution at {}" [proposal-id execution-time])))
  
  (defun execute-timelock:string (proposal-id:string)
    @doc "Execute time-locked proposal"
    (with-read timelock-proposals proposal-id {
      "execution-time" := exec-time,
      "executed" := is-executed,
      "action" := action,
      "params" := params
    }
      (enforce (not is-executed) "Already executed")
      (enforce (>= (at 'block-time (chain-data)) exec-time) "Timelock not expired")
      
      ;; Execute the governance action
      (execute-governance-action action params)
      (update timelock-proposals proposal-id { "executed": true })))
)
```

## Namespace Migration and Versioning

### Version Management

```pact
(module version-manager GOVERNANCE
  @doc "Namespace version management"
  
  (defschema version-info
    namespace:string
    version:string
    modules:[string]
    created:time
    deprecated:bool)
    
  (deftable versions:{version-info})
  
  (defun register-version:string (namespace:string version:string modules:[string])
    @doc "Register namespace version"
    (insert versions (format "{}:{}" [namespace version]) {
      "namespace": namespace,
      "version": version,
      "modules": modules,
      "created": (at 'block-time (chain-data)),
      "deprecated": false
    }))
  
  (defun deprecate-version:string (namespace:string version:string)
    @doc "Mark version as deprecated"
    (with-capability (GOVERNANCE)
      (update versions (format "{}:{}" [namespace version]) {
        "deprecated": true
      })))
  
  (defun get-latest-version:string (namespace:string)
    @doc "Get latest non-deprecated version"
    (let ((all-versions (select versions 
                                (and? (where 'namespace (= namespace))
                                      (where 'deprecated (= false))))))
      (at 'version (last (sort ['created] all-versions)))))
)
```

### Migration Strategies

```pact
(module namespace-migration GOVERNANCE
  @doc "Namespace migration utilities"
  
  (defschema migration
    from-namespace:string
    to-namespace:string
    mapping:object
    completed:bool)
    
  (deftable migrations:{migration})
  
  (defun create-migration:string (migration-id:string from-ns:string to-ns:string mapping:object)
    @doc "Create namespace migration plan"
    (insert migrations migration-id {
      "from-namespace": from-ns,
      "to-namespace": to-ns,
      "mapping": mapping,
      "completed": false
    }))
  
  (defun migrate-reference:string (old-ref:string migration-id:string)
    @doc "Migrate old namespace reference to new"
    (with-read migrations migration-id { "mapping" := map }
      (at old-ref map)))
)
```

## Testing Namespace Organization

### Namespace Testing Strategy

```pact
;; test-namespaces.repl
(begin-tx "Setup test namespaces")

;; Setup governance data
(env-data {
  "test-admin": ["test-admin-key"],
  "test-users": ["user1", "user2", "test-admin-key"]
})

;; Create test namespaces
(define-keyset 'test-admin (read-keyset "test-admin"))
(define-keyset 'test-users (read-keyset "test-users"))

(define-namespace 'test-protocol
  (keyset-ref-guard 'test-users)
  (keyset-ref-guard 'test-admin))

(define-namespace 'test-app
  (keyset-ref-guard 'test-users)
  (keyset-ref-guard 'test-admin))

(commit-tx)

(begin-tx "Test cross-namespace functionality")

;; Define modules in different namespaces
(namespace 'test-protocol)
(module protocol-core GOVERNANCE
  (defcap GOVERNANCE () (enforce-keyset 'test-admin))
  
  (defun core-function:string (x:string)
    (format "Protocol: {}" [x]))
)

(namespace 'test-app)
(module app-logic GOVERNANCE
  (defcap GOVERNANCE () (enforce-keyset 'test-admin))
  
  (use test-protocol.protocol-core)
  
  (defun app-function:string (x:string)
    (format "App using: {}" [(core-function x)]))
)

;; Test cross-namespace calls
(expect "Cross-namespace call works"
  "App using: Protocol: test"
  (app-function "test"))

(commit-tx)
```

### Property Testing for Namespaces

```pact
(defun test-namespace-properties ()
  @doc "Test namespace system properties"
  
  ;; Property 1: Namespace isolation
  (namespace 'isolated-test-1)
  (module test-mod GOVERNANCE
    (defcap GOVERNANCE () true)
    (defconst VALUE "test1"))
  
  (namespace 'isolated-test-2)
  (module test-mod GOVERNANCE  ;; Same name, different namespace
    (defcap GOVERNANCE () true)
    (defconst VALUE "test2"))
  
  ;; Values should be isolated
  (expect "Namespace isolation works"
    "test1"
    isolated-test-1.test-mod.VALUE)
  
  (expect "Namespace isolation works"
    "test2"
    isolated-test-2.test-mod.VALUE)
  
  ;; Property 2: Qualified names resolve correctly
  (expect "Qualified resolution works"
    true
    (!= isolated-test-1.test-mod.VALUE isolated-test-2.test-mod.VALUE))
)
```

## Performance and Gas Optimization

### Namespace Resolution Costs

```pact
;; Optimize namespace usage for gas efficiency
(module gas-efficient GOVERNANCE
  @doc "Gas-efficient namespace usage patterns"
  
  ;; GOOD: Cache cross-namespace references
  (defun optimized-cross-call:[decimal] (values:[decimal])
    (let ((math-functions protocol-infra.math))  ;; Cache namespace reference
      (map (math-functions.safe-divide 100.0) values)))
  
  ;; BAD: Repeated qualified calls
  (defun inefficient-cross-call:[decimal] (values:[decimal])
    (map (lambda (v) (protocol-infra.math.safe-divide v 100.0)) values))
  
  ;; GOOD: Selective imports reduce resolution cost
  (use protocol-infra.math [safe-divide])
  
  (defun efficient-import-usage:[decimal] (values:[decimal])
    (map (safe-divide 100.0) values))
)
```

### Namespace Organization for Performance

```pact
;; Organize frequently-used utilities in root namespace
(namespace 'utils)
(module common GOVERNANCE
  @doc "Commonly used utilities in root namespace"
  ;; High-frequency functions here for faster resolution
)

;; Organize domain-specific modules in sub-namespaces  
(namespace 'defi.lending)
(module pools GOVERNANCE
  @doc "Lending pools - domain-specific functionality"
  ;; Less frequently called functions
)
```

## Summary

Namespaces in Pact provide essential organization capabilities:

- **Hierarchical Organization**: Logical grouping of related modules
- **Name Collision Prevention**: Multiple modules with same names in different namespaces
- **Access Control**: User and admin guards for governance
- **Cross-Namespace References**: Qualified names and selective imports
- **Versioning Support**: Multiple versions can coexist
- **Performance Optimization**: Efficient name resolution and caching

Key principles:
1. **Use namespaces for logical organization** - group related functionality
2. **Implement proper governance** - user and admin guards for access control
3. **Optimize cross-namespace calls** - cache references and use selective imports
4. **Plan for versioning** - design migration strategies early
5. **Test namespace isolation** - ensure proper separation of concerns

Namespaces enable building large-scale, well-organized smart contract ecosystems while maintaining security, performance, and governance.

## Exercises

1. Design a namespace hierarchy for a DeFi protocol with lending, DEX, and governance components
2. Implement a democratic governance system for namespace administration
3. Create a migration strategy for upgrading namespace organization
4. Build a cross-namespace module dependency analyzer
5. Design a namespace-based permission system with role hierarchies

## References

- Namespace types: `/pact/Pact/Core/Namespace.hs`
- Name resolution: `/pact/Pact/Core/Persistence/Utils.hs` 
- Module names: `/pact/Pact/Core/Names.hs`
- Namespace builtins: `/pact/Pact/Core/IR/Eval/CEK/CoreBuiltin.hs`