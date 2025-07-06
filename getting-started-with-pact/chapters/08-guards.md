# Chapter 8: Guards - Unified Security Model

## Introduction

Guards in Pact provide a unified, composable security model that abstracts different authorization mechanisms behind a common interface. They represent the "who can do what" layer of Pact's security architecture, building upon keysets and integrating with capabilities to create flexible, auditable authorization patterns.

## Understanding Guards

### What is a Guard?

A guard is an authorization token that can be enforced to verify that specific conditions are met. Think of guards as "proof of authorization" that can be stored, passed around, and enforced when needed.

```pact
;; Different ways to create guards
(keyset-ref-guard 'my-keyset)              ;; Reference to stored keyset
(create-capability-guard (MY-CAP))         ;; Requires capability
(create-user-guard (my-auth-function))     ;; Custom authorization logic
```

### Guard Types Overview

Pact supports several types of guards:

1. **Keyset Guards** - Cryptographic signature verification
2. **Capability Guards** - Capability-based authorization  
3. **User Guards** - Custom authorization functions
4. **Module Guards** - Module administration (deprecated)
5. **DefPact Guards** - Multi-step transaction protection (deprecated)

## Keyset Guards

### Direct Keyset Guards

```pact
;; Inline keyset guard
(let ((admin-guard 
       { "keys": ["admin-key-1", "admin-key-2"], 
         "pred": "keys-all" }))
  (insert accounts "treasury" {
    "balance": 1000000.0,
    "guard": admin-guard
  }))
```

### Keyset Reference Guards

```pact
;; Reference to named keyset
(define-keyset 'treasury-admin {
  "keys": ["treasury-1", "treasury-2", "treasury-3"],
  "pred": "keys-2"
})

(defun create-treasury-account ()
  (insert accounts "treasury" {
    "balance": 0.0,
    "guard": (keyset-ref-guard 'treasury-admin)
  }))
```

### Custom Keyset Predicates

```pact
(defun business-hours-auth:bool (count:integer matched:integer)
  "Different signature requirements based on time"
  (let ((hour (at 'hour (time))))
    (if (and (>= hour 9) (<= hour 17))
        (>= matched 1)      ;; Business hours: any authorized key
        (>= matched 2))))   ;; After hours: require 2 keys

(define-keyset 'time-sensitive {
  "keys": ["manager", "assistant", "security"],
  "pred": "business-hours-auth"
})
```

## Capability Guards

Capability guards connect Pact's authorization system with its capability model, providing fine-grained, composable security.

### Basic Capability Guards

```pact
(module secure-vault 'admin
  
  (defcap VAULT_ACCESS (vault-id:string user:string)
    @doc "Access to specific vault"
    (with-read vault-permissions vault-id 
      { "authorized-users" := users }
      (enforce (contains user users) "User not authorized")))
  
  (defun create-vault-guard (vault-id:string user:string)
    @doc "Create guard requiring vault access capability"
    (create-capability-guard (VAULT_ACCESS vault-id user)))
  
  (defschema vault
    contents:object
    guard:guard)
  
  (deftable vaults:{vault})
  
  (defun create-secure-vault (vault-id:string user:string contents:object)
    (insert vaults vault-id {
      "contents": contents,
      "guard": (create-vault-guard vault-id user)
    }))
  
  (defun access-vault (vault-id:string user:string)
    (with-read vaults vault-id { "guard" := vault-guard }
      (enforce-guard vault-guard)
      (at 'contents (read vaults vault-id))))
)
```

### Capability Guard with Parameters

```pact
(defcap TRANSFER_LIMIT (from:string amount:decimal)
  @doc "Transfer with amount limit"
  @managed amount TRANSFER_LIMIT_mgr
  (enforce-guard (at 'guard (read accounts from))))

(defun TRANSFER_LIMIT_mgr:decimal (managed:decimal requested:decimal)
  (enforce (<= requested managed) "Transfer exceeds limit")
  (- managed requested))

(defun create-limited-transfer-guard (from:string max-amount:decimal)
  @doc "Guard that allows transfers up to a limit"
  (create-capability-guard (TRANSFER_LIMIT from max-amount)))

;; Usage in escrow or delegation
(defun delegate-transfer-authority (delegator:string delegatee:string limit:decimal)
  (let ((transfer-guard (create-limited-transfer-guard delegator limit)))
    (insert delegations (format "{}:{}" [delegator delegatee]) {
      "delegator": delegator,
      "delegatee": delegatee,
      "limit": limit,
      "guard": transfer-guard
    })))
```

## User Guards

User guards enable custom authorization logic through user-defined functions.

### Basic User Guard

```pact
(defun my-authorization:bool (user:string action:string)
  @doc "Custom authorization function"
  (let ((user-level (at 'level (read user-permissions user)))
        (required-level (at action permission-levels)))
    (>= user-level required-level)))

(defun create-role-based-guard (user:string action:string)
  @doc "Create guard with role-based authorization"
  (create-user-guard (my-authorization user action)))

;; Usage
(defschema protected-resource
  data:object
  guard:guard)

(deftable protected-resources:{protected-resource})

(defun create-protected-resource (id:string user:string action:string data:object)
  (insert protected-resources id {
    "data": data,
    "guard": (create-role-based-guard user action)
  }))
```

### Time-Based User Guard

```pact
(defun time-locked-auth:bool (unlock-time:time owner:string)
  @doc "Authorization that's time-locked"
  (and (>= (at 'block-time (chain-data)) unlock-time)
       (enforce-keyset owner)))

(defun create-time-locked-guard (unlock-time:time owner:string)
  @doc "Create time-locked guard"
  (create-user-guard (time-locked-auth unlock-time owner)))

;; Example: Vesting schedule
(defun create-vested-tokens (beneficiary:string amount:decimal vest-time:time)
  (insert vested-tokens beneficiary {
    "amount": amount,
    "vested": false,
    "guard": (create-time-locked-guard vest-time beneficiary)
  }))

(defun claim-vested-tokens (beneficiary:string)
  (with-read vested-tokens beneficiary 
    { "guard" := vest-guard, "amount" := amount }
    (enforce-guard vest-guard)  ;; Enforces time and ownership
    (update vested-tokens beneficiary { "vested": true })
    (credit-account beneficiary amount)))
```

### Multi-Factor User Guard

```pact
(defun multi-factor-auth:bool (user:string secret:string)
  @doc "Multi-factor authentication"
  (let ((stored-hash (at 'secret-hash (read user-secrets user)))
        (provided-hash (hash secret)))
    (and (= stored-hash provided-hash)
         (enforce-keyset (at 'keyset (read user-keysets user))))))

(defun create-mfa-guard (user:string secret:string)
  @doc "Create multi-factor authentication guard"
  (create-user-guard (multi-factor-auth user secret)))
```

## Guard Enforcement

### Direct Enforcement

```pact
(defun withdraw (account:string amount:decimal)
  @doc "Withdraw with guard enforcement"
  (with-read accounts account 
    { "guard" := account-guard, "balance" := balance }
    
    ;; Enforce the guard
    (enforce-guard account-guard)
    
    ;; Validate business logic
    (enforce (>= balance amount) "Insufficient balance")
    
    ;; Perform operation
    (update accounts account { "balance": (- balance amount) })))
```

### Conditional Guard Enforcement

```pact
(defun conditional-access (resource:string user:string emergency:bool)
  @doc "Access with emergency override"
  (with-read protected-resources resource 
    { "guard" := resource-guard }
    
    (if emergency
        (enforce-keyset 'emergency-admin)  ;; Emergency access
        (enforce-guard resource-guard))))  ;; Normal access
```

### Guard Composition

```pact
(defun multi-path-authorization (resource:string)
  @doc "Multiple authorization paths"
  (with-read protected-resources resource 
    { "owner-guard" := owner, "admin-guard" := admin }
    
    (enforce-one "Access denied"
      [(enforce-guard owner)     ;; Owner can access
       (enforce-guard admin)     ;; Admin can access
       (enforce-keyset 'super-admin)])))  ;; Super admin override
```

## Advanced Guard Patterns

### 1. Hierarchical Authorization

```pact
(module hierarchical-auth 'admin
  
  (defschema role
    level:integer
    permissions:[string])
  
  (deftable roles:{role})
  
  (defun role-check:bool (user:string required-level:integer)
    @doc "Check if user has sufficient role level"
    (let ((user-level (at 'level (read roles user))))
      (>= user-level required-level)))
  
  ;; Different guards for different access levels
  (defun create-level-guard (user:string level:integer)
    (create-user-guard (role-check user level)))
  
  (defun create-admin-resource (id:string)
    (insert resources id {
      "data": {},
      "guard": (create-level-guard (tx-sender) 5)  ;; Admin level
    }))
  
  (defun create-user-resource (id:string)
    (insert resources id {
      "data": {},
      "guard": (create-level-guard (tx-sender) 1)  ;; User level
    }))
)
```

### 2. Delegation Pattern

```pact
(module delegation 'admin
  
  (defschema delegation
    delegator:string
    delegatee:string
    permissions:[string]
    expires:time
    guard:guard)
  
  (deftable delegations:{delegation})
  
  (defun delegate-authority (delegatee:string permissions:[string] expires:time)
    @doc "Delegate authority to another user"
    (let ((delegator (tx-sender))
          (delegation-guard (create-delegation-guard delegator delegatee permissions expires)))
      
      (insert delegations (format "{}:{}" [delegator delegatee]) {
        "delegator": delegator,
        "delegatee": delegatee,
        "permissions": permissions,
        "expires": expires,
        "guard": delegation-guard
      })))
  
  (defun delegation-check:bool (delegator:string delegatee:string 
                               permissions:[string] expires:time)
    @doc "Check delegation validity"
    (and (= (tx-sender) delegatee)
         (< (at 'block-time (chain-data)) expires)
         ;; Additional permission checks...
         true))
  
  (defun create-delegation-guard (delegator:string delegatee:string 
                                 permissions:[string] expires:time)
    (create-user-guard (delegation-check delegator delegatee permissions expires)))
)
```

### 3. Threshold Guards

```pact
(defun threshold-auth:bool (threshold:integer approvers:[string])
  @doc "Require threshold number of approvals"
  (let ((signed-approvers 
         (filter (lambda (approver)
                   (contains approver (env-keys)))
                 approvers)))
    (>= (length signed-approvers) threshold)))

(defun create-threshold-guard (threshold:integer approvers:[string])
  @doc "Create guard requiring threshold approvals"
  (create-user-guard (threshold-auth threshold approvers)))

;; Multi-sig treasury
(defun create-treasury (threshold:integer signers:[string])
  (insert treasuries "main" {
    "balance": 0.0,
    "guard": (create-threshold-guard threshold signers)
  }))
```

## Principal Integration

Guards work seamlessly with Pact's principal system for account creation:

### Principal Creation

```pact
(defun create-principal-account (guard:guard)
  @doc "Create account with deterministic name from guard"
  (let ((principal (create-principal guard)))
    (insert accounts principal {
      "balance": 0.0,
      "guard": guard
    })
    principal))

;; Usage with different guard types
(create-principal-account (keyset-ref-guard 'my-keyset))
(create-principal-account (create-capability-guard (MY-CAP)))
(create-principal-account (create-user-guard (my-auth-function)))
```

### Principal Validation

```pact
(defun validate-principal (account:string)
  @doc "Validate account name matches its guard"
  (with-read accounts account { "guard" := guard }
    (let ((expected-principal (create-principal guard)))
      (enforce (= account expected-principal) "Account name mismatch"))))
```

## Testing Guards

### REPL Testing

```pact
;; test-guards.repl
(begin-tx)

;; Setup test data
(env-data { 
  "user-keyset": { "keys": ["user-key"], "pred": "keys-all" },
  "admin-keyset": { "keys": ["admin-key"], "pred": "keys-all" }
})

(define-keyset 'user-keyset (read-keyset "user-keyset"))
(define-keyset 'admin-keyset (read-keyset "admin-keyset"))

;; Test keyset guard
(env-keys ["user-key"])
(expect "User guard succeeds with user key"
  true
  (enforce-guard (keyset-ref-guard 'user-keyset)))

;; Test capability guard
(expect "Capability guard enforces capability"
  "capability not granted"
  (enforce-guard (create-capability-guard (ADMIN))))

;; Test user guard
(env-keys ["admin-key"])
(expect "Multi-factor auth succeeds"
  true
  (enforce-guard (create-mfa-guard "admin" "secret123")))

(commit-tx)
```

### Property Testing

```pact
(defun test-guard-properties ()
  @doc "Test guard system properties"
  
  ;; Property: Guard enforcement is deterministic
  (let ((guard (keyset-ref-guard 'test-keyset)))
    (enforce (= (enforce-guard guard) (enforce-guard guard))
      "Guard enforcement not deterministic"))
  
  ;; Property: Principal creation is consistent
  (let ((guard (keyset-ref-guard 'test-keyset))
        (principal1 (create-principal guard))
        (principal2 (create-principal guard)))
    (enforce (= principal1 principal2)
      "Principal creation not consistent")))
```

## Guard Performance

### Gas Optimization

Guards have different gas costs:

| Guard Type | Base Cost | Variable Cost |
|------------|-----------|---------------|
| Keyset | Low | Signature verification |
| Capability | Medium | Capability evaluation |
| User | Variable | Function execution |

### Optimization Strategies

```pact
;; GOOD: Cache guard lookups
(defun efficient-multi-check (accounts:[string])
  (let ((admin-guard (keyset-ref-guard 'admin)))
    (map (lambda (account)
           (with-read accounts account { "guard" := guard }
             (enforce-guard guard)))
         accounts)))

;; BAD: Repeated guard creation
(defun inefficient-multi-check (accounts:[string])
  (map (lambda (account)
         (with-read accounts account { "guard" := guard }
           (enforce-guard (keyset-ref-guard 'admin))))  ;; Created each time
       accounts))
```

## Migration and Best Practices

### Modern Guard Usage

```pact
;; RECOMMENDED: Use capability guards for new development
(defcap RESOURCE_ACCESS (resource:string user:string)
  (with-read user-permissions user { "resources" := user-resources }
    (enforce (contains resource user-resources) "Access denied")))

(defun create-modern-guard (resource:string user:string)
  (create-capability-guard (RESOURCE_ACCESS resource user)))

;; LEGACY: Module guards (deprecated)
(create-module-guard "resource-admin")  ;; Avoid in new code

;; MIGRATION: Convert to capability guards
(defcap RESOURCE_ADMIN ()
  (enforce-keyset 'resource-admin))

(create-capability-guard (RESOURCE_ADMIN))  ;; Use instead
```

### Security Best Practices

1. **Validate Guard Inputs**
```pact
(defun safe-guard-creation (user:string)
  (enforce (!= user "") "User cannot be empty")
  (create-capability-guard (USER_ACCESS user)))
```

2. **Use Appropriate Guard Types**
```pact
;; For simple authorization: Keyset guards
(keyset-ref-guard 'admin)

;; For complex logic: Capability guards
(create-capability-guard (COMPLEX_AUTH user resource))

;; For custom authorization: User guards (carefully)
(create-user-guard (custom-auth-function))
```

3. **Test Guard Enforcement**
```pact
;; Always test both success and failure cases
(expect-failure "Unauthorized access fails"
  "guard failure"
  (unauthorized-action))
```

## Summary

Guards provide Pact's unified security model:

- **Keyset Guards**: Cryptographic signature-based authorization
- **Capability Guards**: Fine-grained, composable permissions
- **User Guards**: Custom authorization logic
- **Principal Integration**: Deterministic account creation
- **Performance Aware**: Different costs for different guard types

Key principles:
1. **Choose appropriate guard types** for your security requirements
2. **Use capability guards** for modern development
3. **Test thoroughly** with various authorization scenarios
4. **Consider gas costs** when designing guard patterns
5. **Migrate deprecated guards** to capability-based systems

Guards bridge the gap between cryptographic proof (keysets) and business logic (capabilities), providing a flexible, auditable foundation for smart contract security.

## Exercises

1. Create a multi-level authorization system using capability guards
2. Implement a time-locked user guard with proper validation
3. Build a delegation system using user guards
4. Design a threshold signature system with guards
5. Migrate a module guard system to capability guards

## References

- Guard types: `/pact/Pact/Core/Guards.hs`
- Guard enforcement: `/pact/Pact/Core/IR/Eval/CEK/Evaluator.hs`
- Guard creation: `/pact/Pact/Core/IR/Eval/CEK/CoreBuiltin.hs`
- Principal system: `/pact/Pact/Core/Principal.hs`