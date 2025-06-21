# Chapter 6: Keysets and Security - The Foundation Layer

## Introduction

Keysets form the foundational security layer in Pact. They provide cryptographic authorization based on digital signatures, serving as the bedrock upon which capabilities and other security features are built. This chapter explores every aspect of keysets from basic concepts to advanced security patterns.

## Understanding Keysets

### What is a Keyset?

A keyset is a collection of public keys combined with a predicate function that determines how many signatures are required:

```pact
;; Basic keyset structure
{
  "keys": ["public-key-1", "public-key-2", "public-key-3"],
  "pred": "keys-2"  ;; Requires 2 out of 3 signatures
}
```

### Core Components

1. **Keys**: Set of public keys (Ed25519 or WebAuthn format)
2. **Predicate**: Rule for how many keys must sign
3. **Name**: Optional identifier for reuse

## Keyset Predicates

### Built-in Predicates

#### 1. keys-all
All keys in the keyset must sign:

```pact
(define-keyset 'admin-keyset {
  "keys": ["admin-key-1", "admin-key-2"],
  "pred": "keys-all"
})

;; Both admin-key-1 AND admin-key-2 must sign
```

**Use case**: High-security operations requiring unanimous consent.

#### 2. keys-any
At least one key must sign:

```pact
(define-keyset 'support-keyset {
  "keys": ["support-alice", "support-bob", "support-charlie"],
  "pred": "keys-any"
})

;; ANY of the support staff can authorize
```

**Use case**: Delegated authority where any authorized person can act.

#### 3. keys-2
At least two keys must sign:

```pact
(define-keyset 'treasury-keyset {
  "keys": ["cfo-key", "ceo-key", "treasurer-key"],
  "pred": "keys-2"
})

;; Requires 2 out of 3 signatures
```

**Use case**: Multi-signature authorization with threshold security.

#### 4. Custom Predicates
User-defined functions for complex logic:

```pact
(defun custom-majority:bool (count:integer matched:integer)
  "Require majority of available keys"
  (>= matched (/ (+ count 1) 2)))

(define-keyset 'flexible-keyset {
  "keys": ["key1", "key2", "key3", "key4", "key5"],
  "pred": "custom-majority"
})
```

### Predicate Function Signature

Custom predicates receive:
- `count`: Total number of keys in the keyset
- `matched`: Number of valid signatures found

```pact
(defun time-based-auth:bool (count:integer matched:integer)
  "Different requirements based on time"
  (let ((hour (at 'hour (time))))
    (if (and (>= hour 9) (<= hour 17))
        (>= matched 1)      ;; Business hours: any key
        (>= matched 2))))   ;; After hours: 2 keys required
```

## Keyset Definition and Management

### Defining Keysets

#### From Transaction Data
```pact
;; In transaction data
{
  "admin-keyset": {
    "keys": ["ed25519-hex-key"],
    "pred": "keys-all"
  }
}

;; In Pact code
(define-keyset 'admin-keyset (read-keyset "admin-keyset"))
```

#### Inline Definition
```pact
(define-keyset 'emergency-keyset {
  "keys": ["emergency-key-1", "emergency-key-2"],
  "pred": "keys-all"
})
```

### Keyset Updates

Updating a keyset requires authorization from the existing keyset:

```pact
;; First transaction: Define initial keyset
(env-keys ["old-admin-key"])
(define-keyset 'admin-keyset {
  "keys": ["old-admin-key"],
  "pred": "keys-all"
})

;; Later transaction: Update keyset (must sign with old-admin-key)
(env-keys ["old-admin-key"])  ;; Old key must authorize
(define-keyset 'admin-keyset {
  "keys": ["new-admin-key-1", "new-admin-key-2"],
  "pred": "keys-2"
})
```

### Namespace-Aware Keysets

Keysets can be namespaced for organization:

```pact
;; Define namespace
(define-namespace 'myproject 
  (keyset-ref-guard 'myproject-admin)
  (keyset-ref-guard 'myproject-admin))

;; Define namespaced keyset
(namespace 'myproject)
(define-keyset 'myproject.module-admin {
  "keys": ["project-admin"],
  "pred": "keys-all"
})
```

## Keyset Enforcement

### Direct Enforcement

```pact
(defun admin-only-function ()
  (enforce-keyset 'admin-keyset)
  ;; admin logic here
  )
```

### Guard-Based Enforcement

```pact
(defun flexible-enforcement ()
  (enforce-guard (keyset-ref-guard 'admin-keyset))
  ;; logic here
  )
```

### Conditional Enforcement

```pact
(defun context-aware-function ()
  (if (= (at 'type (read-msg)) "admin")
      (enforce-keyset 'admin-keyset)
      (enforce-keyset 'user-keyset))
  ;; logic here
  )
```

## Key Formats and Validation

### Ed25519 Format
Standard format for Pact keys:

```pact
;; 64-character hexadecimal string
"ba54b224d1924dd98403f5c751abdd10de6cd81b0b30e57c9506338d5f8a9a13"
```

**Validation rules:**
- Exactly 64 characters
- Only hexadecimal digits (0-9, a-f)
- Lowercase only

### WebAuthn Format
For hardware security keys and biometric authentication:

```pact
;; WebAuthn key with prefix
"WEBAUTHN-a501020326200121582047b84e4bd26c0b1ad0b7cd16ff45b63b48de670ee9b56acfbb2c97b87e07b45a22200226"
```

**Features:**
- Hardware-backed security
- Biometric authentication
- FIDO2/WebAuthn compatible
- COSE key format

### Key Validation Implementation

```pact
;; From the Pact implementation
(defun validate-key-format (key:string)
  "Validate key format"
  (enforce (or (ed25519-format? key) (webauthn-format? key))
    "Invalid key format"))

(defun ed25519-format? (key:string)
  "Check Ed25519 format"
  (and (= (length key) 64)
       (str-to-int 16 key)))  ;; Valid hex

(defun webauthn-format? (key:string)
  "Check WebAuthn format"
  (contains "WEBAUTHN-" key))
```

## Transaction Integration

### How Keysets Work with Transactions

When you send a transaction, the signature verification process:

1. **Extract signatures** from transaction
2. **Verify cryptographic signatures** against transaction hash
3. **Match public keys** to keyset requirements
4. **Apply predicate function** to determine if threshold is met

### Transaction Structure with Keysets

```json
{
  "signers": [
    {
      "pubKey": "ba54b224d1924dd98403f5c751abdd10de6cd81b0b30e57c9506338d5f8a9a13",
      "clist": []  // Empty for unscoped signing
    }
  ],
  "payload": {
    "exec": {
      "code": "(my-module.admin-function)"
    }
  }
}
```

### Scoped vs Unscoped Signatures

```javascript
// Scoped signature - safer
{
  "pubKey": "admin-key",
  "clist": [
    {"name": "my-module.ADMIN", "args": []}
  ]
}

// Unscoped signature - dangerous
{
  "pubKey": "admin-key", 
  "clist": []  // Can satisfy any keyset this key is in
}
```

## Advanced Keyset Patterns

### 1. Hierarchical Security

```pact
(module secure-vault 'super-admin
  
  ;; Multiple security levels
  (define-keyset 'super-admin {
    "keys": ["super-key-1", "super-key-2", "super-key-3"],
    "pred": "keys-all"
  })
  
  (define-keyset 'vault-admin {
    "keys": ["vault-key-1", "vault-key-2"],
    "pred": "keys-2"
  })
  
  (define-keyset 'operator {
    "keys": ["op-1", "op-2", "op-3"],
    "pred": "keys-any"
  })
  
  (defun emergency-shutdown ()
    "Only super-admin can shut down"
    (enforce-keyset 'super-admin)
    (update-status "shutdown"))
  
  (defun daily-operations ()
    "Operators can perform routine tasks"
    (enforce-keyset 'operator)
    (process-transactions))
)
```

### 2. Time-Based Security

```pact
(module time-vault 'admin
  
  (defun business-hours-auth:bool (count:integer matched:integer)
    "Different rules for business hours"
    (let* ((hour (at 'hour (time)))
           (business-hours (and (>= hour 9) (<= hour 17))))
      (if business-hours
          (>= matched 1)    ;; Any key during business hours
          (>= matched 2)))) ;; 2 keys required after hours
  
  (define-keyset 'time-based {
    "keys": ["manager", "assistant", "security"],
    "pred": "business-hours-auth"
  })
)
```

### 3. Dynamic Keysets

```pact
(module dynamic-auth 'admin
  
  (defschema auth-level
    keys:[string]
    threshold:integer
    active:bool)
  
  (deftable auth-levels:{auth-level})
  
  (defun get-current-keyset:object ()
    "Build keyset from current auth level"
    (with-read auth-levels "current"
      { 'keys := keys, 'threshold := threshold }
      { "keys": keys, "pred": "dynamic-pred" }))
  
  (defun dynamic-pred:bool (count:integer matched:integer)
    "Use threshold from database"
    (let ((threshold (at 'threshold (read auth-levels "current"))))
      (>= matched threshold)))
)
```

### 4. Rotation Pattern

```pact
(module key-rotation 'admin
  
  (defschema rotation-schedule
    current-keys:[string]
    next-keys:[string] 
    rotation-time:time
    active:bool)
  
  (deftable rotations:{rotation-schedule})
  
  (defcap ROTATE ()
    "Capability to rotate keys"
    (let ((rotation (read rotations "main")))
      (enforce (>= (time) (at 'rotation-time rotation))
        "Rotation time not reached")))
  
  (defun execute-rotation ()
    "Rotate to new keys"
    (with-capability (ROTATE)
      (with-read rotations "main"
        { 'next-keys := new-keys }
        ;; Update the actual keyset
        (define-keyset 'rotating-keyset {
          "keys": new-keys,
          "pred": "keys-2"
        }))))
)
```

## Keyset Security Best Practices

### 1. Choose Appropriate Predicates

```pact
;; GOOD: Use appropriate security level
(define-keyset 'treasury {
  "keys": ["cfo", "ceo", "board-chair"],
  "pred": "keys-2"  ;; Requires majority
})

;; BAD: Too permissive
(define-keyset 'treasury {
  "keys": ["cfo", "ceo", "board-chair"],
  "pred": "keys-any"  ;; Any single person
})
```

### 2. Secure Key Management

```pact
;; GOOD: Separate keys by role
(define-keyset 'daily-ops {
  "keys": ["ops-alice", "ops-bob"],
  "pred": "keys-any"
})

(define-keyset 'critical-ops {
  "keys": ["cso", "cto"],
  "pred": "keys-all"
})

;; BAD: Same keys for everything
(define-keyset 'everything {
  "keys": ["master-key"],
  "pred": "keys-all"
})
```

### 3. Regular Rotation

```pact
(module secure-rotation 'admin
  
  (defconst ROTATION_INTERVAL (days 90))
  
  (defun check-rotation-due ()
    "Check if keys need rotation"
    (let ((last-rotation (at 'last-rotation (read metadata "keys"))))
      (enforce (< (diff-time (time) last-rotation) ROTATION_INTERVAL)
        "Key rotation overdue")))
  
  (defun admin-function ()
    (check-rotation-due)
    (enforce-keyset 'rotating-admin)
    ;; function logic
    )
)
```

### 4. Namespace Organization

```pact
;; Organize keysets by namespace
(namespace 'myapp)

(define-keyset 'myapp.admin {
  "keys": ["app-admin"],
  "pred": "keys-all"
})

(define-keyset 'myapp.users {
  "keys": ["user-1", "user-2"],
  "pred": "keys-any"
})
```

## Row-Level Security with Keysets

### Account Protection Pattern

```pact
(module accounts 'admin
  
  (defschema account
    balance:decimal
    guard:guard        ;; Keyset protection per account
    owner:string)
  
  (deftable accounts:{account})
  
  (defun create-account (id:string owner-keyset:guard)
    "Create account with keyset protection"
    (insert accounts id {
      "balance": 0.0,
      "guard": owner-keyset,
      "owner": id
    }))
  
  (defun transfer (from:string to:string amount:decimal)
    "Transfer requires sender authorization"
    (with-read accounts from { 'guard := from-guard, 'balance := balance }
      ;; Enforce the account's keyset
      (enforce-guard from-guard)
      (enforce (>= balance amount) "Insufficient balance")
      ;; ... transfer logic
      ))
)

;; Usage
(begin-tx)
(env-data { "alice-keyset": { "keys": ["alice-key"], "pred": "keys-all" }})
(accounts.create-account "alice" (read-keyset "alice-keyset"))
(commit-tx)
```

### Multi-Signature Accounts

```pact
(defun create-multisig-account (id:string keys:[string] threshold:integer)
  "Create account requiring multiple signatures"
  (let ((multisig-guard 
         (enforce (>= threshold 1) "Invalid threshold")
         (enforce (<= threshold (length keys)) "Threshold too high")
         { "keys": keys, "pred": "keys-n" }))  ;; Custom predicate
    (insert accounts id {
      "balance": 0.0,
      "guard": multisig-guard,
      "threshold": threshold
    })))

(defun keys-n:bool (count:integer matched:integer)
  "Check against stored threshold"
  (let ((threshold (at 'threshold (read accounts (get-current-account)))))
    (>= matched threshold)))
```

## Testing Keysets

### REPL Testing

```pact
;; test-keysets.repl
(begin-tx)

;; Set up test environment
(env-data { 
  "admin-keyset": { 
    "keys": ["admin-key"], 
    "pred": "keys-all" 
  }
})
(env-keys ["admin-key"])

;; Test keyset definition
(define-keyset 'admin-keyset (read-keyset "admin-keyset"))

;; Test enforcement
(expect "Admin function succeeds with admin key"
  "success"
  (my-module.admin-function))

;; Test failure case
(env-keys [])
(expect-failure "Admin function fails without key"
  "Keyset failure"
  (my-module.admin-function))

(commit-tx)
```

### Multi-Key Testing

```pact
;; test-multisig.repl
(begin-tx)

(env-data {
  "multisig": {
    "keys": ["key1", "key2", "key3"],
    "pred": "keys-2"
  }
})

;; Test with sufficient keys
(env-keys ["key1", "key2"])
(expect "Multisig succeeds with 2 keys"
  "success"
  (my-module.multisig-function))

;; Test with insufficient keys
(env-keys ["key1"])
(expect-failure "Multisig fails with 1 key"
  "Keyset failure"
  (my-module.multisig-function))

(commit-tx)
```

## Keyset Performance Considerations

### Efficient Key Lookups

```pact
;; GOOD: Store keyset reference
(defschema user
  balance:decimal
  keyset-ref:string)  ;; Reference to stored keyset

;; BAD: Inline keyset per user (storage bloat)
(defschema user
  balance:decimal
  guard:guard)  ;; Full keyset in every row
```

### Batch Operations

```pact
(defun batch-authorize (operations:[object])
  "Authorize once for multiple operations"
  (enforce-keyset 'batch-admin)
  (map (process-operation) operations))
```

## Integration with Other Security Features

### Keysets and Capabilities

```pact
(defcap ADMIN ()
  "Admin capability backed by keyset"
  (enforce-keyset 'admin-keyset))

(defun admin-function ()
  (with-capability (ADMIN)
    ;; Capability provides the authorization
    ;; Keyset provides the cryptographic proof
    ))
```

### Keysets and Guards

```pact
(defun create-complex-guard ()
  "Combine multiple authorization methods"
  (enforce-one "Admin or time-based access"
    [(enforce-keyset 'admin-keyset)
     (and (enforce-keyset 'time-admin)
          (enforce-time-window))]))
```

## Common Pitfalls and Solutions

### 1. Key Reuse Across Environments

```pact
;; PROBLEM: Same keys in test and production
(define-keyset 'admin {
  "keys": ["test-key"],  ;; DON'T use test keys in prod
  "pred": "keys-all"
})

;; SOLUTION: Environment-specific keys
(define-keyset 'admin (read-keyset "admin-keyset"))
;; Different keyset data per environment
```

### 2. Overly Permissive Predicates

```pact
;; PROBLEM: Too easy to authorize
(define-keyset 'critical {
  "keys": ["temp-key"],
  "pred": "keys-any"  ;; Single key for critical ops
})

;; SOLUTION: Appropriate threshold
(define-keyset 'critical {
  "keys": ["key1", "key2", "key3"],
  "pred": "keys-2"  ;; Requires majority
})
```

### 3. Missing Key Rotation

```pact
;; SOLUTION: Build in rotation checks
(defun check-key-freshness ()
  (let ((keyset-age (diff-time (time) (at 'created (describe-keyset 'admin)))))
    (enforce (< keyset-age (days 90)) "Keys need rotation")))
```

## Summary

Keysets provide the cryptographic foundation for Pact's security model:

- **Flexible Authorization**: Multiple predicate options for different security needs
- **Strong Cryptography**: Ed25519 and WebAuthn support
- **Composable Security**: Integration with capabilities and guards
- **Namespace Support**: Organized key management
- **Row-Level Protection**: Per-account security models

Key principles:
1. **Choose appropriate predicates** for your security requirements
2. **Separate keys by role** and responsibility  
3. **Implement regular rotation** procedures
4. **Test thoroughly** with different key combinations
5. **Integrate with capabilities** for fine-grained control

Understanding keysets is essential for building secure Pact applications. They form the foundation upon which all other security features are built.

## Exercises

1. Create a 3-of-5 multisig keyset for a treasury function
2. Implement time-based keyset authorization
3. Build a key rotation system with transition periods
4. Design row-level security using keyset guards
5. Test keyset failure scenarios in the REPL

## References

- Keyset implementation: `/pact/Pact/Core/Guards.hs`
- Signature verification: `/pact/Pact/Core/IR/Eval/CEK/Evaluator.hs`
- Builtin functions: `/pact/Pact/Core/IR/Eval/CEK/CoreBuiltin.hs`
- Key validation: `/pact/Pact/Core/Validators.hs`