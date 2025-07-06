# Pact Capabilities: Authorization and Security

## What Are Capabilities?

Capabilities in Pact are a powerful authorization mechanism that controls access to functions and resources. Think of them as "permissions" that must be acquired before performing certain actions.

## Why Capabilities?

Traditional smart contract security often relies on simple address-based checks. Pact's capability system provides:

1. **Fine-grained Control**: Specific permissions for specific actions
2. **Composability**: Capabilities can be combined and reused
3. **Formal Verification**: Capabilities can be formally verified
4. **Gas Efficiency**: More efficient than complex conditional logic

## Basic Capability Structure

```pact
(defcap CAPABILITY-NAME (param1:type param2:type)
  @doc "Description of what this capability allows"
  ;; Validation logic here
  (enforce condition "Error message")
  (enforce-keyset 'relevant-keyset))
```

## Using Capabilities

```pact
(defun protected-function ()
  (with-capability (CAPABILITY-NAME arg1 arg2)
    ;; Protected code here
    "Action performed"))
```

## Types of Capabilities

### 1. Simple Capabilities
No parameters, just check a condition:

```pact
(defcap ADMIN ()
  @doc "Admin capability"
  (enforce-keyset 'admin-keyset))
```

### 2. Parameterized Capabilities
Take parameters for context-specific validation:

```pact
(defcap TRANSFER (from:string to:string amount:decimal)
  @doc "Transfer capability"
  (enforce (> amount 0.0) "Amount must be positive")
  (enforce (!= from to) "Cannot transfer to self"))
```

### 3. Composed Capabilities
Combine multiple capabilities:

```pact
(defcap ATOMIC-SWAP (user1:string user2:string)
  @doc "Atomic swap capability" 
  (compose-capability (DEBIT user1))
  (compose-capability (CREDIT user2)))
```

## Capability Lifecycle

1. **Definition**: Define with `defcap`
2. **Acquisition**: Acquire with `with-capability`
3. **Validation**: Capability code runs and validates
4. **Scope**: Available within the `with-capability` block
5. **Release**: Automatically released when block exits

## Best Practices

### Do:
- ✅ Check preconditions in capability code
- ✅ Use descriptive names (ALL_CAPS convention)
- ✅ Add comprehensive documentation
- ✅ Validate all parameters
- ✅ Use `enforce` for validation

### Don't:
- ❌ Put business logic in capabilities
- ❌ Make capabilities too broad
- ❌ Forget parameter validation
- ❌ Use capabilities for simple access control only

## Common Patterns

### Admin Pattern
```pact
(defcap ADMIN ()
  (enforce-keyset 'admin-keyset))

(defun admin-function ()
  (with-capability (ADMIN)
    "Admin action"))
```

### Resource Access Pattern
```pact
(defcap ACCESS-RESOURCE (user:string resource:string)
  (enforce-guard (at 'guard (read resources resource)))
  (enforce (= user (at 'owner (read resources resource))) "Not owner"))
```

### Multi-Sig Pattern
```pact
(defcap MULTI-SIG (required:integer)
  (enforce (>= (length (read-keyset "signers")) required) "Insufficient signatures"))
```

## Error Handling

Capabilities use `enforce` for validation:

```pact
(defcap WITHDRAW (account:string amount:decimal)
  (enforce (> amount 0.0) "Amount must be positive")
  (enforce (<= amount (get-balance account)) "Insufficient balance"))
```

## Testing Capabilities

Test both success and failure cases:

```pact
;; Success case
(expect "admin function succeeds"
  "Admin action performed"
  (admin-only-function))

;; Failure case  
(expect-failure "non-admin fails"
  "Keyset failure"
  (admin-only-function))
```

## Next Steps

After mastering basic capabilities, learn about:
- Managed capabilities for resource control
- Capability guards for advanced patterns
- Event capabilities for logging
- Autonomous capabilities for one-time use