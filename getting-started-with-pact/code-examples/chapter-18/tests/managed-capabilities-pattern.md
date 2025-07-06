# Managed Capabilities Pattern Guide

Based on the Pact test examples, here are the proper patterns for implementing @managed capabilities:

## Basic Pattern

```pact
(defcap TRANSFER:bool
  ( sender:string
    receiver:string  
    amount:decimal
  )
  @managed amount TRANSFER-mgr
  (enforce (!= sender receiver) "same sender and receiver")
  (enforce-unit amount)
  (enforce (> amount 0.0) "Positive amount")
  (compose-capability (DEBIT sender))
  (compose-capability (CREDIT receiver))
)

(defun TRANSFER-mgr:decimal
  ( managed:decimal
    requested:decimal
  )
  (let ((newbal (- managed requested)))
    (enforce (>= newbal 0.0)
      (format "TRANSFER exceeded for balance {}" [managed]))
    newbal)
)
```

## Key Principles

1. **Manager Function Naming**: The manager function should be named `<CAPABILITY>-mgr` by convention.

2. **Manager Function Signature**: Must take exactly two parameters:
   - `managed`: The current managed value
   - `requested`: The requested amount for this operation

3. **Manager Function Return**: Returns the new managed value after the operation.

4. **Enforcement in Manager**: The manager function should enforce business rules about the managed resource.

## Common Patterns

### 1. Decrementing Balance Pattern (Most Common)
Used for transfers, allowances, and spending limits:

```pact
(defun PAY-mgr (mgd req)
  (let ((bal (- mgd req)))
    (enforce (> req 0) "requested amount > 0")
    (enforce (>= bal 0) (format "sufficient balance: {} {} {}" [bal req mgd]))
    bal))
```

### 2. Auto-Managed Pattern
For one-time use capabilities (no explicit manager function):

```pact
(defcap ROTATE (account:string)
  @doc "Autonomously managed capability for guard rotation"
  @managed
  true)
```

### 3. Boolean/Toggle Pattern
For capabilities that can be turned on/off:

```pact
(defcap D (allow:bool) 
  @managed allow D-mgr 
  (enforce allow "allowed"))

(defun D-mgr (m r) m)
```

### 4. Counter Pattern
For limiting number of operations:

```pact
(defcap CAP_B:bool (name:string times:integer)
  @managed times capBMgr
  true)

(defun capBMgr:integer (mgd:integer rqd:integer)
  (enforce (> mgd 0) "all done")
  (- mgd 1))
```

## Usage with Signatures

Managed capabilities work seamlessly with transaction signatures:

```pact
(env-sigs 
  [{ "key": "alice",
     "caps": [(PAY "alice" "bob" 10)
             ,(PAY "alice" "carl" 1)] }])
```

## Important Notes

1. **Installation**: Managed capabilities can be installed with `install-capability`.

2. **Composition**: Managed capabilities can compose other capabilities.

3. **Events**: Often paired with `@event` for transaction logging.

4. **Guards**: Managed capabilities often check guards before allowing operations.

5. **Multiple Calls**: The manager function tracks state across multiple `with-capability` calls within the same transaction.

## Example: DeFi Allowance Pattern

```pact
(defcap ALLOWANCE (owner:string spender:string amount:decimal)
  @managed amount ALLOWANCE-mgr
  (enforce-guard (at 'guard (read accounts owner)))
)

(defun ALLOWANCE-mgr:decimal (allowed:decimal requested:decimal)
  (let ((new-allowed (- allowed requested)))
    (enforce (>= new-allowed 0.0) "Insufficient allowance")
    new-allowed))
```

This pattern enables secure delegation of spending rights while maintaining strict control over amounts.