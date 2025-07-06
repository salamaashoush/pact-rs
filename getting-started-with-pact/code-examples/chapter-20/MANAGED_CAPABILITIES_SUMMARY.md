# Managed Capabilities in DeFi - Summary

## Overview

Based on the Pact test examples, managed capabilities provide a powerful mechanism for controlling resource limits and enforcing business rules in DeFi protocols. They track state across multiple operations within a transaction, ensuring limits are respected.

## Key Implementation Examples

### 1. Transfer Limits (defi-token.pact)

```pact
(defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
  @managed amount TRANSFER-mgr
  (enforce (!= sender receiver) "Cannot transfer to self")
  (enforce-valid-amount amount)
  (compose-capability (DEBIT sender amount))
  (compose-capability (CREDIT receiver amount)))

(defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
  (let ((remaining (- managed requested)))
    (enforce (>= remaining 0.0) "Transfer exceeds allowance")
    remaining))
```

**Usage**: Limits the total amount that can be transferred in a transaction, enabling secure delegation.

### 2. Borrowing Power (lending-pool-improved.pact)

```pact
(defcap BORROW-POWER (account:string max-borrow-usd:decimal)
  @managed max-borrow-usd BORROW-POWER-mgr
  (enforce-valid-account account)
  (let ((current-collateral (get-total-collateral-value account)))
    (enforce (> current-collateral 0.0) "No collateral deposited")))

(defun BORROW-POWER-mgr:decimal (available:decimal requested:decimal)
  (let ((remaining (- available requested)))
    (enforce (>= remaining 0.0) 
      (format "Insufficient borrowing power: available={}, requested={}" 
              [available requested]))
    remaining))
```

**Usage**: Enforces collateral-based borrowing limits across multiple borrow operations.

### 3. Stake Limits (yield-farming-improved.pact)

```pact
(defcap STAKE-ALLOWANCE (account:string pool-id:string max-stake:decimal)
  @managed max-stake STAKE-ALLOWANCE-mgr
  (enforce-valid-account account)
  (with-read pools pool-id 
    { "start-time" := start, "end-time" := end }
    (let ((now (at 'block-time (chain-data))))
      (enforce (>= now start) "Pool not started")
      (enforce (<= now end) "Pool ended"))))
```

**Usage**: Limits per-user stake amounts in yield farming pools.

### 4. Auto-Managed Capabilities

```pact
(defcap ROTATE (account:string)
  @doc "Autonomously managed capability for guard rotation"
  @managed
  true)
```

**Usage**: One-time operations that can only be performed once per transaction.

## Best Practices

1. **Manager Function Naming**: Always name manager functions as `<CAPABILITY>-mgr`.

2. **Manager Function Signature**: Must accept exactly two parameters:
   - `managed`: Current available amount
   - `requested`: Amount being requested

3. **Return Value**: Manager returns the new available amount after deduction.

4. **Error Messages**: Provide clear error messages explaining why limits were exceeded.

5. **Composition**: Managed capabilities can compose other capabilities for complex authorization.

## Transaction Signatures

Managed capabilities work seamlessly with transaction signatures:

```pact
(env-sigs [
  { "key": "alice"
  , "caps": [(TRANSFER "alice" "bob" 100.0)
            ,(BORROW-POWER "alice" 10000.0)] }
])
```

## Common Patterns

### Decrementing Pattern (Most Common)
Used for spending limits, allowances, and quotas:
```pact
(defun SPEND-mgr (available requested)
  (let ((remaining (- available requested)))
    (enforce (>= remaining 0.0) "Insufficient funds")
    remaining))
```

### Counter Pattern
For limiting number of operations:
```pact
(defun COUNT-mgr (count requested)
  (enforce (> count 0) "No operations remaining")
  (- count 1))
```

### Boolean Toggle Pattern
For on/off capabilities:
```pact
(defun TOGGLE-mgr (enabled requested)
  (enforce enabled "Operation disabled")
  enabled)
```

## Security Benefits

1. **Atomic Limits**: All operations within a transaction share the same limit.
2. **No Double-Spending**: Manager function ensures total usage never exceeds limit.
3. **Signature Integration**: Works with Pact's signature-based authorization.
4. **Composability**: Can be combined with other capabilities for complex rules.

## DeFi Use Cases

1. **Lending Protocols**:
   - Borrowing limits based on collateral
   - Withdrawal restrictions to maintain health factor
   - Liquidation limits (close factor)

2. **DEX/AMM**:
   - Slippage protection
   - Maximum swap amounts
   - Liquidity provision limits

3. **Yield Farming**:
   - Per-user stake limits
   - Reward claim restrictions
   - Time-based vesting

4. **Governance**:
   - Voting power limits
   - Proposal submission quotas
   - Delegation limits

## Testing Managed Capabilities

Always test:
1. Operations within limits succeed
2. Operations exceeding limits fail
3. Multiple operations correctly decrement available amount
4. Edge cases (zero amounts, exact limits)
5. Proper error messages

See `test-managed-capabilities.repl` for comprehensive examples.