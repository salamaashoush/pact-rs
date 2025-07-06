# Pact Capabilities: A Complete Beginner's Guide

## Table of Contents
1. [What Are Capabilities?](#what-are-capabilities)
2. [Why Do We Need Capabilities?](#why-do-we-need-capabilities)
3. [Basic Capability Concepts](#basic-capability-concepts)
4. [Types of Capabilities](#types-of-capabilities)
5. [The Capability Lifecycle](#the-capability-lifecycle)
6. [Managed Capabilities Deep Dive](#managed-capabilities-deep-dive)
7. [Real-World Examples](#real-world-examples)
8. [Common Patterns](#common-patterns)
9. [Troubleshooting](#troubleshooting)
10. [Best Practices](#best-practices)

## What Are Capabilities?

Think of capabilities as **special permissions** or **access tokens** that grant the right to perform specific actions in your smart contract. They're like keys that unlock certain functions.

### Real-World Analogy

Imagine a bank vault:
- **Keyset** = Having the physical key to the vault
- **Capability** = Having permission to withdraw a specific amount

Just having the key isn't enough - you also need authorization for what you want to do!

### Simple Example

```pact
;; Define a capability to withdraw money
(defcap WITHDRAW (account:string amount:decimal)
  "Permission to withdraw funds from an account"
  ;; Check that the person has the right key
  (enforce-guard (at 'guard (read accounts account)))
  ;; Check that the amount is valid
  (enforce (> amount 0.0) "Amount must be positive"))

;; Use the capability
(defun withdraw-money (amount:decimal)
  ;; First, acquire the capability
  (with-capability (WITHDRAW "my-account" amount)
    ;; Now we can perform the protected action
    (deduct-from-account "my-account" amount)))
```

## Why Do We Need Capabilities?

### 1. **Fine-Grained Permissions**
Unlike simple "all-or-nothing" key checks, capabilities let you control:
- WHO can do something
- WHAT they can do
- HOW MUCH they can do
- WHEN they can do it

### 2. **Composable Security**
You can build complex authorization from simple pieces:
```pact
;; TRANSFER capability is composed of DEBIT and CREDIT
(defcap TRANSFER (from:string to:string amount:decimal)
  (compose-capability (DEBIT from amount))
  (compose-capability (CREDIT to amount)))
```

### 3. **Resource Management**
Capabilities can track and limit resource usage:
```pact
;; This capability tracks how much you can spend
(defcap SPEND (amount:decimal)
  @managed amount SPEND-mgr  ;; Tracks remaining spending power
  (enforce-keyset 'spender-keys))
```

## Basic Capability Concepts

### 1. **Capability Definition (`defcap`)**
This is where you define what a capability means and what checks it performs:

```pact
(defcap MY-CAPABILITY (param1:string param2:decimal)
  "Documentation string"
  ;; Body: checks that must pass to grant the capability
  (enforce-guard some-guard)
  (enforce (> param2 0.0) "Must be positive"))
```

### 2. **Capability Acquisition (`with-capability`)**
This is how you request and use a capability:

```pact
(with-capability (MY-CAPABILITY "hello" 10.0)
  ;; Code here runs with the capability active
  (do-protected-action))
```

### 3. **Capability Requirement (`require-capability`)**
This checks that a capability is already active:

```pact
(defun do-protected-action ()
  ;; This will fail if MY-CAPABILITY isn't active
  (require-capability (MY-CAPABILITY "hello" 10.0))
  ;; Protected code here
  )
```

## Types of Capabilities

### 1. **Simple (Unmanaged) Capabilities**
Basic yes/no permissions:

```pact
(defcap ADMIN ()
  "Admin-only capability"
  (enforce-keyset 'admin-keyset))

(defun admin-action ()
  (with-capability (ADMIN)
    (do-admin-stuff)))
```

### 2. **Managed Capabilities**
Track and limit resource usage:

```pact
(defcap WITHDRAW (amount:decimal)
  @managed amount WITHDRAW-mgr  ;; 'amount' is managed
  (enforce-keyset 'wallet-owner))

;; Manager function controls the resource
(defun WITHDRAW-mgr:decimal (managed:decimal requested:decimal)
  "managed = available balance, requested = withdrawal amount"
  (let ((new-balance (- managed requested)))
    (enforce (>= new-balance 0.0) "Insufficient funds")
    new-balance))  ;; Return remaining balance
```

### 3. **Event Capabilities**
Automatically emit blockchain events:

```pact
(defcap TRANSFER-EVENT (from:string to:string amount:decimal)
  @event  ;; This makes it an event capability
  true)   ;; Always succeeds

;; When acquired, automatically emits an event
(with-capability (TRANSFER-EVENT "alice" "bob" 100.0)
  'ok)
;; Event emitted: {"name": "TRANSFER-EVENT", "params": ["alice", "bob", 100.0]}
```

### 4. **Autonomous Capabilities**
Special managed capabilities that don't need signatures:

```pact
(defcap ROTATE (account:string)
  @managed  ;; No manager function = autonomous
  "One-time account rotation capability"
  true)

;; Can be used once per transaction without signature
(defun rotate-keys (account:string new-guard:guard)
  (with-capability (ROTATE account)
    (update-account-guard account new-guard)))
```

## The Capability Lifecycle

### Step 1: Definition
```pact
(defcap ORDER-PIZZA (size:string toppings:[string])
  "Permission to order a pizza"
  (enforce-keyset 'customer-keys)
  (enforce (contains size ["small" "medium" "large"]) "Invalid size"))
```

### Step 2: Installation (for managed capabilities)
```pact
;; Pre-authorize a spending limit
(install-capability (SPEND 100.0))
```

### Step 3: Acquisition
```pact
(with-capability (ORDER-PIZZA "large" ["pepperoni" "mushrooms"])
  ;; Capability is now active
  (place-order "large" ["pepperoni" "mushrooms"]))
```

### Step 4: Enforcement
```pact
(defun place-order (size:string toppings:[string])
  ;; Check that ORDER-PIZZA capability is active
  (require-capability (ORDER-PIZZA size toppings))
  ;; Process the order
  (insert orders (generate-order-id) 
    { 'size: size, 'toppings: toppings }))
```

### Step 5: Scope Exit
```pact
;; After with-capability block ends, capability is no longer active
(place-order "small" ["cheese"])  ;; This will fail!
```

## Managed Capabilities Deep Dive

Managed capabilities are powerful but can be confusing. Let's break them down:

### The Problem They Solve
Imagine you want to limit spending from an account:

```pact
;; Without managed capabilities (UNSAFE!)
(defcap SPEND (amount:decimal)
  (enforce-keyset 'spender))

;; Problem: Someone could call this multiple times!
(with-capability (SPEND 50.0) (withdraw 50.0))
(with-capability (SPEND 50.0) (withdraw 50.0))  ;; Oops, spent 100!
```

### The Solution: Manager Functions
```pact
(defcap SPEND (amount:decimal)
  @managed amount SPEND-mgr  ;; Declare 'amount' as managed
  (enforce-keyset 'spender))

(defun SPEND-mgr:decimal (managed:decimal requested:decimal)
  "Track total spending"
  ;; managed = remaining budget
  ;; requested = amount trying to spend
  (let ((new-budget (- managed requested)))
    (enforce (>= new-budget 0.0) "Budget exceeded")
    new-budget))  ;; Return remaining budget

;; Usage:
(install-capability (SPEND 100.0))  ;; Set budget to 100
(with-capability (SPEND 50.0) (withdraw 50.0))   ;; OK, budget now 50
(with-capability (SPEND 60.0) (withdraw 60.0))   ;; FAILS! Only 50 left
```

### How Manager Functions Work

1. **First Call**: 
   - `managed` = initial installed amount
   - `requested` = amount being requested
   - Returns new managed value

2. **Subsequent Calls**:
   - `managed` = value returned from previous call
   - `requested` = new amount being requested
   - Returns updated managed value

### Manager Function Rules

1. **Must be a pure function** (no side effects)
2. **Must return the same type as the managed parameter**
3. **Should enforce linear constraints** (e.g., decreasing balance)
4. **Cannot access database or other state**

## Real-World Examples

### Example 1: Token Transfer System

```pact
(module token-example 'admin-keyset

  (defschema account
    balance:decimal
    guard:guard)
  
  (deftable accounts:{account})

  ;; Main transfer capability
  (defcap TRANSFER (sender:string receiver:string amount:decimal)
    @managed amount TRANSFER-mgr
    (compose-capability (DEBIT sender))
    (compose-capability (CREDIT receiver)))
  
  ;; Manager ensures transfer amount doesn't change
  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    (enforce (= managed requested) "Transfer amount change not allowed")
    0.0)  ;; No remaining amount after transfer
  
  ;; Debit capability
  (defcap DEBIT (account:string)
    "Capability to debit from account"
    (enforce-guard (at 'guard (read accounts account))))
  
  ;; Credit capability (anyone can receive)
  (defcap CREDIT (account:string)
    "Capability to credit to account"
    true)
  
  ;; Transfer function
  (defun transfer:string (sender:string receiver:string amount:decimal)
    (with-capability (TRANSFER sender receiver amount)
      (with-read accounts sender { 'balance := sender-balance }
        (enforce (>= sender-balance amount) "Insufficient balance")
        (update accounts sender { 'balance: (- sender-balance amount) })
        (with-read accounts receiver { 'balance := receiver-balance }
          (update accounts receiver { 'balance: (+ receiver-balance amount) }))))
    (format "Transferred {} from {} to {}" [amount sender receiver]))
)
```

### Example 2: Time-Locked Withdrawal

```pact
(module timelock-example 'admin-keyset

  (defschema lockup
    amount:decimal
    unlock-time:time
    owner:guard)
  
  (deftable lockups:{lockup})

  ;; Capability that checks time constraint
  (defcap WITHDRAW (lockup-id:string)
    (with-read lockups lockup-id 
      { 'unlock-time := unlock-time
      , 'owner := owner }
      (enforce-guard owner)
      (enforce-time unlock-time)))
  
  ;; Helper to check time
  (defun enforce-time:bool (unlock-time:time)
    (enforce (>= (at 'block-time (chain-data)) unlock-time) 
      "Funds are still locked"))
  
  ;; Withdraw function
  (defun withdraw:string (lockup-id:string)
    (with-capability (WITHDRAW lockup-id)
      (with-read lockups lockup-id { 'amount := amount }
        (delete lockups lockup-id)
        (transfer-to-owner amount)
        (format "Withdrew {} from lockup {}" [amount lockup-id]))))
)
```

### Example 3: Multi-Signature Authorization

```pact
(module multisig-example 'admin-keyset

  ;; Capability that requires multiple signers
  (defcap MULTI-AUTH ()
    "Requires 2 of 3 signers"
    (let ((signer-count 
           (length 
             (filter 
               (lambda (key) (contains key (env-keys)))
               ["alice-key" "bob-key" "charlie-key"]))))
      (enforce (>= signer-count 2) "Need at least 2 signers")))
  
  ;; Protected function
  (defun important-action ()
    (with-capability (MULTI-AUTH)
      (perform-critical-operation)))
)
```

## Common Patterns

### 1. **Guard Composition Pattern**
Combine multiple authorization checks:

```pact
(defcap ADMIN-OR-OWNER (resource:string)
  (enforce-one "Must be admin or owner"
    [(enforce-keyset 'admin-keyset)
     (enforce-guard (at 'owner-guard (read resources resource)))]))
```

### 2. **Delegation Pattern**
Allow users to delegate capabilities:

```pact
(defcap DELEGATE (delegator:string delegatee:string)
  @managed
  "Allow delegatee to act on behalf of delegator"
  (enforce-guard (at 'guard (read accounts delegator))))

(defun delegated-transfer (delegator:string to:string amount:decimal)
  (require-capability (DELEGATE delegator (env-data 'delegatee)))
  (with-capability (TRANSFER delegator to amount)
    (do-transfer delegator to amount)))
```

### 3. **Rate Limiting Pattern**
Limit actions over time:

```pact
(defcap RATE-LIMITED-ACTION (account:string)
  @managed count RATE-LIMIT-mgr
  (enforce-guard (at 'guard (read accounts account))))

(defun RATE-LIMIT-mgr:integer (managed:integer requested:integer)
  "Allow max 5 actions"
  (let ((new-count (+ managed requested)))
    (enforce (<= new-count 5) "Rate limit exceeded")
    new-count))
```

### 4. **Escrow Pattern**
Capabilities for multi-party transactions:

```pact
(defcap ESCROW-RELEASE (escrow-id:string)
  "Both parties must approve release"
  (with-read escrows escrow-id 
    { 'buyer := buyer, 'seller := seller }
    (enforce-one "Approval required"
      [(enforce-keyset buyer)
       (enforce-keyset seller)])))
```

## Troubleshooting

### Common Error: "require-capability: not granted"
This means the capability isn't active. Solutions:
1. Wrap the code in `with-capability`
2. Check that parent function has acquired the capability
3. Verify capability parameters match exactly

### Common Error: "Managed capability not installed"
For managed capabilities, you must either:
1. Call `install-capability` first
2. Have the capability in transaction signatures
3. Use an autonomous capability (`@managed` without manager)

### Common Error: "Cannot compose capability outside defcap"
`compose-capability` only works inside a `defcap` body:
```pact
;; WRONG
(defun my-function ()
  (compose-capability (SOME-CAP)))  ;; Error!

;; RIGHT
(defcap MY-CAP ()
  (compose-capability (SOME-CAP)))  ;; OK
```

## Best Practices

### 1. **Use Descriptive Names**
```pact
;; Good
(defcap WITHDRAW-FROM-SAVINGS (account:string amount:decimal) ...)

;; Bad
(defcap CAP1 (a:string b:decimal) ...)
```

### 2. **Document Capability Behavior**
```pact
(defcap TRANSFER (from:string to:string amount:decimal)
  @doc "Transfers tokens from one account to another. \
      \ Composes DEBIT and CREDIT capabilities. \
      \ Managed by amount to prevent double-spending."
  @managed amount TRANSFER-mgr
  ...)
```

### 3. **Keep Capabilities Simple**
Each capability should have a single, clear purpose:
```pact
;; Good: Single purpose
(defcap DEBIT (account:string) ...)
(defcap CREDIT (account:string) ...)

;; Bad: Doing too much
(defcap TRANSFER-AND-LOG-AND-NOTIFY (...) ...)
```

### 4. **Use Composition for Complex Auth**
```pact
(defcap TRANSFER (from:string to:string amount:decimal)
  (compose-capability (DEBIT from))      ;; Check debit permission
  (compose-capability (CREDIT to))       ;; Check credit permission
  (compose-capability (FEE-PAID from)))  ;; Check fees are paid
```

### 5. **Validate Parameters Early**
```pact
(defcap WITHDRAW (account:string amount:decimal)
  ;; Validate parameters first
  (enforce (!= account "") "Account cannot be empty")
  (enforce (> amount 0.0) "Amount must be positive")
  ;; Then check authorization
  (enforce-guard (at 'guard (read accounts account))))
```

### 6. **Use Events for Audit Trails**
```pact
(defcap IMPORTANT-ACTION (user:string action:string)
  @event  ;; Automatically logged to blockchain
  (enforce-keyset 'authorized-users))
```

## How Capabilities Work with Transactions (The Missing Piece!)

This is perhaps the most important part: how capabilities are actually granted when someone sends a transaction to the blockchain.

### The Transaction Flow

When you send a transaction to Pact, you're not just sending code - you're also sending **signatures** that can **pre-authorize capabilities**.

### Transaction Structure with Capabilities

Here's what a transaction looks like with capability authorization:

```json
{
  "networkId": "testnet04",
  "payload": {
    "exec": {
      "data": {
        "alice-keyset": {
          "keys": ["alice-public-key"],
          "pred": "keys-all"
        }
      },
      "code": "(coin.transfer \"alice\" \"bob\" 50.0)"
    }
  },
  "signers": [
    {
      "pubKey": "alice-public-key",
      "clist": [
        {
          "name": "coin.GAS",
          "args": []
        },
        {
          "name": "coin.TRANSFER",
          "args": ["alice", "bob", 50.0]
        }
      ]
    }
  ],
  "meta": {
    "creationTime": 1234567890,
    "ttl": 600,
    "gasLimit": 10000,
    "chainId": "0",
    "gasPrice": 0.00000001,
    "sender": "alice"
  },
  "nonce": "2023-01-01-transfer"
}
```

### Key Components Explained

1. **`signers`**: List of entities signing the transaction
2. **`pubKey`**: The public key of the signer
3. **`clist`** (Capability List): The capabilities this signer is granting
4. **`name`**: Fully qualified capability name (module.CAPABILITY)
5. **`args`**: Arguments that must match exactly when the capability is invoked

### How It Works Step-by-Step

#### Step 1: Transaction Creation
```javascript
// Client-side code (JavaScript)
const transaction = {
  signers: [{
    pubKey: alicePublicKey,
    clist: [
      // Alice pre-authorizes the TRANSFER capability
      { name: "coin.TRANSFER", args: ["alice", "bob", 50.0] },
      // Also authorizes GAS payment
      { name: "coin.GAS", args: [] }
    ]
  }],
  // ... other transaction fields
};
```

#### Step 2: Signature Verification
When Pact receives the transaction:
1. Verifies each signature against the transaction hash
2. If valid, makes the signer's capabilities available

#### Step 3: Code Execution
```pact
;; In coin.transfer function
(defun transfer (from:string to:string amount:decimal)
  ;; This will automatically succeed if Alice signed with matching capability
  (with-capability (TRANSFER from to amount)
    ;; ... transfer logic ...
  ))
```

#### Step 4: Capability Matching
Pact checks:
- Is `TRANSFER "alice" "bob" 50.0` in Alice's capability list?
- If YES: Capability is automatically granted
- If NO: Pact evaluates the capability body (which might fail)

### Practical Examples

#### Example 1: Simple Transfer
```javascript
// JavaScript client code
async function transferTokens() {
  const cmd = {
    networkId: "mainnet01",
    payload: {
      exec: {
        data: {},
        code: '(coin.transfer "alice" "bob" 25.0)'
      }
    },
    signers: [{
      pubKey: "alice-public-key-hex",
      clist: [
        // Must match exactly what the code will request
        { name: "coin.TRANSFER", args: ["alice", "bob", 25.0] }
      ]
    }],
    meta: {
      creationTime: Math.floor(Date.now() / 1000) - 10,
      ttl: 600,
      gasLimit: 10000,
      chainId: "0",
      gasPrice: 0.00000001,
      sender: "alice"
    },
    nonce: Date.now().toString()
  };
  
  // Sign and send transaction
  const signed = signTransaction(cmd, aliceKeyPair);
  const result = await sendTransaction(signed);
}
```

#### Example 2: Multi-Capability Transaction
```javascript
// Complex transaction with multiple capabilities
const cmd = {
  signers: [
    {
      pubKey: "alice-key",
      clist: [
        { name: "coin.TRANSFER", args: ["alice", "escrow", 100.0] },
        { name: "escrow.INITIATE", args: ["escrow-123"] }
      ]
    },
    {
      pubKey: "bob-key",
      clist: [
        { name: "escrow.ACCEPT", args: ["escrow-123"] }
      ]
    }
  ],
  // Transaction will use both Alice's and Bob's capabilities
};
```

#### Example 3: Scoped vs Unscoped Signatures

```javascript
// Scoped signature - only specific capabilities
{
  pubKey: "alice-key",
  clist: [
    { name: "coin.TRANSFER", args: ["alice", "bob", 50.0] }
  ]
}

// Unscoped signature - dangerous! Grants any capability the key can satisfy
{
  pubKey: "alice-key",
  clist: []  // Empty list = unscoped
}
```

### Important Rules

1. **Exact Matching**: Capability arguments must match EXACTLY
   ```javascript
   // This will NOT work if code requests 50.0
   clist: [{ name: "coin.TRANSFER", args: ["alice", "bob", 50] }]
   // Must be: args: ["alice", "bob", 50.0]
   ```

2. **Type Matching**: Arguments must have correct types
   ```javascript
   // Correct types
   args: ["alice", "bob", 50.0]  // string, string, decimal
   ```

3. **Module Qualification**: Always use full module names
   ```javascript
   // Wrong
   { name: "TRANSFER", args: [...] }
   
   // Correct
   { name: "coin.TRANSFER", args: [...] }
   ```

### Managed Capabilities and Signatures

For managed capabilities, the signature system is even more powerful:

```pact
(defcap TRANSFER (from:string to:string amount:decimal)
  @managed amount TRANSFER-mgr
  ...)
```

When you sign with a managed capability:
```javascript
{
  clist: [
    { name: "coin.TRANSFER", args: ["alice", "bob", 100.0] }
  ]
}
```

This pre-installs the capability with 100.0 as the managed amount. The code can then request portions:
```pact
;; This will succeed if 30.0 <= 100.0
(with-capability (TRANSFER "alice" "bob" 30.0) ...)
;; And then maybe another 50.0
(with-capability (TRANSFER "alice" "bob" 50.0) ...)
;; But this would fail (total would exceed 100.0)
(with-capability (TRANSFER "alice" "bob" 25.0) ...)
```

### Security Best Practices

1. **Always Scope Your Signatures**
   ```javascript
   // Good - specific capabilities
   clist: [{ name: "coin.TRANSFER", args: ["alice", "bob", 50.0] }]
   
   // Bad - unscoped
   clist: []
   ```

2. **Match Arguments Precisely**
   ```javascript
   // If your code does this:
   code: '(coin.transfer "alice" "bob" (+ 25.0 25.0))'
   
   // Your capability must be:
   clist: [{ name: "coin.TRANSFER", args: ["alice", "bob", 50.0] }]
   ```

3. **Use Read-Only for Queries**
   ```javascript
   // For read-only operations, don't include capabilities
   {
     payload: {
       exec: {
         code: '(coin.get-balance "alice")'  // No capabilities needed
       }
     },
     signers: []  // Can even omit signers for read-only
   }
   ```

### Common Integration Patterns

#### Pattern 1: Web Wallet Integration
```javascript
async function transferWithWallet(from, to, amount) {
  // Prepare capability request
  const caps = [
    { name: "coin.TRANSFER", args: [from, to, amount] },
    { name: "coin.GAS", args: [] }
  ];
  
  // Request signature from wallet (e.g., Chainweaver)
  const signed = await wallet.sign({
    pactCode: `(coin.transfer "${from}" "${to}" ${amount})`,
    caps: caps,
    // ... other fields
  });
  
  return submitTransaction(signed);
}
```

#### Pattern 2: Server-Side Signing
```javascript
// Node.js backend
const { sign } = require('@kadena/client');

function createTransferCommand(from, to, amount, keyPair) {
  const caps = [
    { 
      name: "coin.TRANSFER", 
      args: [from, to, parseFloat(amount)] 
    }
  ];
  
  return sign({
    pactCode: `(coin.transfer "${from}" "${to}" ${amount})`,
    caps,
    sender: from,
    chainId: "0",
    // ... meta fields
  }, keyPair);
}
```

## Summary

Capabilities are Pact's way of implementing fine-grained, composable permissions:

- **Simple capabilities**: Basic yes/no permissions
- **Managed capabilities**: Track and limit resource usage
- **Event capabilities**: Automatic blockchain event emission
- **Capability composition**: Build complex auth from simple pieces
- **Transaction integration**: Pre-authorized via signed capability lists

They work through a complete lifecycle of:
1. Definition (`defcap`) - In smart contract code
2. Transaction signing - Client includes in capability list
3. Signature verification - Blockchain verifies signatures
4. Automatic granting - Matching capabilities are pre-authorized
5. Acquisition (`with-capability`) - Code requests the capability
6. Enforcement (`require-capability`) - Functions check capability is active
7. Automatic cleanup - Capability removed when scope exits

The crucial insight is that capabilities bridge the gap between off-chain authorization (signatures) and on-chain execution (smart contract logic), providing a secure, flexible authorization system that's unique to Pact.

By mastering capabilities, you can build secure, flexible smart contracts with clear authorization logic that's easy to audit and understand.
