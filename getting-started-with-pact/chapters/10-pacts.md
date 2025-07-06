# Chapter 10: Pacts - Multi-Step Transactions

## Introduction

Pacts are Pact's unique approach to handling multi-step transactions that span time, participants, or even different blockchains. Unlike traditional smart contracts that execute atomically, pacts enable complex workflows with guaranteed sequential execution, built-in rollback mechanisms, and cross-chain coordination.

## Understanding Pacts

### What is a Pact?

A pact is a multi-step transaction where each step:
- Executes independently with its own gas and transaction context
- Can be executed by different parties
- Maintains state between steps in the blockchain database  
- Can yield data to other chains or resume from yielded data
- Supports rollback for error recovery

```pact
(defpact simple-workflow (user:string value:decimal)
  @doc "Simple two-step workflow"
  
  ;; Step 0: Initialize
  (step
    (enforce (> value 0.0) "Value must be positive")
    (insert workflow-state user { "value": value, "status": "initialized" })
    (format "Workflow initialized for {} with value {}" [user value]))
  
  ;; Step 1: Complete
  (step
    (update workflow-state user { "status": "completed" })
    (format "Workflow completed for {}" [user])))
```

### Pact Architecture

Based on the Haskell implementation in `/pact/Pact/Core/DefPacts/Types.hs`, pacts maintain execution state through:

```haskell
data DefPactExec = DefPactExec
  { _peStepCount :: Int           -- Total number of steps
  , _peYield :: Maybe Yield       -- Cross-chain yield data
  , _peStep :: Int               -- Current step number
  , _peDefPactId :: DefPactId    -- Unique identifier
  , _peContinuation :: DefPactContinuation  -- Next step info
  , _peStepHasRollback :: Bool   -- Rollback support
  , _peNestedDefPactExec :: Map DefPactId NestedDefPactExec  -- Nested pacts
  }
```

## Creating Pacts

### Basic Pact Structure

```pact
(module escrow GOVERNANCE
  @doc "Escrow service with timeout and dispute resolution"
  
  (defschema escrow-account
    buyer:string
    seller:string
    amount:decimal
    created:time
    timeout:time
    status:string)
    
  (deftable escrows:{escrow-account})
  
  (defpact escrow-with-timeout (escrow-id:string buyer:string seller:string 
                               amount:decimal timeout-hours:integer)
    @doc "Escrow with automatic timeout"
    
    ;; Step 0: Create escrow
    (step
      (enforce (> amount 0.0) "Amount must be positive")
      (enforce (> timeout-hours 0) "Timeout must be positive")
      
      (let ((timeout-time (add-time (at 'block-time (chain-data)) (hours timeout-hours))))
        ;; Transfer funds to escrow
        (coin.transfer buyer escrow-id amount)
        
        ;; Record escrow details
        (insert escrows escrow-id {
          "buyer": buyer,
          "seller": seller,
          "amount": amount,
          "created": (at 'block-time (chain-data)),
          "timeout": timeout-time,
          "status": "active"
        })
        
        (format "Escrow {} created with {} timeout" [escrow-id timeout-time])))
    
    ;; Step 1: Release to seller (normal completion)
    (step-with-rollback
      ;; Normal completion
      (with-read escrows escrow-id { "status" := status, "amount" := amt }
        (enforce (= status "active") "Escrow not active")
        (coin.transfer escrow-id seller amt)
        (update escrows escrow-id { "status": "completed" })
        (format "Escrow {} released to seller" [escrow-id]))
      
      ;; Rollback: refund buyer on timeout
      (with-read escrows escrow-id { 
        "timeout" := timeout-time, 
        "amount" := amt,
        "buyer" := buyer-account 
      }
        (enforce (>= (at 'block-time (chain-data)) timeout-time) "Timeout not reached")
        (coin.transfer escrow-id buyer-account amt)
        (update escrows escrow-id { "status": "refunded" })
        (format "Escrow {} refunded to buyer" [escrow-id]))))
)
```

### Pact with Multiple Steps

```pact
(module supply-chain GOVERNANCE
  @doc "Multi-step supply chain tracking"
  
  (defschema shipment
    product:string
    origin:string
    destination:string
    carrier:string
    created:time
    current-location:string
    status:string
    checkpoints:[object])
    
  (deftable shipments:{shipment})
  
  (defpact track-shipment (shipment-id:string product:string origin:string 
                          destination:string carrier:string)
    @doc "Track product through supply chain"
    
    ;; Step 0: Create shipment
    (step
      (insert shipments shipment-id {
        "product": product,
        "origin": origin,
        "destination": destination,
        "carrier": carrier,
        "created": (at 'block-time (chain-data)),
        "current-location": origin,
        "status": "created",
        "checkpoints": []
      })
      (format "Shipment {} created" [shipment-id]))
    
    ;; Step 1: Pickup
    (step
      (with-read shipments shipment-id { 
        "checkpoints" := checkpoints,
        "origin" := pickup-location 
      }
        (let ((checkpoint {
          "location": pickup-location,
          "action": "pickup",
          "timestamp": (at 'block-time (chain-data)),
          "carrier": carrier
        })))
        (update shipments shipment-id {
          "status": "in-transit",
          "current-location": pickup-location,
          "checkpoints": (+ checkpoints [checkpoint])
        }))
      (format "Shipment {} picked up" [shipment-id]))
    
    ;; Step 2: Transit updates (can be called multiple times)
    (step
      (let ((new-location (read-string "location"))
            (checkpoint {
              "location": new-location,
              "action": "transit",
              "timestamp": (at 'block-time (chain-data)),
              "carrier": carrier
            }))
        (with-read shipments shipment-id { "checkpoints" := checkpoints }
          (update shipments shipment-id {
            "current-location": new-location,
            "checkpoints": (+ checkpoints [checkpoint])
          })))
      (format "Shipment {} location updated" [shipment-id]))
    
    ;; Step 3: Delivery
    (step-with-rollback
      ;; Normal delivery
      (with-read shipments shipment-id { 
        "checkpoints" := checkpoints,
        "destination" := dest 
      }
        (let ((checkpoint {
          "location": dest,
          "action": "delivered",
          "timestamp": (at 'block-time (chain-data)),
          "carrier": carrier
        })))
        (update shipments shipment-id {
          "status": "delivered",
          "current-location": dest,
          "checkpoints": (+ checkpoints [checkpoint])
        }))
      (format "Shipment {} delivered" [shipment-id])
      
      ;; Rollback: mark as failed
      (update shipments shipment-id { "status": "failed" })
      (format "Shipment {} delivery failed" [shipment-id])))
)
```

## Step Types and Rollbacks

### Basic Steps

```pact
(defpact simple-process (id:string)
  ;; Basic step - no rollback capability
  (step
    (insert process-log id { "status": "started", "time": (at 'block-time (chain-data)) })
    "Process started")
  
  (step
    (update process-log id { "status": "completed", "time": (at 'block-time (chain-data)) })
    "Process completed"))
```

### Steps with Rollback

```pact
(defpact robust-process (id:string amount:decimal)
  ;; Step with rollback capability
  (step-with-rollback
    ;; Main execution
    (coin.transfer "source" "temp" amount)
    (insert temp-holdings id { "amount": amount, "time": (at 'block-time (chain-data)) })
    "Funds moved to temporary holding"
    
    ;; Rollback handler
    (coin.transfer "temp" "source" amount)
    (update temp-holdings id { "status": "rolled-back" })
    "Funds returned to source"))
```

### Conditional Step Execution

```pact
(defpact conditional-workflow (id:string condition:bool)
  (step
    (insert workflow-state id { "condition": condition })
    "Initial state recorded")
  
  (step
    (with-read workflow-state id { "condition" := cond }
      (if cond
          ;; Condition true path
          (update workflow-state id { "result": "success" })
          ;; Condition false path
          (update workflow-state id { "result": "skipped" })))
    "Conditional logic executed"))
```

## Cross-Chain Pacts

### Understanding Yield and Resume

Cross-chain pacts use `yield` and `resume` for chain-to-chain communication:

```pact
(module cross-chain-transfer GOVERNANCE
  @doc "Cross-chain token transfer using yield/resume"
  
  (defpact transfer-crosschain (from:string to:string amount:decimal target-chain:string)
    @doc "Transfer tokens across chains"
    
    ;; Step 0: Burn on source chain
    (step
      (enforce (> amount 0.0) "Amount must be positive")
      (coin.transfer from "burn-account" amount)
      
      ;; Yield data to target chain
      (yield {
        "from": from,
        "to": to,
        "amount": amount,
        "source-chain": (chain-data 'chain-id)
      } target-chain)
      
      (format "Burned {} tokens for cross-chain transfer" [amount]))
    
    ;; Step 1: Mint on target chain
    (step
      ;; Resume with yielded data
      (resume { "from" := from, "to" := to, "amount" := amt, "source-chain" := source }
        ;; Validate the yield
        (enforce (= source (read-string "expected-source")) "Invalid source chain")
        
        ;; Mint tokens on target chain
        (coin.mint to amt)
        (insert cross-chain-transfers (pact-id) {
          "from": from,
          "to": to,
          "amount": amt,
          "source-chain": source,
          "target-chain": (chain-data 'chain-id),
          "completed": (at 'block-time (chain-data))
        })
        
        (format "Minted {} tokens from cross-chain transfer" [amt]))))
```

### SPV Proof Integration

```pact
(module cross-chain-escrow GOVERNANCE
  @doc "Cross-chain escrow with SPV verification"
  
  (defpact cross-chain-escrow (escrow-id:string buyer:string seller:string 
                              amount:decimal target-chain:string)
    
    ;; Step 0: Lock funds on source chain
    (step
      (coin.transfer buyer escrow-id amount)
      (insert escrow-locks escrow-id {
        "buyer": buyer,
        "seller": seller,
        "amount": amount,
        "target-chain": target-chain,
        "locked": (at 'block-time (chain-data))
      })
      
      ;; Yield to target chain with SPV data
      (yield {
        "escrow-id": escrow-id,
        "buyer": buyer,
        "seller": seller,
        "amount": amount,
        "lock-proof": (create-spv-proof escrow-id)
      } target-chain))
    
    ;; Step 1: Verify delivery on target chain
    (step
      (resume { "escrow-id" := eid, "delivery-proof" := proof }
        ;; Verify SPV proof of delivery
        (verify-spv-proof proof target-chain)
        
        ;; Release funds to seller
        (coin.transfer eid seller amount)
        (update escrow-locks eid { "status": "completed" })))
)
```

## Pact Execution and Continuation

### Pact Lifecycle

From the Haskell implementation, pact execution follows this pattern:

1. **Initialization** (`initPact`): Creates new DefPactExec state
2. **Step Execution** (`applyPact`): Executes current step
3. **State Persistence**: Saves execution state to database
4. **Continuation**: Waits for next transaction to continue
5. **Resume** (`resumePact`): Loads state and continues execution

### Manual Pact Continuation

```pact
;; Continue a pact to next step
(continue-pact (pact-id) false)  ;; Normal continuation

;; Rollback current step  
(continue-pact (pact-id) true)   ;; Rollback continuation
```

### Pact State Management

```pact
(module pact-manager GOVERNANCE
  @doc "Utilities for managing pact execution"
  
  (defun get-pact-status (pact-id:string)
    @doc "Get current pact execution status"
    (let ((pact-data (describe-pact pact-id)))
      {
        "pact-id": pact-id,
        "step": (at 'step pact-data),
        "step-count": (at 'step-count pact-data),
        "executed": (at 'executed pact-data),
        "continuation": (at 'continuation pact-data)
      }))
  
  (defun list-active-pacts ():[string]
    @doc "List all active pact IDs"
    ;; Implementation would query database for active pacts
    ;; This is a simplified example
    (select pact-registry ['pact-id] (where 'status (= "active"))))
  
  (defun force-rollback (pact-id:string reason:string)
    @doc "Force rollback of a pact"
    (with-capability (ADMIN)
      ;; Execute rollback
      (continue-pact pact-id true)
      
      ;; Log the reason
      (insert rollback-log pact-id {
        "reason": reason,
        "timestamp": (at 'block-time (chain-data)),
        "admin": (tx-sender)
      })))
)
```

## Advanced Pact Patterns

### 1. Nested Pacts

```pact
(module nested-pacts GOVERNANCE
  @doc "Example of pacts calling other pacts"
  
  (defpact parent-process (id:string)
    (step
      (insert parent-state id { "status": "started" })
      ;; Start child pact
      (child-process (format "child-{}" [id]))
      "Parent process started")
    
    (step
      ;; Parent continues after child completes
      (update parent-state id { "status": "completed" })
      "Parent process completed"))
  
  (defpact child-process (child-id:string)
    (step
      (insert child-state child-id { "status": "processing" })
      "Child process working")
    
    (step
      (update child-state child-id { "status": "done" })
      "Child process completed"))
)
```

### 2. Time-Based Pacts

```pact
(module time-based-auction GOVERNANCE
  @doc "Auction with time-based phases"
  
  (defpact timed-auction (auction-id:string item:string duration:integer)
    @doc "Auction with automatic phase transitions"
    
    ;; Step 0: Open auction
    (step
      (let ((end-time (add-time (at 'block-time (chain-data)) (seconds duration))))
        (insert auctions auction-id {
          "item": item,
          "start-time": (at 'block-time (chain-data)),
          "end-time": end-time,
          "status": "open",
          "highest-bid": 0.0,
          "highest-bidder": ""
        }))
      "Auction opened")
    
    ;; Step 1: Close auction (time-triggered)
    (step
      (with-read auctions auction-id { 
        "end-time" := end-time,
        "highest-bidder" := winner 
      }
        (enforce (>= (at 'block-time (chain-data)) end-time) "Auction still active")
        (update auctions auction-id { "status": "closed" })
        (if (!= winner "")
            (format "Auction won by {}" [winner])
            "Auction ended with no bids")))
)
```

### 3. Multi-Party Coordination

```pact
(module multi-party-contract GOVERNANCE
  @doc "Contract requiring multiple party approval"
  
  (defschema agreement
    parties:[string]
    signatures:[string]
    terms:object
    status:string)
    
  (deftable agreements:{agreement})
  
  (defpact multi-party-agreement (agreement-id:string parties:[string] terms:object)
    @doc "Agreement requiring all parties to sign"
    
    ;; Step 0: Create agreement
    (step
      (insert agreements agreement-id {
        "parties": parties,
        "signatures": [],
        "terms": terms,
        "status": "pending"
      })
      "Agreement created")
    
    ;; Step 1: Collect signatures (repeatable)
    (step
      (with-read agreements agreement-id { 
        "parties" := required-parties,
        "signatures" := current-sigs 
      }
        (let ((signer (tx-sender)))
          (enforce (contains signer required-parties) "Not authorized to sign")
          (enforce (not (contains signer current-sigs)) "Already signed")
          
          (let ((new-sigs (+ current-sigs [signer])))
            (update agreements agreement-id { "signatures": new-sigs })
            
            (if (= (length new-sigs) (length required-parties))
                ;; All signed - finalize
                (update agreements agreement-id { "status": "executed" })
                ;; More signatures needed
                (update agreements agreement-id { "status": "pending" }))))))
    
    ;; Step 2: Execute agreement
    (step
      (with-read agreements agreement-id { "status" := status }
        (enforce (= status "executed") "Agreement not fully signed")
        ;; Execute the agreement terms
        (execute-terms terms)
        "Agreement executed"))
)
```

## Testing Pacts

### REPL Testing

```pact
;; test-pacts.repl
(begin-tx "Test basic pact")

;; Start pact
(escrow-with-timeout "test-escrow" "alice" "bob" 100.0 24)
(expect "Pact created" "Escrow test-escrow created" (continue-pact (pact-id) false))

;; Check pact status
(let ((status (describe-pact (pact-id))))
  (expect "Pact at step 1" 1 (at 'step status))
  (expect "Pact has 2 steps" 2 (at 'step-count status)))

;; Continue to completion
(continue-pact (pact-id) false)
(expect "Escrow completed" "Escrow test-escrow released to seller" (continue-pact (pact-id) false))

(commit-tx)

(begin-tx "Test pact rollback")

;; Start pact that will be rolled back
(escrow-with-timeout "rollback-test" "alice" "bob" 50.0 1)  ;; 1 hour timeout
(continue-pact (pact-id) false)

;; Fast forward time to trigger timeout
(env-time (add-time (at 'block-time (chain-data)) (hours 2)))

;; Execute rollback
(continue-pact (pact-id) true)
(expect "Rollback executed" "Escrow rollback-test refunded to buyer" (continue-pact (pact-id) true))

(commit-tx)
```

### Property Testing for Pacts

```pact
(defun test-pact-properties ()
  @doc "Test properties that all pacts should satisfy"
  
  ;; Property: Pacts execute in order
  (let ((pact-id (create-test-pact)))
    (expect "Step 0 before step 1" 0 (at 'step (describe-pact pact-id)))
    (continue-pact pact-id false)
    (expect "Step 1 after continuation" 1 (at 'step (describe-pact pact-id))))
  
  ;; Property: Rollback undoes step effects
  (let ((initial-balance (coin.get-balance "test-account"))
        (pact-id (start-transfer-pact "test-account" "temp" 100.0)))
    (continue-pact pact-id true)  ;; Rollback
    (expect "Balance restored" initial-balance (coin.get-balance "test-account"))))
```

## Gas and Performance

### Pact Gas Considerations

Each pact step is a separate transaction with its own gas limit:

```pact
;; Gas-efficient pact design
(defpact efficient-batch-process (items:[string])
  ;; Step 0: Initialize with minimal data
  (step
    (insert batch-state (pact-id) { "total": (length items), "processed": 0 })
    "Batch initialized")
  
  ;; Step 1: Process in chunks to manage gas
  (step
    (let ((chunk-size 10)  ;; Process 10 items per step
          (processed (at 'processed (read batch-state (pact-id)))))
      (map (process-item) (take chunk-size (drop processed items)))
      (update batch-state (pact-id) { "processed": (+ processed chunk-size) })))
)
```

### Optimizing Cross-Chain Pacts

```pact
;; Minimize cross-chain yield data
(defpact optimized-cross-chain (id:string amount:decimal)
  (step
    ;; Only yield essential data
    (yield { "id": id, "amount": amount } "target-chain"))
  
  (step
    ;; Minimal resume validation
    (resume { "id" := resume-id, "amount" := resume-amount }
      (process-transfer resume-id resume-amount))))
```

## Error Handling and Recovery

### Pact Error Patterns

```pact
(module error-handling GOVERNANCE
  @doc "Pact error handling patterns"
  
  (defpact robust-workflow (id:string)
    ;; Step with comprehensive error handling
    (step-with-rollback
      ;; Main execution with validation
      (enforce (!= id "") "ID cannot be empty")
      (enforce (not (exists-in-db? id)) "ID already exists")
      
      (insert workflow-data id { "status": "processing" })
      "Workflow started successfully"
      
      ;; Rollback with cleanup
      (with-default-read workflow-data id
        { "status": "none" }
        { "status" := current-status }
        (if (!= current-status "none")
            (update workflow-data id { "status": "error", "error": "Step failed" })
            "No cleanup needed")
        "Rollback completed"))
)
```

## Summary

Pacts provide Pact's unique multi-step transaction capabilities:

- **Sequential Execution**: Steps execute in guaranteed order
- **State Persistence**: Execution state maintained between steps
- **Rollback Support**: Built-in error recovery mechanisms
- **Cross-Chain Coordination**: Yield/resume for multi-chain workflows
- **Flexible Participation**: Different parties can execute different steps
- **Time-Based Logic**: Steps can be time-triggered or conditional

Key principles:
1. **Each step is independent** - executed in separate transactions
2. **State persists automatically** - between steps in blockchain database
3. **Rollbacks are explicit** - must be designed into step structure
4. **Cross-chain requires yield/resume** - for chain-to-chain communication
5. **Gas per step** - each step has its own gas limit and costs

Pacts enable complex workflows that traditional smart contracts cannot handle, making them ideal for escrow, supply chain, cross-chain operations, and any process requiring guaranteed sequential execution.

## Exercises

1. Create a multi-party voting pact with time-based phases
2. Implement a cross-chain token swap using yield/resume
3. Build a supply chain tracking pact with quality checkpoints
4. Design an auction pact with automatic bidding phases
5. Create a subscription service pact with recurring payments

## References

- Pact types: `/pact/Pact/Core/DefPacts/Types.hs`
- Execution engine: `/pact/Pact/Core/IR/Eval/CEK/Evaluator.hs`
- Builtin functions: `/pact/Pact/Core/IR/Eval/CEK/CoreBuiltin.hs`
- Test suite: `/pact-tests/Pact/Core/Test/PactContinuationTest.hs`