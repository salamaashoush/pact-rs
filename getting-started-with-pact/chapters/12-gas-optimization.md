# Chapter 12: Gas Optimization and Performance

## Introduction

Gas in Pact represents computational cost and serves as a fundamental security mechanism to prevent infinite loops and resource exhaustion attacks. Understanding gas optimization is crucial for building efficient smart contracts that can operate within blockchain gas limits while minimizing transaction costs.

## Understanding Gas in Pact

### Gas Fundamentals

Gas in Pact measures computational complexity across multiple dimensions:
- **CPU cycles** - Processing time for operations
- **Memory usage** - Data structure manipulation costs
- **Database I/O** - Persistent storage operations
- **Network resources** - Cross-chain communication costs

From the Haskell implementation in `/pact/Pact/Core/Gas/Types.hs`, gas is measured in hierarchical units:

```haskell
-- Gas units hierarchy
newtype Gas = Gas Natural        -- 1 Gas = 1000 MilliGas
newtype MilliGas = MilliGas Natural  -- Fine-grained unit (≈2.5 nanoseconds)
newtype GasLimit = GasLimit Gas      -- Maximum gas per transaction
```

### Gas Cost Categories

Based on `/pact/Pact/Core/Gas/TableGasModel.hs`, operations fall into distinct cost categories:

#### 1. Basic Operations (Low Cost: ~100-500 MilliGas)
```pact
;; Arithmetic operations
(+ 42 58)          ;; ~125 MilliGas
(* 10 20)          ;; ~130 MilliGas  
(length [1 2 3])   ;; ~801 MilliGas
```

#### 2. Data Structure Operations (Medium Cost: ~1K-10K MilliGas)
```pact
;; List operations scale with size
(map (+ 1) [1 2 3 4 5])           ;; ~5K MilliGas
(filter (> 10) [5 15 25])         ;; ~3K MilliGas
(fold (+) 0 [1 2 3 4 5])          ;; ~4K MilliGas
```

#### 3. Database Operations (High Cost: 2.5K-40M+ MilliGas)
```pact
;; Database I/O has heavy penalties
(read accounts "alice")            ;; ~2,500 + data size
(write accounts "bob" {...})       ;; ~25,000 + data size
(select accounts (where 'active (= true)))  ;; ~40,000,000 base cost!
```

#### 4. Cryptographic Operations (Variable Cost: 100-12M MilliGas)
```pact
;; Crypto operations vary widely
(hash "data")                      ;; ~100 MilliGas
(verify-signature key sig msg)     ;; ~100K MilliGas
(pairing-check points)             ;; ~12M MilliGas
```

## Gas Optimization Strategies

### 1. Database Operation Optimization

Database operations are the most expensive in Pact. The gas model heavily penalizes them to reflect real I/O costs:

#### Minimize Database Reads

```pact
;; BAD: Multiple reads
(defun inefficient-balance-check (account:string)
  (let ((balance (at 'balance (read accounts account)))
        (status (at 'status (read accounts account)))     ;; Redundant read!
        (guard (at 'guard (read accounts account))))      ;; Another redundant read!
    (and (> balance 0.0) (= status "active"))))

;; GOOD: Single read with destructuring
(defun efficient-balance-check (account:string)
  (with-read accounts account { "balance" := bal, "status" := stat }
    (and (> bal 0.0) (= stat "active"))))
```

#### Batch Database Operations

```pact
;; BAD: Individual operations in loop
(defun inefficient-batch-update (accounts:[string] new-status:string)
  (map (lambda (account)
         (update accounts account { "status": new-status }))  ;; 25K+ gas each
       accounts))

;; BETTER: Batch with single transaction context
(defun efficient-batch-update (accounts:[string] new-status:string)
  (fold (lambda (acc account)
          (update accounts account { "status": new-status })
          (+ acc 1))
        0
        accounts))

;; BEST: Use single write operation when possible
(defun optimal-status-change (account:string new-status:string additional-data:object)
  (with-read accounts account current-data
    (write accounts account (+ current-data { "status": new-status } additional-data))))
```

#### Avoid Expensive Select Operations

```pact
;; EXTREMELY EXPENSIVE: select operation (40M+ MilliGas base cost)
(defun expensive-query ()
  (select accounts (where 'balance (> 100.0))))  ;; Avoid if possible!

;; BETTER: Maintain indexes manually
(defschema account-index
  account:string
  balance-tier:string)

(deftable account-indexes:{account-index})

(defun maintain-balance-index (account:string new-balance:decimal)
  (let ((tier (if (> new-balance 100.0) "high" "low")))
    (write account-indexes account {
      "account": account,
      "balance-tier": tier
    })))

;; Query the index instead
(defun efficient-high-balance-accounts ()
  (select account-indexes (where 'balance-tier (= "high"))))
```

### 2. Algorithmic Optimization

#### Choose Efficient Algorithms

```pact
;; BAD: O(n²) nested loops
(defun inefficient-matching (buyers:[string] sellers:[string])
  (fold (lambda (matches buyer)
          (+ matches (filter (lambda (seller) 
                               (can-match buyer seller))  ;; Called n×m times
                             sellers)))
        []
        buyers))

;; GOOD: O(n) preprocessing + O(n) matching  
(defun efficient-matching (buyers:[string] sellers:[string])
  ;; Pre-process into lookup structure
  (let ((seller-map (fold (lambda (acc seller)
                            (+ acc { seller: (get-seller-criteria seller) }))
                          {}
                          sellers)))
    ;; Single pass matching
    (fold (lambda (matches buyer)
            (let ((criteria (get-buyer-criteria buyer))
                  (compatible-sellers (filter-compatible-sellers seller-map criteria)))
              (+ matches compatible-sellers)))
          []
          buyers)))
```

#### Optimize Data Structures

```pact
;; BAD: Linear search in lists
(defun find-user-inefficient (users:[object] target-id:string)
  (let ((found (filter (lambda (user) (= (at 'id user) target-id)) users)))
    (if (> (length found) 0) (at 0 found) {})))

;; GOOD: Use objects as hash maps for O(1) lookup
(defschema user-registry
  users:object)  ;; Object used as hash map

(defun find-user-efficient (target-id:string)
  (with-read user-registry "main" { "users" := user-map }
    (at target-id user-map)))
```

### 3. Integer and Arithmetic Optimization

Pact's gas model accounts for integer operation complexity based on operand size:

```pact
;; Gas cost scales with operand bit size
(defun optimize-arithmetic ()
  ;; GOOD: Use smaller integers when possible
  (let ((small-calc (+ 100 200)))        ;; Low gas cost
    
    ;; BAD: Unnecessary large integer operations
    (let ((large-calc (+ 10000000000000000000 20000000000000000000)))  ;; Higher gas
      
      ;; OPTIMIZATION: Use scaling for precision instead of large integers
      (let ((scaled-result (round (/ large-calc 1000000000) 6)))  ;; Reduce precision
        scaled-result))))

;; Optimize exponentiation
(defun efficient-power-calculation (base:decimal exponent:integer)
  ;; GOOD: Use built-in ^ operator for small exponents
  (if (< exponent 10)
      (^ base exponent)
      ;; For large exponents, consider approximation or iteration limits
      (fold (lambda (acc _) (* acc base)) 1.0 (enumerate 1 exponent))))
```

### 4. Memory and Data Structure Optimization

#### Minimize Object Field Access

```pact
;; BAD: Repeated field access
(defun inefficient-calculation (trade:object)
  (let ((amount (at 'amount trade))
        (price (at 'price trade))
        (fee-rate (at 'fee-rate trade)))
    (+ (* (at 'amount trade) (at 'price trade))     ;; Redundant access
       (* (at 'amount trade) (at 'fee-rate trade))))) ;; More redundant access

;; GOOD: Destructure once
(defun efficient-calculation (trade:object)
  (bind trade { "amount" := amt, "price" := price, "fee-rate" := fee }
    (+ (* amt price) (* amt fee))))
```

#### Optimize String Operations

```pact
;; BAD: Inefficient string concatenation in loop
(defun build-report-inefficient (items:[object])
  (fold (lambda (report item)
          (+ report (format "Item: {}, Value: {}\n" [(at 'name item) (at 'value item)])))
        ""
        items))

;; GOOD: Build components first, then concatenate
(defun build-report-efficient (items:[object])
  (let ((lines (map (lambda (item)
                      (format "Item: {}, Value: {}" [(at 'name item) (at 'value item)]))
                    items)))
    (fold (lambda (acc line) (+ acc line "\n")) "" lines)))
```

### 5. Function Call Optimization

#### Minimize Capability Overhead

```pact
;; BAD: Capability granted/checked repeatedly
(defun inefficient-multi-transfer (transfers:[object])
  (map (lambda (xfer)
         (with-capability (TRANSFER (at 'from xfer) (at 'to xfer) (at 'amount xfer))
           (transfer-impl (at 'from xfer) (at 'to xfer) (at 'amount xfer))))
       transfers))

;; GOOD: Batch capability for multiple operations
(defcap BATCH_TRANSFER:bool (transfers:[object])
  @doc "Capability for batch transfers"
  (map (lambda (xfer)
         (enforce-guard (account-guard (at 'from xfer))))
       transfers))

(defun efficient-multi-transfer (transfers:[object])
  (with-capability (BATCH_TRANSFER transfers)
    (map (lambda (xfer)
           (transfer-impl (at 'from xfer) (at 'to xfer) (at 'amount xfer)))
         transfers)))
```

#### Reduce Function Call Depth

```pact
;; BAD: Deep recursion with function call overhead
(defun inefficient-sum (numbers:[decimal])
  (if (= 0 (length numbers))
      0.0
      (+ (at 0 numbers) (inefficient-sum (drop 1 numbers)))))

;; GOOD: Use built-in fold (optimized internally)
(defun efficient-sum (numbers:[decimal])
  (fold (+) 0.0 numbers))
```

## Performance Measurement and Monitoring

### Gas Usage Analysis

```pact
(module gas-analyzer GOVERNANCE
  @doc "Tools for analyzing gas consumption"
  
  (defun measure-operation-gas:decimal (operation:string test-data:object)
    @doc "Measure gas consumption of an operation"
    (let ((start-gas (chain-data 'gas-used))
          (result (execute-operation operation test-data))
          (end-gas (chain-data 'gas-used)))
      (- end-gas start-gas)))
  
  (defun benchmark-alternatives:[object] (alternatives:[object] test-data:object)
    @doc "Compare gas consumption of different implementations"
    (map (lambda (alt)
           { "name": (at 'name alt),
             "gas-used": (measure-operation-gas (at 'operation alt) test-data),
             "result": (execute-operation (at 'operation alt) test-data) })
         alternatives))
  
  (defun profile-function:object (function-name:string iterations:integer test-inputs:[object])
    @doc "Profile function performance across multiple inputs"
    (let ((measurements (map (lambda (input)
                               (measure-operation-gas function-name input))
                             test-inputs)))
      { "function": function-name,
        "iterations": iterations,
        "min-gas": (fold min 999999999.0 measurements),
        "max-gas": (fold max 0.0 measurements),
        "avg-gas": (/ (fold (+) 0.0 measurements) (length measurements)),
        "total-gas": (fold (+) 0.0 measurements) }))
)
```

### Performance Testing Framework

```pact
(module performance-tests GOVERNANCE
  @doc "Performance testing utilities"
  
  (defschema performance-test
    name:string
    operation:string
    input-size:integer
    expected-complexity:string
    gas-limit:decimal)
    
  (deftable performance-benchmarks:{performance-test})
  
  (defun run-performance-suite:[object] (test-suite:[object])
    @doc "Run comprehensive performance test suite"
    (map run-single-performance-test test-suite))
  
  (defun run-single-performance-test:object (test:object)
    @doc "Run individual performance test"
    (bind test { "name" := name, "operation" := op, "input-size" := size }
      (let ((test-data (generate-test-data op size))
            (gas-used (measure-operation-gas op test-data)))
        { "test-name": name,
          "input-size": size,
          "gas-used": gas-used,
          "gas-per-item": (/ gas-used size),
          "passed": (< gas-used (at 'gas-limit test)) })))
  
  (defun generate-test-data:object (operation:string size:integer)
    @doc "Generate test data for performance testing"
    (cond
      ((= operation "list-processing")
       { "list": (enumerate 1 size) })
      ((= operation "object-lookup")
       { "object": (fold (lambda (acc i) (+ acc { (format "key{}" [i]): i })) {} (enumerate 1 size)) })
      ((= operation "database-operations")
       { "accounts": (map (lambda (i) (format "account{}" [i])) (enumerate 1 size)) })
      {}))
)
```

## Gas Limit Management

### Transaction Size Planning

```pact
(module transaction-planner GOVERNANCE
  @doc "Tools for planning transaction gas usage"
  
  (defconst MAX_TRANSACTION_GAS:decimal 150000.0
    @doc "Maximum gas per transaction on Kadena")
  
  (defconst SAFETY_MARGIN:decimal 0.9
    @doc "Safety margin for gas estimation (90%)")
  
  (defun estimate-batch-size:integer (operation-gas:decimal)
    @doc "Estimate maximum batch size for operation"
    (floor (/ (* MAX_TRANSACTION_GAS SAFETY_MARGIN) operation-gas)))
  
  (defun plan-batch-execution:[object] (operations:[object] estimated-gas-per-op:decimal)
    @doc "Plan batch execution to stay within gas limits"
    (let ((max-batch-size (estimate-batch-size estimated-gas-per-op))
          (total-ops (length operations)))
      (if (<= total-ops max-batch-size)
          [{ "batch": operations, "estimated-gas": (* total-ops estimated-gas-per-op) }]
          ;; Split into multiple batches
          (create-batches operations max-batch-size estimated-gas-per-op))))
  
  (defun create-batches:[object] (operations:[object] batch-size:integer gas-per-op:decimal)
    @doc "Split operations into gas-efficient batches"
    (let ((num-batches (ceiling (/ (length operations) batch-size))))
      (map (lambda (batch-index)
             (let ((start-idx (* batch-index batch-size))
                   (end-idx (min (+ start-idx batch-size) (length operations))))
               { "batch-index": batch-index,
                 "operations": (take (- end-idx start-idx) (drop start-idx operations)),
                 "estimated-gas": (* (- end-idx start-idx) gas-per-op) }))
           (enumerate 0 (- num-batches 1)))))
)
```

### Gas-Efficient Design Patterns

#### 1. Lazy Evaluation Pattern

```pact
(module lazy-evaluation GOVERNANCE
  @doc "Lazy evaluation for gas efficiency"
  
  (defschema lazy-computation
    computed:bool
    result:object
    computation-data:object)
    
  (deftable lazy-cache:{lazy-computation})
  
  (defun lazy-compute:object (computation-id:string computation-fn:string data:object)
    @doc "Compute value only when needed"
    (with-default-read lazy-cache computation-id
      { "computed": false, "result": {}, "computation-data": data }
      { "computed" := is-computed, "result" := cached-result }
      (if is-computed
          cached-result  ;; Return cached result (low gas)
          ;; Compute and cache (high gas, but only once)
          (let ((result (apply-computation computation-fn data)))
            (write lazy-cache computation-id {
              "computed": true,
              "result": result,
              "computation-data": data
            })
            result))))
)
```

#### 2. Incremental Processing Pattern

```pact
(module incremental-processor GOVERNANCE
  @doc "Process large datasets incrementally"
  
  (defschema processing-state
    total-items:integer
    processed-count:integer
    current-batch:integer
    result:object
    status:string)
    
  (deftable processing-jobs:{processing-state})
  
  (defun start-incremental-job:string (job-id:string items:[object] batch-size:integer)
    @doc "Start incremental processing job"
    (insert processing-jobs job-id {
      "total-items": (length items),
      "processed-count": 0,
      "current-batch": 0,
      "result": { "processed": [], "summary": {} },
      "status": "in-progress"
    })
    (process-next-batch job-id items batch-size))
  
  (defun process-next-batch:string (job-id:string items:[object] batch-size:integer)
    @doc "Process next batch of items"
    (with-read processing-jobs job-id {
      "processed-count" := processed,
      "current-batch" := batch-num,
      "result" := current-result
    }
      (let ((start-idx processed)
            (end-idx (min (+ processed batch-size) (length items)))
            (batch-items (take (- end-idx start-idx) (drop start-idx items))))
        
        ;; Process current batch
        (let ((batch-result (process-batch batch-items)))
          ;; Update job state
          (update processing-jobs job-id {
            "processed-count": end-idx,
            "current-batch": (+ batch-num 1),
            "result": (merge-results current-result batch-result),
            "status": (if (= end-idx (length items)) "completed" "in-progress")
          })
          
          (if (= end-idx (length items))
              "Job completed"
              "Batch processed, more remain")))))
)
```

## Real-World Optimization Examples

### Optimized Token Transfer

```pact
(module optimized-token GOVERNANCE
  @doc "Gas-optimized token implementation"
  
  ;; Optimized for minimal gas usage
  (defun efficient-transfer:string (from:string to:string amount:decimal)
    @doc "Gas-optimized transfer function"
    
    ;; Single capability check for entire operation
    (with-capability (TRANSFER from to amount)
      
      ;; Single read for from account with all needed fields
      (with-read accounts from { 
        "balance" := from-balance,
        "guard" := from-guard
      }
        ;; Validation (cheap operations first)
        (enforce (>= from-balance amount) "Insufficient funds")
        
        ;; Single read/write for to account (create if needed)
        (with-default-read accounts to 
          { "balance": 0.0, "guard": (read-keyset "to-guard") }
          { "balance" := to-balance }
          
          ;; Batch update both accounts in single transaction
          (update accounts from { "balance": (- from-balance amount) })
          (write accounts to { 
            "balance": (+ to-balance amount),
            "guard": (at 'guard (read accounts to))
          })))))
  
  ;; Gas-efficient batch transfer
  (defun batch-transfer:string (transfers:[object])
    @doc "Process multiple transfers efficiently"
    
    ;; Pre-validate all transfers before any state changes
    (let ((validation-results (map validate-transfer-requirements transfers)))
      (enforce (all-true validation-results) "Transfer validation failed")
      
      ;; Group transfers by account to minimize database operations
      (let ((grouped-transfers (group-transfers-by-account transfers)))
        ;; Process each account's net change once
        (map process-account-changes grouped-transfers)
        "Batch transfer completed")))
)
```

### Optimized Order Book

```pact
(module optimized-orderbook GOVERNANCE
  @doc "Gas-optimized order book implementation"
  
  ;; Use efficient data structures
  (defschema price-level
    price:decimal
    total-quantity:decimal
    order-count:integer)
    
  (deftable price-levels:{price-level})  ;; Aggregate by price
  (deftable order-details:{order})       ;; Individual orders
  
  (defun place-order-optimized:string (order:object)
    @doc "Gas-optimized order placement"
    
    (bind order { "price" := price, "quantity" := qty }
      ;; Update price level aggregate (single write)
      (with-default-read price-levels (format "{}" [price])
        { "total-quantity": 0.0, "order-count": 0 }
        { "total-quantity" := current-qty, "order-count" := current-count }
        
        (write price-levels (format "{}" [price]) {
          "price": price,
          "total-quantity": (+ current-qty qty),
          "order-count": (+ current-count 1)
        }))
      
      ;; Store individual order (single write)
      (write order-details (at 'id order) order)))
  
  ;; Efficient order matching
  (defun match-orders-optimized:string (buy-order:object sell-order:object)
    @doc "Gas-optimized order matching"
    
    ;; Calculate match details first (cheap operations)
    (let ((match-qty (min (at 'quantity buy-order) (at 'quantity sell-order)))
          (match-price (at 'price sell-order)))  ;; Use sell price
      
      ;; Single batch update for both orders
      (batch-update-orders buy-order sell-order match-qty)
      
      ;; Single price level update
      (update-price-level-after-match match-price match-qty)
      
      (format "Matched {} at price {}" [match-qty match-price])))
)
```

## Summary

Gas optimization in Pact requires understanding the cost model and applying strategic optimizations:

**High-Impact Optimizations:**
1. **Minimize database operations** - Heaviest gas cost (2.5K-40M+ MilliGas)
2. **Batch operations** - Reduce transaction overhead
3. **Efficient algorithms** - Choose O(1) over O(n) when possible
4. **Smart data structures** - Objects for lookups, careful list usage

**Cost-Aware Design:**
1. **Database reads**: 2,500 + data size MilliGas
2. **Database writes**: 25,000 + data size MilliGas
3. **Select operations**: 40,000,000+ MilliGas base cost
4. **Basic arithmetic**: ~100-500 MilliGas

**Key Principles:**
1. **Measure first** - Profile before optimizing
2. **Database operations are expensive** - Minimize and batch
3. **Algorithm choice matters** - O(n²) vs O(n) has real cost
4. **Plan for gas limits** - Design around 150K gas limit per transaction
5. **Use built-ins** - Optimized internally for better performance

Gas optimization is critical for production Pact applications, enabling complex smart contracts to operate efficiently within blockchain constraints.

## Exercises

1. Optimize a token contract to minimize gas usage per transfer
2. Design a gas-efficient batch processing system for large datasets
3. Compare gas costs of different sorting algorithms in Pact
4. Build a gas profiling tool for performance analysis
5. Create an order book that minimizes gas per trade execution

## References

- Gas types: `/pact/Pact/Core/Gas/Types.hs`
- Gas model: `/pact/Pact/Core/Gas/TableGasModel.hs`
- Gas testing: `/pact-tests/Pact/Core/Test/GasGolden.hs`
- Gas benchmarks: `/gasmodel/Pact/Core/GasModel/BuiltinsGas.hs`