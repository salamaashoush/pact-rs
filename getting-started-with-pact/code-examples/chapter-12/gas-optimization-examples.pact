;; gas-optimization-examples.pact
;; Comprehensive examples of gas optimization techniques

(module gas-optimization-examples GOVERNANCE
  @doc "Examples demonstrating gas optimization techniques"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'admin))
  
  ;; Test data structures
  (defschema account
    balance:decimal
    guard:guard
    status:string
    last-update:time)
  
  (deftable accounts:{account})
  
  (defschema transaction-log
    from:string
    to:string
    amount:decimal
    timestamp:time)
  
  (deftable transactions:{transaction-log})
  
  ;; ==========================================================================
  ;; DATABASE OPTIMIZATION EXAMPLES
  ;; ==========================================================================
  
  ;; BAD: Multiple database reads
  (defun inefficient-transfer:string (from:string to:string amount:decimal)
    @doc "INEFFICIENT: Multiple database reads waste gas"
    
    ;; BAD: Three separate reads for same account
    (let ((from-balance (at 'balance (read accounts from)))      ;; Read 1: ~2,500 gas
          (from-status (at 'status (read accounts from)))       ;; Read 2: ~2,500 gas  
          (from-guard (at 'guard (read accounts from))))        ;; Read 3: ~2,500 gas
      
      ;; More inefficient reads
      (let ((to-balance (at 'balance (read accounts to)))       ;; Read 4: ~2,500 gas
            (to-status (at 'status (read accounts to))))        ;; Read 5: ~2,500 gas
        
        ;; Validation
        (enforce (>= from-balance amount) "Insufficient funds")
        (enforce (= from-status "active") "Account inactive")
        (enforce (= to-status "active") "Recipient inactive")
        
        ;; Updates (expensive writes)
        (update accounts from { "balance": (- from-balance amount) })  ;; Write 1: ~25,000 gas
        (update accounts to { "balance": (+ to-balance amount) })      ;; Write 2: ~25,000 gas
        
        "Transfer completed with poor gas efficiency")))
  
  ;; GOOD: Optimized single reads
  (defun efficient-transfer:string (from:string to:string amount:decimal)
    @doc "EFFICIENT: Single read per account saves gas"
    
    ;; GOOD: Single read with destructuring
    (with-read accounts from { 
      "balance" := from-balance,
      "status" := from-status,
      "guard" := from-guard
    }
      (with-read accounts to {
        "balance" := to-balance,
        "status" := to-status
      }
        ;; Validation (cheap operations)
        (enforce (>= from-balance amount) "Insufficient funds")
        (enforce (= from-status "active") "Account inactive")
        (enforce (= to-status "active") "Recipient inactive")
        
        ;; Batch updates
        (update accounts from { 
          "balance": (- from-balance amount),
          "last-update": (at 'block-time (chain-data))
        })
        (update accounts to { 
          "balance": (+ to-balance amount),
          "last-update": (at 'block-time (chain-data))
        })
        
        "Transfer completed efficiently")))
  
  ;; BETTER: Single transaction with error handling
  (defun optimal-transfer:string (from:string to:string amount:decimal)
    @doc "OPTIMAL: Minimize database operations with smart batching"
    
    ;; Single read for source account
    (with-read accounts from { 
      "balance" := from-bal,
      "status" := from-stat,
      "guard" := from-guard
    }
      ;; Early validation to avoid unnecessary operations
      (enforce (= from-stat "active") "Account inactive")
      (enforce (>= from-bal amount) "Insufficient funds")
      
      ;; Conditional read/write for destination
      (with-default-read accounts to
        { "balance": 0.0, "status": "active", "guard": (read-keyset "default-guard") }
        { "balance" := to-bal, "status" := to-stat }
        
        (enforce (= to-stat "active") "Recipient inactive")
        
        ;; Atomic batch update
        (update accounts from { 
          "balance": (- from-bal amount),
          "last-update": (at 'block-time (chain-data))
        })
        (write accounts to { 
          "balance": (+ to-bal amount),
          "status": to-stat,
          "guard": (at 'guard (with-default-read accounts to 
                                { "guard": (read-keyset "default-guard") }
                                { "guard" := g } g)),
          "last-update": (at 'block-time (chain-data))
        })
        
        "Optimal transfer completed")))
  
  ;; ==========================================================================
  ;; SELECT OPTIMIZATION EXAMPLES
  ;; ==========================================================================
  
  ;; EXTREMELY EXPENSIVE: Direct select usage
  (defun expensive-rich-accounts:[object] (threshold:decimal)
    @doc "EXPENSIVE: Select operation costs 40M+ gas!"
    ;; AVOID: This single operation costs 40+ million gas!
    (select accounts (where 'balance (> threshold))))
  
  ;; GOOD: Maintain indexes to avoid select
  (defschema balance-index
    account:string
    balance-tier:string
    balance:decimal)
  
  (deftable balance-indexes:{balance-index})
  
  (defun maintain-balance-index:string (account:string balance:decimal)
    @doc "Maintain balance index to avoid expensive selects"
    (let ((tier (cond
                  ((> balance 10000.0) "whale")
                  ((> balance 1000.0) "high")
                  ((> balance 100.0) "medium")
                  "low")))
      (write balance-indexes account {
        "account": account,
        "balance-tier": tier,
        "balance": balance
      })))
  
  (defun efficient-rich-accounts:[string] ()
    @doc "EFFICIENT: Query index instead of full table scan"
    ;; Much cheaper: query the pre-computed index
    (map (at 'account) 
         (select balance-indexes (where 'balance-tier (= "whale")))))
  
  ;; ==========================================================================
  ;; ALGORITHMIC OPTIMIZATION EXAMPLES  
  ;; ==========================================================================
  
  ;; BAD: O(n²) algorithm
  (defun inefficient-find-duplicates:[string] (items:[string])
    @doc "INEFFICIENT: O(n²) nested loops"
    (fold (lambda (duplicates item)
            (let ((occurrences (length (filter (= item) items))))
              (if (> occurrences 1)
                  (+ duplicates [item])
                  duplicates)))
          []
          items))  ;; Each item checks entire list
  
  ;; GOOD: O(n) algorithm using object as hash map
  (defun efficient-find-duplicates:[string] (items:[string])
    @doc "EFFICIENT: O(n) using filter and distinct"
    ;; Use filter with count - more gas efficient than nested loops
    (filter (lambda (item)
              (> (length (filter (lambda (x) (= x item)) items)) 1))
            (distinct items)))
  
  ;; ==========================================================================
  ;; DATA STRUCTURE OPTIMIZATION EXAMPLES
  ;; ==========================================================================
  
  ;; BAD: Linear search in lists
  (defun inefficient-user-lookup:object (users:[object] target-id:string)
    @doc "INEFFICIENT: Linear search through list"
    (let ((matches (filter (lambda (user) (= (at 'id user) target-id)) users)))
      (if (> (length matches) 0)
          (at 0 matches)
          {})))
  
  ;; GOOD: Use object as hash map for O(1) lookup
  (defschema user-registry
    user-id:string
    name:string
    email:string
    created:time)
  
  (deftable user-registries:{user-registry})
  
  (defun register-user:string (user-id:string user-name:string user-email:string)
    @doc "Register user in efficient lookup structure"
    (insert user-registries user-id {
      "user-id": user-id,
      "name": user-name,
      "email": user-email,
      "created": (at 'block-time (chain-data))
    }))
  
  (defun efficient-user-lookup:object (target-id:string)
    @doc "EFFICIENT: O(1) lookup using table"
    (read user-registries target-id))
  
  ;; ==========================================================================
  ;; STRING AND LIST OPTIMIZATION EXAMPLES
  ;; ==========================================================================
  
  ;; BAD: Inefficient string concatenation
  (defun inefficient-report-generation:[string] (items:[object])
    @doc "INEFFICIENT: String concatenation in loop"
    (fold (lambda (report item)
            ;; Inefficient: String concatenation has O(n) cost each time
            (+ report (format "Item {}: {} units\n" [(at 'id item) (at 'quantity item)])))
          ""
          items))
  
  ;; GOOD: Build components first, then join
  (defun efficient-report-generation:string (items:[object])
    @doc "EFFICIENT: Build components first, then join"
    ;; Build all components first
    (let ((lines (map (lambda (item)
                        (format "Item {}: {} units" [(at 'id item) (at 'quantity item)]))
                      items)))
      ;; Single join operation
      (fold (lambda (acc line) (+ acc line "\n")) "" lines)))
  
  ;; BAD: Inefficient list operations
  (defun inefficient-list-processing:[decimal] (numbers:[decimal])
    @doc "INEFFICIENT: Multiple list traversals"
    (let ((positives (filter (< 0.0) numbers))           ;; Pass 1
          (squared (map (lambda (x) (* x x)) positives)) ;; Pass 2  
          (large (filter (< 100.0) squared)))            ;; Pass 3
      large))
  
  ;; GOOD: Single pass with combined logic
  (defun efficient-list-processing:[decimal] (numbers:[decimal])
    @doc "EFFICIENT: Single pass with combined operations"
    (fold (lambda (acc num)
            (if (> num 0.0)
                (let ((squared (* num num)))
                  (if (> squared 100.0)
                      (+ acc [squared])
                      acc))
                acc))
          []
          numbers))
  
  ;; ==========================================================================
  ;; CAPABILITY OPTIMIZATION EXAMPLES
  ;; ==========================================================================
  
  ;; BAD: Repeated capability checks
  (defun inefficient-batch-transfer:string (transfers:[object])
    @doc "INEFFICIENT: Capability checked for each transfer"
    (map (lambda (xfer)
           ;; Expensive: Capability granted/revoked for each transfer
           (with-capability (TRANSFER (at 'from xfer) (at 'to xfer) (at 'amount xfer))
             (transfer-internal (at 'from xfer) (at 'to xfer) (at 'amount xfer))))
         transfers)
    "Batch completed inefficiently")
  
  ;; GOOD: Batch capability
  (defcap BATCH_TRANSFER:bool (transfers:[object])
    @doc "Batch transfer capability"
    ;; Validate all transfers upfront
    (map (lambda (xfer)
           (enforce-guard (account-guard (at 'from xfer))))
         transfers))
  
  (defun efficient-batch-transfer:string (transfers:[object])
    @doc "EFFICIENT: Single capability for entire batch"
    (with-capability (BATCH_TRANSFER transfers)
      ;; All transfers under single capability
      (map (lambda (xfer)
             (transfer-internal (at 'from xfer) (at 'to xfer) (at 'amount xfer)))
           transfers))
    "Batch completed efficiently")
  
  ;; ==========================================================================
  ;; INTEGER ARITHMETIC OPTIMIZATION
  ;; ==========================================================================
  
  ;; Gas cost scales with integer size
  (defun demonstrate-integer-costs ()
    @doc "Show how integer size affects gas costs"
    
    ;; CHEAP: Small integers
    (let ((small-calc (+ 100 200)))                    ;; Low gas cost
      
      ;; EXPENSIVE: Large integers  
      (let ((large-calc (+ 123456789012345678901234567890
                          987654321098765432109876543210))) ;; High gas cost
        
        ;; OPTIMIZATION: Use scaling for precision
        (let ((scaled-a (round (/ 123456789012345678901234567890 1000000000000) 6))
              (scaled-b (round (/ 987654321098765432109876543210 1000000000000) 6)))
          ;; Lower gas cost due to smaller operands
          (+ scaled-a scaled-b)))))
  
  ;; GOOD: Optimize exponentiation
  (defun efficient-compound-interest:decimal (principal:decimal rate:decimal periods:integer)
    @doc "EFFICIENT: Optimized compound interest calculation"
    (if (< periods 20)
        ;; Use built-in for small exponents
        (* principal (^ (+ 1.0 rate) periods))
        ;; Use approximation for large exponents to control gas
        (let ((monthly-rate (/ rate 12.0))
              (monthly-periods (* periods 12)))
          (* principal (^ (+ 1.0 monthly-rate) (if (< monthly-periods 240) monthly-periods 240)))))) ;; Cap iterations
  
  ;; ==========================================================================
  ;; MEMORY OPTIMIZATION EXAMPLES
  ;; ==========================================================================
  
  ;; BAD: Repeated object field access
  (defun inefficient-trade-calculation:decimal (trade:object)
    @doc "INEFFICIENT: Repeated field access"
    (let ((base-value (* (at 'quantity trade) (at 'price trade)))        ;; Access 1 & 2
          (fee (* (at 'quantity trade) (at 'fee-rate trade)))           ;; Access 3 & 4 (repeated)
          (tax (* (at 'quantity trade) (at 'tax-rate trade))))          ;; Access 5 & 6 (repeated)
      (- (- base-value fee) tax)))
  
  ;; GOOD: Destructure once
  (defun efficient-trade-calculation:decimal (trade:object)
    @doc "EFFICIENT: Single destructuring"
    (bind trade { "quantity" := qty, "price" := price, "fee-rate" := fee, "tax-rate" := tax }
      (let ((base-value (* qty price))
            (fee-amount (* qty fee))
            (tax-amount (* qty tax)))
        (- (- base-value fee-amount) tax-amount))))
  
  ;; ==========================================================================
  ;; BATCH PROCESSING OPTIMIZATION
  ;; ==========================================================================
  
  (defschema batch-job
    id:string
    total-items:integer
    processed:integer
    batch-size:integer
    status:string
    result:object)
  
  (deftable batch-jobs:{batch-job})
  
  (defun start-batch-job:string (job-id:string items:[object] batch-size:integer)
    @doc "Start optimized batch processing job"
    (insert batch-jobs job-id {
      "id": job-id,
      "total-items": (length items),
      "processed": 0,
      "batch-size": batch-size,
      "status": "processing",
      "result": { "processed-items": [], "summary": {} }
    })
    (process-batch-chunk job-id items 0 batch-size))
  
  (defun process-batch-chunk:string (job-id:string items:[object] start-idx:integer batch-size:integer)
    @doc "Process a chunk of items efficiently"
    (let ((end-idx (let ((max-idx (+ start-idx batch-size))
                       (list-len (length items)))
                     (if (< max-idx list-len) max-idx list-len)))
          (chunk (take (- end-idx start-idx) (drop start-idx items))))
      
      ;; Process chunk efficiently
      (let ((chunk-result (map process-single-item chunk)))
        
        ;; Update job state
        (with-read batch-jobs job-id { "result" := current-result }
          (update batch-jobs job-id {
            "processed": end-idx,
            "status": (if (= end-idx (length items)) "completed" "processing"),
            "result": {
              "processed-items": (+ (at 'processed-items current-result) chunk-result),
              "summary": (update-summary (at 'summary current-result) chunk-result)
            }
          }))
        
        (if (< end-idx (length items))
            "Batch chunk processed, more remain"
            "Batch job completed"))))
  
  ;; ==========================================================================
  ;; HELPER FUNCTIONS
  ;; ==========================================================================
  
  (defcap TRANSFER:bool (from:string to:string amount:decimal)
    @doc "Transfer capability"
    (enforce-guard (account-guard from)))
  
  (defun account-guard:guard (account:string)
    @doc "Get account guard"
    (at 'guard (read accounts account)))
  
  (defun transfer-internal:string (from:string to:string amount:decimal)
    @doc "Internal transfer implementation"
    "Transfer completed")
  
  (defun process-single-item:object (item:object)
    @doc "Process a single item"
    (+ item { "processed": true, "timestamp": (at 'block-time (chain-data)) }))
  
  (defun update-summary:object (current:object new-items:[object])
    @doc "Update processing summary"
    (+ current { "total-processed": (+ (at 'total-processed current) (length new-items)) }))
  
  ;; ==========================================================================
  ;; GAS MEASUREMENT UTILITIES
  ;; ==========================================================================
  
  (defun measure-function-gas:decimal (function-name:string test-data:object)
    @doc "Measure gas consumption of a function (conceptual)"
    ;; Note: This is conceptual - actual implementation would need 
    ;; integration with gas metering system
    (let ((start-time (at 'block-time (chain-data))))
      ;; Execute function based on name
      (cond
        ((= function-name "inefficient-transfer")
         (inefficient-transfer (at 'from test-data) (at 'to test-data) (at 'amount test-data)))
        ((= function-name "efficient-transfer")
         (efficient-transfer (at 'from test-data) (at 'to test-data) (at 'amount test-data)))
        "Unknown function")
      
      ;; Return estimated gas (in real implementation, would use actual gas meter)
      (let ((end-time (at 'block-time (chain-data))))
        (* (diff-time end-time start-time) 1000.0))))  ;; Simplified estimation
  
  (defun compare-implementations:[object] (test-scenarios:[object])
    @doc "Compare gas efficiency of different implementations"
    (map (lambda (scenario)
           (bind scenario { "name" := name, "functions" := funcs, "data" := test-data }
             { "scenario": name,
               "results": (map (lambda (func)
                                { "function": func,
                                  "estimated-gas": (measure-function-gas func test-data) })
                              funcs) }))
         test-scenarios))
)

;; Initialize tables
(create-table accounts)
(create-table transactions)
(create-table balance-indexes)
(create-table user-registries)
(create-table batch-jobs)