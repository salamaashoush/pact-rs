# Chapter 13: Testing Strategies and Best Practices

## Introduction

Testing is crucial for smart contract development due to their immutable nature and financial implications. Pact provides comprehensive testing infrastructure including REPL-based testing, property-based testing, formal verification capabilities, and gas profiling. This chapter explores testing strategies proven effective in the Pact ecosystem.

## Testing Philosophy in Pact

### Levels of Testing

Pact supports multiple testing approaches, each serving different purposes:

1. **Unit Tests** - Individual function verification
2. **Integration Tests** - Module interaction testing
3. **Property Tests** - Generative testing for edge cases
4. **Formal Verification** - Mathematical correctness proofs
5. **Gas Testing** - Performance and cost verification
6. **Regression Tests** - Backward compatibility assurance

## REPL-Based Testing

### Basic REPL Testing Structure

The Pact REPL provides an interactive environment for testing smart contracts:

```pact
;; test-basic.repl
;; Basic REPL testing example

(begin-tx "Setup test environment")

;; Load test data
(env-data {
  "admin-keyset": ["admin-key"],
  "user-keyset": ["user-key"]
})

;; Load contract
(load "my-contract.pact")

(commit-tx)

(begin-tx "Test basic functionality")

;; Test account creation
(expect "Account creation succeeds"
  "Account alice created"
  (create-account "alice" (read-keyset "user-keyset")))

;; Test balance query
(expect "Initial balance is zero"
  0.0
  (get-balance "alice"))

(commit-tx)
```

### Advanced REPL Testing Patterns

#### Test Data Management

```pact
;; test-data-management.repl
;; Advanced test data setup and management

(begin-tx "Setup comprehensive test data")

;; Complex test data structure
(env-data {
  "admin": ["admin-key-1", "admin-key-2"],
  "users": {
    "alice": { "keys": ["alice-key"], "pred": "keys-all" },
    "bob": { "keys": ["bob-key"], "pred": "keys-all" },
    "charlie": { "keys": ["charlie-key"], "pred": "keys-all" }
  },
  "test-accounts": [
    { "name": "alice", "balance": 1000.0 },
    { "name": "bob", "balance": 500.0 },
    { "name": "charlie", "balance": 250.0 }
  ],
  "test-config": {
    "transfer-limit": 100.0,
    "fee-rate": 0.02,
    "admin-override": true
  }
})

;; Create keysets from test data
(define-keyset 'admin (read-keyset "admin"))
(map (lambda (user)
       (define-keyset (format '{}' [user]) 
                     (read-keyset (format "users.{}" [user]))))
     ["alice", "bob", "charlie"])

;; Load and initialize contract
(load "token-contract.pact")

;; Setup test accounts using data
(map (lambda (account)
       (bind account { "name" := name, "balance" := balance }
         (create-account name (read-keyset name))
         (mint name balance)))
     (read-msg "test-accounts"))

(commit-tx)
```

#### Environment Manipulation

```pact
;; test-environment.repl
;; Testing with environment manipulation

(begin-tx "Test time-dependent functionality")

;; Set specific time for testing
(env-time (time "2024-01-01T00:00:00Z"))

;; Test time-locked functionality
(expect "Time lock prevents early access"
  "Time lock not expired"
  (access-time-locked-resource "resource-1"))

;; Fast forward time
(env-time (time "2024-06-01T00:00:00Z"))

(expect "Time lock allows access after expiration"
  "Access granted"
  (access-time-locked-resource "resource-1"))

(commit-tx)

(begin-tx "Test block height dependent functionality")

;; Set specific block height
(env-block-height 1000)

(expect "Early block height restricts access"
  false
  (is-mature-enough "block-dependent-resource"))

;; Advance block height
(env-block-height 2000)

(expect "Sufficient block height allows access"
  true
  (is-mature-enough "block-dependent-resource"))

(commit-tx)

(begin-tx "Test different signers")

;; Test with different key combinations
(env-keys ["alice-key"])
(expect "Alice can access her account"
  1000.0
  (get-balance "alice"))

(env-keys ["bob-key"])
(expect-failure "Bob cannot access Alice's account"
  "keyset failure"
  (get-balance "alice"))

;; Test multi-sig scenarios
(env-keys ["admin-key-1", "admin-key-2"])
(expect "Admin multi-sig enables admin functions"
  "Admin function executed"
  (admin-function))

(commit-tx)
```

### Error Testing Patterns

```pact
;; test-errors.repl
;; Comprehensive error testing

(begin-tx "Test input validation errors")

;; Test negative amounts
(expect-failure "Negative transfer fails"
  "Amount must be positive"
  (transfer "alice" "bob" -10.0))

;; Test zero amounts
(expect-failure "Zero transfer fails"
  "Amount must be positive"
  (transfer "alice" "bob" 0.0))

;; Test invalid accounts
(expect-failure "Invalid account fails"
  "Account does not exist"
  (transfer "nonexistent" "bob" 10.0))

;; Test self-transfer
(expect-failure "Self-transfer fails"
  "Cannot transfer to self"
  (transfer "alice" "alice" 10.0))

(commit-tx)

(begin-tx "Test authorization errors")

;; Test unauthorized access
(env-keys [])  ;; No keys
(expect-failure "Unauthorized transfer fails"
  "keyset failure"
  (transfer "alice" "bob" 10.0))

;; Test insufficient permissions
(env-keys ["user-key"])
(expect-failure "Non-admin cannot call admin function"
  "administrative keyset failure"
  (admin-function))

(commit-tx)

(begin-tx "Test business logic errors")

;; Test insufficient balance
(env-keys ["charlie-key"])
(expect-failure "Insufficient balance fails"
  "Insufficient funds"
  (transfer "charlie" "alice" 1000.0))  ;; Charlie only has 250

;; Test amount limits
(env-keys ["alice-key"])
(expect-failure "Transfer exceeds limit"
  "Transfer limit exceeded"
  (transfer "alice" "bob" 1000.0))  ;; Exceeds 100.0 limit

(commit-tx)
```

## Property-Based Testing

### Understanding Property-Based Testing

Property-based testing generates many test cases automatically, focusing on invariants that should always hold true rather than specific input-output pairs.

#### Basic Property Testing

```pact
;; test-properties.repl
;; Property-based testing examples

(begin-tx "Setup for property testing")

(load "token-contract.pact")

;; Setup test accounts with various balances
(create-account "alice" (read-keyset "alice"))
(create-account "bob" (read-keyset "bob"))
(mint "alice" 10000.0)
(mint "bob" 5000.0)

(commit-tx)

(begin-tx "Test transfer properties")

;; Property: Balance conservation
(defun test-balance-conservation (from:string to:string amount:decimal)
  @doc "Total balance should be conserved in transfers"
  (let ((initial-from (get-balance from))
        (initial-to (get-balance to))
        (initial-total (+ initial-from initial-to)))
    
    ;; Perform transfer (if valid)
    (if (and (> amount 0.0) (<= amount initial-from) (!= from to))
        (progn
          (transfer from to amount)
          ;; Check balance conservation
          (let ((final-from (get-balance from))
                (final-to (get-balance to))
                (final-total (+ final-from final-to)))
            (enforce (= initial-total final-total) "Balance not conserved")))
        "Invalid transfer parameters")))

;; Property: Non-negative balances
(defun test-non-negative-balances ()
  @doc "All balances should remain non-negative"
  (let ((alice-balance (get-balance "alice"))
        (bob-balance (get-balance "bob")))
    (enforce (>= alice-balance 0.0) "Alice balance negative")
    (enforce (>= bob-balance 0.0) "Bob balance negative")))

;; Property: Transfer commutativity (in terms of end state)
(defun test-transfer-order-independence (amount1:decimal amount2:decimal)
  @doc "Order of independent transfers shouldn't matter"
  (let ((alice-initial (get-balance "alice"))
        (bob-initial (get-balance "bob")))
    
    ;; Save initial state
    (let ((checkpoint { "alice": alice-initial, "bob": bob-initial }))
      
      ;; Scenario 1: Transfer A then B
      (transfer "alice" "bob" amount1)
      (transfer "alice" "bob" amount2)
      (let ((scenario1-alice (get-balance "alice"))
            (scenario1-bob (get-balance "bob")))
        
        ;; Reset to initial state
        (update accounts "alice" { "balance": alice-initial })
        (update accounts "bob" { "balance": bob-initial })
        
        ;; Scenario 2: Transfer B then A
        (transfer "alice" "bob" amount2)
        (transfer "alice" "bob" amount1)
        (let ((scenario2-alice (get-balance "alice"))
              (scenario2-bob (get-balance "bob")))
          
          ;; End states should be identical
          (enforce (= scenario1-alice scenario2-alice) "Alice balances differ")
          (enforce (= scenario1-bob scenario2-bob) "Bob balances differ"))))))

;; Run property tests
(test-balance-conservation "alice" "bob" 100.0)
(test-non-negative-balances)
(test-transfer-order-independence 50.0 75.0)

(commit-tx)
```

### Advanced Property Testing

#### Invariant Testing

```pact
;; test-invariants.repl
;; Advanced invariant testing

(begin-tx "Comprehensive invariant testing")

(load "defi-protocol.pact")

;; Setup DeFi protocol state
(create-pool "TOKEN-A" "TOKEN-B" 100000.0 200000.0)
(add-liquidity "alice" "TOKEN-A" "TOKEN-B" 1000.0 2000.0)

;; Invariant: Constant product formula (x * y = k)
(defun test-constant-product-invariant (pool-id:string)
  @doc "AMM constant product should be maintained"
  (with-read pools pool-id { 
    "reserve-x" := x, 
    "reserve-y" := y,
    "k" := constant-k 
  }
    (let ((current-k (* x y)))
      (enforce (>= current-k constant-k) "Constant product invariant violated")
      ;; Allow for slight increase due to fees, but never decrease
      (enforce (<= current-k (* constant-k 1.01)) "Constant product increased too much"))))

;; Invariant: Total supply equals sum of all balances
(defun test-supply-balance-invariant (token:string)
  @doc "Total supply should equal sum of all individual balances"
  (let ((total-supply (get-total-supply token))
        (all-accounts (get-all-accounts token))
        (sum-balances (fold (+) 0.0 (map (get-balance token) all-accounts))))
    (enforce (= total-supply sum-balances) "Supply/balance invariant violated")))

;; Invariant: Pool shares represent proportional ownership
(defun test-liquidity-share-invariant (pool-id:string user:string)
  @doc "User's LP tokens should represent correct pool ownership"
  (with-read pools pool-id { 
    "total-shares" := total,
    "reserve-x" := reserve-x,
    "reserve-y" := reserve-y
  }
    (let ((user-shares (get-lp-balance user pool-id))
          (user-x-entitlement (* reserve-x (/ user-shares total)))
          (user-y-entitlement (* reserve-y (/ user-shares total))))
      
      ;; User should be able to withdraw proportional amounts
      (withdraw-liquidity user pool-id user-shares)
      (let ((withdrawn-x (get-withdrawal-amount user "TOKEN-A"))
            (withdrawn-y (get-withdrawal-amount user "TOKEN-B")))
        
        ;; Allow for small rounding differences
        (enforce (< (abs (- withdrawn-x user-x-entitlement)) 0.01) "X withdrawal incorrect")
        (enforce (< (abs (- withdrawn-y user-y-entitlement)) 0.01) "Y withdrawal incorrect")))))

;; Run invariant tests
(test-constant-product-invariant "TOKEN-A:TOKEN-B")
(test-supply-balance-invariant "TOKEN-A")
(test-liquidity-share-invariant "TOKEN-A:TOKEN-B" "alice")

(commit-tx)
```

#### Generative Testing Patterns

```pact
;; test-generative.repl
;; Generative testing with multiple scenarios

(begin-tx "Generative test scenarios")

;; Generate test scenarios
(defun generate-transfer-scenarios:[object] (num-scenarios:integer)
  @doc "Generate random transfer scenarios for testing"
  (map (lambda (i)
         { "from": (at (mod i 3) ["alice", "bob", "charlie"]),
           "to": (at (mod (+ i 1) 3) ["alice", "bob", "charlie"]),
           "amount": (+ 1.0 (* (mod i 47) 10.0)),  ;; Pseudo-random amounts
           "scenario": i })
       (enumerate 1 num-scenarios)))

;; Test multiple scenarios
(defun test-multiple-scenarios (scenarios:[object])
  @doc "Test multiple transfer scenarios and verify invariants"
  (map (lambda (scenario)
         (bind scenario { "from" := from, "to" := to, "amount" := amount, "scenario" := num }
           ;; Record initial state
           (let ((initial-from (get-balance from))
                 (initial-to (get-balance to))
                 (initial-total (get-total-supply)))
             
             ;; Attempt transfer
             (let ((result (try-transfer from to amount)))
               ;; Verify invariants regardless of success/failure
               (test-non-negative-balances)
               (test-supply-balance-invariant "TOKEN")
               
               { "scenario": num,
                 "result": result,
                 "invariants-preserved": true }))))
       scenarios))

;; Generate and run scenarios
(let ((scenarios (generate-transfer-scenarios 20)))
  (let ((results (test-multiple-scenarios scenarios)))
    (expect "All scenarios preserve invariants"
      20
      (length (filter (at 'invariants-preserved) results)))))

(commit-tx)
```

## Formal Verification and Model Testing

### Mathematical Properties

```pact
;; test-formal-properties.repl
;; Formal verification examples using Pact's @model annotations

(begin-tx "Formal property verification")

(module formally-verified GOVERNANCE
  @doc "Example module with formal verification"
  
  (defschema account
    balance:decimal
    guard:guard)
    
  (deftable accounts:{account})
  
  ;; Function with formal specification
  (defun transfer:string (from:string to:string amount:decimal)
    @doc "Transfer with formal verification"
    @model [
      ;; Preconditions
      (property (> amount 0.0))
      (property (!= from to))
      (property (>= (at 'balance (read accounts from)) amount))
      
      ;; Postconditions
      (property (= (+ (at 'balance (read accounts from))
                      (at 'balance (read accounts to)))
                   (+ (at 'balance (read accounts from before))
                      (at 'balance (read accounts to before)))))
      (property (= (at 'balance (read accounts from))
                   (- (at 'balance (read accounts from before)) amount)))
      (property (= (at 'balance (read accounts to))
                   (+ (at 'balance (read accounts to before)) amount)))
    ]
    
    ;; Implementation
    (with-read accounts from { "balance" := from-bal, "guard" := from-guard }
      (enforce-guard from-guard)
      (enforce (>= from-bal amount) "Insufficient funds")
      (enforce (!= from to) "Cannot transfer to self")
      
      (with-read accounts to { "balance" := to-bal }
        (update accounts from { "balance": (- from-bal amount) })
        (update accounts to { "balance": (+ to-bal amount) })
        "Transfer completed")))
  
  ;; Properties that should always hold
  (defun verify-account-invariants:bool ()
    @doc "Verify all accounts have non-negative balances"
    @model [(property (>= result true))]
    (let ((all-accounts (keys accounts)))
      (all-true (map (lambda (account)
                       (>= (at 'balance (read accounts account)) 0.0))
                     all-accounts))))
)

(commit-tx)
```

### Model-Based Testing

```pact
;; test-models.repl
;; Model-based testing approach

(begin-tx "Model-based testing")

;; Abstract model of the system
(defun model-transfer (balances:object from:string to:string amount:decimal)
  @doc "Abstract model of transfer operation"
  (if (and (> amount 0.0)
           (!= from to)
           (contains from balances)
           (contains to balances)
           (>= (at from balances) amount))
      ;; Valid transfer: update balances
      (+ balances 
         { from: (- (at from balances) amount),
           to: (+ (at to balances) amount) })
      ;; Invalid transfer: no change
      balances))

;; Test implementation against model
(defun test-against-model (operations:[object])
  @doc "Test concrete implementation against abstract model"
  (let ((initial-balances { "alice": 1000.0, "bob": 500.0, "charlie": 250.0 }))
    
    ;; Initialize both systems
    (setup-accounts initial-balances)
    
    ;; Apply operations to both model and implementation
    (fold (lambda (acc operation)
            (bind operation { "from" := from, "to" := to, "amount" := amount }
              ;; Update model
              (let ((new-model (model-transfer (at 'model acc) from to amount)))
                
                ;; Update implementation (if valid)
                (if (is-valid-transfer from to amount)
                    (transfer from to amount)
                    "Skipped invalid transfer")
                
                ;; Compare states
                (let ((impl-balances (get-all-balances)))
                  (enforce (= new-model impl-balances) "Model/implementation diverged")
                  { "model": new-model, "step": (+ (at 'step acc) 1) }))))
          { "model": initial-balances, "step": 0 }
          operations)))

;; Run model-based test
(let ((test-operations [
  { "from": "alice", "to": "bob", "amount": 100.0 },
  { "from": "bob", "to": "charlie", "amount": 50.0 },
  { "from": "charlie", "to": "alice", "amount": 25.0 },
  { "from": "alice", "to": "bob", "amount": -10.0 },  ;; Invalid
  { "from": "alice", "to": "alice", "amount": 10.0 }  ;; Invalid
]))
  (test-against-model test-operations))

(commit-tx)
```

## Gas and Performance Testing

### Gas Consumption Testing

```pact
;; test-gas-consumption.repl
;; Comprehensive gas testing

(begin-tx "Gas consumption testing")

(load "gas-heavy-contract.pact")

;; Test gas usage for different operations
(defun measure-gas-usage:object (operation:string test-data:object)
  @doc "Measure gas consumption for operation"
  (let ((gas-before (chain-data 'gas-used)))
    ;; Execute operation based on type
    (let ((result (cond
                    ((= operation "simple-transfer")
                     (transfer (at 'from test-data) (at 'to test-data) (at 'amount test-data)))
                    ((= operation "complex-calculation")
                     (complex-interest-calculation (at 'principal test-data) (at 'rate test-data)))
                    ((= operation "database-query")
                     (query-user-transactions (at 'user test-data)))
                    "Unknown operation")))
      (let ((gas-after (chain-data 'gas-used)))
        { "operation": operation,
          "gas-consumed": (- gas-after gas-before),
          "result": result }))))

;; Test gas limits
(defun test-gas-limits ()
  @doc "Test operations near gas limit boundaries"
  
  ;; Test efficient operation (should use minimal gas)
  (let ((efficient-gas (measure-gas-usage "simple-transfer" 
                                          { "from": "alice", "to": "bob", "amount": 10.0 })))
    (expect "Efficient operation uses reasonable gas"
      true
      (< (at 'gas-consumed efficient-gas) 10000)))
  
  ;; Test expensive operation (verify it's within limits)
  (let ((expensive-gas (measure-gas-usage "complex-calculation"
                                         { "principal": 1000.0, "rate": 0.05 })))
    (expect "Expensive operation stays within reasonable bounds"
      true
      (< (at 'gas-consumed expensive-gas) 100000)))
  
  ;; Test batch operations for efficiency
  (let ((single-op-gas (measure-gas-usage "simple-transfer"
                                         { "from": "alice", "to": "bob", "amount": 10.0 }))
        (batch-ops-gas (measure-gas-usage "batch-transfer"
                                         { "transfers": [
                                           { "from": "alice", "to": "bob", "amount": 10.0 },
                                           { "from": "alice", "to": "charlie", "amount": 15.0 }
                                         ]})))
    (expect "Batch operations are more efficient than individual operations"
      true
      (< (at 'gas-consumed batch-ops-gas) (* 2 (at 'gas-consumed single-op-gas))))))

(test-gas-limits)

(commit-tx)
```

### Performance Regression Testing

```pact
;; test-performance-regression.repl
;; Performance regression testing

(begin-tx "Performance regression testing")

;; Baseline performance metrics
(defconst BASELINE_TRANSFER_GAS:decimal 5000.0)
(defconst BASELINE_QUERY_GAS:decimal 15000.0)
(defconst BASELINE_CALCULATION_GAS:decimal 25000.0)

;; Performance threshold (allow 10% degradation)
(defconst PERFORMANCE_THRESHOLD:decimal 1.1)

(defun test-performance-regression ()
  @doc "Test for performance regressions"
  
  ;; Test transfer performance
  (let ((transfer-gas (at 'gas-consumed 
                         (measure-gas-usage "simple-transfer"
                                          { "from": "alice", "to": "bob", "amount": 100.0 }))))
    (expect "Transfer performance within acceptable range"
      true
      (<= transfer-gas (* BASELINE_TRANSFER_GAS PERFORMANCE_THRESHOLD))))
  
  ;; Test query performance
  (let ((query-gas (at 'gas-consumed
                      (measure-gas-usage "database-query"
                                       { "user": "alice" }))))
    (expect "Query performance within acceptable range"
      true
      (<= query-gas (* BASELINE_QUERY_GAS PERFORMANCE_THRESHOLD))))
  
  ;; Test calculation performance
  (let ((calc-gas (at 'gas-consumed
                     (measure-gas-usage "complex-calculation"
                                      { "principal": 1000.0, "rate": 0.05 }))))
    (expect "Calculation performance within acceptable range"
      true
      (<= calc-gas (* BASELINE_CALCULATION_GAS PERFORMANCE_THRESHOLD)))))

(test-performance-regression)

(commit-tx)
```

## Integration Testing

### Multi-Module Testing

```pact
;; test-integration.repl
;; Integration testing across multiple modules

(begin-tx "Multi-module integration testing")

;; Load all related modules
(load "token-contract.pact")
(load "exchange-contract.pact")
(load "governance-contract.pact")

;; Test cross-module functionality
(defun test-cross-module-integration ()
  @doc "Test integration between multiple modules"
  
  ;; Setup: Create tokens and exchange
  (create-token "TOKEN-A" 1000000.0)
  (create-token "TOKEN-B" 2000000.0)
  (create-exchange-pair "TOKEN-A" "TOKEN-B" 100000.0 200000.0)
  
  ;; Test: Token -> Exchange integration
  (transfer-to-exchange "alice" "TOKEN-A" 1000.0)
  (expect "Tokens transferred to exchange"
    1000.0
    (get-exchange-balance "alice" "TOKEN-A"))
  
  ;; Test: Exchange -> Governance integration
  (delegate-voting-power "alice" "TOKEN-A" 500.0)
  (expect "Voting power delegated correctly"
    500.0
    (get-voting-power "alice"))
  
  ;; Test: Governance -> Token integration
  (vote-on-proposal "alice" "proposal-1" "yes")
  (execute-proposal "proposal-1")
  (expect "Proposal execution affects token parameters"
    true
    (proposal-executed? "proposal-1")))

(test-cross-module-integration)

(commit-tx)
```

### End-to-End Workflow Testing

```pact
;; test-e2e-workflows.repl
;; End-to-end workflow testing

(begin-tx "End-to-end workflow testing")

;; Test complete DeFi workflow
(defun test-defi-workflow ()
  @doc "Test complete DeFi user workflow"
  
  ;; Step 1: User onboarding
  (create-account "new-user" (read-keyset "new-user"))
  (airdrop-tokens "new-user" 1000.0)
  
  ;; Step 2: Liquidity provision
  (approve-spending "new-user" "DEX" 500.0)
  (add-liquidity "new-user" "TOKEN-A" "TOKEN-B" 250.0 250.0)
  
  ;; Step 3: Trading
  (swap-tokens "new-user" "TOKEN-A" "TOKEN-B" 100.0)
  
  ;; Step 4: Yield farming
  (stake-lp-tokens "new-user" "TOKEN-A:TOKEN-B" 100.0)
  
  ;; Step 5: Governance participation
  (vote-on-proposal "new-user" "protocol-upgrade" "yes")
  
  ;; Step 6: Reward claiming
  (claim-farming-rewards "new-user")
  
  ;; Step 7: Exit liquidity
  (unstake-lp-tokens "new-user" 100.0)
  (remove-liquidity "new-user" "TOKEN-A:TOKEN-B" 150.0)
  
  ;; Verify final state
  (expect "User completed full DeFi workflow"
    true
    (> (get-balance "new-user") 900.0)))  ;; Should have gained rewards

(test-defi-workflow)

(commit-tx)
```

## Test Organization and Best Practices

### Test Suite Organization

```pact
;; test-suite-organization.repl
;; Structured test suite organization

;; Test categories
(defconst TEST_CATEGORIES:[string] [
  "unit-tests",
  "integration-tests", 
  "property-tests",
  "performance-tests",
  "security-tests",
  "edge-case-tests"
])

;; Test runner with categorization
(defun run-test-category:object (category:string)
  @doc "Run all tests in a specific category"
  (cond
    ((= category "unit-tests")
     (run-unit-tests))
    ((= category "integration-tests")
     (run-integration-tests))
    ((= category "property-tests")
     (run-property-tests))
    ((= category "performance-tests")
     (run-performance-tests))
    ((= category "security-tests")
     (run-security-tests))
    ((= category "edge-case-tests")
     (run-edge-case-tests))
    { "error": "Unknown test category" }))

(defun run-unit-tests:object ()
  @doc "Run all unit tests"
  { "category": "unit-tests",
    "tests": [
      { "name": "test-account-creation", "result": (test-account-creation) },
      { "name": "test-balance-queries", "result": (test-balance-queries) },
      { "name": "test-transfer-validation", "result": (test-transfer-validation) }
    ]})

(defun run-integration-tests:object ()
  @doc "Run all integration tests"
  { "category": "integration-tests",
    "tests": [
      { "name": "test-cross-module-calls", "result": (test-cross-module-calls) },
      { "name": "test-workflow-integration", "result": (test-workflow-integration) }
    ]})

;; Run comprehensive test suite
(defun run-all-tests:[object] ()
  @doc "Run complete test suite"
  (map run-test-category TEST_CATEGORIES))

;; Generate test report
(defun generate-test-report:object (test-results:[object])
  @doc "Generate comprehensive test report"
  (let ((total-tests (fold (+) 0 (map (lambda (category)
                                        (length (at 'tests category)))
                                      test-results)))
        (passed-tests (fold (+) 0 (map (lambda (category)
                                         (length (filter (lambda (test)
                                                           (= (at 'result test) "PASS"))
                                                         (at 'tests category))))
                                       test-results))))
    { "summary": {
        "total-tests": total-tests,
        "passed-tests": passed-tests,
        "failed-tests": (- total-tests passed-tests),
        "success-rate": (/ passed-tests total-tests)
      },
      "detailed-results": test-results,
      "timestamp": (chain-data 'time) }))
```

### Test Utilities and Helpers

```pact
;; test-utilities.repl
;; Reusable test utilities

(defun setup-test-environment:string ()
  @doc "Setup standard test environment"
  ;; Create standard test accounts
  (create-account "alice" (read-keyset "alice"))
  (create-account "bob" (read-keyset "bob"))
  (create-account "charlie" (read-keyset "charlie"))
  
  ;; Mint initial balances
  (mint "alice" 10000.0)
  (mint "bob" 5000.0)
  (mint "charlie" 2500.0)
  
  "Test environment initialized")

(defun cleanup-test-environment:string ()
  @doc "Clean up test environment"
  ;; Reset all test accounts
  (update accounts "alice" { "balance": 0.0 })
  (update accounts "bob" { "balance": 0.0 })
  (update accounts "charlie" { "balance": 0.0 })
  
  "Test environment cleaned up")

(defun assert-equal (expected actual description:string)
  @doc "Assert two values are equal with description"
  (expect description expected actual))

(defun assert-true (condition:bool description:string)
  @doc "Assert condition is true with description"
  (expect description true condition))

(defun assert-false (condition:bool description:string)
  @doc "Assert condition is false with description"
  (expect description false condition))

(defun assert-error (error-message:string action description:string)
  @doc "Assert action throws specific error"
  (expect-failure description error-message action))

;; Performance assertion helpers
(defun assert-gas-limit (max-gas:decimal action description:string)
  @doc "Assert action uses less than max gas"
  (let ((gas-before (chain-data 'gas-used)))
    (let ((result action))
      (let ((gas-used (- (chain-data 'gas-used) gas-before)))
        (assert-true (< gas-used max-gas) 
                    (format "{} - used {} gas" [description gas-used]))
        result))))

;; Fuzzing utilities
(defun generate-random-amounts:[decimal] (count:integer seed:integer)
  @doc "Generate pseudo-random amounts for testing"
  (map (lambda (i)
         (let ((pseudo-random (mod (+ (* i seed) 17) 100)))
           (+ 1.0 (* pseudo-random 10.0))))
       (enumerate 1 count)))

(defun generate-random-accounts:[string] (count:integer)
  @doc "Generate random account names for testing"
  (map (lambda (i) (format "test-account-{}" [i]))
       (enumerate 1 count)))
```

## Security Testing

### Vulnerability Testing

```pact
;; test-security.repl
;; Security and vulnerability testing

(begin-tx "Security testing")

;; Test reentrancy protection
(defun test-reentrancy-protection ()
  @doc "Test contract protects against reentrancy attacks"
  
  ;; Attempt reentrancy attack
  (expect-failure "Reentrancy attack fails"
    "reentrancy detected"
    (malicious-reentrant-call "alice" "bob" 100.0)))

;; Test integer overflow/underflow
(defun test-integer-overflow-protection ()
  @doc "Test protection against integer overflow"
  
  ;; Test large number handling
  (expect-failure "Overflow protection works"
    "arithmetic overflow"
    (add-to-balance "alice" 99999999999999999999999999999999999999.0)))

;; Test access control bypasses
(defun test-access-control-bypasses ()
  @doc "Test various access control bypass attempts"
  
  ;; Test unauthorized admin access
  (env-keys ["user-key"])
  (expect-failure "Unauthorized admin access fails"
    "administrative keyset failure"
    (admin-mint "alice" 1000000.0))
  
  ;; Test capability bypass attempts
  (expect-failure "Capability bypass fails"
    "capability not granted"
    (direct-balance-manipulation "alice" 1000000.0)))

;; Test input validation bypasses
(defun test-input-validation ()
  @doc "Test comprehensive input validation"
  
  ;; Test malformed data
  (expect-failure "Malformed input rejected"
    "invalid input format"
    (process-malformed-transaction {}))
  
  ;; Test boundary conditions
  (expect-failure "Negative amounts rejected"
    "amount must be positive"
    (transfer "alice" "bob" -0.000001))
  
  ;; Test extremely large values
  (expect-failure "Extremely large values rejected"
    "amount exceeds maximum"
    (transfer "alice" "bob" 1e50)))

;; Run security tests
(test-reentrancy-protection)
(test-integer-overflow-protection)
(test-access-control-bypasses)
(test-input-validation)

(commit-tx)
```

## Summary

Effective testing in Pact requires a multi-layered approach:

**Testing Levels:**
1. **Unit Tests** - Individual function verification with `expect` and `expect-failure`
2. **Property Tests** - Invariant testing with generative scenarios
3. **Integration Tests** - Cross-module workflow verification  
4. **Performance Tests** - Gas consumption and regression testing
5. **Security Tests** - Vulnerability and attack vector testing

**Best Practices:**
1. **Test early and often** - Write tests alongside development
2. **Use property-based testing** - Find edge cases automatically
3. **Test error conditions** - Verify failures behave correctly
4. **Monitor gas consumption** - Prevent performance regressions
5. **Test cross-module integration** - Verify module interactions
6. **Maintain test environments** - Consistent, reproducible setups

**REPL Testing Benefits:**
- **Interactive development** - Immediate feedback during development
- **Environment control** - Precise control over blockchain state
- **Comprehensive coverage** - Unit, integration, and property testing
- **Performance monitoring** - Built-in gas measurement
- **Regression prevention** - Automated test execution

Testing is critical for smart contract reliability, especially given their immutable deployment and financial implications. Pact's comprehensive testing infrastructure enables confidence in contract correctness and performance.

## Exercises

1. Create a comprehensive test suite for a token contract with unit, property, and integration tests
2. Implement property-based testing for a DEX with invariant verification
3. Build performance regression tests for a lending protocol
4. Design security tests for common smart contract vulnerabilities
5. Create a test framework for multi-chain pact operations

## References

- REPL testing: `/pact-tests/Pact/Core/Test/ReplTestUtils.hs`
- Property testing: `/test-utils/Pact/Core/Gen.hs`
- Gas testing: `/pact-tests/Pact/Core/Test/GasGolden.hs`
- Test organization: `/pact-tests/PactCoreTests.hs`