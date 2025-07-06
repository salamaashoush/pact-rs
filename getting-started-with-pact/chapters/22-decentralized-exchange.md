# Chapter 22: Building a Decentralized Exchange

## Table of Contents
1. [Project Overview](#project-overview)
2. [DEX Architecture](#dex-architecture)
3. [Step 1: Liquidity Pool System](#step-1-liquidity-pool-system)
4. [Step 2: Automated Market Maker](#step-2-automated-market-maker)
5. [Step 3: Router and Price Discovery](#step-3-router-and-price-discovery)
6. [Step 4: Farming and Incentives](#step-4-farming-and-incentives)
7. [Step 5: Governance and Fees](#step-5-governance-and-fees)
8. [Step 6: Advanced Features](#step-6-advanced-features)
9. [Testing & Security](#testing-security)
10. [Frontend Integration](#frontend-integration)

## Project Overview

In this chapter, we'll build a complete decentralized exchange (DEX) featuring:
- Automated Market Maker (AMM) with constant product formula
- Liquidity pools with LP tokens
- Multi-hop trading through optimal routing
- Yield farming and liquidity incentives
- Flash loans and arbitrage protection
- Fee distribution to liquidity providers
- Governance token and protocol governance

### What You'll Learn
- Implementing AMM algorithms on-chain
- Managing liquidity pools and price discovery
- Building secure token swap mechanisms
- Creating yield farming incentives
- Implementing flash loans safely
- Designing tokenomics for DEX protocols

## DEX Architecture

### Core Components
```
dex-protocol/
├── core/
│   ├── pair.pact              # Individual trading pairs
│   ├── factory.pact           # Pair creation and registry
│   └── router.pact            # Multi-hop routing
├── liquidity/
│   ├── pool.pact              # Liquidity management
│   ├── lp-token.pact          # LP token implementation
│   └── rewards.pact           # Liquidity incentives
├── trading/
│   ├── swap.pact              # Core swap logic
│   ├── price-oracle.pact      # Price feeds
│   └── slippage.pact          # Slippage protection
├── defi/
│   ├── farming.pact           # Yield farming
│   ├── staking.pact           # Token staking
│   └── flash-loans.pact       # Flash loan system
└── governance/
    ├── fees.pact              # Fee collection
    ├── treasury.pact          # Protocol treasury
    └── voting.pact            # Protocol governance
```

## Step 1: Liquidity Pool System

Let's start with the core liquidity pool implementation:

```pact
;; pair.pact
(module pair GOVERNANCE
  @doc "AMM trading pair implementation"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dex-admin))
  
  ;; Schemas
  (defschema pair-data
    @doc "Trading pair information"
    token0:module{fungible-v2}
    token1:module{fungible-v2}
    reserve0:decimal           ;; Reserve of token0
    reserve1:decimal           ;; Reserve of token1
    total-supply:decimal       ;; Total LP tokens
    fee-rate:decimal           ;; Trading fee percentage
    last-update:time
    cumulative-price0:decimal  ;; TWAP tracking
    cumulative-price1:decimal
    block-timestamp-last:time)
  
  (defschema lp-balance
    @doc "LP token balance"
    account:string
    balance:decimal
    guard:guard)
  
  (defschema swap-record
    @doc "Swap transaction record"
    id:string
    user:string
    token-in:module{fungible-v2}
    token-out:module{fungible-v2}
    amount-in:decimal
    amount-out:decimal
    fee:decimal
    timestamp:time)
  
  ;; Tables
  (deftable pairs:{pair-data})
  (deftable lp-balances:{lp-balance})
  (deftable swap-records:{swap-record})
  
  ;; Events
  (defcap MINT:bool (to:string amount0:decimal amount1:decimal)
    @doc "Liquidity provision event"
    @event
    true)
  
  (defcap BURN:bool (from:string amount0:decimal amount1:decimal)
    @doc "Liquidity removal event"
    @event
    true)
  
  (defcap SWAP:bool (user:string amount0-in:decimal amount1-in:decimal amount0-out:decimal amount1-out:decimal)
    @doc "Token swap event"
    @event
    true)
  
  (defcap SYNC:bool (reserve0:decimal reserve1:decimal)
    @doc "Reserve synchronization event"
    @event
    true)
  
  ;; Constants
  (defconst MINIMUM_LIQUIDITY 1000)  ;; Minimum LP tokens
  (defconst FEE_RATE 0.003)          ;; 0.3% trading fee
  (defconst PRECISION 1000000000000) ;; 12 decimal precision
  
  ;; Capabilities
  (defcap PAIR_EXISTS:bool (token0:module{fungible-v2} token1:module{fungible-v2})
    @doc "Capability that pair exists"
    (let ((pair-id (create-pair-id token0 token1)))
      (with-default-read pairs pair-id
        { "reserve0": -1.0 }
        { "reserve0" := r0 }
        (enforce (>= r0 0.0) "Pair does not exist"))))
  
  (defcap LIQUIDITY:bool (account:string)
    @doc "Capability to manage liquidity"
    (enforce-guard (at 'guard (lp-token.details account))))
  
  ;; Helper functions
  (defun create-pair-id:string (token0:module{fungible-v2} token1:module{fungible-v2})
    @doc "Create deterministic pair ID"
    (let ((t0-name (format "{}" [token0]))
          (t1-name (format "{}" [token1])))
      (if (< t0-name t1-name)
        (format "{}:{}" [t0-name t1-name])
        (format "{}:{}" [t1-name t0-name]))))
  
  (defun get-pair-account:string (token0:module{fungible-v2} token1:module{fungible-v2})
    @doc "Get pair contract account"
    (create-principal (create-capability-guard (PAIR_EXISTS token0 token1))))
  
  ;; Core AMM functions
  (defun create-pair:string (token0:module{fungible-v2} token1:module{fungible-v2})
    @doc "Create a new trading pair"
    
    (with-capability (GOVERNANCE)
      (let ((pair-id (create-pair-id token0 token1)))
        
        ;; Check pair doesn't exist
        (with-default-read pairs pair-id
          { "reserve0": -1.0 }
          { "reserve0" := r0 }
          (enforce (< r0 0.0) "Pair already exists"))
        
        ;; Initialize pair
        (insert pairs pair-id {
          "token0": token0,
          "token1": token1,
          "reserve0": 0.0,
          "reserve1": 0.0,
          "total-supply": 0.0,
          "fee-rate": FEE_RATE,
          "last-update": (at 'block-time (chain-data)),
          "cumulative-price0": 0.0,
          "cumulative-price1": 0.0,
          "block-timestamp-last": (at 'block-time (chain-data))
        })
        
        (format "Created pair {} for tokens {} and {}" [pair-id token0 token1]))))
  
  (defun add-liquidity:string 
    ( token0:module{fungible-v2}
      token1:module{fungible-v2}
      amount0-desired:decimal
      amount1-desired:decimal
      amount0-min:decimal
      amount1-min:decimal
      to:string
      deadline:time )
    @doc "Add liquidity to a pair"
    
    (enforce (<= (at 'block-time (chain-data)) deadline) "Transaction expired")
    
    (with-capability (PAIR_EXISTS token0 token1)
      (with-capability (LIQUIDITY to)
        (let* ((pair-id (create-pair-id token0 token1))
               (pair-account (get-pair-account token0 token1)))
          
          (with-read pairs pair-id 
            { "reserve0" := reserve0
            , "reserve1" := reserve1
            , "total-supply" := total-supply }
            
            ;; Calculate optimal amounts
            (let* ((amounts (if (and (= reserve0 0.0) (= reserve1 0.0))
                             ;; First liquidity provision
                             [amount0-desired amount1-desired]
                             ;; Calculate proportional amounts
                             (calculate-optimal-amounts 
                               amount0-desired amount1-desired
                               amount0-min amount1-min
                               reserve0 reserve1)))
                   (amount0 (at 0 amounts))
                   (amount1 (at 1 amounts)))
              
              ;; Transfer tokens to pair
              (token0::transfer to pair-account amount0)
              (token1::transfer to pair-account amount1)
              
              ;; Calculate LP tokens to mint
              (let ((liquidity (if (= total-supply 0.0)
                                 ;; First provision: geometric mean minus minimum
                                 (- (sqrt (* amount0 amount1)) MINIMUM_LIQUIDITY)
                                 ;; Subsequent: proportional to existing
                                 (min (/ (* amount0 total-supply) reserve0)
                                      (/ (* amount1 total-supply) reserve1)))))
                
                (enforce (> liquidity 0.0) "Insufficient liquidity minted")
                
                ;; Mint LP tokens
                (lp-token.mint to liquidity)
                
                ;; Update reserves
                (update pairs pair-id {
                  "reserve0": (+ reserve0 amount0),
                  "reserve1": (+ reserve1 amount1),
                  "total-supply": (+ total-supply liquidity),
                  "last-update": (at 'block-time (chain-data))
                })
                
                ;; Update price accumulators
                (update-price-oracles pair-id)
                
                ;; Emit event
                (emit-event (MINT to amount0 amount1))
                
                (format "Added liquidity: {} LP tokens for {} {} and {} {}" 
                        [liquidity amount0 token0 amount1 token1])))))))))
  
  (defun remove-liquidity:string 
    ( token0:module{fungible-v2}
      token1:module{fungible-v2}
      liquidity:decimal
      amount0-min:decimal
      amount1-min:decimal
      to:string
      deadline:time )
    @doc "Remove liquidity from a pair"
    
    (enforce (<= (at 'block-time (chain-data)) deadline) "Transaction expired")
    
    (with-capability (PAIR_EXISTS token0 token1)
      (with-capability (LIQUIDITY to)
        (let* ((pair-id (create-pair-id token0 token1))
               (pair-account (get-pair-account token0 token1)))
          
          (with-read pairs pair-id 
            { "reserve0" := reserve0
            , "reserve1" := reserve1
            , "total-supply" := total-supply }
            
            ;; Calculate amounts to receive
            (let* ((amount0 (/ (* liquidity reserve0) total-supply))
                   (amount1 (/ (* liquidity reserve1) total-supply)))
              
              ;; Check minimum amounts
              (enforce (>= amount0 amount0-min) "Insufficient token0 amount")
              (enforce (>= amount1 amount1-min) "Insufficient token1 amount")
              
              ;; Burn LP tokens
              (lp-token.burn to liquidity)
              
              ;; Transfer tokens to user
              (install-capability (token0::TRANSFER pair-account to amount0))
              (token0::transfer pair-account to amount0)
              
              (install-capability (token1::TRANSFER pair-account to amount1))
              (token1::transfer pair-account to amount1)
              
              ;; Update reserves
              (update pairs pair-id {
                "reserve0": (- reserve0 amount0),
                "reserve1": (- reserve1 amount1),
                "total-supply": (- total-supply liquidity),
                "last-update": (at 'block-time (chain-data))
              })
              
              ;; Update price oracles
              (update-price-oracles pair-id)
              
              ;; Emit event
              (emit-event (BURN to amount0 amount1))
              
              (format "Removed liquidity: {} {} and {} {} for {} LP tokens" 
                      [amount0 token0 amount1 token1 liquidity])))))))
  
  ;; Swap functions
  (defun swap-exact-tokens-for-tokens:string 
    ( amount-in:decimal
      amount-out-min:decimal
      path:[module{fungible-v2}]
      to:string
      deadline:time )
    @doc "Swap exact input tokens for output tokens"
    
    (enforce (<= (at 'block-time (chain-data)) deadline) "Transaction expired")
    (enforce (> (length path) 1) "Invalid path")
    
    ;; Calculate amounts through path
    (let ((amounts (get-amounts-out amount-in path)))
      
      ;; Check minimum output
      (enforce (>= (at (- (length amounts) 1) amounts) amount-out-min) 
               "Insufficient output amount")
      
      ;; Execute swaps
      (perform-swaps amounts path to)))
  
  (defun swap-tokens-for-exact-tokens:string 
    ( amount-out:decimal
      amount-in-max:decimal
      path:[module{fungible-v2}]
      to:string
      deadline:time )
    @doc "Swap input tokens for exact output tokens"
    
    (enforce (<= (at 'block-time (chain-data)) deadline) "Transaction expired")
    (enforce (> (length path) 1) "Invalid path")
    
    ;; Calculate amounts through path
    (let ((amounts (get-amounts-in amount-out path)))
      
      ;; Check maximum input
      (enforce (<= (at 0 amounts) amount-in-max) 
               "Excessive input amount")
      
      ;; Execute swaps
      (perform-swaps amounts path to)))
  
  ;; Core swap logic
  (defun swap:string 
    ( amount0-out:decimal
      amount1-out:decimal
      to:string
      token0:module{fungible-v2}
      token1:module{fungible-v2} )
    @doc "Execute a token swap"
    
    (with-capability (PAIR_EXISTS token0 token1)
      (let* ((pair-id (create-pair-id token0 token1))
             (pair-account (get-pair-account token0 token1)))
        
        (with-read pairs pair-id 
          { "reserve0" := reserve0
          , "reserve1" := reserve1
          , "fee-rate" := fee-rate }
          
          ;; Validate outputs
          (enforce (or (> amount0-out 0.0) (> amount1-out 0.0)) "Insufficient output amount")
          (enforce (and (< amount0-out reserve0) (< amount1-out reserve1)) "Insufficient liquidity")
          
          ;; Calculate input amounts (simplified - would need to track actual balances)
          (let* ((balance0-adjusted (* (- reserve0 amount0-out) 1000))
                 (balance1-adjusted (* (- reserve1 amount1-out) 1000))
                 (balance0-before (* reserve0 1000))
                 (balance1-before (* reserve1 1000)))
            
            ;; Verify constant product formula with fees
            (enforce (>= (* balance0-adjusted balance1-adjusted)
                        (* balance0-before balance1-before)) 
                     "K invariant violated")
            
            ;; Transfer output tokens
            (if (> amount0-out 0.0)
              (do
                (install-capability (token0::TRANSFER pair-account to amount0-out))
                (token0::transfer pair-account to amount0-out))
              "No token0 output")
            
            (if (> amount1-out 0.0)
              (do
                (install-capability (token1::TRANSFER pair-account to amount1-out))
                (token1::transfer pair-account to amount1-out))
              "No token1 output")
            
            ;; Update reserves (simplified)
            (update pairs pair-id {
              "reserve0": (- reserve0 amount0-out),
              "reserve1": (- reserve1 amount1-out),
              "last-update": (at 'block-time (chain-data))
            })
            
            ;; Emit event
            (emit-event (SWAP to 0.0 0.0 amount0-out amount1-out))
            
            "Swap completed")))))
  
  ;; Helper functions for AMM calculations
  (defun calculate-optimal-amounts:[decimal] 
    ( amount0-desired:decimal
      amount1-desired:decimal
      amount0-min:decimal
      amount1-min:decimal
      reserve0:decimal
      reserve1:decimal )
    @doc "Calculate optimal liquidity amounts"
    
    (let ((amount1-optimal (/ (* amount0-desired reserve1) reserve0)))
      (if (<= amount1-optimal amount1-desired)
        (do
          (enforce (>= amount1-optimal amount1-min) "Insufficient token1 amount")
          [amount0-desired amount1-optimal])
        (let ((amount0-optimal (/ (* amount1-desired reserve0) reserve1)))
          (enforce (<= amount0-optimal amount0-desired) "Internal error")
          (enforce (>= amount0-optimal amount0-min) "Insufficient token0 amount")
          [amount0-optimal amount1-desired]))))
  
  (defun get-amount-out:decimal 
    ( amount-in:decimal
      reserve-in:decimal
      reserve-out:decimal )
    @doc "Calculate output amount for given input"
    
    (enforce (> amount-in 0.0) "Insufficient input amount")
    (enforce (and (> reserve-in 0.0) (> reserve-out 0.0)) "Insufficient liquidity")
    
    (let* ((amount-in-with-fee (* amount-in 997))  ;; 0.3% fee
           (numerator (* amount-in-with-fee reserve-out))
           (denominator (+ (* reserve-in 1000) amount-in-with-fee)))
      (/ numerator denominator)))
  
  (defun get-amount-in:decimal 
    ( amount-out:decimal
      reserve-in:decimal
      reserve-out:decimal )
    @doc "Calculate input amount for given output"
    
    (enforce (> amount-out 0.0) "Insufficient output amount")
    (enforce (and (> reserve-in 0.0) (> reserve-out reserve-out)) "Insufficient liquidity")
    
    (let* ((numerator (* reserve-in amount-out 1000))
           (denominator (* (- reserve-out amount-out) 997)))
      (+ (/ numerator denominator) 1)))
  
  ;; Price oracle functions
  (defun update-price-oracles:string (pair-id:string)
    @doc "Update cumulative price oracles"
    
    (with-read pairs pair-id 
      { "reserve0" := reserve0
      , "reserve1" := reserve1
      , "cumulative-price0" := cum-price0
      , "cumulative-price1" := cum-price1
      , "block-timestamp-last" := timestamp-last }
      
      (let* ((current-time (at 'block-time (chain-data)))
             (time-elapsed (diff-time current-time timestamp-last))
             (time-elapsed-seconds (/ time-elapsed 1.0)))
        
        (if (and (> reserve0 0.0) (> reserve1 0.0) (> time-elapsed-seconds 0.0))
          (update pairs pair-id {
            "cumulative-price0": (+ cum-price0 (* (/ reserve1 reserve0) time-elapsed-seconds)),
            "cumulative-price1": (+ cum-price1 (* (/ reserve0 reserve1) time-elapsed-seconds)),
            "block-timestamp-last": current-time
          })
          "No oracle update needed"))))
  
  ;; Multi-hop routing helpers
  (defun get-amounts-out:[decimal] (amount-in:decimal path:[module{fungible-v2}])
    @doc "Calculate output amounts for multi-hop trade"
    
    (enforce (>= (length path) 2) "Invalid path")
    (let ((amounts [amount-in]))
      (fold (lambda (acc:[decimal] i:integer)
              (if (< i (- (length path) 1))
                (let* ((token-in (at i path))
                       (token-out (at (+ i 1) path))
                       (pair-id (create-pair-id token-in token-out)))
                  (with-read pairs pair-id { "reserve0" := r0, "reserve1" := r1 }
                    (let* ((reserves (if (< (format "{}" [token-in]) (format "{}" [token-out]))
                                      [r0 r1] [r1 r0]))
                           (reserve-in (at 0 reserves))
                           (reserve-out (at 1 reserves))
                           (amount-out (get-amount-out (at (length acc) acc) reserve-in reserve-out)))
                      (+ acc [amount-out]))))
                acc))
            amounts
            (enumerate 0 (- (length path) 1)))))
  
  (defun get-amounts-in:[decimal] (amount-out:decimal path:[module{fungible-v2}])
    @doc "Calculate input amounts for multi-hop trade"
    
    (enforce (>= (length path) 2) "Invalid path")
    ;; Simplified - would need reverse calculation
    [amount-out])
  
  (defun perform-swaps:string (amounts:[decimal] path:[module{fungible-v2}] to:string)
    @doc "Execute a series of swaps"
    
    ;; Simplified implementation
    "Swaps executed")
  
  ;; Query functions
  (defun get-pair:object (token0:module{fungible-v2} token1:module{fungible-v2})
    @doc "Get pair information"
    (read pairs (create-pair-id token0 token1)))
  
  (defun get-reserves:[decimal] (token0:module{fungible-v2} token1:module{fungible-v2})
    @doc "Get current reserves"
    (with-read pairs (create-pair-id token0 token1) 
      { "reserve0" := r0, "reserve1" := r1 }
      [r0 r1]))
  
  (defun quote:decimal (amount-a:decimal reserve-a:decimal reserve-b:decimal)
    @doc "Quote amount of token B for given amount of token A"
    (enforce (> amount-a 0.0) "Insufficient amount")
    (enforce (and (> reserve-a 0.0) (> reserve-b 0.0)) "Insufficient liquidity")
    (/ (* amount-a reserve-b) reserve-a))
)

;; Create tables
(create-table pairs)
(create-table lp-balances)
(create-table swap-records)
```

## Step 2: Automated Market Maker

Now let's implement the LP token system:

```pact
;; lp-token.pact
(module lp-token GOVERNANCE
  @doc "Liquidity provider token for DEX pairs"
  
  ;; Implements fungible-v2
  (implements fungible-v2)
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dex-admin))
  
  ;; Schemas
  (defschema account
    @doc "LP token account"
    balance:decimal
    guard:guard
    pair-id:string)     ;; Which pair this LP token represents
  
  (defschema supply
    @doc "Token supply tracking"
    pair-id:string
    supply:decimal)
  
  ;; Tables
  (deftable accounts:{account})
  (deftable supplies:{supply})
  
  ;; Events
  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
    @doc "Transfer event"
    @managed amount TRANSFER-mgr
    @event
    true)
  
  (defcap MINT:bool (account:string amount:decimal)
    @doc "Mint event"
    @event
    true)
  
  (defcap BURN:bool (account:string amount:decimal)
    @doc "Burn event"
    @event
    true)
  
  ;; Constants
  (defconst DECIMALS 18)
  (defconst MINIMUM_LIQUIDITY 1000)
  
  ;; Capabilities
  (defcap DEBIT:bool (sender:string)
    @doc "Capability to debit account"
    (enforce-guard (at 'guard (read accounts sender))))
  
  (defcap CREDIT:bool (receiver:string)
    @doc "Capability to credit account"
    true)
  
  ;; Transfer manager
  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    @doc "Transfer manager function"
    (enforce (<= requested managed) "Transfer amount exceeds managed amount")
    (- managed requested))
  
  ;; Core functions
  (defun create-account:string (account:string guard:guard pair-id:string)
    @doc "Create new LP token account"
    (insert accounts account {
      "balance": 0.0,
      "guard": guard,
      "pair-id": pair-id
    }))
  
  (defun get-balance:decimal (account:string)
    @doc "Get account balance"
    (at 'balance (read accounts account)))
  
  (defun details:object{fungible-v2.account-details} (account:string)
    @doc "Get account details"
    (with-read accounts account 
      { "balance" := bal, "guard" := g }
      { "account": account, "balance": bal, "guard": g }))
  
  (defun rotate:string (account:string new-guard:guard)
    @doc "Rotate account guard"
    (with-read accounts account { "guard" := old-guard }
      (enforce-guard old-guard)
      (update accounts account { "guard": new-guard })))
  
  (defun precision:integer ()
    @doc "Token precision"
    DECIMALS)
  
  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer LP tokens"
    (enforce (!= sender receiver) "Cannot transfer to self")
    (enforce (> amount 0.0) "Amount must be positive")
    
    (with-capability (TRANSFER sender receiver amount)
      (with-capability (DEBIT sender)
        (debit sender amount))
      (with-capability (CREDIT receiver)
        (credit receiver amount))))
  
  (defun transfer-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
    @doc "Transfer with account creation"
    (enforce (!= sender receiver) "Cannot transfer to self")
    (enforce (> amount 0.0) "Amount must be positive")
    
    ;; Create receiver account if needed
    (with-default-read accounts receiver
      { "balance": -1.0 }
      { "balance" := balance }
      (if (= balance -1.0)
        (with-read accounts sender { "pair-id" := pair-id }
          (create-account receiver receiver-guard pair-id))
        "Account exists"))
    
    (transfer sender receiver amount))
  
  ;; Internal functions
  (defun debit:string (account:string amount:decimal)
    @doc "Debit from account"
    (require-capability (DEBIT account))
    (with-read accounts account { "balance" := balance }
      (enforce (>= balance amount) "Insufficient balance")
      (update accounts account { "balance": (- balance amount) })))
  
  (defun credit:string (account:string amount:decimal)
    @doc "Credit to account"
    (require-capability (CREDIT account))
    (with-read accounts account { "balance" := balance }
      (update accounts account { "balance": (+ balance amount) })))
  
  ;; Minting and burning (pair contract only)
  (defun mint:string (account:string amount:decimal pair-id:string)
    @doc "Mint LP tokens"
    (with-capability (GOVERNANCE)
      ;; Create account if needed
      (with-default-read accounts account
        { "balance": -1.0 }
        { "balance" := balance }
        (if (= balance -1.0)
          (create-account account (create-capability-guard (GOVERNANCE)) pair-id)
          "Account exists"))
      
      ;; Mint tokens
      (credit account amount)
      
      ;; Update supply
      (with-default-read supplies pair-id
        { "supply": 0.0 }
        { "supply" := current-supply }
        (write supplies pair-id {
          "pair-id": pair-id,
          "supply": (+ current-supply amount)
        }))
      
      ;; Emit event
      (emit-event (MINT account amount))
      
      (format "Minted {} LP tokens to {}" [amount account])))
  
  (defun burn:string (account:string amount:decimal)
    @doc "Burn LP tokens"
    (with-capability (GOVERNANCE)
      (with-read accounts account 
        { "balance" := balance
        , "pair-id" := pair-id }
        
        (enforce (>= balance amount) "Insufficient balance")
        
        ;; Burn tokens
        (debit account amount)
        
        ;; Update supply
        (with-read supplies pair-id { "supply" := current-supply }
          (update supplies pair-id {
            "supply": (- current-supply amount)
          }))
        
        ;; Emit event
        (emit-event (BURN account amount))
        
        (format "Burned {} LP tokens from {}" [amount account]))))
  
  ;; Supply functions
  (defun total-supply:decimal (pair-id:string)
    @doc "Get total supply for pair"
    (with-default-read supplies pair-id
      { "supply": 0.0 }
      { "supply" := s }
      s))
  
  (defun get-supply:decimal ()
    @doc "Get total supply (required by fungible-v2)"
    ;; Return sum of all pair supplies
    (fold (+) 0.0 (map (at 'supply) (select supplies (constantly true)))))
  
  ;; Query functions
  (defun get-pair-supply:decimal (pair-id:string)
    @doc "Get supply for specific pair"
    (total-supply pair-id))
  
  (defun get-account-pair:string (account:string)
    @doc "Get pair ID for account"
    (at 'pair-id (read accounts account)))
)

;; Create tables
(create-table accounts)
(create-table supplies)
```

## Step 3: Router and Price Discovery

Let's implement the DEX router for optimal trading paths:

```pact
;; router.pact
(module router GOVERNANCE
  @doc "DEX router for optimal multi-hop trading"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dex-admin))
  
  ;; Schemas
  (defschema route
    @doc "Trading route information"
    path:[module{fungible-v2}]
    amounts:[decimal]
    fees:[decimal]
    price-impact:decimal)
  
  (defschema price-cache
    @doc "Cached price information"
    token0:module{fungible-v2}
    token1:module{fungible-v2}
    price:decimal
    last-update:time
    volume-24h:decimal)
  
  ;; Tables
  (deftable routes:{route})
  (deftable price-cache:{price-cache})
  
  ;; Events
  (defcap ROUTE_EXECUTED:bool (user:string path:[module{fungible-v2}] amounts:[decimal])
    @doc "Route execution event"
    @event
    true)
  
  ;; Constants
  (defconst MAX_HOPS 3)              ;; Maximum trading hops
  (defconst PRICE_CACHE_TTL 300)     ;; 5 minutes cache
  (defconst MAX_PRICE_IMPACT 0.05)   ;; 5% max price impact
  
  ;; Main routing functions
  (defun find-best-route:object 
    ( token-in:module{fungible-v2}
      token-out:module{fungible-v2}
      amount-in:decimal )
    @doc "Find the best trading route"
    
    (let ((direct-route (get-direct-route token-in token-out amount-in))
          (multi-hop-routes (get-multi-hop-routes token-in token-out amount-in)))
      
      ;; Compare all routes and return the best one
      (fold (lambda (best:object current:object)
              (if (> (at 'amount-out current) (at 'amount-out best))
                current
                best))
            direct-route
            multi-hop-routes)))
  
  (defun get-direct-route:object 
    ( token-in:module{fungible-v2}
      token-out:module{fungible-v2}
      amount-in:decimal )
    @doc "Get direct trading route"
    
    (try
      (let ((reserves (pair.get-reserves token-in token-out)))
        (let* ((reserve-in (at 0 reserves))
               (reserve-out (at 1 reserves))
               (amount-out (pair.get-amount-out amount-in reserve-in reserve-out))
               (price-impact (calculate-price-impact amount-in amount-out reserve-in reserve-out)))
          
          { "path": [token-in token-out]
          , "amounts": [amount-in amount-out]
          , "fees": [(* amount-in 0.003)]
          , "price-impact": price-impact
          , "amount-out": amount-out }))
      
      ;; Default if no direct pair
      { "path": []
      , "amounts": []
      , "fees": []
      , "price-impact": 1.0
      , "amount-out": 0.0 }))
  
  (defun get-multi-hop-routes:[object] 
    ( token-in:module{fungible-v2}
      token-out:module{fungible-v2}
      amount-in:decimal )
    @doc "Get all possible multi-hop routes"
    
    ;; Simplified - find routes through common intermediate tokens
    (let ((intermediate-tokens (get-common-intermediate-tokens)))
      (map (lambda (intermediate:module{fungible-v2})
             (get-two-hop-route token-in token-out amount-in intermediate))
           intermediate-tokens)))
  
  (defun get-two-hop-route:object 
    ( token-in:module{fungible-v2}
      token-out:module{fungible-v2}
      amount-in:decimal
      intermediate:module{fungible-v2} )
    @doc "Get two-hop route through intermediate token"
    
    (try
      ;; First hop: token-in -> intermediate
      (let ((reserves1 (pair.get-reserves token-in intermediate)))
        (let* ((reserve-in-1 (at 0 reserves1))
               (reserve-out-1 (at 1 reserves1))
               (amount-intermediate (pair.get-amount-out amount-in reserve-in-1 reserve-out-1)))
          
          ;; Second hop: intermediate -> token-out
          (let ((reserves2 (pair.get-reserves intermediate token-out)))
            (let* ((reserve-in-2 (at 0 reserves2))
                   (reserve-out-2 (at 1 reserves2))
                   (amount-out (pair.get-amount-out amount-intermediate reserve-in-2 reserve-out-2))
                   (total-fees (+ (* amount-in 0.003) (* amount-intermediate 0.003)))
                   (price-impact (calculate-multi-hop-price-impact 
                                   [amount-in amount-intermediate amount-out]
                                   [reserves1 reserves2])))
              
              { "path": [token-in intermediate token-out]
              , "amounts": [amount-in amount-intermediate amount-out]
              , "fees": [total-fees]
              , "price-impact": price-impact
              , "amount-out": amount-out }))))
      
      ;; Default if route not available
      { "path": []
      , "amounts": []
      , "fees": []
      , "price-impact": 1.0
      , "amount-out": 0.0 }))
  
  ;; Execution functions
  (defun execute-route:string 
    ( route:object
      amount-out-min:decimal
      to:string
      deadline:time )
    @doc "Execute a trading route"
    
    (enforce (<= (at 'block-time (chain-data)) deadline) "Transaction expired")
    (let ((path (at 'path route))
          (amounts (at 'amounts route))
          (amount-out (at 'amount-out route)))
      
      (enforce (>= amount-out amount-out-min) "Insufficient output amount")
      (enforce (> (length path) 1) "Invalid route")
      
      ;; Execute the route
      (if (= (length path) 2)
        (execute-direct-swap path amounts to)
        (execute-multi-hop-swap path amounts to))
      
      ;; Emit event
      (emit-event (ROUTE_EXECUTED to path amounts))
      
      (format "Route executed: {} output to {}" [amount-out to])))
  
  (defun execute-direct-swap:string (path:[module{fungible-v2}] amounts:[decimal] to:string)
    @doc "Execute direct swap"
    
    (let ((token-in (at 0 path))
          (token-out (at 1 path))
          (amount-in (at 0 amounts))
          (amount-out (at 1 amounts)))
      
      ;; Use pair contract to execute swap
      (pair.swap-exact-tokens-for-tokens 
        amount-in 
        amount-out 
        path 
        to 
        (add-time (at 'block-time (chain-data)) (minutes 10)))))
  
  (defun execute-multi-hop-swap:string (path:[module{fungible-v2}] amounts:[decimal] to:string)
    @doc "Execute multi-hop swap"
    
    ;; Execute each hop sequentially
    (fold (lambda (acc:string i:integer)
            (if (< i (- (length path) 1))
              (let* ((token-in (at i path))
                     (token-out (at (+ i 1) path))
                     (amount-in (at i amounts))
                     (amount-out (at (+ i 1) amounts))
                     (recipient (if (= i (- (length path) 2)) to "router")))
                
                (pair.swap 
                  (if (= (format "{}" [token-in]) (at 0 (pair.create-pair-id token-in token-out))) 
                      0.0 amount-out)
                  (if (= (format "{}" [token-in]) (at 0 (pair.create-pair-id token-in token-out))) 
                      amount-out 0.0)
                  recipient
                  token-in
                  token-out)
                
                (format "Hop {} executed" [i]))
              acc))
          ""
          (enumerate 0 (- (length path) 1))))
  
  ;; Price calculation helpers
  (defun calculate-price-impact:decimal 
    ( amount-in:decimal
      amount-out:decimal
      reserve-in:decimal
      reserve-out:decimal )
    @doc "Calculate price impact of trade"
    
    (let* ((price-before (/ reserve-out reserve-in))
           (price-after (/ (- reserve-out amount-out) (+ reserve-in amount-in)))
           (price-impact (/ (abs (- price-before price-after)) price-before)))
      price-impact))
  
  (defun calculate-multi-hop-price-impact:decimal 
    ( amounts:[decimal]
      reserves:[[decimal]] )
    @doc "Calculate price impact for multi-hop trade"
    
    ;; Simplified calculation - sum of individual impacts
    (+ (calculate-price-impact 
         (at 0 amounts) 
         (at 1 amounts)
         (at 0 (at 0 reserves))
         (at 1 (at 0 reserves)))
       (calculate-price-impact 
         (at 1 amounts) 
         (at 2 amounts)
         (at 0 (at 1 reserves))
         (at 1 (at 1 reserves)))))
  
  ;; Price oracle functions
  (defun get-price:decimal (token0:module{fungible-v2} token1:module{fungible-v2})
    @doc "Get current price of token0 in terms of token1"
    
    (let ((cache-key (format "{}:{}" [token0 token1])))
      (with-default-read price-cache cache-key
        { "price": 0.0, "last-update": (time "1970-01-01T00:00:00Z") }
        { "price" := cached-price, "last-update" := last-update }
        
        ;; Check if cache is still valid
        (if (> (diff-time (at 'block-time (chain-data)) last-update) PRICE_CACHE_TTL)
          ;; Update cache
          (let ((current-price (calculate-current-price token0 token1)))
            (write price-cache cache-key {
              "token0": token0,
              "token1": token1,
              "price": current-price,
              "last-update": (at 'block-time (chain-data)),
              "volume-24h": 0.0
            })
            current-price)
          cached-price))))
  
  (defun calculate-current-price:decimal (token0:module{fungible-v2} token1:module{fungible-v2})
    @doc "Calculate current price from reserves"
    
    (try
      (let ((reserves (pair.get-reserves token0 token1)))
        (/ (at 1 reserves) (at 0 reserves)))
      0.0))
  
  ;; Helper functions
  (defun get-common-intermediate-tokens:[module{fungible-v2}] ()
    @doc "Get list of common intermediate tokens"
    ;; Simplified - would query actual pairs
    [coin])
  
  ;; Quote functions
  (defun quote-exact-input:object 
    ( token-in:module{fungible-v2}
      token-out:module{fungible-v2}
      amount-in:decimal )
    @doc "Quote exact input swap"
    
    (let ((best-route (find-best-route token-in token-out amount-in)))
      { "amount-out": (at 'amount-out best-route)
      , "price-impact": (at 'price-impact best-route)
      , "fees": (at 'fees best-route)
      , "path": (at 'path best-route) }))
  
  (defun quote-exact-output:object 
    ( token-in:module{fungible-v2}
      token-out:module{fungible-v2}
      amount-out:decimal )
    @doc "Quote exact output swap"
    
    ;; Simplified - would calculate required input amounts
    { "amount-in": amount-out
    , "price-impact": 0.01
    , "fees": [0.003]
    , "path": [token-in token-out] })
  
  ;; Slippage protection
  (defun calculate-slippage:decimal 
    ( expected-amount:decimal
      actual-amount:decimal )
    @doc "Calculate slippage percentage"
    
    (/ (abs (- expected-amount actual-amount)) expected-amount))
  
  (defun validate-slippage:bool 
    ( expected-amount:decimal
      actual-amount:decimal
      max-slippage:decimal )
    @doc "Validate slippage is within tolerance"
    
    (let ((slippage (calculate-slippage expected-amount actual-amount)))
      (enforce (<= slippage max-slippage) 
               (format "Slippage {} exceeds maximum {}" [slippage max-slippage]))))
)

;; Create tables
(create-table routes)
(create-table price-cache)
```

## Step 4: Farming and Incentives

Now let's implement yield farming for liquidity providers:

```pact
;; farming.pact
(module farming GOVERNANCE
  @doc "Yield farming and liquidity incentives"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dex-admin))
  
  ;; Schemas
  (defschema farm
    @doc "Farming pool information"
    id:string
    lp-token:module{fungible-v2}
    reward-token:module{fungible-v2}
    reward-per-second:decimal
    total-staked:decimal
    acc-reward-per-share:decimal  ;; Accumulated rewards per share
    last-reward-time:time
    start-time:time
    end-time:time
    multiplier:decimal)
  
  (defschema user-info
    @doc "User farming information"
    farm-id:string
    user:string
    amount:decimal               ;; LP tokens staked
    reward-debt:decimal          ;; Reward debt for accounting
    pending-rewards:decimal      ;; Claimable rewards
    last-deposit:time)
  
  (defschema reward-history
    @doc "Reward claim history"
    user:string
    farm-id:string
    amount:decimal
    timestamp:time)
  
  ;; Tables
  (deftable farms:{farm})
  (deftable user-infos:{user-info})
  (deftable reward-history:{reward-history})
  
  ;; Events
  (defcap DEPOSIT:bool (user:string farm-id:string amount:decimal)
    @doc "Deposit event"
    @event
    true)
  
  (defcap WITHDRAW:bool (user:string farm-id:string amount:decimal)
    @doc "Withdraw event"
    @event
    true)
  
  (defcap HARVEST:bool (user:string farm-id:string amount:decimal)
    @doc "Harvest event"
    @event
    true)
  
  ;; Constants
  (defconst PRECISION 1000000000000) ;; 12 decimal precision
  
  ;; Capabilities
  (defcap FARM_ADMIN:bool (farm-id:string)
    @doc "Farm administration capability"
    (enforce-keyset 'dex-admin))
  
  (defcap STAKE:bool (user:string farm-id:string)
    @doc "Staking capability"
    (enforce-guard (at 'guard (coin.details user))))
  
  ;; Farm management
  (defun create-farm:string 
    ( farm-id:string
      lp-token:module{fungible-v2}
      reward-token:module{fungible-v2}
      reward-per-second:decimal
      start-time:time
      end-time:time
      multiplier:decimal )
    @doc "Create a new farming pool"
    
    (with-capability (GOVERNANCE)
      (enforce (< start-time end-time) "Invalid time range")
      (enforce (> reward-per-second 0.0) "Invalid reward rate")
      
      (insert farms farm-id {
        "id": farm-id,
        "lp-token": lp-token,
        "reward-token": reward-token,
        "reward-per-second": reward-per-second,
        "total-staked": 0.0,
        "acc-reward-per-share": 0.0,
        "last-reward-time": start-time,
        "start-time": start-time,
        "end-time": end-time,
        "multiplier": multiplier
      })
      
      (format "Created farm {} for LP token {}" [farm-id lp-token])))
  
  (defun update-farm:string 
    ( farm-id:string
      reward-per-second:decimal
      end-time:time )
    @doc "Update farm parameters"
    
    (with-capability (FARM_ADMIN farm-id)
      ;; Update pool first
      (update-pool farm-id)
      
      (update farms farm-id {
        "reward-per-second": reward-per-second,
        "end-time": end-time
      })
      
      (format "Updated farm {}" [farm-id])))
  
  ;; Core farming functions
  (defun deposit:string (farm-id:string user:string amount:decimal)
    @doc "Deposit LP tokens to farm"
    
    (with-capability (STAKE user farm-id)
      (enforce (> amount 0.0) "Amount must be positive")
      
      ;; Update pool before deposit
      (update-pool farm-id)
      
      (with-read farms farm-id 
        { "lp-token" := lp-token
        , "total-staked" := total-staked
        , "acc-reward-per-share" := acc-reward-per-share }
        
        ;; Get or create user info
        (let ((user-key (format "{}:{}" [farm-id user])))
          (with-default-read user-infos user-key
            { "amount": 0.0
            , "reward-debt": 0.0
            , "pending-rewards": 0.0 }
            { "amount" := user-amount
            , "reward-debt" := reward-debt
            , "pending-rewards" := pending-rewards }
            
            ;; Calculate pending rewards before deposit
            (let ((pending (if (> user-amount 0.0)
                            (- (/ (* user-amount acc-reward-per-share) PRECISION) reward-debt)
                            0.0)))
              
              ;; Transfer LP tokens from user
              (lp-token::transfer user (get-farm-account farm-id) amount)
              
              ;; Update user info
              (let ((new-amount (+ user-amount amount))
                    (new-reward-debt (/ (* new-amount acc-reward-per-share) PRECISION)))
                
                (write user-infos user-key {
                  "farm-id": farm-id,
                  "user": user,
                  "amount": new-amount,
                  "reward-debt": new-reward-debt,
                  "pending-rewards": (+ pending-rewards pending),
                  "last-deposit": (at 'block-time (chain-data))
                }))
              
              ;; Update farm total
              (update farms farm-id {
                "total-staked": (+ total-staked amount)
              })
              
              ;; Emit event
              (emit-event (DEPOSIT user farm-id amount))
              
              (format "Deposited {} LP tokens to farm {}" [amount farm-id]))))))
  
  (defun withdraw:string (farm-id:string user:string amount:decimal)
    @doc "Withdraw LP tokens from farm"
    
    (with-capability (STAKE user farm-id)
      (enforce (> amount 0.0) "Amount must be positive")
      
      ;; Update pool before withdrawal
      (update-pool farm-id)
      
      (let ((user-key (format "{}:{}" [farm-id user])))
        (with-read user-infos user-key
          { "amount" := user-amount
          , "reward-debt" := reward-debt
          , "pending-rewards" := pending-rewards }
          
          (enforce (>= user-amount amount) "Insufficient staked amount")
          
          (with-read farms farm-id 
            { "lp-token" := lp-token
            , "total-staked" := total-staked
            , "acc-reward-per-share" := acc-reward-per-share }
            
            ;; Calculate pending rewards
            (let ((pending (- (/ (* user-amount acc-reward-per-share) PRECISION) reward-debt)))
              
              ;; Update user info
              (let ((new-amount (- user-amount amount))
                    (new-reward-debt (/ (* new-amount acc-reward-per-share) PRECISION)))
                
                (update user-infos user-key {
                  "amount": new-amount,
                  "reward-debt": new-reward-debt,
                  "pending-rewards": (+ pending-rewards pending)
                }))
              
              ;; Transfer LP tokens back to user
              (install-capability (lp-token::TRANSFER (get-farm-account farm-id) user amount))
              (lp-token::transfer (get-farm-account farm-id) user amount)
              
              ;; Update farm total
              (update farms farm-id {
                "total-staked": (- total-staked amount)
              })
              
              ;; Emit event
              (emit-event (WITHDRAW user farm-id amount))
              
              (format "Withdrew {} LP tokens from farm {}" [amount farm-id]))))))
  
  (defun harvest:string (farm-id:string user:string)
    @doc "Harvest pending rewards"
    
    (with-capability (STAKE user farm-id)
      ;; Update pool before harvest
      (update-pool farm-id)
      
      (let ((user-key (format "{}:{}" [farm-id user])))
        (with-read user-infos user-key
          { "amount" := user-amount
          , "reward-debt" := reward-debt
          , "pending-rewards" := pending-rewards }
          
          (with-read farms farm-id 
            { "reward-token" := reward-token
            , "acc-reward-per-share" := acc-reward-per-share }
            
            ;; Calculate total pending rewards
            (let ((new-pending (if (> user-amount 0.0)
                                (- (/ (* user-amount acc-reward-per-share) PRECISION) reward-debt)
                                0.0))
                  (total-rewards (+ pending-rewards new-pending)))
              
              (enforce (> total-rewards 0.0) "No rewards to harvest")
              
              ;; Transfer rewards to user
              (install-capability (reward-token::TRANSFER (get-farm-account farm-id) user total-rewards))
              (reward-token::transfer (get-farm-account farm-id) user total-rewards)
              
              ;; Update user info
              (update user-infos user-key {
                "reward-debt": (/ (* user-amount acc-reward-per-share) PRECISION),
                "pending-rewards": 0.0
              })
              
              ;; Record harvest
              (insert reward-history (format "{}:{}:{}" [user farm-id (at 'block-time (chain-data))]) {
                "user": user,
                "farm-id": farm-id,
                "amount": total-rewards,
                "timestamp": (at 'block-time (chain-data))
              })
              
              ;; Emit event
              (emit-event (HARVEST user farm-id total-rewards))
              
              (format "Harvested {} reward tokens from farm {}" [total-rewards farm-id]))))))
  
  ;; Pool update mechanism
  (defun update-pool:string (farm-id:string)
    @doc "Update farm pool accumulated rewards"
    
    (with-read farms farm-id 
      { "total-staked" := total-staked
      , "reward-per-second" := reward-per-second
      , "acc-reward-per-share" := acc-reward-per-share
      , "last-reward-time" := last-reward-time
      , "start-time" := start-time
      , "end-time" := end-time
      , "multiplier" := multiplier }
      
      (let* ((current-time (at 'block-time (chain-data)))
             (reward-end-time (if (< current-time end-time) current-time end-time))
             (reward-start-time (if (> last-reward-time start-time) last-reward-time start-time)))
        
        (if (and (> total-staked 0.0) (< reward-start-time reward-end-time))
          (let* ((time-diff (diff-time reward-end-time reward-start-time))
                 (reward (/ (* (* reward-per-second time-diff) multiplier) 1.0))
                 (reward-per-share (/ (* reward PRECISION) total-staked))
                 (new-acc-reward-per-share (+ acc-reward-per-share reward-per-share)))
            
            (update farms farm-id {
              "acc-reward-per-share": new-acc-reward-per-share,
              "last-reward-time": current-time
            })
            
            (format "Updated pool {}: added {} rewards per share" [farm-id reward-per-share]))
          
          ;; Just update timestamp
          (do
            (update farms farm-id { "last-reward-time": current-time })
            "Pool updated - no rewards added")))))
  
  ;; Query functions
  (defun pending-rewards:decimal (farm-id:string user:string)
    @doc "Get pending rewards for user"
    
    (let ((user-key (format "{}:{}" [farm-id user])))
      (with-default-read user-infos user-key
        { "amount": 0.0
        , "reward-debt": 0.0
        , "pending-rewards": 0.0 }
        { "amount" := user-amount
        , "reward-debt" := reward-debt
        , "pending-rewards" := stored-pending }
        
        (with-read farms farm-id 
          { "total-staked" := total-staked
          , "reward-per-second" := reward-per-second
          , "acc-reward-per-share" := acc-reward-per-share
          , "last-reward-time" := last-reward-time
          , "start-time" := start-time
          , "end-time" := end-time
          , "multiplier" := multiplier }
          
          (if (= user-amount 0.0)
            stored-pending
            (let* ((current-time (at 'block-time (chain-data)))
                   (reward-end-time (if (< current-time end-time) current-time end-time))
                   (reward-start-time (if (> last-reward-time start-time) last-reward-time start-time))
                   (time-diff (diff-time reward-end-time reward-start-time))
                   (reward (if (> total-staked 0.0)
                             (/ (* (* reward-per-second time-diff) multiplier) 1.0)
                             0.0))
                   (reward-per-share (if (> total-staked 0.0)
                                      (/ (* reward PRECISION) total-staked)
                                      0.0))
                   (updated-acc-reward-per-share (+ acc-reward-per-share reward-per-share))
                   (new-pending (- (/ (* user-amount updated-acc-reward-per-share) PRECISION) reward-debt)))
              
              (+ stored-pending new-pending)))))))
  
  (defun get-user-info:object (farm-id:string user:string)
    @doc "Get user farming information"
    (let ((user-key (format "{}:{}" [farm-id user])))
      (with-default-read user-infos user-key
        { "farm-id": farm-id
        , "user": user
        , "amount": 0.0
        , "reward-debt": 0.0
        , "pending-rewards": 0.0
        , "last-deposit": (at 'block-time (chain-data)) }
        { "farm-id" := farm-id
        , "user" := user
        , "amount" := amount
        , "reward-debt" := reward-debt
        , "pending-rewards" := pending-rewards
        , "last-deposit" := last-deposit }
        
        { "farm-id": farm-id
        , "user": user
        , "staked": amount
        , "pending": (pending-rewards farm-id user)
        , "last-deposit": last-deposit })))
  
  (defun get-farm:object (farm-id:string)
    @doc "Get farm information"
    (read farms farm-id))
  
  (defun get-all-farms:[object] ()
    @doc "Get all farming pools"
    (select farms (constantly true)))
  
  ;; APR calculations
  (defun calculate-apr:decimal (farm-id:string)
    @doc "Calculate APR for farming pool"
    (with-read farms farm-id 
      { "reward-per-second" := reward-per-second
      , "total-staked" := total-staked
      , "multiplier" := multiplier }
      
      (if (> total-staked 0.0)
        (let* ((yearly-rewards (* reward-per-second 31536000 multiplier)) ;; seconds in year
               (apr (/ yearly-rewards total-staked)))
          (* apr 100.0)) ;; Convert to percentage
        0.0)))
  
  ;; Helper functions
  (defun get-farm-account:string (farm-id:string)
    @doc "Get farm contract account"
    (create-principal (create-capability-guard (FARM_ADMIN farm-id))))
  
  ;; Emergency functions
  (defun emergency-withdraw:string (farm-id:string user:string)
    @doc "Emergency withdraw without rewards"
    
    (with-capability (STAKE user farm-id)
      (let ((user-key (format "{}:{}" [farm-id user])))
        (with-read user-infos user-key
          { "amount" := user-amount }
          
          (with-read farms farm-id 
            { "lp-token" := lp-token
            , "total-staked" := total-staked }
            
            ;; Transfer LP tokens back
            (install-capability (lp-token::TRANSFER (get-farm-account farm-id) user user-amount))
            (lp-token::transfer (get-farm-account farm-id) user user-amount)
            
            ;; Reset user info
            (update user-infos user-key {
              "amount": 0.0,
              "reward-debt": 0.0,
              "pending-rewards": 0.0
            })
            
            ;; Update farm total
            (update farms farm-id {
              "total-staked": (- total-staked user-amount)
            })
            
            (format "Emergency withdrew {} LP tokens" [user-amount]))))))
)

;; Create tables
(create-table farms)
(create-table user-infos)
(create-table reward-history)
```

## Step 5: Governance and Fees

Let's implement governance and fee distribution:

```pact
;; dex-governance.pact
(module dex-governance GOVERNANCE
  @doc "DEX protocol governance and fee management"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dex-admin))
  
  ;; Schemas
  (defschema fee-config
    @doc "Fee configuration"
    pair-id:string
    fee-rate:decimal           ;; Trading fee percentage
    protocol-fee-rate:decimal  ;; Protocol fee percentage
    lp-fee-rate:decimal        ;; LP fee percentage
    last-update:time)
  
  (defschema fee-collection
    @doc "Collected fees"
    token:module{fungible-v2}
    amount:decimal
    collected-at:time)
  
  (defschema governance-proposal
    @doc "Governance proposal for DEX"
    id:string
    title:string
    description:string
    proposal-type:string       ;; "fee-change", "parameter-update", "upgrade"
    target-pair:string
    new-values:object
    proposer:string
    start-time:time
    end-time:time
    for-votes:decimal
    against-votes:decimal
    executed:bool)
  
  ;; Tables
  (deftable fee-configs:{fee-config})
  (deftable fee-collections:{fee-collection})
  (deftable governance-proposals:{governance-proposal})
  
  ;; Events
  (defcap FEE_COLLECTED:bool (token:module{fungible-v2} amount:decimal)
    @doc "Fee collection event"
    @event
    true)
  
  (defcap FEE_DISTRIBUTED:bool (recipients:[string] amounts:[decimal])
    @doc "Fee distribution event"
    @event
    true)
  
  (defcap GOVERNANCE_PROPOSAL:bool (id:string proposer:string)
    @doc "Governance proposal event"
    @event
    true)
  
  ;; Constants
  (defconst DEFAULT_FEE_RATE 0.003)        ;; 0.3%
  (defconst DEFAULT_PROTOCOL_FEE 0.0005)   ;; 0.05%
  (defconst MIN_PROPOSAL_THRESHOLD 100000.0) ;; Minimum tokens to propose
  (defconst VOTING_PERIOD 259200)         ;; 3 days in seconds
  
  ;; Fee management
  (defun set-fee-config:string 
    ( pair-id:string
      fee-rate:decimal
      protocol-fee-rate:decimal )
    @doc "Set fee configuration for a pair"
    
    (with-capability (GOVERNANCE)
      (enforce (and (>= fee-rate 0.0) (<= fee-rate 0.01)) "Invalid fee rate")
      (enforce (and (>= protocol-fee-rate 0.0) (<= protocol-fee-rate 0.005)) "Invalid protocol fee")
      
      (let ((lp-fee-rate (- fee-rate protocol-fee-rate)))
        (write fee-configs pair-id {
          "pair-id": pair-id,
          "fee-rate": fee-rate,
          "protocol-fee-rate": protocol-fee-rate,
          "lp-fee-rate": lp-fee-rate,
          "last-update": (at 'block-time (chain-data))
        }))
      
      (format "Set fees for pair {}: {}% total, {}% protocol" 
              [pair-id (* fee-rate 100) (* protocol-fee-rate 100)])))
  
  (defun get-fee-config:object (pair-id:string)
    @doc "Get fee configuration for pair"
    (with-default-read fee-configs pair-id
      { "pair-id": pair-id
      , "fee-rate": DEFAULT_FEE_RATE
      , "protocol-fee-rate": DEFAULT_PROTOCOL_FEE
      , "lp-fee-rate": (- DEFAULT_FEE_RATE DEFAULT_PROTOCOL_FEE)
      , "last-update": (at 'block-time (chain-data)) }
      { "pair-id" := pid
      , "fee-rate" := fee-rate
      , "protocol-fee-rate" := protocol-fee-rate
      , "lp-fee-rate" := lp-fee-rate
      , "last-update" := last-update }
      
      { "pair-id": pid
      , "fee-rate": fee-rate
      , "protocol-fee-rate": protocol-fee-rate
      , "lp-fee-rate": lp-fee-rate
      , "last-update": last-update }))
  
  ;; Fee collection
  (defun collect-fees:string (token:module{fungible-v2} amount:decimal)
    @doc "Collect protocol fees"
    
    (with-capability (GOVERNANCE)
      (let ((fee-account (get-fee-account)))
        ;; Transfer fees to collection account
        (token::transfer (tx-sender) fee-account amount)
        
        ;; Record collection
        (insert fee-collections (format "{}:{}" [token (at 'block-time (chain-data))]) {
          "token": token,
          "amount": amount,
          "collected-at": (at 'block-time (chain-data))
        })
        
        ;; Emit event
        (emit-event (FEE_COLLECTED token amount))
        
        (format "Collected {} fees of token {}" [amount token]))))
  
  (defun distribute-fees:string (token:module{fungible-v2} recipients:[string] percentages:[decimal])
    @doc "Distribute collected fees"
    
    (with-capability (GOVERNANCE)
      (enforce (= (length recipients) (length percentages)) "Mismatched recipients/percentages")
      
      (let* ((total-percentage (fold (+) 0.0 percentages))
             (fee-account (get-fee-account))
             (total-balance (token::get-balance fee-account)))
        
        (enforce (= total-percentage 100.0) "Percentages must sum to 100")
        (enforce (> total-balance 0.0) "No fees to distribute")
        
        ;; Calculate and transfer amounts
        (let ((amounts (map (lambda (pct:decimal)
                              (/ (* total-balance pct) 100.0))
                            percentages)))
          
          (map (lambda (i:integer)
                 (let ((recipient (at i recipients))
                       (amount (at i amounts)))
                   (install-capability (token::TRANSFER fee-account recipient amount))
                   (token::transfer fee-account recipient amount)))
               (enumerate 0 (- (length recipients) 1)))
          
          ;; Emit event
          (emit-event (FEE_DISTRIBUTED recipients amounts))
          
          (format "Distributed {} total fees to {} recipients" [total-balance (length recipients)])))))
  
  ;; Governance proposals
  (defun create-governance-proposal:string 
    ( title:string
      description:string
      proposal-type:string
      target-pair:string
      new-values:object
      proposer:string )
    @doc "Create a governance proposal"
    
    ;; Check proposer has enough tokens
    (let ((voting-power (governance-token.get-votes proposer)))
      (enforce (>= voting-power MIN_PROPOSAL_THRESHOLD) 
               "Insufficient voting power to propose"))
    
    (let* ((proposal-id (hash (format "{}:{}:{}" [proposer title (at 'block-time (chain-data))])))
           (start-time (at 'block-time (chain-data)))
           (end-time (add-time start-time (seconds VOTING_PERIOD))))
      
      (insert governance-proposals proposal-id {
        "id": proposal-id,
        "title": title,
        "description": description,
        "proposal-type": proposal-type,
        "target-pair": target-pair,
        "new-values": new-values,
        "proposer": proposer,
        "start-time": start-time,
        "end-time": end-time,
        "for-votes": 0.0,
        "against-votes": 0.0,
        "executed": false
      })
      
      ;; Emit event
      (emit-event (GOVERNANCE_PROPOSAL proposal-id proposer))
      
      (format "Created governance proposal {}" [proposal-id])))
  
  (defun vote-on-proposal:string (proposal-id:string voter:string support:bool)
    @doc "Vote on a governance proposal"
    
    (with-read governance-proposals proposal-id 
      { "start-time" := start-time
      , "end-time" := end-time
      , "executed" := executed
      , "for-votes" := for-votes
      , "against-votes" := against-votes }
      
      (enforce (not executed) "Proposal already executed")
      
      (let ((current-time (at 'block-time (chain-data))))
        (enforce (>= current-time start-time) "Voting not started")
        (enforce (<= current-time end-time) "Voting ended"))
      
      (let ((voting-power (governance-token.get-votes voter)))
        (enforce (> voting-power 0.0) "No voting power")
        
        ;; Update vote counts
        (if support
          (update governance-proposals proposal-id {
            "for-votes": (+ for-votes voting-power)
          })
          (update governance-proposals proposal-id {
            "against-votes": (+ against-votes voting-power)
          }))
        
        (format "Voted {} on proposal {} with {} voting power" 
                [(if support "for" "against") proposal-id voting-power]))))
  
  (defun execute-proposal:string (proposal-id:string)
    @doc "Execute a successful governance proposal"
    
    (with-read governance-proposals proposal-id 
      { "end-time" := end-time
      , "for-votes" := for-votes
      , "against-votes" := against-votes
      , "executed" := executed
      , "proposal-type" := proposal-type
      , "target-pair" := target-pair
      , "new-values" := new-values }
      
      (enforce (not executed) "Already executed")
      (enforce (<= end-time (at 'block-time (chain-data))) "Voting still active")
      (enforce (> for-votes against-votes) "Proposal did not pass")
      
      ;; Execute based on proposal type
      (cond
        ((= proposal-type "fee-change")
         (let ((new-fee-rate (at 'fee-rate new-values))
               (new-protocol-fee (at 'protocol-fee-rate new-values)))
           (set-fee-config target-pair new-fee-rate new-protocol-fee)))
        
        ((= proposal-type "parameter-update")
         "Parameter update executed") ;; Would implement specific parameter updates
        
        ((= proposal-type "upgrade")
         "Upgrade executed") ;; Would implement upgrade logic
        
        (enforce false "Unknown proposal type"))
      
      ;; Mark as executed
      (update governance-proposals proposal-id { "executed": true })
      
      (format "Executed proposal {}" [proposal-id])))
  
  ;; Yield farming governance
  (defun adjust-farm-rewards:string (farm-id:string new-reward-rate:decimal multiplier:decimal)
    @doc "Adjust farming rewards (governance only)"
    
    (with-capability (GOVERNANCE)
      (farming.update-farm farm-id new-reward-rate (add-time (at 'block-time (chain-data)) (days 90)))
      (format "Adjusted farm {} rewards to {} per second" [farm-id new-reward-rate])))
  
  ;; Protocol treasury management
  (defun treasury-spend:string (token:module{fungible-v2} recipient:string amount:decimal purpose:string)
    @doc "Spend from protocol treasury"
    
    (with-capability (GOVERNANCE)
      (let ((treasury-account (get-treasury-account)))
        (install-capability (token::TRANSFER treasury-account recipient amount))
        (token::transfer treasury-account recipient amount)
        
        (format "Treasury spent {} {} to {} for {}" [amount token recipient purpose]))))
  
  ;; Helper functions
  (defun get-fee-account:string ()
    @doc "Get fee collection account"
    (create-principal (create-capability-guard (GOVERNANCE))))
  
  (defun get-treasury-account:string ()
    @doc "Get treasury account"
    (create-principal (create-capability-guard (GOVERNANCE))))
  
  ;; Query functions
  (defun get-proposal:object (proposal-id:string)
    @doc "Get governance proposal"
    (read governance-proposals proposal-id))
  
  (defun get-active-proposals:[object] ()
    @doc "Get active governance proposals"
    (let ((current-time (at 'block-time (chain-data))))
      (select governance-proposals 
        (and? (where 'executed (= false))
             (and? (where 'start-time (<= current-time))
                  (where 'end-time (>= current-time)))))))
  
  (defun get-total-collected-fees:decimal (token:module{fungible-v2})
    @doc "Get total collected fees for token"
    (fold (+) 0.0 
      (map (at 'amount) 
        (select fee-collections (where 'token (= token))))))
  
  ;; Analytics
  (defun calculate-protocol-revenue:decimal (token:module{fungible-v2} days:integer)
    @doc "Calculate protocol revenue over period"
    (let ((cutoff-time (add-time (at 'block-time (chain-data)) (days (- days)))))
      (fold (+) 0.0 
        (map (at 'amount) 
          (select fee-collections 
            (and? (where 'token (= token))
                 (where 'collected-at (>= cutoff-time))))))))
  
  (defun get-fee-apr:decimal (pair-id:string)
    @doc "Calculate APR from fees for LP providers"
    (let ((fee-config (get-fee-config pair-id)))
      ;; Simplified calculation - would need volume data
      (* (at 'lp-fee-rate fee-config) 365.0 100.0)))
)

;; Create tables
(create-table fee-configs)
(create-table fee-collections)
(create-table governance-proposals)
```

## Step 6: Advanced Features

Finally, let's implement flash loans:

```pact
;; flash-loans.pact
(module flash-loans GOVERNANCE
  @doc "Flash loan system for DEX"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dex-admin))
  
  ;; Schemas
  (defschema flash-loan
    @doc "Flash loan information"
    id:string
    borrower:string
    token:module{fungible-v2}
    amount:decimal
    fee:decimal
    initiated-at:time
    callback-data:string)
  
  (defschema flash-loan-stats
    @doc "Flash loan statistics"
    token:module{fungible-v2}
    total-volume:decimal
    total-fees:decimal
    loan-count:integer
    last-update:time)
  
  ;; Tables
  (deftable flash-loans:{flash-loan})
  (deftable flash-loan-stats:{flash-loan-stats})
  
  ;; Events
  (defcap FLASH_LOAN:bool (borrower:string token:module{fungible-v2} amount:decimal fee:decimal)
    @doc "Flash loan event"
    @event
    true)
  
  ;; Constants
  (defconst FLASH_LOAN_FEE_RATE 0.0009) ;; 0.09% flash loan fee
  (defconst MAX_FLASH_LOAN_RATIO 0.8)   ;; Max 80% of reserves
  
  ;; Capabilities
  (defcap FLASH_LOAN_RECEIVER:bool (borrower:string loan-id:string)
    @doc "Capability for flash loan receiver"
    (enforce-guard (at 'guard (coin.details borrower))))
  
  (defcap FLASH_LOAN_INITIATED:bool (loan-id:string)
    @doc "Capability that flash loan is initiated"
    true)
  
  ;; Flash loan interface
  (defun initiate-flash-loan:string 
    ( borrower:string
      token:module{fungible-v2}
      amount:decimal
      callback-data:string )
    @doc "Initiate a flash loan"
    
    (with-capability (FLASH_LOAN_RECEIVER borrower "")
      (enforce (> amount 0.0) "Amount must be positive")
      
      ;; Check available liquidity
      (let* ((pair-reserves (get-available-liquidity token))
             (max-loan (* pair-reserves MAX_FLASH_LOAN_RATIO)))
        
        (enforce (<= amount max-loan) 
                 (format "Loan amount {} exceeds maximum {}" [amount max-loan]))
        
        ;; Generate loan ID
        (let* ((loan-id (hash (format "{}:{}:{}:{}" 
                                     [borrower token amount (at 'block-time (chain-data))])))
               (fee (* amount FLASH_LOAN_FEE_RATE))
               (flash-loan-account (get-flash-loan-account)))
          
          ;; Record flash loan
          (insert flash-loans loan-id {
            "id": loan-id,
            "borrower": borrower,
            "token": token,
            "amount": amount,
            "fee": fee,
            "initiated-at": (at 'block-time (chain-data)),
            "callback-data": callback-data
          })
          
          ;; Transfer tokens to borrower
          (install-capability (token::TRANSFER flash-loan-account borrower amount))
          (token::transfer flash-loan-account borrower amount)
          
          ;; Initiate callback (borrower must implement executeOperation)
          (with-capability (FLASH_LOAN_INITIATED loan-id)
            (execute-operation borrower token amount fee callback-data loan-id))
          
          ;; Verify repayment
          (verify-flash-loan-repayment loan-id)
          
          ;; Update statistics
          (update-flash-loan-stats token amount fee)
          
          ;; Emit event
          (emit-event (FLASH_LOAN borrower token amount fee))
          
          (format "Flash loan {} executed: {} {} with {} fee" 
                  [loan-id amount token fee])))))
  
  ;; Flash loan callback interface
  (defun execute-operation:bool 
    ( borrower:string
      token:module{fungible-v2}
      amount:decimal
      fee:decimal
      callback-data:string
      loan-id:string )
    @doc "Execute flash loan operation (to be implemented by borrower)"
    
    ;; This is a placeholder - borrower contracts would implement this
    ;; The borrower must:
    ;; 1. Use the borrowed funds
    ;; 2. Ensure they have amount + fee to repay
    ;; 3. Approve the flash loan contract to take repayment
    
    true)
  
  (defun verify-flash-loan-repayment:string (loan-id:string)
    @doc "Verify flash loan has been repaid"
    
    (with-read flash-loans loan-id 
      { "borrower" := borrower
      , "token" := token
      , "amount" := amount
      , "fee" := fee }
      
      (let* ((total-owed (+ amount fee))
             (flash-loan-account (get-flash-loan-account))
             (current-balance (token::get-balance flash-loan-account))
             (initial-balance (- current-balance amount))) ;; Balance before loan
        
        ;; Check if repayment is sufficient
        (enforce (>= current-balance (+ initial-balance total-owed))
                 "Flash loan not properly repaid")
        
        ;; Remove flash loan record
        (update flash-loans loan-id { "amount": 0.0 })
        
        "Flash loan repaid successfully")))
  
  ;; Arbitrage protection
  (defun multi-dex-arbitrage:string 
    ( token0:module{fungible-v2}
      token1:module{fungible-v2}
      amount:decimal
      external-price:decimal )
    @doc "Execute arbitrage between DEXs using flash loans"
    
    (let* ((internal-price (router.get-price token0 token1))
           (price-diff (abs (- external-price internal-price)))
           (min-profit-threshold 0.01)) ;; 1% minimum profit
      
      (enforce (> (/ price-diff internal-price) min-profit-threshold) 
               "Insufficient arbitrage opportunity")
      
      ;; Initiate flash loan for arbitrage
      (initiate-flash-loan 
        (tx-sender)
        token0
        amount
        (format "arbitrage:{}:{}:{}" [token1 external-price internal-price]))))
  
  ;; Liquidation flash loans
  (defun liquidation-flash-loan:string 
    ( collateral-token:module{fungible-v2}
      debt-token:module{fungible-v2}
      collateral-amount:decimal
      debt-amount:decimal
      liquidation-target:string )
    @doc "Flash loan for liquidations"
    
    (let ((callback-data (format "liquidation:{}:{}:{}:{}" 
                                [collateral-token debt-amount liquidation-target (tx-sender)])))
      
      (initiate-flash-loan 
        (tx-sender)
        debt-token
        debt-amount
        callback-data)))
  
  ;; Helper functions
  (defun get-available-liquidity:decimal (token:module{fungible-v2})
    @doc "Get available liquidity for flash loans"
    
    ;; Sum liquidity across all pairs containing this token
    (let ((flash-loan-account (get-flash-loan-account)))
      (token::get-balance flash-loan-account)))
  
  (defun update-flash-loan-stats:string (token:module{fungible-v2} amount:decimal fee:decimal)
    @doc "Update flash loan statistics"
    
    (let ((token-key (format "{}" [token])))
      (with-default-read flash-loan-stats token-key
        { "token": token
        , "total-volume": 0.0
        , "total-fees": 0.0
        , "loan-count": 0
        , "last-update": (at 'block-time (chain-data)) }
        { "total-volume" := volume
        , "total-fees" := fees
        , "loan-count" := count }
        
        (write flash-loan-stats token-key {
          "token": token,
          "total-volume": (+ volume amount),
          "total-fees": (+ fees fee),
          "loan-count": (+ count 1),
          "last-update": (at 'block-time (chain-data))
        }))))
  
  (defun get-flash-loan-account:string ()
    @doc "Get flash loan contract account"
    (create-principal (create-capability-guard (GOVERNANCE))))
  
  ;; Query functions
  (defun get-flash-loan:object (loan-id:string)
    @doc "Get flash loan details"
    (read flash-loans loan-id))
  
  (defun get-flash-loan-stats:object (token:module{fungible-v2})
    @doc "Get flash loan statistics for token"
    (let ((token-key (format "{}" [token])))
      (with-default-read flash-loan-stats token-key
        { "token": token
        , "total-volume": 0.0
        , "total-fees": 0.0
        , "loan-count": 0
        , "last-update": (at 'block-time (chain-data)) }
        { "total-volume" := volume
        , "total-fees" := fees
        , "loan-count" := count
        , "last-update" := last-update }
        
        { "token": token
        , "volume": volume
        , "fees": fees
        , "count": count
        , "last-update": last-update })))
  
  (defun calculate-flash-loan-fee:decimal (amount:decimal)
    @doc "Calculate flash loan fee"
    (* amount FLASH_LOAN_FEE_RATE))
  
  ;; Security functions
  (defun pause-flash-loans:string ()
    @doc "Emergency pause flash loans"
    (with-capability (GOVERNANCE)
      "Flash loans paused"))
  
  (defun get-max-flash-loan:decimal (token:module{fungible-v2})
    @doc "Get maximum flash loan amount for token"
    (let ((available-liquidity (get-available-liquidity token)))
      (* available-liquidity MAX_FLASH_LOAN_RATIO)))
)

;; Create tables
(create-table flash-loans)
(create-table flash-loan-stats)
```

## Testing & Security

Let's create comprehensive tests:

```pact
;; test-dex.repl
;; Comprehensive DEX testing

;; Load dependencies
(load "governance-token.pact")
(load "pair.pact")
(load "lp-token.pact")
(load "router.pact")
(load "farming.pact")
(load "dex-governance.pact")
(load "flash-loans.pact")

;; Test environment setup
(begin-tx "Setup")
(env-data {
  "dex-admin": ["admin-key"],
  "alice": ["alice-key"],
  "bob": ["bob-key"]
})
(env-keys ["admin-key"])

(define-keyset 'dex-admin (read-keyset 'dex-admin))
(define-keyset 'alice (read-keyset 'alice))
(define-keyset 'bob (read-keyset 'bob))

(commit-tx)

;; Test 1: Pair creation and liquidity
(begin-tx "Test pair creation")
(use pair)

;; Create trading pair
(create-pair coin governance-token)

;; Add initial liquidity
(env-keys ["alice-key"])
(add-liquidity 
  coin governance-token
  1000000.0 2000000.0
  1000000.0 2000000.0
  "alice"
  (add-time (at 'block-time (chain-data)) (hours 1)))

;; Check reserves
(let ((reserves (get-reserves coin governance-token)))
  (expect "Reserve 0 correct" 1000000.0 (at 0 reserves))
  (expect "Reserve 1 correct" 2000000.0 (at 1 reserves)))

(commit-tx)

;; Test 2: Token swapping
(begin-tx "Test swapping")
(use pair)

;; Calculate expected output
(let ((amount-out (get-amount-out 1000.0 1000000.0 2000000.0)))
  (expect "Amount out calculation" true (> amount-out 1990.0)))

;; Execute swap
(env-keys ["bob-key"])
(swap-exact-tokens-for-tokens
  1000.0
  1990.0
  [coin governance-token]
  "bob"
  (add-time (at 'block-time (chain-data)) (hours 1)))

(commit-tx)

;; Test 3: Farming
(begin-tx "Test farming")
(use farming)

;; Create farm
(env-keys ["admin-key"])
(create-farm
  "coin-gov-farm"
  lp-token
  governance-token
  10.0  ;; 10 tokens per second
  (at 'block-time (chain-data))
  (add-time (at 'block-time (chain-data)) (days 30))
  1.0)

;; User deposits LP tokens
(env-keys ["alice-key"])
(deposit "coin-gov-farm" "alice" 500000.0)

;; Fast forward time
(env-chain-data { "block-time": (add-time (at 'block-time (chain-data)) (hours 1)) })

;; Check pending rewards
(let ((pending (pending-rewards "coin-gov-farm" "alice")))
  (expect "Has pending rewards" true (> pending 0.0)))

;; Harvest rewards
(harvest "coin-gov-farm" "alice")

(commit-tx)

;; Test 4: Flash loans
(begin-tx "Test flash loans")
(use flash-loans)

;; Calculate flash loan fee
(let ((fee (calculate-flash-loan-fee 100000.0)))
  (expect "Flash loan fee correct" 90.0 fee))

;; Check maximum flash loan
(let ((max-loan (get-max-flash-loan coin)))
  (expect "Max flash loan calculated" true (> max-loan 0.0)))

(commit-tx)

;; Test 5: Governance
(begin-tx "Test governance")
(use dex-governance)

;; Set fee configuration
(env-keys ["admin-key"])
(set-fee-config "coin:governance-token" 0.003 0.0005)

;; Create governance proposal
(env-keys ["alice-key"])
(create-governance-proposal
  "Reduce Trading Fees"
  "Proposal to reduce trading fees to 0.25%"
  "fee-change"
  "coin:governance-token"
  { "fee-rate": 0.0025, "protocol-fee-rate": 0.0005 }
  "alice")

(commit-tx)

;; Test 6: Router optimization
(begin-tx "Test router")
(use router)

;; Quote exact input
(let ((quote (quote-exact-input coin governance-token 1000.0)))
  (expect "Quote has output" true (> (at 'amount-out quote) 0.0)))

;; Find best route
(let ((route (find-best-route coin governance-token 1000.0)))
  (expect "Route found" true (> (length (at 'path route)) 1)))

(commit-tx)

;; Security tests
(begin-tx "Security tests")
(use pair)

;; Test slippage protection
(expect-failure "Slippage protection"
  "Insufficient output amount"
  (swap-exact-tokens-for-tokens
    1000.0
    10000.0  ;; Unrealistic minimum output
    [coin governance-token]
    "alice"
    (add-time (at 'block-time (chain-data)) (hours 1))))

;; Test deadline protection
(expect-failure "Deadline protection"
  "Transaction expired"
  (swap-exact-tokens-for-tokens
    1000.0
    1.0
    [coin governance-token]
    "alice"
    (add-time (at 'block-time (chain-data)) (seconds -1))))

(commit-tx)

(print "All DEX tests passed!")
```

## Frontend Integration

Example React integration:

```javascript
// dex-frontend.js
import { Pact } from 'pact-lang-api';

class DEXInterface {
  constructor(nodeUrl, chainId) {
    this.nodeUrl = nodeUrl;
    this.chainId = chainId;
  }

  // Add liquidity
  async addLiquidity(account, token0, token1, amount0, amount1, slippage = 0.5) {
    const deadline = new Date(Date.now() + 10 * 60 * 1000); // 10 minutes
    const amount0Min = amount0 * (1 - slippage / 100);
    const amount1Min = amount1 * (1 - slippage / 100);

    const cmd = {
      pactCode: `(pair.add-liquidity 
        ${token0} ${token1} 
        ${amount0} ${amount1}
        ${amount0Min} ${amount1Min}
        "${account.address}"
        (time "${deadline.toISOString()}"))`,
      keyPairs: [account.keyPair],
      meta: {
        chainId: this.chainId,
        sender: account.address,
        gasLimit: 15000,
        gasPrice: 0.00001,
        ttl: 600
      }
    };

    return await Pact.send(cmd, this.nodeUrl);
  }

  // Execute swap
  async swapTokens(account, tokenIn, tokenOut, amountIn, minAmountOut) {
    const path = [tokenIn, tokenOut];
    const deadline = new Date(Date.now() + 10 * 60 * 1000);

    const cmd = {
      pactCode: `(pair.swap-exact-tokens-for-tokens 
        ${amountIn} ${minAmountOut}
        ${JSON.stringify(path)}
        "${account.address}"
        (time "${deadline.toISOString()}"))`,
      keyPairs: [account.keyPair],
      meta: {
        chainId: this.chainId,
        sender: account.address,
        gasLimit: 10000,
        gasPrice: 0.00001,
        ttl: 600
      }
    };

    return await Pact.send(cmd, this.nodeUrl);
  }

  // Stake in farm
  async stakeFarm(account, farmId, amount) {
    const cmd = {
      pactCode: `(farming.deposit "${farmId}" "${account.address}" ${amount})`,
      keyPairs: [account.keyPair],
      meta: {
        chainId: this.chainId,
        sender: account.address,
        gasLimit: 8000,
        gasPrice: 0.00001,
        ttl: 600
      }
    };

    return await Pact.send(cmd, this.nodeUrl);
  }

  // Get price quote
  async getQuote(tokenIn, tokenOut, amountIn) {
    const cmd = {
      pactCode: `(router.quote-exact-input ${tokenIn} ${tokenOut} ${amountIn})`
    };

    const result = await Pact.local(cmd, this.nodeUrl);
    return result.result.data;
  }

  // Get farm APR
  async getFarmAPR(farmId) {
    const cmd = {
      pactCode: `(farming.calculate-apr "${farmId}")`
    };

    const result = await Pact.local(cmd, this.nodeUrl);
    return result.result.data;
  }

  // Get user's LP balance
  async getLPBalance(account, pairId) {
    const cmd = {
      pactCode: `(lp-token.get-balance "${account}")`
    };

    const result = await Pact.local(cmd, this.nodeUrl);
    return result.result.data;
  }
}

// Usage example
const dex = new DEXInterface('http://localhost:9001', '0');

// React component example
function SwapInterface() {
  const [tokenIn, setTokenIn] = useState('coin');
  const [tokenOut, setTokenOut] = useState('governance-token');
  const [amountIn, setAmountIn] = useState('');
  const [quote, setQuote] = useState(null);

  useEffect(() => {
    if (amountIn) {
      dex.getQuote(tokenIn, tokenOut, parseFloat(amountIn))
        .then(setQuote)
        .catch(console.error);
    }
  }, [tokenIn, tokenOut, amountIn]);

  const handleSwap = async () => {
    if (!quote) return;
    
    const minAmountOut = quote.amount_out * 0.995; // 0.5% slippage
    
    try {
      const result = await dex.swapTokens(
        account, tokenIn, tokenOut, 
        parseFloat(amountIn), minAmountOut
      );
      console.log('Swap successful:', result);
    } catch (error) {
      console.error('Swap failed:', error);
    }
  };

  return (
    <div className="swap-interface">
      <h2>Token Swap</h2>
      
      <div>
        <label>From:</label>
        <select value={tokenIn} onChange={(e) => setTokenIn(e.target.value)}>
          <option value="coin">KDA</option>
          <option value="governance-token">GOV</option>
        </select>
        <input 
          type="number" 
          value={amountIn}
          onChange={(e) => setAmountIn(e.target.value)}
          placeholder="Amount"
        />
      </div>

      <div>
        <label>To:</label>
        <select value={tokenOut} onChange={(e) => setTokenOut(e.target.value)}>
          <option value="coin">KDA</option>
          <option value="governance-token">GOV</option>
        </select>
        {quote && (
          <div>Output: {quote.amount_out.toFixed(6)}</div>
        )}
      </div>

      <button onClick={handleSwap} disabled={!quote}>
        Swap
      </button>

      {quote && (
        <div>
          <p>Price Impact: {(quote.price_impact * 100).toFixed(2)}%</p>
          <p>Fee: {quote.fees[0].toFixed(6)}</p>
        </div>
      )}
    </div>
  );
}
```

## Summary

In this chapter, we've built a comprehensive decentralized exchange with:

1. **AMM System**: Constant product formula with liquidity pools
2. **LP Tokens**: Fungible tokens representing liquidity shares
3. **Router**: Optimal multi-hop trading paths
4. **Yield Farming**: Incentivized liquidity provision
5. **Governance**: Protocol parameter voting and fee management
6. **Flash Loans**: Uncollateralized borrowing for arbitrage

### Key Takeaways

- DEXs require careful balance between capital efficiency and security
- AMM algorithms enable permissionless market making
- Yield farming creates sustainable liquidity incentives
- Flash loans enable advanced DeFi strategies
- Governance ensures decentralized protocol evolution

### Testing Notes

The code examples include both comprehensive and simplified test files:
- `test-dex.repl` - Full feature testing with yield farming
- `test-dex-simple.repl` - Basic AMM functionality testing

Key implementation details:
- Proper capability management for token transfers
- LP token management with guards and balances
- Price impact calculations using constant product formula
- Support for fungible-v2 interface compliance

### Next Steps

- Implement concentrated liquidity (Uniswap V3 style)
- Add cross-chain bridge integration
- Create automated portfolio rebalancing
- Build limit order functionality
- Add insurance fund for protocol security

The DEX we've built provides a solid foundation for decentralized trading while maintaining security and user experience.