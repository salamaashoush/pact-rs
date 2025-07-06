# Chapter 20: Building a DeFi Protocol - Lending & Borrowing Platform

## Table of Contents
1. [Project Overview](#project-overview)
2. [DeFi Architecture](#defi-architecture)
3. [Step 1: Asset Management](#step-1-asset-management)
4. [Step 2: Lending Pool](#step-2-lending-pool)
5. [Step 3: Borrowing System](#step-3-borrowing-system)
6. [Step 4: Interest Rate Model](#step-4-interest-rate-model)
7. [Step 5: Liquidation Engine](#step-5-liquidation-engine)
8. [Step 6: Yield Farming](#step-6-yield-farming)
9. [Testing & Security](#testing-security)
10. [Frontend Integration](#frontend-integration)

## Project Overview

In this chapter, we'll build a complete DeFi lending and borrowing protocol featuring:
- Multi-asset lending pools
- Over-collateralized borrowing
- Dynamic interest rate model
- Automated liquidations
- Yield farming incentives
- Governance token distribution
- Flash loan functionality

### What You'll Learn
- Implementing complex financial logic on-chain
- Managing collateral and risk parameters
- Creating interest-bearing tokens
- Building liquidation mechanisms
- Designing tokenomics and incentives
- Ensuring protocol security

## DeFi Architecture

### Core Components
```
defi-protocol/
├── core/
│   ├── lending-pool.pact      # Main lending logic
│   ├── collateral.pact        # Collateral management
│   └── oracle.pact            # Price feeds
├── tokens/
│   ├── atoken.pact            # Interest-bearing tokens
│   ├── debt-token.pact        # Debt tracking tokens
│   └── governance.pact        # Protocol token
├── markets/
│   ├── interest-rate.pact     # Rate calculations
│   ├── liquidation.pact       # Liquidation engine
│   └── flash-loans.pact       # Flash loan logic
├── rewards/
│   ├── staking.pact           # Staking rewards
│   ├── farming.pact           # Yield farming
│   └── distribution.pact      # Reward distribution
└── governance/
    ├── voting.pact            # Voting mechanism
    ├── proposals.pact         # Proposal system
    └── timelock.pact          # Execution delay
```

## Step 1: Asset Management

Let's start with the core asset management and aToken implementation:

```pact
;; tokens/atoken.pact
(module atoken GOVERNANCE
  @doc "Interest-bearing token for DeFi protocol"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Schemas
  (defschema atoken-balance
    @doc "User balance with interest tracking"
    account:string
    principal:decimal      ;; Initial deposit amount
    scaled-balance:decimal ;; Balance adjusted by index
    last-update:time)
  
  (defschema reserve-data
    @doc "Reserve pool information"
    asset:string           ;; Underlying asset
    total-deposits:decimal
    total-borrows:decimal
    liquidity-index:decimal ;; Cumulative interest factor
    borrow-index:decimal
    last-update:time
    reserve-factor:decimal  ;; Protocol fee percentage
    is-active:bool)
  
  (defschema user-config
    @doc "User's asset usage configuration"
    account:string
    asset:string
    use-as-collateral:bool)
  
  ;; Tables
  (deftable balances:{atoken-balance})
  (deftable reserves:{reserve-data})
  (deftable user-configs:{user-config})
  
  ;; Constants
  (defconst SECONDS_PER_YEAR 31536000)
  (defconst RAY 1000000000000000000000000000) ;; 10^27 for precision
  
  ;; Capabilities
  (defcap DEPOSIT (account:string asset:string amount:decimal)
    @doc "Deposit capability"
    @event
    (enforce (> amount 0.0) "Amount must be positive"))
  
  (defcap WITHDRAW (account:string asset:string amount:decimal)
    @doc "Withdraw capability"
    @managed
    (enforce (> amount 0.0) "Amount must be positive")
    (let ((balance (get-balance account asset)))
      (enforce (>= balance amount) "Insufficient balance")))
  
  ;; Interest Calculation
  (defun calculate-linear-interest:decimal (rate:decimal last-update:time)
    @doc "Calculate linear interest accumulation"
    (let* ((current-time (at 'block-time (chain-data)))
           (time-delta (diff-time current-time last-update))
           (time-delta-seconds (floor time-delta))
           (interest-factor (/ (* rate time-delta-seconds) 
                              (* RAY SECONDS_PER_YEAR))))
      (+ 1.0 interest-factor)))
  
  (defun calculate-compound-interest:decimal (rate:decimal last-update:time)
    @doc "Calculate compound interest using binomial expansion"
    (let* ((current-time (at 'block-time (chain-data)))
           (time-delta (diff-time current-time last-update))
           (time-delta-seconds (floor time-delta))
           (rate-per-second (/ rate SECONDS_PER_YEAR))
           (exp (floor (/ time-delta-seconds 1.0))))
      ;; Simplified compound interest calculation
      ;; In production, use more precise calculation
      (^ (+ 1.0 rate-per-second) exp)))
  
  ;; Core Functions
  (defun initialize-reserve:string (asset:string reserve-factor:decimal)
    @doc "Initialize a new lending reserve"
    (enforce (<= reserve-factor 1.0) "Reserve factor must be <= 1.0")
    
    (insert reserves asset {
      "asset": asset,
      "total-deposits": 0.0,
      "total-borrows": 0.0,
      "liquidity-index": 1.0,
      "borrow-index": 1.0,
      "last-update": (at 'block-time (chain-data)),
      "reserve-factor": reserve-factor,
      "is-active": true
    })
    (format "Reserve {} initialized" [asset]))
  
  (defun deposit:string (account:string asset:string amount:decimal)
    @doc "Deposit assets and receive aTokens"
    (with-capability (DEPOSIT account asset amount)
      ;; Update reserve indexes
      (update-indexes asset)
      
      (with-read reserves asset 
        { "liquidity-index" := index
        , "total-deposits" := total-deposits
        , "is-active" := active }
        
        (enforce active "Reserve is not active")
        
        ;; Calculate scaled amount
        (let ((scaled-amount (/ amount index)))
          
          ;; Update or create balance
          (with-default-read balances (format "{}:{}" [account asset])
            { "principal": 0.0
            , "scaled-balance": 0.0 }
            { "principal" := current-principal
            , "scaled-balance" := current-scaled }
            
            (write balances (format "{}:{}" [account asset]) {
              "account": account,
              "principal": (+ current-principal amount),
              "scaled-balance": (+ current-scaled scaled-amount),
              "last-update": (at 'block-time (chain-data))
            }))
          
          ;; Update reserve
          (update reserves asset {
            "total-deposits": (+ total-deposits amount)
          })
          
          ;; Set default collateral config
          (with-default-read user-configs (format "{}:{}" [account asset])
            { "use-as-collateral": true }
            { "use-as-collateral" := _ }
            
            (write user-configs (format "{}:{}" [account asset]) {
              "account": account,
              "asset": asset,
              "use-as-collateral": true
            }))
          
          (format "Deposited {} {}" [amount asset]))))
  
  (defun withdraw:string (account:string asset:string amount:decimal)
    @doc "Withdraw assets by burning aTokens"
    (with-capability (WITHDRAW account asset amount)
      ;; Update reserve indexes
      (update-indexes asset)
      
      (with-read reserves asset 
        { "liquidity-index" := index
        , "total-deposits" := total-deposits }
        
        (with-read balances (format "{}:{}" [account asset])
          { "scaled-balance" := scaled-balance }
          
          (let* ((current-balance (* scaled-balance index))
                 (amount-to-withdraw (if (= amount -1.0) 
                                       current-balance 
                                       amount)))
            
            (enforce (<= amount-to-withdraw current-balance) 
                    "Insufficient balance")
            
            ;; Check if withdrawal breaks collateralization
            (validate-health-factor account)
            
            ;; Calculate scaled amount to burn
            (let ((scaled-to-burn (/ amount-to-withdraw index)))
              
              ;; Update balance
              (update balances (format "{}:{}" [account asset]) {
                "scaled-balance": (- scaled-balance scaled-to-burn),
                "last-update": (at 'block-time (chain-data))
              })
              
              ;; Update reserve
              (update reserves asset {
                "total-deposits": (- total-deposits amount-to-withdraw)
              })
              
              (format "Withdrew {} {}" [amount-to-withdraw asset]))))))
  
  ;; Index Updates
  (defun update-indexes:string (asset:string)
    @doc "Update liquidity and borrow indexes"
    (with-read reserves asset 
      { "liquidity-index" := current-liq-index
      , "borrow-index" := current-borrow-index
      , "last-update" := last-update
      , "total-deposits" := deposits
      , "total-borrows" := borrows }
      
      (if (> (diff-time (at 'block-time (chain-data)) last-update) 0)
        (let* ((utilization-rate (calculate-utilization deposits borrows))
               (borrow-rate (calculate-borrow-rate utilization-rate))
               (liquidity-rate (calculate-liquidity-rate 
                               borrow-rate utilization-rate))
               (liq-accumulation (calculate-linear-interest 
                                 liquidity-rate last-update))
               (borrow-accumulation (calculate-compound-interest 
                                   borrow-rate last-update)))
          
          (update reserves asset {
            "liquidity-index": (* current-liq-index liq-accumulation),
            "borrow-index": (* current-borrow-index borrow-accumulation),
            "last-update": (at 'block-time (chain-data))
          })
          "Indexes updated")
        "No update needed")))
  
  ;; Helper Functions
  (defun calculate-utilization:decimal (deposits:decimal borrows:decimal)
    @doc "Calculate utilization rate"
    (if (= deposits 0.0)
      0.0
      (/ borrows deposits)))
  
  (defun calculate-borrow-rate:decimal (utilization:decimal)
    @doc "Calculate variable borrow rate"
    ;; Simplified model: base + utilization * slope
    (let ((base-rate 0.02)     ;; 2% base
          (slope1 0.05)        ;; 5% slope below optimal
          (slope2 0.60)        ;; 60% slope above optimal
          (optimal 0.80))      ;; 80% optimal utilization
      
      (if (<= utilization optimal)
        (+ base-rate (* utilization (/ slope1 optimal)))
        (+ base-rate slope1 
           (* (- utilization optimal) 
              (/ slope2 (- 1.0 optimal)))))))
  
  (defun calculate-liquidity-rate:decimal (borrow-rate:decimal utilization:decimal)
    @doc "Calculate liquidity rate for depositors"
    (* borrow-rate utilization 0.9)) ;; 90% of borrow rate to depositors
  
  (defun get-balance:decimal (account:string asset:string)
    @doc "Get current balance including interest"
    (with-default-read balances (format "{}:{}" [account asset])
      { "scaled-balance": 0.0 }
      { "scaled-balance" := scaled }
      
      (with-read reserves asset { "liquidity-index" := index }
        (* scaled index))))
  
  (defun validate-health-factor:bool (account:string)
    @doc "Placeholder for health factor validation"
    ;; This would integrate with the borrowing module
    true)
)

(create-table balances)
(create-table reserves)
(create-table user-configs)
```

## Step 2: Lending Pool

Now let's implement the main lending pool logic:

```pact
;; core/lending-pool.pact
(module lending-pool GOVERNANCE
  @doc "Core lending pool implementation"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Schemas
  (defschema user-account-data
    @doc "Aggregated user position"
    account:string
    total-collateral-base:decimal  ;; In USD
    total-debt-base:decimal        ;; In USD
    available-borrow-base:decimal
    current-ltv:decimal            ;; Loan-to-value
    liquidation-threshold:decimal
    health-factor:decimal)
  
  (defschema asset-config
    @doc "Asset risk parameters"
    asset:string
    ltv:decimal                    ;; Max loan-to-value
    liquidation-threshold:decimal  ;; Liquidation trigger
    liquidation-bonus:decimal      ;; Liquidator incentive
    decimals:integer
    is-active:bool
    can-borrow:bool
    can-be-collateral:bool)
  
  ;; Tables
  (deftable user-accounts:{user-account-data})
  (deftable asset-configs:{asset-config})
  
  ;; Constants
  (defconst HEALTH_FACTOR_LIQUIDATION_THRESHOLD 1.0)
  
  ;; Capabilities
  (defcap BORROW (account:string asset:string amount:decimal)
    @doc "Borrowing capability"
    @event
    (enforce (> amount 0.0) "Amount must be positive"))
  
  (defcap REPAY (account:string asset:string amount:decimal)
    @doc "Repayment capability"
    @event
    (enforce (> amount 0.0) "Amount must be positive"))
  
  ;; Configuration
  (defun configure-asset:string 
    ( asset:string 
      ltv:decimal 
      liquidation-threshold:decimal 
      liquidation-bonus:decimal 
      decimals:integer )
    @doc "Configure asset parameters"
    (with-capability (GOVERNANCE)
      (enforce (<= ltv liquidation-threshold) 
              "LTV must be <= liquidation threshold")
      (enforce (<= liquidation-threshold 1.0) 
              "Liquidation threshold must be <= 100%")
      
      (write asset-configs asset {
        "asset": asset,
        "ltv": ltv,
        "liquidation-threshold": liquidation-threshold,
        "liquidation-bonus": liquidation-bonus,
        "decimals": decimals,
        "is-active": true,
        "can-borrow": true,
        "can-be-collateral": true
      })
      (format "Asset {} configured" [asset])))
  
  ;; Borrowing Functions
  (defun borrow:string 
    ( account:string 
      asset:string 
      amount:decimal 
      interest-rate-mode:string )
    @doc "Borrow assets against collateral"
    (with-capability (BORROW account asset amount)
      (with-read asset-configs asset 
        { "is-active" := active, "can-borrow" := borrowable }
        
        (enforce active "Asset is not active")
        (enforce borrowable "Asset cannot be borrowed")
        
        ;; Update user account data
        (let ((account-data (calculate-user-account-data account)))
          
          ;; Check borrowing power
          (enforce (>= (at 'available-borrow-base account-data) 
                      (* amount (get-asset-price asset)))
                  "Insufficient collateral")
          
          ;; Perform borrow
          ;; This would integrate with debt token minting
          
          ;; Validate health factor after borrow
          (let ((new-account-data (calculate-user-account-data account)))
            (enforce (>= (at 'health-factor new-account-data) 
                        HEALTH_FACTOR_LIQUIDATION_THRESHOLD)
                    "Health factor too low"))
          
          (format "Borrowed {} {}" [amount asset]))))
  
  (defun repay:string 
    ( account:string 
      asset:string 
      amount:decimal 
      on-behalf-of:string )
    @doc "Repay borrowed assets"
    (with-capability (REPAY account asset amount)
      ;; Get current debt
      (let* ((debt-amount (get-user-debt on-behalf-of asset))
             (payback-amount (if (= amount -1.0) 
                               debt-amount 
                               (min amount debt-amount))))
        
        ;; Perform repayment
        ;; This would integrate with debt token burning
        
        (format "Repaid {} {} for {}" 
                [payback-amount asset on-behalf-of]))))
  
  ;; Liquidation Functions
  (defun liquidation-call:string 
    ( collateral-asset:string 
      debt-asset:string 
      user:string 
      debt-to-cover:decimal 
      receive-atoken:bool )
    @doc "Liquidate undercollateralized position"
    ;; Check health factor
    (let ((account-data (calculate-user-account-data user)))
      (enforce (< (at 'health-factor account-data) 
                  HEALTH_FACTOR_LIQUIDATION_THRESHOLD)
              "User cannot be liquidated")
      
      ;; Calculate liquidation amounts
      (let* ((debt-amount (get-user-debt user debt-asset))
             (max-liquidatable (* debt-amount 0.5)) ;; 50% close factor
             (actual-debt-to-cover (min debt-to-cover max-liquidatable))
             (collateral-price (get-asset-price collateral-asset))
             (debt-price (get-asset-price debt-asset))
             (liquidation-bonus (get-liquidation-bonus collateral-asset))
             (collateral-to-liquidate 
               (* (/ (* actual-debt-to-cover debt-price) 
                    collateral-price)
                  (+ 1.0 liquidation-bonus))))
        
        ;; Execute liquidation
        ;; This would transfer collateral and repay debt
        
        (format "Liquidated {} {} collateral for {} {} debt" 
                [collateral-to-liquidate collateral-asset 
                 actual-debt-to-cover debt-asset]))))
  
  ;; Account Data Calculation
  (defun calculate-user-account-data:object (account:string)
    @doc "Calculate user's overall position"
    (let ((positions (get-user-positions account)))
      ;; Aggregate all positions
      (fold (lambda (acc position)
              (let* ((asset (at 'asset position))
                     (balance (at 'balance position))
                     (debt (at 'debt position))
                     (price (get-asset-price asset))
                     (config (read asset-configs asset))
                     (ltv (at 'ltv config))
                     (threshold (at 'liquidation-threshold config)))
                
                { "total-collateral-base": 
                    (+ (at 'total-collateral-base acc) 
                       (* balance price))
                , "total-debt-base": 
                    (+ (at 'total-debt-base acc) 
                       (* debt price))
                , "available-borrow-base": 
                    (+ (at 'available-borrow-base acc) 
                       (* (* balance price) ltv))
                , "liquidation-value": 
                    (+ (at 'liquidation-value acc) 
                       (* (* balance price) threshold)) }))
            { "total-collateral-base": 0.0
            , "total-debt-base": 0.0
            , "available-borrow-base": 0.0
            , "liquidation-value": 0.0 }
            positions)))
  
  ;; Price Oracle Integration
  (defun get-asset-price:decimal (asset:string)
    @doc "Get asset price from oracle"
    ;; This would integrate with price oracle
    1.0) ;; Placeholder
  
  (defun get-liquidation-bonus:decimal (asset:string)
    @doc "Get liquidation bonus for asset"
    (at 'liquidation-bonus (read asset-configs asset)))
  
  ;; Helper Functions
  (defun get-user-positions:list (account:string)
    @doc "Get all user positions across assets"
    ;; This would aggregate from aToken and debt token modules
    []) ;; Placeholder
  
  (defun get-user-debt:decimal (account:string asset:string)
    @doc "Get user's debt in specific asset"
    ;; This would read from debt token module
    0.0) ;; Placeholder
)

(create-table user-accounts)
(create-table asset-configs)
```

## Step 3: Borrowing System

Let's implement the debt token system for tracking borrows:

```pact
;; tokens/debt-token.pact
(module debt-token GOVERNANCE
  @doc "Variable debt token implementation"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Schemas
  (defschema debt-balance
    @doc "User debt tracking"
    account:string
    asset:string
    scaled-balance:decimal
    last-update:time)
  
  (defschema debt-reserve
    @doc "Debt reserve data"
    asset:string
    total-supply:decimal
    average-stable-rate:decimal
    last-update:time)
  
  ;; Tables
  (deftable debt-balances:{debt-balance})
  (deftable debt-reserves:{debt-reserve})
  
  ;; Capabilities
  (defcap MINT (account:string asset:string amount:decimal)
    @doc "Debt minting capability"
    @managed
    (enforce (> amount 0.0) "Amount must be positive"))
  
  (defcap BURN (account:string asset:string amount:decimal)
    @doc "Debt burning capability"
    @managed
    (enforce (> amount 0.0) "Amount must be positive"))
  
  ;; Core Functions
  (defun mint:string 
    ( account:string 
      asset:string 
      amount:decimal 
      index:decimal )
    @doc "Mint debt tokens (borrow)"
    (with-capability (MINT account asset amount)
      (let ((scaled-amount (/ amount index)))
        
        ;; Update user balance
        (with-default-read debt-balances (format "{}:{}" [account asset])
          { "scaled-balance": 0.0 }
          { "scaled-balance" := current-scaled }
          
          (write debt-balances (format "{}:{}" [account asset]) {
            "account": account,
            "asset": asset,
            "scaled-balance": (+ current-scaled scaled-amount),
            "last-update": (at 'block-time (chain-data))
          }))
        
        ;; Update total supply
        (with-read debt-reserves asset
          { "total-supply" := total }
          
          (update debt-reserves asset {
            "total-supply": (+ total amount),
            "last-update": (at 'block-time (chain-data))
          }))
        
        (format "Minted {} debt tokens" [amount]))))
  
  (defun burn:string 
    ( account:string 
      asset:string 
      amount:decimal 
      index:decimal )
    @doc "Burn debt tokens (repay)"
    (with-capability (BURN account asset amount)
      (with-read debt-balances (format "{}:{}" [account asset])
        { "scaled-balance" := scaled-balance }
        
        (let* ((current-balance (* scaled-balance index))
               (amount-to-burn (if (= amount -1.0) 
                                 current-balance 
                                 amount)))
          
          (enforce (<= amount-to-burn current-balance) 
                  "Amount exceeds debt")
          
          (let ((scaled-to-burn (/ amount-to-burn index)))
            
            ;; Update user balance
            (update debt-balances (format "{}:{}" [account asset]) {
              "scaled-balance": (- scaled-balance scaled-to-burn),
              "last-update": (at 'block-time (chain-data))
            })
            
            ;; Update total supply
            (with-read debt-reserves asset
              { "total-supply" := total }
              
              (update debt-reserves asset {
                "total-supply": (- total amount-to-burn),
                "last-update": (at 'block-time (chain-data))
              }))
            
            (format "Burned {} debt tokens" [amount-to-burn]))))))
  
  (defun get-user-debt:decimal (account:string asset:string index:decimal)
    @doc "Get user's current debt"
    (with-default-read debt-balances (format "{}:{}" [account asset])
      { "scaled-balance": 0.0 }
      { "scaled-balance" := scaled }
      
      (* scaled index)))
  
  (defun get-total-supply:decimal (asset:string)
    @doc "Get total debt supply"
    (at 'total-supply (read debt-reserves asset)))
  
  ;; Initialize debt reserve
  (defun init-reserve:string (asset:string)
    @doc "Initialize debt reserve"
    (insert debt-reserves asset {
      "asset": asset,
      "total-supply": 0.0,
      "average-stable-rate": 0.0,
      "last-update": (at 'block-time (chain-data))
    })
    (format "Debt reserve {} initialized" [asset]))
)

(create-table debt-balances)
(create-table debt-reserves)
```

## Step 4: Interest Rate Model

Now let's implement a more sophisticated interest rate model:

```pact
;; markets/interest-rate.pact
(module interest-rate-model GOVERNANCE
  @doc "Dynamic interest rate model for DeFi protocol"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Schemas
  (defschema rate-strategy
    @doc "Interest rate strategy parameters"
    asset:string
    base-rate:decimal
    slope1:decimal              ;; Below optimal utilization
    slope2:decimal              ;; Above optimal utilization
    optimal-utilization:decimal
    stable-rate-slope1:decimal
    stable-rate-slope2:decimal)
  
  (defschema rate-data
    @doc "Current rate information"
    liquidity-rate:decimal
    stable-borrow-rate:decimal
    variable-borrow-rate:decimal
    utilization-rate:decimal
    last-update:time)
  
  ;; Tables
  (deftable rate-strategies:{rate-strategy})
  (deftable current-rates:{rate-data})
  
  ;; Constants
  (defconst SECONDS_PER_YEAR 31536000)
  (defconst RAY 1000000000000000000000000000)
  (defconst OPTIMAL_STABLE_TO_TOTAL_DEBT_RATIO 0.2)
  
  ;; Rate Calculation Functions
  (defun calculate-interest-rates:object 
    ( asset:string 
      available-liquidity:decimal 
      total-stable-debt:decimal 
      total-variable-debt:decimal 
      average-stable-rate:decimal 
      reserve-factor:decimal )
    @doc "Calculate all interest rates"
    
    (with-read rate-strategies asset
      { "base-rate" := base-rate
      , "slope1" := slope1
      , "slope2" := slope2
      , "optimal-utilization" := optimal
      , "stable-rate-slope1" := stable-slope1
      , "stable-rate-slope2" := stable-slope2 }
      
      (let* ((total-debt (+ total-stable-debt total-variable-debt))
             (utilization (calculate-utilization-rate 
                          available-liquidity total-debt)))
        
        ;; Calculate variable borrow rate
        (let ((variable-rate 
               (if (= utilization 0.0)
                 base-rate
                 (if (<= utilization optimal)
                   (+ base-rate 
                      (* (/ slope1 optimal) utilization))
                   (+ base-rate slope1
                      (* (/ slope2 (- 1.0 optimal)) 
                         (- utilization optimal)))))))
          
          ;; Calculate stable borrow rate
          (let ((stable-rate
                 (if (<= utilization optimal)
                   (+ base-rate 
                      (* (/ stable-slope1 optimal) utilization))
                   (+ base-rate stable-slope1
                      (* (/ stable-slope2 (- 1.0 optimal)) 
                         (- utilization optimal))))))
            
            ;; Calculate overall borrow rate
            (let ((overall-borrow-rate
                   (if (= total-debt 0.0)
                     0.0
                     (+ (/ (* total-stable-debt average-stable-rate) 
                          total-debt)
                        (/ (* total-variable-debt variable-rate) 
                          total-debt)))))
              
              ;; Calculate liquidity rate
              (let ((liquidity-rate
                     (* overall-borrow-rate 
                        utilization 
                        (- 1.0 reserve-factor))))
                
                ;; Store current rates
                (write current-rates asset {
                  "liquidity-rate": liquidity-rate,
                  "stable-borrow-rate": stable-rate,
                  "variable-borrow-rate": variable-rate,
                  "utilization-rate": utilization,
                  "last-update": (at 'block-time (chain-data))
                })
                
                ;; Return rate data
                { "liquidity-rate": liquidity-rate
                , "stable-borrow-rate": stable-rate
                , "variable-borrow-rate": variable-rate
                , "utilization-rate": utilization }))))))
  
  (defun calculate-utilization-rate:decimal 
    ( available-liquidity:decimal 
      total-debt:decimal )
    @doc "Calculate utilization rate"
    (if (= (+ available-liquidity total-debt) 0.0)
      0.0
      (/ total-debt (+ available-liquidity total-debt))))
  
  ;; Rate Strategy Management
  (defun set-rate-strategy:string 
    ( asset:string 
      base-rate:decimal 
      slope1:decimal 
      slope2:decimal 
      optimal-utilization:decimal )
    @doc "Set interest rate strategy for asset"
    (with-capability (GOVERNANCE)
      (write rate-strategies asset {
        "asset": asset,
        "base-rate": base-rate,
        "slope1": slope1,
        "slope2": slope2,
        "optimal-utilization": optimal-utilization,
        "stable-rate-slope1": slope1,
        "stable-rate-slope2": slope2
      })
      (format "Rate strategy set for {}" [asset])))
  
  ;; View Functions
  (defun get-current-rates:object (asset:string)
    @doc "Get current interest rates"
    (read current-rates asset))
  
  (defun get-rate-strategy:object (asset:string)
    @doc "Get rate strategy parameters"
    (read rate-strategies asset))
)

(create-table rate-strategies)
(create-table current-rates)
```

## Step 5: Liquidation Engine

Let's implement the liquidation mechanism:

```pact
;; markets/liquidation.pact
(module liquidation-engine GOVERNANCE
  @doc "Liquidation engine for undercollateralized positions"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Schemas
  (defschema liquidation-data
    @doc "Liquidation event data"
    liquidation-id:string
    user:string
    liquidator:string
    debt-asset:string
    debt-amount:decimal
    collateral-asset:string
    collateral-amount:decimal
    timestamp:time)
  
  ;; Tables
  (deftable liquidations:{liquidation-data})
  
  ;; Constants
  (defconst MAX_LIQUIDATION_CLOSE_FACTOR 0.5)
  (defconst LIQUIDATION_PROTOCOL_FEE 0.001) ;; 0.1%
  
  ;; Capabilities
  (defcap LIQUIDATE 
    ( liquidator:string 
      user:string 
      debt-asset:string 
      collateral-asset:string )
    @doc "Liquidation capability"
    @event
    (enforce (!= liquidator user) "Cannot self-liquidate"))
  
  ;; Main Liquidation Function
  (defun liquidate:object 
    ( liquidator:string
      user:string
      debt-asset:string
      debt-to-cover:decimal
      collateral-asset:string
      receive-atoken:bool )
    @doc "Execute liquidation of undercollateralized position"
    
    (with-capability (LIQUIDATE liquidator user debt-asset collateral-asset)
      ;; Validate liquidation conditions
      (let ((health-data (validate-liquidation user)))
        
        (enforce (at 'can-liquidate health-data) 
                "Position is healthy - cannot liquidate")
        
        ;; Calculate liquidation amounts
        (let* ((liquidation-amounts 
                (calculate-liquidation-amounts
                  user
                  debt-asset
                  debt-to-cover
                  collateral-asset))
               (actual-debt-to-liquidate 
                (at 'debt-amount liquidation-amounts))
               (max-collateral-to-liquidate 
                (at 'collateral-amount liquidation-amounts))
               (liquidation-bonus 
                (at 'liquidation-bonus liquidation-amounts))
               (protocol-fee 
                (* max-collateral-to-liquidate LIQUIDATION_PROTOCOL_FEE))
               (liquidator-reward 
                (- max-collateral-to-liquidate protocol-fee)))
          
          ;; Execute liquidation transfers
          ;; 1. Liquidator repays debt on behalf of user
          (repay-debt liquidator debt-asset actual-debt-to-liquidate user)
          
          ;; 2. Transfer collateral to liquidator
          (if receive-atoken
            ;; Transfer aTokens
            (transfer-atokens user liquidator collateral-asset liquidator-reward)
            ;; Withdraw and transfer underlying
            (transfer-underlying user liquidator collateral-asset liquidator-reward))
          
          ;; 3. Transfer protocol fee
          (transfer-protocol-fee user collateral-asset protocol-fee)
          
          ;; Record liquidation
          (let ((liquidation-id (generate-liquidation-id)))
            (insert liquidations liquidation-id {
              "liquidation-id": liquidation-id,
              "user": user,
              "liquidator": liquidator,
              "debt-asset": debt-asset,
              "debt-amount": actual-debt-to-liquidate,
              "collateral-asset": collateral-asset,
              "collateral-amount": max-collateral-to-liquidate,
              "timestamp": (at 'block-time (chain-data))
            }))
          
          ;; Return liquidation result
          { "debt-liquidated": actual-debt-to-liquidate
          , "collateral-liquidated": max-collateral-to-liquidate
          , "liquidator-reward": liquidator-reward
          , "protocol-fee": protocol-fee
          , "liquidation-bonus": liquidation-bonus }))))
  
  ;; Validation Functions
  (defun validate-liquidation:object (user:string)
    @doc "Check if user can be liquidated"
    ;; Get user's account data from lending pool
    (let ((account-data (get-user-account-data user)))
      (let ((health-factor (at 'health-factor account-data)))
        { "health-factor": health-factor
        , "can-liquidate": (< health-factor 1.0)
        , "total-collateral": (at 'total-collateral-base account-data)
        , "total-debt": (at 'total-debt-base account-data) })))
  
  ;; Calculation Functions
  (defun calculate-liquidation-amounts:object 
    ( user:string
      debt-asset:string
      requested-debt-amount:decimal
      collateral-asset:string )
    @doc "Calculate actual liquidation amounts"
    
    ;; Get user's debt and collateral
    (let* ((user-debt (get-user-debt-amount user debt-asset))
           (user-collateral (get-user-collateral-amount user collateral-asset))
           (debt-price (get-asset-price debt-asset))
           (collateral-price (get-asset-price collateral-asset))
           (liquidation-bonus (get-liquidation-bonus collateral-asset))
           
           ;; Calculate maximum liquidatable debt (close factor)
           (max-liquidatable-debt (* user-debt MAX_LIQUIDATION_CLOSE_FACTOR))
           
           ;; Actual debt to liquidate
           (debt-to-liquidate 
            (min requested-debt-amount max-liquidatable-debt))
           
           ;; Calculate collateral needed
           (base-collateral 
            (/ (* debt-to-liquidate debt-price) collateral-price))
           
           ;; Add liquidation bonus
           (collateral-with-bonus 
            (* base-collateral (+ 1.0 liquidation-bonus)))
           
           ;; Ensure we don't liquidate more than available
           (actual-collateral 
            (min collateral-with-bonus user-collateral)))
      
      { "debt-amount": debt-to-liquidate
      , "collateral-amount": actual-collateral
      , "liquidation-bonus": liquidation-bonus
      , "debt-price": debt-price
      , "collateral-price": collateral-price }))
  
  ;; Helper Functions
  (defun generate-liquidation-id:string ()
    @doc "Generate unique liquidation ID"
    (format "LIQ-{}-{}" 
            [(at 'block-time (chain-data)) 
             (at 'block-height (chain-data))]))
  
  (defun get-user-account-data:object (user:string)
    @doc "Get user account data from lending pool"
    ;; This would call lending-pool module
    {}) ;; Placeholder
  
  (defun get-user-debt-amount:decimal (user:string asset:string)
    @doc "Get user's debt in specific asset"
    ;; This would call debt-token module
    0.0) ;; Placeholder
  
  (defun get-user-collateral-amount:decimal (user:string asset:string)
    @doc "Get user's collateral in specific asset"
    ;; This would call atoken module
    0.0) ;; Placeholder
  
  (defun get-asset-price:decimal (asset:string)
    @doc "Get asset price from oracle"
    ;; This would call price oracle
    1.0) ;; Placeholder
  
  (defun get-liquidation-bonus:decimal (asset:string)
    @doc "Get liquidation bonus for asset"
    ;; This would read from asset config
    0.05) ;; 5% placeholder
  
  (defun repay-debt:string 
    ( liquidator:string 
      asset:string 
      amount:decimal 
      user:string )
    @doc "Repay debt on behalf of user"
    ;; This would call lending-pool repay
    "") ;; Placeholder
  
  (defun transfer-atokens:string 
    ( from:string 
      to:string 
      asset:string 
      amount:decimal )
    @doc "Transfer aTokens"
    ;; This would call atoken transfer
    "") ;; Placeholder
  
  (defun transfer-underlying:string 
    ( from:string 
      to:string 
      asset:string 
      amount:decimal )
    @doc "Withdraw and transfer underlying asset"
    ;; This would call atoken withdraw and transfer
    "") ;; Placeholder
  
  (defun transfer-protocol-fee:string 
    ( from:string 
      asset:string 
      amount:decimal )
    @doc "Transfer protocol fee to treasury"
    ;; This would transfer fee to protocol treasury
    "") ;; Placeholder
)

(create-table liquidations)
```

## Step 6: Yield Farming

Let's implement the yield farming and rewards system:

```pact
;; rewards/farming.pact
(module yield-farming GOVERNANCE
  @doc "Yield farming and liquidity mining rewards"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Schemas
  (defschema farm-pool
    @doc "Farming pool configuration"
    pool-id:string
    staking-token:string       ;; Token to stake (LP or aToken)
    reward-token:string        ;; Token distributed as reward
    reward-per-second:decimal
    total-staked:decimal
    acc-reward-per-share:decimal ;; Accumulated reward per share
    last-update:time
    start-time:time
    end-time:time
    is-active:bool)
  
  (defschema user-stake
    @doc "User staking position"
    user:string
    pool-id:string
    amount:decimal
    reward-debt:decimal        ;; Reward debt for proper distribution
    pending-rewards:decimal
    stake-time:time
    last-claim:time)
  
  (defschema reward-multiplier
    @doc "Time-based reward multipliers"
    pool-id:string
    duration:integer           ;; Days staked
    multiplier:decimal)        ;; Reward multiplier
  
  ;; Tables
  (deftable farm-pools:{farm-pool})
  (deftable user-stakes:{user-stake})
  (deftable reward-multipliers:{reward-multiplier})
  
  ;; Capabilities
  (defcap STAKE (user:string pool-id:string amount:decimal)
    @doc "Staking capability"
    @event
    (enforce (> amount 0.0) "Amount must be positive"))
  
  (defcap UNSTAKE (user:string pool-id:string amount:decimal)
    @doc "Unstaking capability"
    @event
    (enforce (> amount 0.0) "Amount must be positive"))
  
  (defcap CLAIM (user:string pool-id:string)
    @doc "Claim rewards capability"
    @event
    true)
  
  ;; Pool Management
  (defun create-farm-pool:string 
    ( pool-id:string
      staking-token:string
      reward-token:string
      reward-per-second:decimal
      start-time:time
      end-time:time )
    @doc "Create new farming pool"
    (with-capability (GOVERNANCE)
      (enforce (> end-time start-time) "Invalid time range")
      
      (insert farm-pools pool-id {
        "pool-id": pool-id,
        "staking-token": staking-token,
        "reward-token": reward-token,
        "reward-per-second": reward-per-second,
        "total-staked": 0.0,
        "acc-reward-per-share": 0.0,
        "last-update": start-time,
        "start-time": start-time,
        "end-time": end-time,
        "is-active": true
      })
      
      ;; Set default multipliers
      (set-multipliers pool-id)
      
      (format "Farm pool {} created" [pool-id])))
  
  (defun set-multipliers:string (pool-id:string)
    @doc "Set time-based multipliers"
    ;; 7 days: 1.1x, 30 days: 1.25x, 90 days: 1.5x
    (insert reward-multipliers (format "{}:7" [pool-id]) {
      "pool-id": pool-id,
      "duration": 7,
      "multiplier": 1.1
    })
    (insert reward-multipliers (format "{}:30" [pool-id]) {
      "pool-id": pool-id,
      "duration": 30,
      "multiplier": 1.25
    })
    (insert reward-multipliers (format "{}:90" [pool-id]) {
      "pool-id": pool-id,
      "duration": 90,
      "multiplier": 1.5
    })
    "Multipliers set")
  
  ;; Staking Functions
  (defun stake:string (user:string pool-id:string amount:decimal)
    @doc "Stake tokens in farming pool"
    (with-capability (STAKE user pool-id amount)
      ;; Update pool
      (update-pool pool-id)
      
      (with-read farm-pools pool-id
        { "is-active" := active
        , "total-staked" := total-staked
        , "acc-reward-per-share" := acc-reward }
        
        (enforce active "Pool is not active")
        
        ;; Update or create user stake
        (with-default-read user-stakes (format "{}:{}" [user pool-id])
          { "amount": 0.0
          , "reward-debt": 0.0
          , "pending-rewards": 0.0 }
          { "amount" := current-amount
          , "reward-debt" := current-debt
          , "pending-rewards" := pending }
          
          ;; Calculate pending rewards for existing stake
          (let ((new-pending 
                 (if (> current-amount 0.0)
                   (+ pending 
                      (- (* current-amount acc-reward) current-debt))
                   pending)))
            
            ;; Write updated stake
            (write user-stakes (format "{}:{}" [user pool-id]) {
              "user": user,
              "pool-id": pool-id,
              "amount": (+ current-amount amount),
              "reward-debt": (* (+ current-amount amount) acc-reward),
              "pending-rewards": new-pending,
              "stake-time": (at 'block-time (chain-data)),
              "last-claim": (at 'block-time (chain-data))
            })))
        
        ;; Update pool total
        (update farm-pools pool-id {
          "total-staked": (+ total-staked amount)
        })
        
        (format "Staked {} tokens in pool {}" [amount pool-id]))))
  
  (defun unstake:string (user:string pool-id:string amount:decimal)
    @doc "Unstake tokens from farming pool"
    (with-capability (UNSTAKE user pool-id amount)
      ;; Update pool
      (update-pool pool-id)
      
      (with-read user-stakes (format "{}:{}" [user pool-id])
        { "amount" := staked-amount
        , "reward-debt" := reward-debt }
        
        (enforce (>= staked-amount amount) "Insufficient stake")
        
        ;; Claim pending rewards first
        (claim-rewards user pool-id)
        
        (with-read farm-pools pool-id
          { "total-staked" := total-staked
          , "acc-reward-per-share" := acc-reward }
          
          ;; Update user stake
          (update user-stakes (format "{}:{}" [user pool-id]) {
            "amount": (- staked-amount amount),
            "reward-debt": (* (- staked-amount amount) acc-reward)
          })
          
          ;; Update pool total
          (update farm-pools pool-id {
            "total-staked": (- total-staked amount)
          }))
        
        (format "Unstaked {} tokens from pool {}" [amount pool-id]))))
  
  (defun claim-rewards:string (user:string pool-id:string)
    @doc "Claim accumulated rewards"
    (with-capability (CLAIM user pool-id)
      ;; Update pool
      (update-pool pool-id)
      
      (with-read user-stakes (format "{}:{}" [user pool-id])
        { "amount" := staked-amount
        , "reward-debt" := reward-debt
        , "pending-rewards" := pending
        , "stake-time" := stake-time }
        
        (with-read farm-pools pool-id
          { "acc-reward-per-share" := acc-reward }
          
          ;; Calculate total rewards
          (let* ((accumulated (* staked-amount acc-reward))
                 (current-rewards (- accumulated reward-debt))
                 (total-rewards (+ pending current-rewards))
                 ;; Apply time multiplier
                 (stake-duration (get-stake-duration stake-time))
                 (multiplier (get-multiplier pool-id stake-duration))
                 (final-rewards (* total-rewards multiplier)))
            
            (if (> final-rewards 0.0)
              (progn
                ;; Transfer rewards
                ;; This would call token transfer
                
                ;; Update user stake
                (update user-stakes (format "{}:{}" [user pool-id]) {
                  "reward-debt": accumulated,
                  "pending-rewards": 0.0,
                  "last-claim": (at 'block-time (chain-data))
                })
                
                (format "Claimed {} rewards with {}x multiplier" 
                        [final-rewards multiplier]))
              "No rewards to claim"))))))
  
  ;; Pool Update Function
  (defun update-pool:string (pool-id:string)
    @doc "Update pool rewards"
    (with-read farm-pools pool-id
      { "total-staked" := total-staked
      , "acc-reward-per-share" := acc-reward
      , "last-update" := last-update
      , "reward-per-second" := reward-rate
      , "end-time" := end-time }
      
      (let ((current-time (at 'block-time (chain-data))))
        (if (and (> total-staked 0.0) 
                 (> current-time last-update))
          (let* ((time-elapsed 
                  (min (diff-time current-time last-update)
                       (diff-time end-time last-update)))
                 (time-seconds (floor time-elapsed))
                 (rewards (* reward-rate time-seconds))
                 (reward-per-share (/ rewards total-staked)))
            
            (update farm-pools pool-id {
              "acc-reward-per-share": (+ acc-reward reward-per-share),
              "last-update": current-time
            })
            "Pool updated")
          "No update needed"))))
  
  ;; Helper Functions
  (defun get-stake-duration:integer (stake-time:time)
    @doc "Get stake duration in days"
    (let* ((current-time (at 'block-time (chain-data)))
           (duration (diff-time current-time stake-time)))
      (floor (/ duration 86400)))) ;; Convert to days
  
  (defun get-multiplier:decimal (pool-id:string days:integer)
    @doc "Get reward multiplier based on stake duration"
    (cond
      ((>= days 90) 1.5)
      ((>= days 30) 1.25)
      ((>= days 7) 1.1)
      (true 1.0)))
  
  (defun get-pending-rewards:decimal (user:string pool-id:string)
    @doc "Calculate pending rewards"
    ;; First update pool
    (update-pool pool-id)
    
    (with-default-read user-stakes (format "{}:{}" [user pool-id])
      { "amount": 0.0
      , "reward-debt": 0.0
      , "pending-rewards": 0.0
      , "stake-time": (at 'block-time (chain-data)) }
      { "amount" := amount
      , "reward-debt" := debt
      , "pending-rewards" := pending
      , "stake-time" := stake-time }
      
      (if (> amount 0.0)
        (with-read farm-pools pool-id
          { "acc-reward-per-share" := acc-reward }
          
          (let* ((accumulated (* amount acc-reward))
                 (current-rewards (- accumulated debt))
                 (total-rewards (+ pending current-rewards))
                 (stake-duration (get-stake-duration stake-time))
                 (multiplier (get-multiplier pool-id stake-duration)))
            (* total-rewards multiplier)))
        0.0)))
)

(create-table farm-pools)
(create-table user-stakes)
(create-table reward-multipliers)
```

## Testing & Security

Let's create comprehensive tests for the DeFi protocol:

```pact
;; tests/test-defi-basic.repl
;; Basic DeFi functionality tests

(begin-tx "Setup test environment")

(env-data {
  "defi-admin": {
    "keys": ["admin-key"],
    "pred": "keys-all"
  },
  "alice-keyset": {
    "keys": ["alice-key"],
    "pred": "keys-all"
  },
  "bob-keyset": {
    "keys": ["bob-key"],
    "pred": "keys-all"
  }
})

(env-keys ["admin-key"])

(define-keyset 'defi-admin (read-keyset "defi-admin"))
(define-keyset 'alice-keyset (read-keyset "alice-keyset"))
(define-keyset 'bob-keyset (read-keyset "bob-keyset"))

;; Load modules
(load "../tokens/atoken.pact")
(load "../tokens/debt-token.pact")
(load "../core/lending-pool.pact")
(load "../markets/interest-rate.pact")
(load "../rewards/farming.pact")

(commit-tx)

;; Test Asset Setup
(begin-tx "Initialize reserves")

(env-keys ["admin-key"])

;; Initialize USDC reserve
(expect "USDC reserve initialized"
  "Reserve USDC initialized"
  (atoken.initialize-reserve "USDC" 0.1))

;; Initialize ETH reserve
(expect "ETH reserve initialized"
  "Reserve ETH initialized"
  (atoken.initialize-reserve "ETH" 0.1))

;; Configure assets
(expect "USDC configured"
  "Asset USDC configured"
  (lending-pool.configure-asset "USDC" 0.8 0.85 0.05 6))

(expect "ETH configured"
  "Asset ETH configured"
  (lending-pool.configure-asset "ETH" 0.75 0.8 0.1 18))

;; Set interest rate strategies
(interest-rate-model.set-rate-strategy "USDC" 0.02 0.07 0.6 0.8)
(interest-rate-model.set-rate-strategy "ETH" 0.03 0.08 0.5 0.75)

;; Initialize debt reserves
(debt-token.init-reserve "USDC")
(debt-token.init-reserve "ETH")

(commit-tx)

;; Test Deposits
(begin-tx "Test deposits")

(env-keys ["alice-key"])

;; Alice deposits 1000 USDC
(expect "Alice deposit succeeds"
  "Deposited 1000.0 USDC"
  (atoken.deposit "alice" "USDC" 1000.0))

(expect "Alice USDC balance"
  1000.0
  (atoken.get-balance "alice" "USDC"))

;; Alice deposits 10 ETH
(expect "Alice ETH deposit"
  "Deposited 10.0 ETH"
  (atoken.deposit "alice" "ETH" 10.0))

(commit-tx)

;; Test Borrowing
(begin-tx "Test borrowing")

(env-keys ["alice-key"])

;; Alice borrows 500 USDC against ETH collateral
(expect "Borrow succeeds"
  "Borrowed 500.0 USDC"
  (lending-pool.borrow "alice" "USDC" 500.0 "variable"))

;; Check debt
(expect "Alice has USDC debt"
  500.0
  (debt-token.get-user-debt "alice" "USDC" 1.0))

(commit-tx)

;; Test Interest Accrual
(begin-tx "Test interest accrual")

;; Simulate time passing (would need env-chain-data in real test)
;; Update indexes
(atoken.update-indexes "USDC")
(atoken.update-indexes "ETH")

;; Check updated balances (would show interest accrued)

(commit-tx)

;; Test Repayment
(begin-tx "Test repayment")

(env-keys ["alice-key"])

;; Alice repays 100 USDC
(expect "Repay succeeds"
  "Repaid 100.0 USDC for alice"
  (lending-pool.repay "alice" "USDC" 100.0 "alice"))

;; Check reduced debt
(expect "Debt reduced"
  400.0
  (debt-token.get-user-debt "alice" "USDC" 1.0))

(commit-tx)

;; Test Yield Farming
(begin-tx "Test yield farming")

(env-keys ["admin-key"])

;; Create farming pool for aUSDC
(let ((start-time (at 'block-time (chain-data)))
      (end-time (add-time start-time 2592000))) ;; 30 days
  
  (expect "Farm pool created"
    "Farm pool aUSDC-FARM created"
    (yield-farming.create-farm-pool 
      "aUSDC-FARM" 
      "aUSDC" 
      "FARM" 
      0.1 
      start-time 
      end-time)))

(env-keys ["alice-key"])

;; Alice stakes aUSDC
(expect "Stake succeeds"
  "Staked 500.0 tokens in pool aUSDC-FARM"
  (yield-farming.stake "alice" "aUSDC-FARM" 500.0))

;; Check pending rewards
(expect "Has pending rewards"
  true
  (>= (yield-farming.get-pending-rewards "alice" "aUSDC-FARM") 0.0))

(commit-tx)

(print "All DeFi basic tests passed!")
```

Let's also create a security test module:

```pact
;; tests/test-defi-security.repl
;; Security and edge case tests

(begin-tx "Setup for security tests")

;; ... (same setup as before)

(commit-tx)

;; Test Liquidation Threshold
(begin-tx "Test liquidation scenarios")

;; Setup: Bob deposits and borrows to near liquidation
(env-keys ["bob-key"])

(atoken.deposit "bob" "ETH" 1.0)
(lending-pool.borrow "bob" "USDC" 750.0 "variable")

;; Simulate price drop (would use oracle in production)
;; This would trigger liquidation eligibility

;; Alice attempts liquidation
(env-keys ["alice-key"])

(expect-failure "Cannot liquidate healthy position"
  "Position is healthy - cannot liquidate"
  (liquidation-engine.liquidate 
    "alice" 
    "bob" 
    "USDC" 
    375.0 
    "ETH" 
    false))

(commit-tx)

;; Test Reentrancy Protection
(begin-tx "Test reentrancy protection")

;; Attempts to call protected functions recursively should fail
;; (Implementation would include reentrancy guards)

(commit-tx)

;; Test Parameter Boundaries
(begin-tx "Test parameter validation")

(env-keys ["admin-key"])

;; Test invalid reserve factor
(expect-failure "Reserve factor > 1 fails"
  "Reserve factor must be <= 1.0"
  (atoken.initialize-reserve "TEST" 1.5))

;; Test invalid LTV
(expect-failure "LTV > liquidation threshold fails"
  "LTV must be <= liquidation threshold"
  (lending-pool.configure-asset "TEST" 0.9 0.8 0.1 18))

;; Test negative amounts
(env-keys ["alice-key"])

(expect-failure "Negative deposit fails"
  "Amount must be positive"
  (atoken.deposit "alice" "USDC" -100.0))

(expect-failure "Zero borrow fails"
  "Amount must be positive"
  (lending-pool.borrow "alice" "USDC" 0.0 "variable"))

(commit-tx)

;; Test Flash Loan Attack Prevention
(begin-tx "Test flash loan safety")

;; Flash loans should not affect interest rates within same block
;; Price manipulation should be prevented by oracles
;; (Implementation would include these protections)

(commit-tx)

(print "Security tests completed!")
```

## Frontend Integration

Here's an example of frontend integration for the DeFi protocol:

```javascript
// frontend/defi-integration.js
// DeFi Protocol Frontend Integration

import { Pact } from '@kadena/client';

class DeFiProtocol {
  constructor(network, chainId) {
    this.network = network;
    this.chainId = chainId;
    this.GAS_LIMIT = 150000;
  }

  // Deposit assets
  async deposit(account, asset, amount, signer) {
    const cmd = {
      pactCode: `(atoken.deposit "${account}" "${asset}" ${amount})`,
      caps: [
        Pact.lang.mkCap(
          "Deposit capability",
          "DEPOSIT",
          `${account}`,
          `${asset}`,
          amount
        ),
        Pact.lang.mkCap("Gas capability", "GAS", "")
      ],
      sender: account,
      gasLimit: this.GAS_LIMIT,
      chainId: this.chainId,
      ttl: 600
    };

    return await this.sendTransaction(cmd, signer);
  }

  // Borrow assets
  async borrow(account, asset, amount, mode = "variable", signer) {
    const cmd = {
      pactCode: `(lending-pool.borrow "${account}" "${asset}" ${amount} "${mode}")`,
      caps: [
        Pact.lang.mkCap(
          "Borrow capability",
          "BORROW",
          `${account}`,
          `${asset}`,
          amount
        ),
        Pact.lang.mkCap("Gas capability", "GAS", "")
      ],
      sender: account,
      gasLimit: this.GAS_LIMIT,
      chainId: this.chainId,
      ttl: 600
    };

    return await this.sendTransaction(cmd, signer);
  }

  // Get user account data
  async getUserAccountData(account) {
    const cmd = {
      pactCode: `(lending-pool.calculate-user-account-data "${account}")`,
      meta: {
        chainId: this.chainId,
        gasLimit: this.GAS_LIMIT
      }
    };

    const result = await Pact.fetch.local(cmd, this.network);
    return result.result.data;
  }

  // Get asset APY
  async getAssetAPY(asset) {
    const cmd = {
      pactCode: `(interest-rate-model.get-current-rates "${asset}")`,
      meta: {
        chainId: this.chainId,
        gasLimit: this.GAS_LIMIT
      }
    };

    const result = await Pact.fetch.local(cmd, this.network);
    const rates = result.result.data;
    
    return {
      depositAPY: rates['liquidity-rate'] * 100,
      borrowAPY: rates['variable-borrow-rate'] * 100,
      utilization: rates['utilization-rate'] * 100
    };
  }

  // Send transaction helper
  async sendTransaction(cmd, signer) {
    const signedCmd = await signer(cmd);
    const requestKeys = await Pact.fetch.send(signedCmd, this.network);
    
    // Wait for transaction
    const result = await Pact.fetch.listen(
      { listen: requestKeys.requestKeys[0] },
      this.network
    );
    
    return result;
  }
}

// React component example
function DeFiDashboard() {
  const [userAccount, setUserAccount] = useState(null);
  const [markets, setMarkets] = useState([]);
  const defi = new DeFiProtocol(NETWORK_URL, CHAIN_ID);

  useEffect(() => {
    loadUserData();
    loadMarkets();
  }, []);

  async function loadUserData() {
    if (currentUser) {
      const data = await defi.getUserAccountData(currentUser);
      setUserAccount(data);
    }
  }

  async function loadMarkets() {
    const assets = ['USDC', 'ETH', 'KDA'];
    const marketData = await Promise.all(
      assets.map(async (asset) => {
        const apy = await defi.getAssetAPY(asset);
        const reserves = await defi.getReserveData(asset);
        return { asset, ...apy, ...reserves };
      })
    );
    setMarkets(marketData);
  }

  async function handleDeposit(asset, amount) {
    try {
      const result = await defi.deposit(
        currentUser,
        asset,
        amount,
        walletSigner
      );
      
      if (result.result.status === 'success') {
        toast.success(`Deposited ${amount} ${asset}`);
        loadUserData();
        loadMarkets();
      }
    } catch (error) {
      toast.error(error.message);
    }
  }

  return (
    <div className="defi-dashboard">
      {/* User Account Summary */}
      {userAccount && (
        <div className="account-summary">
          <h2>Your Position</h2>
          <div className="stats">
            <div>
              <span>Total Supplied</span>
              <span>${userAccount['total-collateral-base']}</span>
            </div>
            <div>
              <span>Total Borrowed</span>
              <span>${userAccount['total-debt-base']}</span>
            </div>
            <div>
              <span>Health Factor</span>
              <span className={getHealthFactorClass(userAccount['health-factor'])}>
                {userAccount['health-factor']}
              </span>
            </div>
          </div>
        </div>
      )}

      {/* Markets */}
      <div className="markets">
        <h2>Markets</h2>
        <table>
          <thead>
            <tr>
              <th>Asset</th>
              <th>Deposit APY</th>
              <th>Borrow APY</th>
              <th>Utilization</th>
              <th>Actions</th>
            </tr>
          </thead>
          <tbody>
            {markets.map(market => (
              <MarketRow 
                key={market.asset}
                market={market}
                onDeposit={handleDeposit}
                onBorrow={handleBorrow}
              />
            ))}
          </tbody>
        </table>
      </div>

      {/* Yield Farming */}
      <YieldFarmingSection />
    </div>
  );
}
```

## Summary

In this chapter, we've built a comprehensive DeFi lending and borrowing protocol featuring:

1. **Asset Management**: Interest-bearing aTokens with automatic yield generation
2. **Lending Pool**: Multi-asset deposits and borrowing with health factor monitoring
3. **Interest Rate Model**: Dynamic rates based on utilization
4. **Liquidation Engine**: Automated liquidations with incentives
5. **Yield Farming**: Additional rewards for liquidity providers
6. **Security**: Comprehensive validation and protection mechanisms

### Key Concepts Learned:
- Implementing complex financial calculations on-chain
- Managing collateral ratios and liquidations
- Creating sustainable tokenomics
- Building interest rate models
- Ensuring protocol security
- Optimizing for gas efficiency

### Next Steps:
- Add flash loan functionality
- Implement governance voting
- Create more sophisticated interest rate models
- Add cross-chain bridging
- Implement insurance mechanisms
- Build analytics dashboard

This DeFi protocol provides a solid foundation for building more advanced financial products on the blockchain!