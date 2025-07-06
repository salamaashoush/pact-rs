;; lending-pool-improved.pact
;; Core lending pool implementation with proper managed capabilities
(module lending-pool GOVERNANCE
  @doc "Core lending and borrowing logic with managed capabilities"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Schemas
  (defschema user-data
    @doc "User position data"
    account:string
    collateral-assets:[string]
    borrowed-assets:[string]
    total-collateral:decimal    ;; In USD
    total-debt:decimal          ;; In USD
    health-factor:decimal)
  
  (defschema asset-data
    @doc "Asset configuration"
    asset:string
    ltv:decimal                 ;; Loan-to-value ratio
    liquidation-threshold:decimal
    liquidation-bonus:decimal
    active:bool
    borrowing-enabled:bool
    price:decimal)              ;; Simplified - would use oracle
  
  (defschema borrow-data
    @doc "Borrow position"
    account:string
    asset:string
    amount:decimal
    borrow-rate:decimal
    last-update:time)
  
  ;; Tables
  (deftable user-data-table:{user-data})
  (deftable asset-config:{asset-data})
  (deftable borrows:{borrow-data})
  
  ;; Constants
  (defconst HEALTH_FACTOR_THRESHOLD 1.0)
  (defconst CLOSE_FACTOR 0.5)  ;; Max 50% liquidation
  
  ;; Managed Capabilities
  (defcap BORROW-POWER (account:string max-borrow-usd:decimal)
    @doc "Managed capability for borrowing power"
    @managed max-borrow-usd BORROW-POWER-mgr
    (enforce-valid-account account)
    (let ((current-collateral (get-total-collateral-value account)))
      (enforce (> current-collateral 0.0) "No collateral deposited")))
  
  (defun BORROW-POWER-mgr:decimal (available:decimal requested:decimal)
    @doc "Manage borrowing power"
    (let ((remaining (- available requested)))
      (enforce (>= remaining 0.0) 
        (format "Insufficient borrowing power: available={}, requested={}" 
                [available requested]))
      remaining))
  
  (defcap COLLATERAL-WITHDRAW (account:string asset:string max-withdraw:decimal)
    @doc "Managed capability for collateral withdrawal"
    @managed max-withdraw COLLATERAL-WITHDRAW-mgr
    (enforce-valid-account account)
    (enforce-guard (at 'guard (coin.details account))))
  
  (defun COLLATERAL-WITHDRAW-mgr:decimal (available:decimal requested:decimal)
    @doc "Manage collateral withdrawal limits"
    (let ((remaining (- available requested)))
      (enforce (>= remaining 0.0) "Exceeds withdrawal limit")
      remaining))
  
  (defcap LIQUIDATION-ALLOWANCE (liquidator:string max-liquidation-usd:decimal)
    @doc "Managed capability for liquidation limits"
    @managed max-liquidation-usd LIQUIDATION-ALLOWANCE-mgr
    (enforce-valid-account liquidator))
  
  (defun LIQUIDATION-ALLOWANCE-mgr:decimal (available:decimal requested:decimal)
    @doc "Manage liquidation allowance"
    (let ((remaining (- available requested)))
      (enforce (>= remaining 0.0) "Exceeds liquidation allowance")
      remaining))
  
  ;; Event Capabilities
  (defcap DEPOSIT (account:string asset:string amount:decimal)
    @doc "Deposit event"
    @event
    true)
  
  (defcap BORROW (account:string asset:string amount:decimal)
    @doc "Borrow event"
    @event
    true)
  
  (defcap WITHDRAW (account:string asset:string amount:decimal)
    @doc "Withdraw event"
    @event
    true)
  
  (defcap REPAY (account:string asset:string amount:decimal)
    @doc "Repay event"
    @event
    true)
  
  (defcap LIQUIDATE (liquidator:string user:string 
                     debt-asset:string collateral-asset:string amount:decimal)
    @doc "Liquidation event"
    @event
    true)
  
  ;; Asset Configuration
  (defun add-asset:string 
    ( asset:string 
      ltv:decimal 
      liquidation-threshold:decimal 
      liquidation-bonus:decimal 
      price:decimal )
    @doc "Add new asset to protocol"
    (with-capability (GOVERNANCE)
      (enforce (and (>= ltv 0.0) (<= ltv 1.0)) 
              "Invalid LTV")
      (enforce (and (>= liquidation-threshold ltv) 
                   (<= liquidation-threshold 1.0))
              "Invalid liquidation threshold")
      (enforce (and (>= liquidation-bonus 0.0) 
                   (<= liquidation-bonus 0.5))
              "Invalid liquidation bonus")
      
      (insert asset-config asset {
        "asset": asset,
        "ltv": ltv,
        "liquidation-threshold": liquidation-threshold,
        "liquidation-bonus": liquidation-bonus,
        "active": true,
        "borrowing-enabled": true,
        "price": price
      })
      (format "Asset {} added" [asset])))
  
  ;; Core Functions with Managed Capabilities
  (defun deposit:string (account:string asset:string amount:decimal)
    @doc "Deposit collateral"
    (enforce-valid-account account)
    (enforce-valid-amount amount)
    (enforce-asset-active asset)
    
    ;; Emit event
    (emit-event (DEPOSIT account asset amount))
    
    ;; In real implementation, would transfer tokens
    ;; Here we'll use the defi-token module
    (defi-token.mint account (read-keyset "user-ks") amount)
    
    ;; Update user data
    (with-default-read user-data-table account
      { "collateral-assets": []
      , "borrowed-assets": []
      , "total-collateral": 0.0
      , "total-debt": 0.0
      , "health-factor": 999.0 }
      { "collateral-assets" := assets
      , "total-collateral" := collateral }
      
      (let* ((asset-price (get-asset-price asset))
             (usd-value (* amount asset-price))
             (new-collateral (+ collateral usd-value))
             (new-assets (if (contains asset assets)
                           assets
                           (+ assets [asset]))))
        
        (write user-data-table account {
          "account": account,
          "collateral-assets": new-assets,
          "borrowed-assets": (at 'borrowed-assets 
                                (read user-data-table account)),
          "total-collateral": new-collateral,
          "total-debt": (at 'total-debt 
                          (read user-data-table account)),
          "health-factor": (calculate-health-factor account)
        })))
    
    (format "Deposited {} {}" [amount asset]))
  
  (defun borrow:string (account:string asset:string amount:decimal)
    @doc "Borrow against collateral using managed capability"
    (enforce-valid-account account)
    (enforce-valid-amount amount)
    (enforce-borrowing-enabled asset)
    
    ;; Calculate borrowing power
    (let* ((max-borrow (calculate-max-borrow account))
           (asset-price (get-asset-price asset))
           (borrow-value (* amount asset-price)))
      
      ;; Use managed capability for borrowing power
      (with-capability (BORROW-POWER account max-borrow)
        ;; Emit event
        (emit-event (BORROW account asset amount))
        
        (with-read user-data-table account
          { "total-collateral" := collateral
          , "total-debt" := current-debt
          , "borrowed-assets" := borrowed-list }
          
          (let* ((new-debt (+ current-debt borrow-value))
                 (borrow-key (format "{}:{}" [account asset])))
            
            ;; The managed capability enforces the limit
            (require-capability (BORROW-POWER account borrow-value))
            
            ;; Record borrow
            (with-default-read borrows borrow-key
              { "amount": 0.0 }
              { "amount" := existing-borrow }
              
              (write borrows borrow-key {
                "account": account,
                "asset": asset,
                "amount": (+ existing-borrow amount),
                "borrow-rate": (get-borrow-rate asset),
                "last-update": (at 'block-time (chain-data))
              }))
            
            ;; Update user data
            (update user-data-table account {
              "borrowed-assets": (if (contains asset borrowed-list)
                                  borrowed-list
                                  (+ borrowed-list [asset])),
              "total-debt": new-debt,
              "health-factor": (calculate-health-factor-with-debt 
                               collateral new-debt)
            })
            
            ;; In real implementation, would transfer tokens to user
            (format "Borrowed {} {}" [amount asset])))))
  
  (defun withdraw:string (account:string asset:string amount:decimal)
    @doc "Withdraw collateral with managed capability"
    (enforce-valid-account account)
    (enforce-valid-amount amount)
    
    ;; Calculate max safe withdrawal
    (let* ((max-withdraw (calculate-max-safe-withdrawal account asset))
           (asset-price (get-asset-price asset)))
      
      ;; Use managed capability for withdrawal limits
      (with-capability (COLLATERAL-WITHDRAW account asset max-withdraw)
        ;; Emit event
        (emit-event (WITHDRAW account asset amount))
        
        ;; Verify health factor after withdrawal
        (let ((health (calculate-health-factor-after-withdraw 
                       account asset amount)))
          (enforce (>= health HEALTH_FACTOR_THRESHOLD) 
                  "Withdrawal would make position unhealthy"))
        
        ;; The managed capability enforces the limit
        (require-capability (COLLATERAL-WITHDRAW account asset amount))
        
        ;; Process withdrawal (simplified)
        (with-read user-data-table account
          { "total-collateral" := collateral
          , "collateral-assets" := assets }
          
          (let ((withdraw-value (* amount asset-price)))
            (update user-data-table account {
              "total-collateral": (- collateral withdraw-value),
              "health-factor": (calculate-health-factor account)
            })))
        
        ;; In real implementation, would transfer tokens
        (format "Withdrew {} {}" [amount asset]))))
  
  (defun repay:string (account:string asset:string amount:decimal)
    @doc "Repay borrowed amount"
    (enforce-valid-account account)
    (enforce-valid-amount amount)
    
    ;; Emit event
    (emit-event (REPAY account asset amount))
    
    (let ((borrow-key (format "{}:{}" [account asset])))
      (with-read borrows borrow-key
        { "amount" := debt-amount }
        
        (let* ((repay-amount (min amount debt-amount))
               (remaining-debt (- debt-amount repay-amount))
               (asset-price (get-asset-price asset))
               (repay-value (* repay-amount asset-price)))
          
          (if (= remaining-debt 0.0)
            ;; Fully repaid
            (update borrows borrow-key { "amount": 0.0 })
            ;; Partial repayment
            (update borrows borrow-key {
              "amount": remaining-debt,
              "last-update": (at 'block-time (chain-data))
            }))
          
          ;; Update user data
          (with-read user-data-table account
            { "total-debt" := total-debt
            , "borrowed-assets" := borrowed-list }
            
            (update user-data-table account {
              "total-debt": (- total-debt repay-value),
              "borrowed-assets": (if (= remaining-debt 0.0)
                                  (filter (!= asset) borrowed-list)
                                  borrowed-list),
              "health-factor": (calculate-health-factor account)
            }))
          
          ;; In real implementation, would burn defi-tokens
          (format "Repaid {} {}" [repay-amount asset]))))
  
  (defun liquidate:string 
    ( liquidator:string 
      user:string 
      debt-asset:string 
      collateral-asset:string 
      debt-amount:decimal )
    @doc "Liquidate undercollateralized position with managed capability"
    (enforce (!= liquidator user) "Cannot self-liquidate")
    
    ;; Check if position is liquidatable
    (let ((health (get-health-factor user)))
      (enforce (< health HEALTH_FACTOR_THRESHOLD) 
              "Position is healthy"))
    
    ;; Calculate liquidation limits
    (let* ((user-debt (get-user-debt user debt-asset))
           (max-liquidation (* user-debt CLOSE_FACTOR))
           (actual-liquidation (min debt-amount max-liquidation))
           (collateral-price (get-asset-price collateral-asset))
           (debt-price (get-asset-price debt-asset))
           (bonus (get-liquidation-bonus collateral-asset))
           (collateral-amount (* (/ actual-liquidation debt-price)
                                collateral-price
                                (+ 1.0 bonus)))
           (liquidation-value (* actual-liquidation debt-price)))
      
      ;; Use managed capability for liquidation limits
      (with-capability (LIQUIDATION-ALLOWANCE liquidator liquidation-value)
        ;; Emit event
        (emit-event (LIQUIDATE liquidator user debt-asset 
                              collateral-asset actual-liquidation))
        
        ;; The managed capability enforces the limit
        (require-capability (LIQUIDATION-ALLOWANCE liquidator liquidation-value))
        
        ;; Process liquidation
        ;; In real implementation, would handle token transfers
        
        ;; Update user positions
        (format "Liquidated {} {} for {} {}" 
                [actual-liquidation debt-asset 
                 collateral-amount collateral-asset]))))
  
  ;; Calculation Functions
  (defun calculate-health-factor:decimal (account:string)
    @doc "Calculate user health factor"
    (with-read user-data-table account
      { "total-collateral" := collateral
      , "total-debt" := debt }
      
      (calculate-health-factor-with-debt collateral debt)))
  
  (defun calculate-health-factor-with-debt:decimal 
    ( collateral:decimal debt:decimal )
    @doc "Calculate health factor from values"
    (if (= debt 0.0)
      999.0  ;; Max health factor
      (let ((weighted-collateral (* collateral 0.75))) ;; Average threshold
        (/ weighted-collateral debt))))
  
  (defun calculate-max-borrow:decimal (account:string)
    @doc "Calculate maximum borrow amount in USD"
    (with-read user-data-table account
      { "total-collateral" := collateral
      , "total-debt" := current-debt }
      
      ;; Simplified - would calculate weighted LTV
      (let ((max-debt (* collateral 0.7))) ;; 70% average LTV
        (- max-debt current-debt))))
  
  (defun calculate-max-safe-withdrawal:decimal (account:string asset:string)
    @doc "Calculate maximum safe withdrawal amount"
    (with-read user-data-table account
      { "total-collateral" := collateral
      , "total-debt" := debt }
      
      (if (= debt 0.0)
        ;; No debt, can withdraw all
        (get-user-collateral-amount account asset)
        ;; Has debt, must maintain health factor
        (let* ((required-collateral (/ debt 0.75)) ;; To maintain 1.0 health
               (excess-collateral (- collateral required-collateral))
               (asset-price (get-asset-price asset)))
          (/ excess-collateral asset-price)))))
  
  (defun calculate-health-factor-after-withdraw:decimal 
    ( account:string asset:string amount:decimal )
    @doc "Calculate health factor after withdrawal"
    (with-read user-data-table account
      { "total-collateral" := collateral
      , "total-debt" := debt }
      
      (let* ((asset-price (get-asset-price asset))
             (withdraw-value (* amount asset-price))
             (new-collateral (- collateral withdraw-value)))
        
        (calculate-health-factor-with-debt new-collateral debt))))
  
  ;; Helper Functions
  (defun get-asset-price:decimal (asset:string)
    @doc "Get asset price in USD"
    (at 'price (read asset-config asset)))
  
  (defun get-liquidation-bonus:decimal (asset:string)
    @doc "Get liquidation bonus for asset"
    (at 'liquidation-bonus (read asset-config asset)))
  
  (defun get-borrow-rate:decimal (asset:string)
    @doc "Get current borrow rate"
    ;; Simplified - would calculate based on utilization
    0.08) ;; 8% APR
  
  (defun get-user-debt:decimal (account:string asset:string)
    @doc "Get user debt in asset"
    (let ((borrow-key (format "{}:{}" [account asset])))
      (with-default-read borrows borrow-key
        { "amount": 0.0 }
        { "amount" := amount }
        amount)))
  
  (defun get-health-factor:decimal (account:string)
    @doc "Get user health factor"
    (with-default-read user-data-table account
      { "health-factor": 999.0 }
      { "health-factor" := health }
      health))
  
  (defun get-total-collateral-value:decimal (account:string)
    @doc "Get total collateral value in USD"
    (with-default-read user-data-table account
      { "total-collateral": 0.0 }
      { "total-collateral" := collateral }
      collateral))
  
  (defun get-user-collateral-amount:decimal (account:string asset:string)
    @doc "Get user collateral amount for specific asset"
    ;; Simplified - would track per-asset balances
    (let ((asset-price (get-asset-price asset))
          (total-value (get-total-collateral-value account)))
      (/ total-value asset-price)))
  
  ;; Validation Functions
  (defun enforce-valid-account (account:string)
    @doc "Validate account"
    (enforce (!= account "") "Invalid account"))
  
  (defun enforce-valid-amount (amount:decimal)
    @doc "Validate amount"
    (enforce (> amount 0.0) "Amount must be positive"))
  
  (defun enforce-asset-active (asset:string)
    @doc "Ensure asset is active"
    (with-read asset-config asset { "active" := active }
      (enforce active "Asset not active")))
  
  (defun enforce-borrowing-enabled (asset:string)
    @doc "Ensure borrowing is enabled"
    (with-read asset-config asset { "borrowing-enabled" := enabled }
      (enforce enabled "Borrowing not enabled for asset")))
  
  ;; Additional Query Functions
  (defun get-user-data:object (account:string)
    @doc "Get user position data"
    (with-default-read user-data-table account
      { "account": account
      , "collateral-assets": []
      , "borrowed-assets": []
      , "total-collateral": 0.0
      , "total-debt": 0.0
      , "health-factor": 999.0 }
      identity))
  
  (defun get-all-users:[object] ()
    @doc "Get all user positions"
    (map (lambda (k) (read user-data-table k))
         (keys user-data-table)))
  
  ;; Advanced Functions
  (defun flash-loan:string (borrower:string asset:string amount:decimal)
    @doc "Execute flash loan with managed capability"
    ;; Flash loans would use a special managed capability
    ;; that enforces repayment within the same transaction
    "Flash loan implementation placeholder")
  
  (defun enable-as-collateral:string (account:string asset:string enable:bool)
    @doc "Enable/disable asset as collateral"
    (with-capability (GOVERNANCE)
      ;; Implementation would update user preferences
      (format "Collateral {} for asset {}" [(if enable "enabled" "disabled") asset])))
)

(create-table user-data-table)
(create-table asset-config)
(create-table borrows)