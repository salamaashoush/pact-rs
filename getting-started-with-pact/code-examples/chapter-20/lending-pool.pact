;; lending-pool.pact
;; Core lending pool implementation
(module lending-pool GOVERNANCE
  @doc "Core lending and borrowing logic"
  
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
  
  ;; Capabilities
  (defcap DEPOSIT (account:string asset:string amount:decimal)
    @doc "Deposit capability"
    @event
    (enforce-valid-account account)
    (enforce-valid-amount amount)
    (enforce-asset-active asset))
  
  (defcap BORROW (account:string asset:string amount:decimal)
    @doc "Borrow capability"
    @event
    (enforce-valid-account account)
    (enforce-valid-amount amount)
    (enforce-borrowing-enabled asset)
    (enforce-healthy-position account amount asset))
  
  (defcap WITHDRAW (account:string asset:string amount:decimal)
    @doc "Withdraw capability"
    @event
    (enforce-valid-account account)
    (enforce-valid-amount amount)
    (let ((health (calculate-health-factor-after-withdraw 
                   account asset amount)))
      (enforce (>= health HEALTH_FACTOR_THRESHOLD) 
              "Withdrawal would make position unhealthy")))
  
  (defcap REPAY (account:string asset:string amount:decimal)
    @doc "Repay capability"
    @event
    (enforce-valid-account account)
    (enforce-valid-amount amount))
  
  (defcap LIQUIDATE (liquidator:string user:string 
                     debt-asset:string collateral-asset:string)
    @doc "Liquidation capability"
    @event
    (enforce (!= liquidator user) "Cannot self-liquidate")
    (let ((health (get-health-factor user)))
      (enforce (< health HEALTH_FACTOR_THRESHOLD) 
              "Position is healthy")))
  
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
  
  ;; Core Functions
  (defun deposit:string (account:string asset:string amount:decimal)
    @doc "Deposit collateral"
    (with-capability (DEPOSIT account asset amount)
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
    @doc "Borrow against collateral"
    (with-capability (BORROW account asset amount)
      (with-read user-data-table account
        { "total-collateral" := collateral
        , "total-debt" := current-debt
        , "borrowed-assets" := borrowed-list }
        
        (let* ((asset-price (get-asset-price asset))
               (borrow-value (* amount asset-price))
               (new-debt (+ current-debt borrow-value))
               (max-borrow (calculate-max-borrow account))
               (borrow-key (format "{}:{}" [account asset])))
          
          (enforce (<= new-debt max-borrow) 
                  "Insufficient collateral")
          
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
          (format "Borrowed {} {}" [amount asset]))))
  
  (defun repay:string (account:string asset:string amount:decimal)
    @doc "Repay borrowed amount"
    (with-capability (REPAY account asset amount)
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
            (format "Repaid {} {}" [repay-amount asset])))))
  
  (defun liquidate:string 
    ( liquidator:string 
      user:string 
      debt-asset:string 
      collateral-asset:string 
      debt-amount:decimal )
    @doc "Liquidate undercollateralized position"
    (with-capability (LIQUIDATE liquidator user debt-asset collateral-asset)
      (let* ((user-debt (get-user-debt user debt-asset))
             (max-liquidation (* user-debt CLOSE_FACTOR))
             (actual-liquidation (min debt-amount max-liquidation))
             (collateral-price (get-asset-price collateral-asset))
             (debt-price (get-asset-price debt-asset))
             (bonus (get-liquidation-bonus collateral-asset))
             (collateral-amount (* (/ actual-liquidation debt-price)
                                  collateral-price
                                  (+ 1.0 bonus))))
        
        ;; Liquidator repays debt
        ;; In real implementation, would handle token transfers
        
        ;; Transfer collateral to liquidator with bonus
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
      { "total-collateral" := collateral }
      
      ;; Simplified - would calculate weighted LTV
      (* collateral 0.7))) ;; 70% average LTV
  
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
  
  (defun enforce-healthy-position 
    ( account:string borrow-amount:decimal asset:string )
    @doc "Ensure position stays healthy after borrow"
    (let* ((price (get-asset-price asset))
           (borrow-value (* borrow-amount price))
           (user-info (read user-data-table account))
           (new-debt (+ (at 'total-debt user-info) borrow-value))
           (collateral (at 'total-collateral user-info))
           (new-health (calculate-health-factor-with-debt collateral new-debt)))
      
      (enforce (>= new-health HEALTH_FACTOR_THRESHOLD) 
              "Borrow would make position unhealthy")))
  
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
  
  (defun update-user-position:string (account:string)
    @doc "Recalculate user position"
    (with-read user-data-table account
      { "total-collateral" := collateral
      , "total-debt" := debt }
      
      (update user-data-table account {
        "health-factor": (calculate-health-factor-with-debt collateral debt)
      })
      "Position updated"))
  
  (defun repay-on-behalf:string 
    ( payer:string borrower:string asset:string amount:decimal )
    @doc "Allow repayment on behalf of another user"
    (with-capability (REPAY payer asset amount)
      ;; Process repayment for borrower
      (repay borrower asset amount)))
)

(create-table user-data-table)
(create-table asset-config)
(create-table borrows)