;; liquidation.pact
;; Liquidation engine for the lending protocol
(module liquidation GOVERNANCE
  @doc "Handles liquidation of undercollateralized positions"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Schemas
  (defschema liquidation-event
    @doc "Liquidation history"
    liquidation-id:string
    timestamp:time
    liquidator:string
    user:string
    collateral-asset:string
    collateral-amount:decimal
    debt-asset:string
    debt-repaid:decimal
    liquidation-bonus:decimal)
  
  ;; Tables
  (deftable liquidations:{liquidation-event})
  
  ;; Constants
  (defconst MAX_LIQUIDATION_DISCOUNT 0.5)  ;; Max 50% at once
  (defconst LIQUIDATION_INCENTIVE 1.05)    ;; 5% bonus
  
  ;; Capabilities
  (defcap LIQUIDATE:bool 
    ( liquidator:string 
      user:string 
      collateral-asset:string 
      debt-asset:string 
      amount:decimal )
    @doc "Capability to perform liquidation"
    @managed amount LIQUIDATE-mgr
    (enforce (!= liquidator user) "Cannot self-liquidate")
    (enforce-liquidatable user)
    (enforce (> amount 0.0) "Amount must be positive"))
  
  (defun LIQUIDATE-mgr:decimal (managed:decimal requested:decimal)
    @doc "Liquidation amount manager"
    (enforce (>= managed requested) "Liquidation exceeds allowance")
    (- managed requested))
  
  ;; Main Liquidation Function
  (defun liquidate-position:object 
    ( liquidator:string
      user:string
      collateral-asset:string
      debt-asset:string
      debt-to-cover:decimal )
    @doc "Execute liquidation"
    (with-capability (LIQUIDATE liquidator user collateral-asset 
                               debt-asset debt-to-cover)
      ;; Get user's position
      (let* ((user-data (lending-pool.get-user-data user))
             (health-factor (at 'health-factor user-data))
             (user-debt (lending-pool.get-user-debt user debt-asset))
             (max-liquidation (* user-debt MAX_LIQUIDATION_DISCOUNT))
             (actual-debt-to-cover (min debt-to-cover max-liquidation)))
        
        ;; Verify liquidation conditions
        (enforce (< health-factor 1.0) "User position is healthy")
        (enforce (> user-debt 0.0) "User has no debt in this asset")
        
        ;; Calculate collateral to liquidate
        (let* ((collateral-price (lending-pool.get-asset-price collateral-asset))
               (debt-price (lending-pool.get-asset-price debt-asset))
               (base-collateral (/ (* actual-debt-to-cover debt-price) 
                                  collateral-price))
               (bonus (lending-pool.get-liquidation-bonus collateral-asset))
               (collateral-with-bonus (* base-collateral (+ 1.0 bonus)))
               (user-collateral (get-user-collateral-balance user collateral-asset))
               (collateral-to-liquidate (min collateral-with-bonus user-collateral)))
          
          ;; Execute liquidation
          (execute-liquidation 
            liquidator user collateral-asset debt-asset 
            collateral-to-liquidate actual-debt-to-cover)
          
          ;; Record liquidation event
          (let ((liquidation-id (generate-liquidation-id)))
            (insert liquidations liquidation-id {
              "liquidation-id": liquidation-id,
              "timestamp": (at 'block-time (chain-data)),
              "liquidator": liquidator,
              "user": user,
              "collateral-asset": collateral-asset,
              "collateral-amount": collateral-to-liquidate,
              "debt-asset": debt-asset,
              "debt-repaid": actual-debt-to-cover,
              "liquidation-bonus": bonus
            }))
          
          ;; Return liquidation details
          { "liquidation-id": liquidation-id
          , "collateral-liquidated": collateral-to-liquidate
          , "debt-covered": actual-debt-to-cover
          , "health-factor-before": health-factor
          , "health-factor-after": (lending-pool.get-health-factor user)
          , "liquidation-bonus": (* base-collateral bonus) }))))
  
  ;; Liquidation Execution
  (defun execute-liquidation:string 
    ( liquidator:string
      user:string
      collateral-asset:string
      debt-asset:string
      collateral-amount:decimal
      debt-amount:decimal )
    @doc "Execute the actual liquidation transfers"
    ;; Step 1: Liquidator repays user's debt
    ;; In real implementation, would transfer debt tokens from liquidator
    (lending-pool.repay-on-behalf liquidator user debt-asset debt-amount)
    
    ;; Step 2: Transfer collateral from user to liquidator
    ;; In real implementation, would transfer aTokens
    (transfer-collateral user liquidator collateral-asset collateral-amount)
    
    ;; Step 3: Update user's position
    (lending-pool.update-user-position user)
    
    "Liquidation executed")
  
  ;; Batch Liquidation
  (defun batch-liquidate:[object] (liquidations:[object])
    @doc "Execute multiple liquidations"
    (map (lambda (liq:object)
           (liquidate-position 
             (at 'liquidator liq)
             (at 'user liq)
             (at 'collateral-asset liq)
             (at 'debt-asset liq)
             (at 'amount liq)))
         liquidations))
  
  ;; Helper Functions
  (defun calculate-profitability:object 
    ( collateral-asset:string
      debt-asset:string
      debt-amount:decimal )
    @doc "Calculate liquidation profitability"
    (let* ((collateral-price (lending-pool.get-asset-price collateral-asset))
           (debt-price (lending-pool.get-asset-price debt-asset))
           (debt-value (* debt-amount debt-price))
           (bonus (lending-pool.get-liquidation-bonus collateral-asset))
           (collateral-received (/ debt-value collateral-price))
           (collateral-value-with-bonus (* collateral-received 
                                          (+ 1.0 bonus)))
           (profit (- (* collateral-value-with-bonus collateral-price)
                     debt-value))
           (profit-percentage (/ profit debt-value)))
      
      { "debt-value": debt-value
      , "collateral-received": collateral-received
      , "collateral-value": (* collateral-value-with-bonus collateral-price)
      , "profit": profit
      , "profit-percentage": profit-percentage
      , "profitable": (> profit 0.0) }))
  
  (defun find-liquidation-opportunities:[object] ()
    @doc "Find all liquidatable positions"
    (let ((all-users (lending-pool.get-all-users)))
      (filter (lambda (user-data:object)
                (< (at 'health-factor user-data) 1.0))
              all-users)))
  
  (defun get-user-collateral-balance:decimal (user:string asset:string)
    @doc "Get user's collateral balance in asset"
    ;; In real implementation, would read from aToken balances
    (defi-token.get-balance user))
  
  (defun transfer-collateral:string 
    ( from:string to:string asset:string amount:decimal )
    @doc "Transfer collateral between users"
    ;; In real implementation, would transfer aTokens
    (defi-token.transfer from to amount))
  
  (defun generate-liquidation-id:string ()
    @doc "Generate unique liquidation ID"
    (format "LIQ-{}-{}" 
            [(at 'block-time (chain-data))
             (at 'block-height (chain-data))]))
  
  (defun enforce-liquidatable (user:string)
    @doc "Ensure user can be liquidated"
    (let ((health (lending-pool.get-health-factor user)))
      (enforce (< health 1.0) "Position is healthy - cannot liquidate")))
  
  ;; Query Functions
  (defun get-liquidation-history:[object] (user:string)
    @doc "Get liquidation history for user"
    (select liquidations (where 'user (= user))))
  
  (defun get-recent-liquidations:[object] (hours:integer)
    @doc "Get recent liquidations"
    (let ((cutoff-time (add-time (at 'block-time (chain-data)) 
                                (- (* hours 3600)))))
      (select liquidations (where 'timestamp (> cutoff-time)))))
  
  (defun get-liquidator-stats:object (liquidator:string)
    @doc "Get liquidator statistics"
    (let ((liquidations (select liquidations 
                               (where 'liquidator (= liquidator)))))
      { "total-liquidations": (length liquidations)
      , "total-debt-covered": (fold (+) 0.0 
                                   (map (at 'debt-repaid) liquidations))
      , "total-collateral-received": (fold (+) 0.0 
                                          (map (at 'collateral-amount) 
                                               liquidations))
      , "recent-liquidations": (take 10 liquidations) }))
)

(create-table liquidations)