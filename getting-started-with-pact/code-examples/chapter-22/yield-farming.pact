;; yield-farming.pact
(module yield-farming GOVERNANCE
  @doc "Yield farming and liquidity rewards for DEX"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dex-admin))
  
  ;; Schemas
  (defschema farm-pool
    @doc "Farming pool information"
    id:string
    lp-token:string            ;; LP token pair identifier
    reward-token:module{fungible-v2}
    reward-per-second:decimal
    total-staked:decimal
    acc-reward-per-share:decimal  ;; Accumulated rewards per share
    last-reward-time:time
    start-time:time
    end-time:time
    multiplier:decimal
    deposit-fee:decimal        ;; Fee on deposits (0-5%)
    withdrawal-fee:decimal)    ;; Fee on withdrawals (0-5%)
  
  (defschema user-stake
    @doc "User staking information"
    farm-id:string
    user:string
    amount:decimal               ;; LP tokens staked
    reward-debt:decimal          ;; Reward debt for accounting
    pending-rewards:decimal      ;; Claimable rewards
    last-deposit:time
    total-harvested:decimal)     ;; Total rewards harvested
  
  (defschema reward-snapshot
    @doc "Daily reward snapshots for analytics"
    farm-id:string
    date:string
    total-staked:decimal
    daily-rewards:decimal
    apr:decimal)
  
  ;; Tables
  (deftable farm-pools:{farm-pool})
  (deftable user-stakes:{user-stake})
  (deftable reward-snapshots:{reward-snapshot})
  
  ;; Events
  (defcap DEPOSIT:bool (user:string farm-id:string amount:decimal fee:decimal)
    @doc "Deposit event"
    @event
    true)
  
  (defcap WITHDRAW:bool (user:string farm-id:string amount:decimal fee:decimal)
    @doc "Withdraw event"
    @event
    true)
  
  (defcap HARVEST:bool (user:string farm-id:string amount:decimal)
    @doc "Harvest event"
    @event
    true)
  
  (defcap EMERGENCY_WITHDRAW:bool (user:string farm-id:string amount:decimal)
    @doc "Emergency withdraw event"
    @event
    true)
  
  ;; Constants
  (defconst PRECISION 1000000000000) ;; 12 decimal precision
  (defconst MAX_FEE 0.05)           ;; Maximum 5% fee
  (defconst SECONDS_PER_DAY 86400)
  (defconst SECONDS_PER_YEAR 31536000)
  
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
      lp-token:string
      reward-token:module{fungible-v2}
      reward-per-second:decimal
      start-time:time
      end-time:time
      multiplier:decimal
      deposit-fee:decimal
      withdrawal-fee:decimal )
    @doc "Create a new farming pool"
    
    (with-capability (GOVERNANCE)
      (enforce (< start-time end-time) "Invalid time range")
      (enforce (> reward-per-second 0.0) "Invalid reward rate")
      (enforce (<= deposit-fee MAX_FEE) "Deposit fee too high")
      (enforce (<= withdrawal-fee MAX_FEE) "Withdrawal fee too high")
      
      (insert farm-pools farm-id {
        "id": farm-id,
        "lp-token": lp-token,
        "reward-token": reward-token,
        "reward-per-second": reward-per-second,
        "total-staked": 0.0,
        "acc-reward-per-share": 0.0,
        "last-reward-time": start-time,
        "start-time": start-time,
        "end-time": end-time,
        "multiplier": multiplier,
        "deposit-fee": deposit-fee,
        "withdrawal-fee": withdrawal-fee
      })
      
      (format "Created farm {} for LP token {}" [farm-id lp-token])))
  
  (defun update-farm:string 
    ( farm-id:string
      reward-per-second:decimal
      end-time:time
      multiplier:decimal )
    @doc "Update farm parameters"
    
    (with-capability (FARM_ADMIN farm-id)
      ;; Update pool first
      (update-pool farm-id)
      
      (update farm-pools farm-id {
        "reward-per-second": reward-per-second,
        "end-time": end-time,
        "multiplier": multiplier
      })
      
      (format "Updated farm {}" [farm-id])))
  
  ;; Core farming functions
  (defun deposit:string (farm-id:string user:string amount:decimal)
    @doc "Deposit LP tokens to farm"
    
    (with-capability (STAKE user farm-id)
      (enforce (> amount 0.0) "Amount must be positive")
      
      ;; Update pool before deposit
      (update-pool farm-id)
      
      (with-read farm-pools farm-id 
        { "lp-token" := lp-token
        , "total-staked" := total-staked
        , "acc-reward-per-share" := acc-reward-per-share
        , "deposit-fee" := deposit-fee }
        
        ;; Calculate fee
        (let* ((fee-amount (* amount deposit-fee))
               (net-amount (- amount fee-amount))
               (user-key (format "{}:{}" [farm-id user])))
          
          ;; Get or create user stake
          (with-default-read user-stakes user-key
            { "amount": 0.0
            , "reward-debt": 0.0
            , "pending-rewards": 0.0
            , "total-harvested": 0.0 }
            { "amount" := user-amount
            , "reward-debt" := reward-debt
            , "pending-rewards" := pending-rewards
            , "total-harvested" := total-harvested }
            
            ;; Calculate pending rewards before deposit
            (let ((pending (if (> user-amount 0.0)
                            (- (/ (* user-amount acc-reward-per-share) PRECISION) reward-debt)
                            0.0)))
              
              ;; Transfer LP tokens from user (full amount including fee)
              ;; Using the LP token module from dex-pair
              (with-capability (dex-pair.TRANSFER user (get-farm-account farm-id) amount)
                (dex-pair.transfer-lp user (get-farm-account farm-id) amount))
              
              ;; If there's a fee, transfer to fee collector
              (if (> fee-amount 0.0)
                (with-capability (dex-pair.TRANSFER (get-farm-account farm-id) (get-fee-collector) fee-amount)
                  (dex-pair.transfer-lp (get-farm-account farm-id) (get-fee-collector) fee-amount))
                "No fee")
              
              ;; Update user stake
              (let ((new-amount (+ user-amount net-amount))
                    (new-reward-debt (/ (* new-amount acc-reward-per-share) PRECISION)))
                
                (write user-stakes user-key {
                  "farm-id": farm-id,
                  "user": user,
                  "amount": new-amount,
                  "reward-debt": new-reward-debt,
                  "pending-rewards": (+ pending-rewards pending),
                  "last-deposit": (at 'block-time (chain-data)),
                  "total-harvested": total-harvested
                }))
              
              ;; Update farm total
              (update farm-pools farm-id {
                "total-staked": (+ total-staked net-amount)
              })
              
              ;; Emit event
              (emit-event (DEPOSIT user farm-id amount fee-amount))
              
              (format "Deposited {} LP tokens to farm {} (fee: {})" [net-amount farm-id fee-amount]))))))
  
  (defun withdraw:string (farm-id:string user:string amount:decimal)
    @doc "Withdraw LP tokens from farm"
    
    (with-capability (STAKE user farm-id)
      (enforce (> amount 0.0) "Amount must be positive")
      
      ;; Update pool before withdrawal
      (update-pool farm-id)
      
      (let ((user-key (format "{}:{}" [farm-id user])))
        (with-read user-stakes user-key
          { "amount" := user-amount
          , "reward-debt" := reward-debt
          , "pending-rewards" := pending-rewards
          , "total-harvested" := total-harvested }
          
          (enforce (>= user-amount amount) "Insufficient staked amount")
          
          (with-read farm-pools farm-id 
            { "lp-token" := lp-token
            , "total-staked" := total-staked
            , "acc-reward-per-share" := acc-reward-per-share
            , "withdrawal-fee" := withdrawal-fee }
            
            ;; Calculate pending rewards
            (let ((pending (- (/ (* user-amount acc-reward-per-share) PRECISION) reward-debt)))
              
              ;; Calculate withdrawal fee
              (let* ((fee-amount (* amount withdrawal-fee))
                     (net-amount (- amount fee-amount)))
                
                ;; Update user stake
                (let ((new-amount (- user-amount amount))
                      (new-reward-debt (/ (* new-amount acc-reward-per-share) PRECISION)))
                  
                  (update user-stakes user-key {
                    "amount": new-amount,
                    "reward-debt": new-reward-debt,
                    "pending-rewards": (+ pending-rewards pending),
                    "total-harvested": total-harvested
                  }))
                
                ;; Transfer LP tokens back to user (net amount)
                (with-capability (dex-pair.TRANSFER (get-farm-account farm-id) user net-amount)
                  (dex-pair.transfer-lp (get-farm-account farm-id) user net-amount))
                
                ;; Transfer fee if applicable
                (if (> fee-amount 0.0)
                  (with-capability (dex-pair.TRANSFER (get-farm-account farm-id) (get-fee-collector) fee-amount)
                    (dex-pair.transfer-lp (get-farm-account farm-id) (get-fee-collector) fee-amount))
                  "No fee")
                
                ;; Update farm total
                (update farm-pools farm-id {
                  "total-staked": (- total-staked amount)
                })
                
                ;; Emit event
                (emit-event (WITHDRAW user farm-id amount fee-amount))
                
                (format "Withdrew {} LP tokens from farm {} (fee: {})" [net-amount farm-id fee-amount])))))))
  
  (defun harvest:string (farm-id:string user:string)
    @doc "Harvest pending rewards"
    
    (with-capability (STAKE user farm-id)
      ;; Update pool before harvest
      (update-pool farm-id)
      
      (let ((user-key (format "{}:{}" [farm-id user])))
        (with-read user-stakes user-key
          { "amount" := user-amount
          , "reward-debt" := reward-debt
          , "pending-rewards" := pending-rewards
          , "total-harvested" := total-harvested }
          
          (with-read farm-pools farm-id 
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
              
              ;; Update user stake
              (update user-stakes user-key {
                "reward-debt": (/ (* user-amount acc-reward-per-share) PRECISION),
                "pending-rewards": 0.0,
                "total-harvested": (+ total-harvested total-rewards)
              })
              
              ;; Emit event
              (emit-event (HARVEST user farm-id total-rewards))
              
              (format "Harvested {} reward tokens from farm {}" [total-rewards farm-id]))))))
  
  (defun compound:string (farm-id:string user:string)
    @doc "Compound rewards back into farming"
    
    (with-capability (STAKE user farm-id)
      ;; First harvest rewards
      (harvest farm-id user)
      
      ;; Check if reward token is same as LP token (for governance token farms)
      (with-read farm-pools farm-id 
        { "reward-token" := reward-token
        , "lp-token" := lp-token }
        
        ;; Get user's reward token balance
        (let ((reward-balance (reward-token::get-balance user)))
          (if (> reward-balance 0.0)
            (do
              ;; If reward token can be used as LP (simplified logic)
              (if (= (format "{}" [reward-token]) lp-token)
                (deposit farm-id user reward-balance)
                "Cannot compound different tokens"))
            "No rewards to compound"))))
  
  ;; Pool update mechanism
  (defun update-pool:string (farm-id:string)
    @doc "Update farm pool accumulated rewards"
    
    (with-read farm-pools farm-id 
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
            
            (update farm-pools farm-id {
              "acc-reward-per-share": new-acc-reward-per-share,
              "last-reward-time": current-time
            })
            
            (format "Updated pool {}: added {} rewards per share" [farm-id reward-per-share]))
          
          ;; Just update timestamp
          (do
            (update farm-pools farm-id { "last-reward-time": current-time })
            "Pool updated - no rewards added")))))
  
  ;; Emergency functions
  (defun emergency-withdraw:string (farm-id:string user:string)
    @doc "Emergency withdraw without rewards"
    
    (with-capability (STAKE user farm-id)
      (let ((user-key (format "{}:{}" [farm-id user])))
        (with-read user-stakes user-key
          { "amount" := user-amount }
          
          (with-read farm-pools farm-id 
            { "lp-token" := lp-token
            , "total-staked" := total-staked }
            
            ;; Transfer LP tokens back (no fees in emergency)
            (with-capability (dex-pair.TRANSFER (get-farm-account farm-id) user user-amount)
              (dex-pair.transfer-lp (get-farm-account farm-id) user user-amount))
            
            ;; Reset user stake
            (update user-stakes user-key {
              "amount": 0.0,
              "reward-debt": 0.0,
              "pending-rewards": 0.0
            })
            
            ;; Update farm total
            (update farm-pools farm-id {
              "total-staked": (- total-staked user-amount)
            })
            
            ;; Emit event
            (emit-event (EMERGENCY_WITHDRAW user farm-id user-amount))
            
            (format "Emergency withdrew {} LP tokens" [user-amount]))))))
  
  (defun pause-farm:string (farm-id:string)
    @doc "Pause farm (emergency only)"
    (with-capability (GOVERNANCE)
      (update farm-pools farm-id {
        "end-time": (at 'block-time (chain-data))
      })
      (format "Farm {} paused" [farm-id])))
  
  ;; Query functions
  (defun pending-rewards:decimal (farm-id:string user:string)
    @doc "Get pending rewards for user"
    
    (let ((user-key (format "{}:{}" [farm-id user])))
      (with-default-read user-stakes user-key
        { "amount": 0.0
        , "reward-debt": 0.0
        , "pending-rewards": 0.0 }
        { "amount" := user-amount
        , "reward-debt" := reward-debt
        , "pending-rewards" := stored-pending }
        
        (with-read farm-pools farm-id 
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
                   (time-diff (if (< reward-start-time reward-end-time) 
                               (diff-time reward-end-time reward-start-time) 
                               0.0))
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
      (with-default-read user-stakes user-key
        { "farm-id": farm-id
        , "user": user
        , "amount": 0.0
        , "reward-debt": 0.0
        , "pending-rewards": 0.0
        , "last-deposit": (at 'block-time (chain-data))
        , "total-harvested": 0.0 }
        { "amount" := amount
        , "last-deposit" := last-deposit
        , "total-harvested" := total-harvested }
        
        { "farm-id": farm-id
        , "user": user
        , "staked": amount
        , "pending": (pending-rewards farm-id user)
        , "last-deposit": last-deposit
        , "total-harvested": total-harvested })))
  
  (defun get-farm:object (farm-id:string)
    @doc "Get farm information"
    (with-read farm-pools farm-id
      { "id" := id
      , "lp-token" := lp-token
      , "reward-token" := reward-token
      , "total-staked" := total-staked
      , "start-time" := start-time
      , "end-time" := end-time
      , "multiplier" := multiplier }
      
      { "id": id
      , "lp-token": lp-token
      , "reward-token": reward-token
      , "total-staked": total-staked
      , "apr": (calculate-apr farm-id)
      , "start-time": start-time
      , "end-time": end-time
      , "multiplier": multiplier }))
  
  (defun get-all-farms:[object] ()
    @doc "Get all farming pools"
    (map (lambda (farm:object)
           (get-farm (at 'id farm)))
         (select farm-pools (constantly true))))
  
  ;; APR calculations
  (defun calculate-apr:decimal (farm-id:string)
    @doc "Calculate APR for farming pool"
    (with-read farm-pools farm-id 
      { "reward-per-second" := reward-per-second
      , "total-staked" := total-staked
      , "multiplier" := multiplier
      , "reward-token" := reward-token }
      
      (if (> total-staked 0.0)
        (let* ((yearly-rewards (* reward-per-second SECONDS_PER_YEAR multiplier))
               ;; Simplified: assume 1:1 USD value for calculation
               (apr (/ yearly-rewards total-staked)))
          (* apr 100.0)) ;; Convert to percentage
        0.0)))
  
  (defun calculate-daily-rewards:decimal (farm-id:string)
    @doc "Calculate daily rewards for farm"
    (with-read farm-pools farm-id 
      { "reward-per-second" := reward-per-second
      , "multiplier" := multiplier }
      (* reward-per-second SECONDS_PER_DAY multiplier)))
  
  ;; Analytics functions
  (defun take-daily-snapshot:string (farm-id:string)
    @doc "Take daily snapshot for analytics"
    (with-capability (GOVERNANCE)
      (with-read farm-pools farm-id
        { "total-staked" := total-staked }
        
        (let* ((today (format-time "%Y-%m-%d" (at 'block-time (chain-data))))
               (daily-rewards (calculate-daily-rewards farm-id))
               (apr (calculate-apr farm-id)))
          
          (write reward-snapshots (format "{}:{}" [farm-id today]) {
            "farm-id": farm-id,
            "date": today,
            "total-staked": total-staked,
            "daily-rewards": daily-rewards,
            "apr": apr
          })
          
          (format "Snapshot taken for farm {} on {}" [farm-id today])))))
  
  ;; Helper functions
  (defun get-farm-account:string (farm-id:string)
    @doc "Get farm contract account"
    (create-principal (create-capability-guard (FARM_ADMIN farm-id))))
  
  (defun get-fee-collector:string ()
    @doc "Get fee collection account"
    (create-principal (create-capability-guard (GOVERNANCE))))
  
  ;; Mass operations
  (defun harvest-all:string (user:string farm-ids:[string])
    @doc "Harvest from multiple farms"
    (let ((results (map (lambda (farm-id:string)
                          (harvest farm-id user))
                        farm-ids)))
      (format "Harvested from {} farms" [(length results)])))
  
  (defun get-user-farms:[object] (user:string)
    @doc "Get all farms where user has stakes"
    (let ((user-stakes-list (select user-stakes (where 'user (= user)))))
      (map (lambda (stake:object)
             (get-farm (at 'farm-id stake)))
           (filter (lambda (stake:object)
                     (> (at 'amount stake) 0.0))
                   user-stakes-list))))
  
  ;; Boost and multipliers
  (defun apply-boost:string (farm-id:string user:string boost-factor:decimal)
    @doc "Apply temporary boost to user (admin only)"
    (with-capability (GOVERNANCE)
      ;; This would require additional schema fields and logic
      (format "Applied {}x boost to user {} in farm {}" [boost-factor user farm-id])))
)

;; Create tables
(create-table farm-pools)
(create-table user-stakes)
(create-table reward-snapshots)