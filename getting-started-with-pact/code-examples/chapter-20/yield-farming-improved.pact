;; yield-farming-improved.pact
;; Yield farming rewards with managed capabilities for stake limits
(module yield-farming GOVERNANCE
  @doc "Rewards distribution with managed stake limits"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Schemas
  (defschema farm-pool
    @doc "Farming pool configuration"
    pool-id:string
    staking-token:string      ;; Token to stake (e.g., aToken)
    reward-token:string       ;; Token given as reward
    reward-per-second:decimal
    total-staked:decimal
    acc-reward-per-share:decimal
    last-update:time
    start-time:time
    end-time:time
    max-stake-per-user:decimal ;; Maximum stake per user
    min-stake-amount:decimal)  ;; Minimum stake amount
  
  (defschema user-stake
    @doc "User staking position"
    account:string
    pool-id:string
    amount:decimal
    reward-debt:decimal
    pending-rewards:decimal
    stake-time:time
    last-claim:time)
  
  ;; Tables
  (deftable pools:{farm-pool})
  (deftable stakes:{user-stake})
  
  ;; Managed Capabilities
  (defcap STAKE-ALLOWANCE (account:string pool-id:string max-stake:decimal)
    @doc "Managed capability for stake limits"
    @managed max-stake STAKE-ALLOWANCE-mgr
    (enforce-valid-account account)
    (with-read pools pool-id 
      { "start-time" := start, "end-time" := end }
      (let ((now (at 'block-time (chain-data))))
        (enforce (>= now start) "Pool not started")
        (enforce (<= now end) "Pool ended"))))
  
  (defun STAKE-ALLOWANCE-mgr:decimal (available:decimal requested:decimal)
    @doc "Manage stake allowance"
    (let ((remaining (- available requested)))
      (enforce (>= remaining 0.0) 
        (format "Stake limit exceeded: available={}, requested={}" 
                [available requested]))
      remaining))
  
  (defcap UNSTAKE-ALLOWANCE (account:string pool-id:string max-unstake:decimal)
    @doc "Managed capability for unstake limits"
    @managed max-unstake UNSTAKE-ALLOWANCE-mgr
    (enforce-valid-account account)
    (let ((stake-key (format "{}:{}" [account pool-id])))
      (with-read stakes stake-key { "amount" := staked }
        (enforce (> staked 0.0) "No active stake"))))
  
  (defun UNSTAKE-ALLOWANCE-mgr:decimal (available:decimal requested:decimal)
    @doc "Manage unstake allowance"
    (let ((remaining (- available requested)))
      (enforce (>= remaining 0.0) "Unstake limit exceeded")
      remaining))
  
  (defcap REWARD-CLAIM (account:string pool-id:string max-rewards:decimal)
    @doc "Managed capability for reward claims"
    @managed max-rewards REWARD-CLAIM-mgr
    (enforce-valid-account account))
  
  (defun REWARD-CLAIM-mgr:decimal (available:decimal requested:decimal)
    @doc "Manage reward claims"
    (let ((remaining (- available requested)))
      (enforce (>= remaining 0.0) "Reward claim exceeds available")
      remaining))
  
  ;; Event Capabilities
  (defcap STAKE (account:string pool-id:string amount:decimal)
    @doc "Staking event"
    @event
    true)
  
  (defcap UNSTAKE (account:string pool-id:string amount:decimal)
    @doc "Unstaking event"
    @event
    true)
  
  (defcap CLAIM (account:string pool-id:string amount:decimal)
    @doc "Claim rewards event"
    @event
    true)
  
  (defcap EMERGENCY-WITHDRAW (account:string pool-id:string amount:decimal)
    @doc "Emergency withdrawal event"
    @event
    true)
  
  ;; Pool Management
  (defun create-pool:string 
    ( pool-id:string
      staking-token:string
      reward-token:string
      reward-per-second:decimal
      duration:integer
      max-stake-per-user:decimal
      min-stake-amount:decimal )
    @doc "Create new farming pool"
    (with-capability (GOVERNANCE)
      (enforce (> reward-per-second 0.0) "Invalid reward rate")
      (enforce (> max-stake-per-user 0.0) "Invalid max stake")
      (enforce (> min-stake-amount 0.0) "Invalid min stake")
      (enforce (<= min-stake-amount max-stake-per-user) 
              "Min stake exceeds max stake")
      
      (let ((now (at 'block-time (chain-data))))
        (insert pools pool-id {
          "pool-id": pool-id,
          "staking-token": staking-token,
          "reward-token": reward-token,
          "reward-per-second": reward-per-second,
          "total-staked": 0.0,
          "acc-reward-per-share": 0.0,
          "last-update": now,
          "start-time": now,
          "end-time": (add-time now duration),
          "max-stake-per-user": max-stake-per-user,
          "min-stake-amount": min-stake-amount
        })
        (format "Pool {} created" [pool-id]))))
  
  ;; Update pool rewards
  (defun update-pool:string (pool-id:string)
    @doc "Update pool reward variables"
    (with-read pools pool-id
      { "total-staked" := total-staked
      , "acc-reward-per-share" := acc-reward
      , "last-update" := last-update
      , "reward-per-second" := reward-rate
      , "end-time" := end-time }
      
      (let ((now (at 'block-time (chain-data))))
        (if (and (> total-staked 0.0) (> now last-update))
          (let* ((time-elapsed (min (diff-time now last-update)
                                   (diff-time end-time last-update)))
                 (rewards (* reward-rate (/ time-elapsed 1.0)))
                 (reward-per-share (/ rewards total-staked)))
            
            (update pools pool-id {
              "acc-reward-per-share": (+ acc-reward reward-per-share),
              "last-update": (min now end-time)
            })
            "Pool updated")
          "No update needed"))))
  
  ;; Staking Functions with Managed Capabilities
  (defun stake:string (account:string pool-id:string amount:decimal)
    @doc "Stake tokens in pool with managed limits"
    (enforce-valid-amount amount)
    
    ;; Get pool config
    (with-read pools pool-id
      { "max-stake-per-user" := max-stake
      , "min-stake-amount" := min-stake }
      
      (enforce (>= amount min-stake) 
              (format "Amount below minimum: {}" [min-stake]))
      
      ;; Get current stake
      (let* ((stake-key (format "{}:{}" [account pool-id]))
             (current-stake (with-default-read stakes stake-key
                             { "amount": 0.0 }
                             { "amount" := amt }
                             amt))
             (new-total (+ current-stake amount))
             (available-stake (- max-stake current-stake)))
        
        (enforce (<= new-total max-stake) 
                (format "Would exceed max stake: {}" [max-stake]))
        
        ;; Use managed capability for stake limits
        (with-capability (STAKE-ALLOWANCE account pool-id available-stake)
          ;; Emit event
          (emit-event (STAKE account pool-id amount))
          
          ;; Update pool
          (update-pool pool-id)
          
          (with-read pools pool-id
            { "total-staked" := total-staked
            , "acc-reward-per-share" := acc-reward }
            
            ;; The managed capability enforces the limit
            (require-capability (STAKE-ALLOWANCE account pool-id amount))
            
            ;; Calculate pending rewards for existing stake
            (with-default-read stakes stake-key
              { "amount": 0.0
              , "reward-debt": 0.0
              , "pending-rewards": 0.0 }
              { "amount" := current-amount
              , "reward-debt" := debt
              , "pending-rewards" := pending }
              
              (let ((earned (if (> current-amount 0.0)
                              (- (* current-amount acc-reward) debt)
                              0.0)))
                
                ;; Update stake
                (write stakes stake-key {
                  "account": account,
                  "pool-id": pool-id,
                  "amount": (+ current-amount amount),
                  "reward-debt": (* (+ current-amount amount) acc-reward),
                  "pending-rewards": (+ pending earned),
                  "stake-time": (if (= current-amount 0.0)
                                 (at 'block-time (chain-data))
                                 (at 'stake-time (read stakes stake-key))),
                  "last-claim": (at 'block-time (chain-data))
                })))
            
            ;; Update pool total
            (update pools pool-id {
              "total-staked": (+ total-staked amount)
            })
            
            ;; Transfer staking tokens (simplified)
            (format "Staked {} tokens" [amount]))))))
  
  (defun unstake:string (account:string pool-id:string amount:decimal)
    @doc "Unstake tokens from pool with managed limits"
    (enforce-valid-amount amount)
    
    (let ((stake-key (format "{}:{}" [account pool-id])))
      (with-read stakes stake-key
        { "amount" := staked-amount }
        
        (enforce (>= staked-amount amount) "Insufficient stake")
        
        ;; Use managed capability for unstake limits
        (with-capability (UNSTAKE-ALLOWANCE account pool-id staked-amount)
          ;; Emit event
          (emit-event (UNSTAKE account pool-id amount))
          
          ;; Update pool
          (update-pool pool-id)
          
          ;; The managed capability enforces the limit
          (require-capability (UNSTAKE-ALLOWANCE account pool-id amount))
          
          ;; Claim pending rewards first
          (claim-rewards account pool-id)
          
          (with-read pools pool-id
            { "total-staked" := total-staked
            , "acc-reward-per-share" := acc-reward }
            
            ;; Update stake
            (if (= amount staked-amount)
              ;; Full unstake
              (update stakes stake-key {
                "amount": 0.0,
                "reward-debt": 0.0
              })
              ;; Partial unstake
              (update stakes stake-key {
                "amount": (- staked-amount amount),
                "reward-debt": (* (- staked-amount amount) acc-reward)
              }))
            
            ;; Update pool total
            (update pools pool-id {
              "total-staked": (- total-staked amount)
            }))
          
          ;; Transfer tokens back (simplified)
          (format "Unstaked {} tokens" [amount]))))
  
  (defun claim-rewards:string (account:string pool-id:string)
    @doc "Claim accumulated rewards with managed limits"
    ;; Update pool
    (update-pool pool-id)
    
    (let ((stake-key (format "{}:{}" [account pool-id])))
      (with-read stakes stake-key
        { "amount" := staked-amount
        , "reward-debt" := debt
        , "pending-rewards" := pending }
        
        (with-read pools pool-id
          { "acc-reward-per-share" := acc-reward }
          
          ;; Calculate total rewards
          (let* ((earned (if (> staked-amount 0.0)
                          (- (* staked-amount acc-reward) debt)
                          0.0))
                 (total-rewards (+ pending earned)))
            
            (if (> total-rewards 0.0)
              ;; Use managed capability for reward claims
              (with-capability (REWARD-CLAIM account pool-id total-rewards)
                ;; Emit event
                (emit-event (CLAIM account pool-id total-rewards))
                
                ;; The managed capability enforces the limit
                (require-capability (REWARD-CLAIM account pool-id total-rewards))
                
                ;; Update stake
                (update stakes stake-key {
                  "reward-debt": (* staked-amount acc-reward),
                  "pending-rewards": 0.0,
                  "last-claim": (at 'block-time (chain-data))
                })
                
                ;; Transfer rewards (simplified)
                (format "Claimed {} rewards" [total-rewards]))
              "No rewards to claim")))))
  
  (defun emergency-withdraw:string (account:string pool-id:string)
    @doc "Emergency withdrawal without rewards"
    (let ((stake-key (format "{}:{}" [account pool-id])))
      (with-read stakes stake-key
        { "amount" := staked-amount }
        
        (enforce (> staked-amount 0.0) "No active stake")
        
        ;; Emit event
        (emit-event (EMERGENCY-WITHDRAW account pool-id staked-amount))
        
        ;; Clear stake without claiming rewards
        (update stakes stake-key {
          "amount": 0.0,
          "reward-debt": 0.0,
          "pending-rewards": 0.0
        })
        
        ;; Update pool total
        (with-read pools pool-id
          { "total-staked" := total-staked }
          
          (update pools pool-id {
            "total-staked": (- total-staked staked-amount)
          }))
        
        ;; Transfer tokens back (simplified)
        (format "Emergency withdrawal of {} tokens" [staked-amount]))))
  
  ;; View Functions
  (defun get-pending-rewards:decimal (account:string pool-id:string)
    @doc "Calculate pending rewards"
    ;; Update pool first
    (update-pool pool-id)
    
    (let ((stake-key (format "{}:{}" [account pool-id])))
      (with-default-read stakes stake-key
        { "amount": 0.0
        , "reward-debt": 0.0
        , "pending-rewards": 0.0 }
        { "amount" := amount
        , "reward-debt" := debt
        , "pending-rewards" := pending }
        
        (if (> amount 0.0)
          (with-read pools pool-id
            { "acc-reward-per-share" := acc-reward }
            
            (+ pending (- (* amount acc-reward) debt)))
          0.0))))
  
  (defun get-pool-info:object (pool-id:string)
    @doc "Get pool information"
    (+ (read pools pool-id) 
       { "current-time": (at 'block-time (chain-data)) }))
  
  (defun get-user-stake:object (account:string pool-id:string)
    @doc "Get user stake info with calculated rewards"
    (let ((stake-key (format "{}:{}" [account pool-id])))
      (+ (with-default-read stakes stake-key
           { "amount": 0.0
           , "reward-debt": 0.0
           , "pending-rewards": 0.0
           , "stake-time": (at 'block-time (chain-data))
           , "last-claim": (at 'block-time (chain-data)) }
           identity)
         { "calculated-rewards": (get-pending-rewards account pool-id) })))
  
  (defun get-all-user-stakes:[object] (account:string)
    @doc "Get all stakes for a user"
    (let ((user-keys (filter (lambda (k) (= (take 64 k) account))
                            (keys stakes))))
      (map (lambda (k) 
             (let ((pool-id (drop (+ 1 64) k)))
               (get-user-stake account pool-id)))
           user-keys)))
  
  (defun get-pool-apy:decimal (pool-id:string)
    @doc "Calculate pool APY"
    (with-read pools pool-id
      { "reward-per-second" := rps
      , "total-staked" := staked }
      
      (if (> staked 0.0)
        ;; Simple APY calculation (reward rate * seconds per year / total staked)
        (* (/ (* rps 31536000.0) staked) 100.0)
        0.0)))
  
  ;; Utility Functions
  (defun enforce-valid-account (account:string)
    @doc "Validate account"
    (enforce (!= account "") "Invalid account"))
  
  (defun enforce-valid-amount (amount:decimal)
    @doc "Validate amount"
    (enforce (> amount 0.0) "Amount must be positive"))
  
  ;; Admin Functions
  (defun update-reward-rate:string (pool-id:string new-rate:decimal)
    @doc "Update pool reward rate"
    (with-capability (GOVERNANCE)
      (enforce (>= new-rate 0.0) "Invalid reward rate")
      
      ;; Update pool before changing rate
      (update-pool pool-id)
      
      (update pools pool-id {
        "reward-per-second": new-rate
      })
      
      (format "Reward rate updated to {}" [new-rate])))
  
  (defun extend-pool:string (pool-id:string additional-seconds:integer)
    @doc "Extend pool end time"
    (with-capability (GOVERNANCE)
      (enforce (> additional-seconds 0) "Invalid duration")
      
      (with-read pools pool-id { "end-time" := current-end }
        (update pools pool-id {
          "end-time": (add-time current-end additional-seconds)
        }))
      
      (format "Pool extended by {} seconds" [additional-seconds])))
)

(create-table pools)
(create-table stakes)