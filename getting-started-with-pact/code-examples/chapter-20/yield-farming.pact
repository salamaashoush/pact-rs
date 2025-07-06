;; yield-farming.pact
;; Yield farming rewards for liquidity providers
(module yield-farming GOVERNANCE
  @doc "Rewards distribution for DeFi protocol participants"
  
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
    end-time:time)
  
  (defschema user-stake
    @doc "User staking position"
    account:string
    pool-id:string
    amount:decimal
    reward-debt:decimal
    pending-rewards:decimal
    stake-time:time)
  
  ;; Tables
  (deftable pools:{farm-pool})
  (deftable stakes:{user-stake})
  
  ;; Capabilities
  (defcap STAKE (account:string pool-id:string amount:decimal)
    @doc "Staking capability"
    @event
    (enforce-valid-account account)
    (enforce (> amount 0.0) "Amount must be positive")
    (with-read pools pool-id { "start-time" := start, "end-time" := end }
      (let ((now (at 'block-time (chain-data))))
        (enforce (>= now start) "Pool not started")
        (enforce (<= now end) "Pool ended"))))
  
  (defcap UNSTAKE (account:string pool-id:string amount:decimal)
    @doc "Unstaking capability"
    @event
    (enforce-valid-account account)
    (enforce (> amount 0.0) "Amount must be positive"))
  
  (defcap CLAIM (account:string pool-id:string)
    @doc "Claim rewards capability"
    @event
    (enforce-valid-account account))
  
  ;; Pool Management
  (defun create-pool:string 
    ( pool-id:string
      staking-token:string
      reward-token:string
      reward-per-second:decimal
      duration:integer )
    @doc "Create new farming pool"
    (with-capability (GOVERNANCE)
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
          "end-time": (add-time now duration)
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
  
  ;; Staking Functions
  (defun stake:string (account:string pool-id:string amount:decimal)
    @doc "Stake tokens in pool"
    (with-capability (STAKE account pool-id amount)
      ;; Update pool
      (update-pool pool-id)
      
      (with-read pools pool-id
        { "total-staked" := total-staked
        , "acc-reward-per-share" := acc-reward }
        
        ;; Get or create stake
        (let ((stake-key (format "{}:{}" [account pool-id])))
          (with-default-read stakes stake-key
            { "amount": 0.0
            , "reward-debt": 0.0
            , "pending-rewards": 0.0 }
            { "amount" := current-amount
            , "reward-debt" := debt
            , "pending-rewards" := pending }
            
            ;; Calculate pending rewards for existing stake
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
                "stake-time": (at 'block-time (chain-data))
              }))))
        
        ;; Update pool total
        (update pools pool-id {
          "total-staked": (+ total-staked amount)
        })
        
        ;; Transfer staking tokens (simplified)
        (format "Staked {} tokens" [amount]))))
  
  (defun unstake:string (account:string pool-id:string amount:decimal)
    @doc "Unstake tokens from pool"
    (with-capability (UNSTAKE account pool-id amount)
      ;; Update pool
      (update-pool pool-id)
      
      (let ((stake-key (format "{}:{}" [account pool-id])))
        (with-read stakes stake-key
          { "amount" := staked-amount }
          
          (enforce (>= staked-amount amount) "Insufficient stake")
          
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
          (format "Unstaked {} tokens" [amount])))))
  
  (defun claim-rewards:string (account:string pool-id:string)
    @doc "Claim accumulated rewards"
    (with-capability (CLAIM account pool-id)
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
                (progn
                  ;; Update stake
                  (update stakes stake-key {
                    "reward-debt": (* staked-amount acc-reward),
                    "pending-rewards": 0.0
                  })
                  
                  ;; Transfer rewards (simplified)
                  (format "Claimed {} rewards" [total-rewards]))
                "No rewards to claim")))))))
  
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
    (read pools pool-id))
  
  (defun get-user-stake:object (account:string pool-id:string)
    @doc "Get user stake info"
    (let ((stake-key (format "{}:{}" [account pool-id])))
      (with-default-read stakes stake-key
        { "amount": 0.0
        , "reward-debt": 0.0
        , "pending-rewards": 0.0
        , "stake-time": (at 'block-time (chain-data)) }
        identity)))
  
  ;; Utility Functions
  (defun enforce-valid-account (account:string)
    @doc "Validate account"
    (enforce (!= account "") "Invalid account"))
)

(create-table pools)
(create-table stakes)