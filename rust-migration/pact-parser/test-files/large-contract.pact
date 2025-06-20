;; Large test contract for parser benchmarking
(module advanced-defi 'defi-admin
  
  ;; Complex schemas
  (defschema liquidity-pool
    token-a:string
    token-b:string
    reserve-a:decimal
    reserve-b:decimal
    total-supply:decimal
    fee-rate:decimal
    created:time
    active:bool)
    
  (defschema user-position
    user:string
    pool-id:string
    liquidity-tokens:decimal
    last-reward-claim:time
    total-fees-earned:decimal)
    
  (defschema swap-order
    user:string
    token-in:string
    token-out:string
    amount-in:decimal
    min-amount-out:decimal
    deadline:time
    executed:bool)
    
  (defschema governance-proposal
    id:string
    proposer:string
    title:string
    description:string
    votes-for:decimal
    votes-against:decimal
    deadline:time
    executed:bool
    proposal-type:string)
    
  (defschema yield-farm
    pool-id:string
    reward-token:string
    reward-rate:decimal
    start-time:time
    end-time:time
    total-staked:decimal
    active:bool)
  
  ;; Tables
  (deftable pools:{liquidity-pool})
  (deftable positions:{user-position})
  (deftable orders:{swap-order})
  (deftable proposals:{governance-proposal})
  (deftable farms:{yield-farm})
  
  ;; Advanced capabilities
  (defcap GOVERNANCE ()
    @doc "Governance capability for admin functions"
    (enforce-guard (keyset-ref-guard 'defi-admin)))
    
  (defcap LIQUIDITY (user:string pool-id:string amount:decimal)
    @doc "Capability for liquidity operations"
    @managed amount LIQUIDITY-mgr
    (enforce-valid-user user)
    (enforce (> amount 0.0) "Amount must be positive")
    (enforce-pool-exists pool-id))
    
  (defcap SWAP (user:string token-in:string token-out:string amount:decimal)
    @doc "Capability for token swaps"
    @managed amount SWAP-mgr
    (enforce-valid-user user)
    (enforce (> amount 0.0) "Amount must be positive")
    (enforce (!= token-in token-out) "Cannot swap same token"))
    
  (defcap STAKING (user:string farm-id:string amount:decimal)
    @doc "Capability for yield farming"
    @managed amount STAKING-mgr
    (enforce-valid-user user)
    (enforce (> amount 0.0) "Amount must be positive"))
    
  (defcap VOTING (user:string proposal-id:string power:decimal)
    @doc "Capability for governance voting"
    (enforce-valid-user user)
    (enforce (> power 0.0) "Voting power must be positive"))
    
  (defcap REWARD_CLAIM (user:string)
    @doc "Capability for claiming rewards"
    (enforce-valid-user user))
    
  ;; Capability managers
  (defun LIQUIDITY-mgr:decimal (managed:decimal requested:decimal)
    @doc "Manages liquidity allowances"
    (enforce (>= managed requested) "Insufficient liquidity allowance")
    (- managed requested))
    
  (defun SWAP-mgr:decimal (managed:decimal requested:decimal)
    @doc "Manages swap allowances"
    (enforce (>= managed requested) "Insufficient swap allowance")
    (- managed requested))
    
  (defun STAKING-mgr:decimal (managed:decimal requested:decimal)
    @doc "Manages staking allowances"
    (enforce (>= managed requested) "Insufficient staking allowance")
    (- managed requested))
  
  ;; Utility functions
  (defun enforce-valid-user (user:string)
    @doc "Validates user account format"
    (enforce (> (length user) 0) "User cannot be empty")
    (enforce (< (length user) 256) "User name too long"))
    
  (defun enforce-pool-exists (pool-id:string)
    @doc "Ensures pool exists"
    (with-default-read pools pool-id { "active": false } { "active" := active }
      (enforce active "Pool does not exist or is inactive")))
      
  (defun get-current-time:time ()
    @doc "Gets current block time"
    (at 'block-time (chain-data)))
    
  (defun calculate-hash:string (data:object)
    @doc "Calculates hash of arbitrary data"
    (hash data))
    
  (defun generate-pool-id:string (token-a:string token-b:string)
    @doc "Generates unique pool identifier"
    (hash [token-a token-b (get-current-time)]))
    
  (defun sqrt:decimal (x:decimal)
    @doc "Calculates square root (simplified)"
    (^ x 0.5))
    
  ;; Advanced mathematical functions
  (defun calculate-constant-product:decimal (reserve-a:decimal reserve-b:decimal)
    @doc "Calculates constant product for AMM"
    (* reserve-a reserve-b))
    
  (defun calculate-swap-output:decimal (amount-in:decimal reserve-in:decimal reserve-out:decimal fee-rate:decimal)
    @doc "Calculates swap output using constant product formula"
    (let* ((amount-in-with-fee (* amount-in (- 1.0 fee-rate)))
           (numerator (* amount-in-with-fee reserve-out))
           (denominator (+ reserve-in amount-in-with-fee)))
      (/ numerator denominator)))
      
  (defun calculate-price-impact:decimal (amount-in:decimal reserve-in:decimal reserve-out:decimal)
    @doc "Calculates price impact of a swap"
    (let* ((price-before (/ reserve-out reserve-in))
           (amount-out (calculate-swap-output amount-in reserve-in reserve-out 0.003))
           (new-reserve-in (+ reserve-in amount-in))
           (new-reserve-out (- reserve-out amount-out))
           (price-after (/ new-reserve-out new-reserve-in))
           (price-change (abs (- price-after price-before))))
      (/ price-change price-before)))
      
  ;; Liquidity pool operations
  (defun create-pool:string (token-a:string token-b:string initial-a:decimal initial-b:decimal fee-rate:decimal)
    @doc "Creates a new liquidity pool"
    (with-capability (GOVERNANCE)
      (enforce (> initial-a 0.0) "Initial amount A must be positive")
      (enforce (> initial-b 0.0) "Initial amount B must be positive")
      (enforce (and (>= fee-rate 0.0) (<= fee-rate 0.01)) "Fee rate must be between 0 and 1%")
      (let ((pool-id (generate-pool-id token-a token-b))
            (initial-liquidity (sqrt (* initial-a initial-b))))
        (insert pools pool-id {
          "token-a": token-a,
          "token-b": token-b,
          "reserve-a": initial-a,
          "reserve-b": initial-b,
          "total-supply": initial-liquidity,
          "fee-rate": fee-rate,
          "created": (get-current-time),
          "active": true })
        (format "Pool {} created with {} {} and {} {}" [pool-id initial-a token-a initial-b token-b]))))
        
  (defun add-liquidity:string (pool-id:string amount-a:decimal amount-b:decimal min-liquidity:decimal)
    @doc "Adds liquidity to an existing pool"
    (with-capability (LIQUIDITY (at "sender" (chain-data)) pool-id (+ amount-a amount-b))
      (with-read pools pool-id 
        { "reserve-a" := reserve-a, "reserve-b" := reserve-b, "total-supply" := total-supply }
        (let* ((liquidity-a (/ (* amount-a total-supply) reserve-a))
               (liquidity-b (/ (* amount-b total-supply) reserve-b))
               (liquidity (min liquidity-a liquidity-b)))
          (enforce (>= liquidity min-liquidity) "Insufficient liquidity received")
          (update pools pool-id {
            "reserve-a": (+ reserve-a amount-a),
            "reserve-b": (+ reserve-b amount-b),
            "total-supply": (+ total-supply liquidity) })
          (format "Added {} liquidity tokens to pool {}" [liquidity pool-id])))))
          
  (defun remove-liquidity:string (pool-id:string liquidity-amount:decimal min-a:decimal min-b:decimal)
    @doc "Removes liquidity from a pool"
    (let ((user (at "sender" (chain-data))))
      (with-capability (LIQUIDITY user pool-id liquidity-amount)
        (with-read pools pool-id
          { "reserve-a" := reserve-a, "reserve-b" := reserve-b, "total-supply" := total-supply }
          (let* ((amount-a (/ (* liquidity-amount reserve-a) total-supply))
                 (amount-b (/ (* liquidity-amount reserve-b) total-supply)))
            (enforce (>= amount-a min-a) "Insufficient amount A")
            (enforce (>= amount-b min-b) "Insufficient amount B")
            (update pools pool-id {
              "reserve-a": (- reserve-a amount-a),
              "reserve-b": (- reserve-b amount-b),
              "total-supply": (- total-supply liquidity-amount) })
            (format "Removed {} {} and {} {} from pool {}" [amount-a "token-a" amount-b "token-b" pool-id]))))))
            
  ;; Trading operations
  (defun swap-exact-tokens-for-tokens:string (amount-in:decimal min-amount-out:decimal path:[string] deadline:time)
    @doc "Swaps exact input tokens for minimum output tokens"
    (let ((user (at "sender" (chain-data))))
      (enforce (< (get-current-time) deadline) "Transaction deadline exceeded")
      (enforce (>= (length path) 2) "Path must contain at least 2 tokens")
      (with-capability (SWAP user (at 0 path) (at (- (length path) 1) path) amount-in)
        (let ((amount-out (calculate-multi-hop-swap amount-in path)))
          (enforce (>= amount-out min-amount-out) "Insufficient output amount")
          (execute-multi-hop-swap user amount-in path)
          (format "Swapped {} {} for {} {}" [amount-in (at 0 path) amount-out (at (- (length path) 1) path)])))))
          
  (defun calculate-multi-hop-swap:decimal (amount-in:decimal path:[string])
    @doc "Calculates output for multi-hop swap"
    (fold (lambda (amount token-pair)
            (let ((pool-id (generate-pool-id (at 0 token-pair) (at 1 token-pair))))
              (with-read pools pool-id { "reserve-a" := reserve-a, "reserve-b" := reserve-b, "fee-rate" := fee-rate }
                (calculate-swap-output amount reserve-a reserve-b fee-rate))))
          amount-in
          (zip (take (- (length path) 1) path) (drop 1 path))))
          
  (defun execute-multi-hop-swap:string (user:string amount-in:decimal path:[string])
    @doc "Executes multi-hop swap through multiple pools"
    (fold (lambda (amount token-pair)
            (let ((pool-id (generate-pool-id (at 0 token-pair) (at 1 token-pair))))
              (execute-single-swap user amount pool-id (at 0 token-pair) (at 1 token-pair))))
          amount-in
          (zip (take (- (length path) 1) path) (drop 1 path))))
          
  (defun execute-single-swap:decimal (user:string amount-in:decimal pool-id:string token-in:string token-out:string)
    @doc "Executes a single swap in a pool"
    (with-read pools pool-id 
      { "reserve-a" := reserve-a, "reserve-b" := reserve-b, "fee-rate" := fee-rate, "token-a" := token-a }
      (let* ((is-token-a-in (= token-in token-a))
             (reserve-in (if is-token-a-in reserve-a reserve-b))
             (reserve-out (if is-token-a-in reserve-b reserve-a))
             (amount-out (calculate-swap-output amount-in reserve-in reserve-out fee-rate))
             (new-reserve-in (+ reserve-in amount-in))
             (new-reserve-out (- reserve-out amount-out)))
        (update pools pool-id (if is-token-a-in
          { "reserve-a": new-reserve-in, "reserve-b": new-reserve-out }
          { "reserve-a": new-reserve-out, "reserve-b": new-reserve-in }))
        amount-out)))
        
  ;; Yield farming operations
  (defun create-farm:string (pool-id:string reward-token:string reward-rate:decimal duration:integer)
    @doc "Creates a new yield farm"
    (with-capability (GOVERNANCE)
      (let* ((start-time (get-current-time))
             (end-time (add-time start-time (days duration)))
             (farm-id (hash [pool-id reward-token start-time])))
        (insert farms farm-id {
          "pool-id": pool-id,
          "reward-token": reward-token,
          "reward-rate": reward-rate,
          "start-time": start-time,
          "end-time": end-time,
          "total-staked": 0.0,
          "active": true })
        (format "Farm {} created for pool {}" [farm-id pool-id]))))
        
  (defun stake-tokens:string (farm-id:string amount:decimal)
    @doc "Stakes tokens in a yield farm"
    (let ((user (at "sender" (chain-data))))
      (with-capability (STAKING user farm-id amount)
        (with-read farms farm-id { "total-staked" := total-staked, "active" := active }
          (enforce active "Farm is not active")
          (update farms farm-id { "total-staked": (+ total-staked amount) })
          (with-default-read positions (compound-key user farm-id) 
            { "liquidity-tokens": 0.0, "last-reward-claim": (get-current-time) }
            { "liquidity-tokens" := current-stake }
            (write positions (compound-key user farm-id) {
              "user": user,
              "pool-id": farm-id,
              "liquidity-tokens": (+ current-stake amount),
              "last-reward-claim": (get-current-time),
              "total-fees-earned": 0.0 }))
          (format "Staked {} tokens in farm {}" [amount farm-id])))))
          
  (defun claim-rewards:string (farm-id:string)
    @doc "Claims accumulated farming rewards"
    (let ((user (at "sender" (chain-data))))
      (with-capability (REWARD_CLAIM user)
        (with-read positions (compound-key user farm-id)
          { "liquidity-tokens" := staked, "last-reward-claim" := last-claim }
          (with-read farms farm-id { "reward-rate" := reward-rate, "total-staked" := total-staked }
            (let* ((current-time (get-current-time))
                   (time-diff (diff-time current-time last-claim))
                   (user-share (/ staked total-staked))
                   (rewards (* reward-rate user-share (/ time-diff 86400))))
              (update positions (compound-key user farm-id) { "last-reward-claim": current-time })
              (format "Claimed {} rewards from farm {}" [rewards farm-id])))))))
              
  ;; Governance operations
  (defun create-proposal:string (title:string description:string proposal-type:string voting-period:integer)
    @doc "Creates a new governance proposal"
    (let ((proposer (at "sender" (chain-data)))
          (proposal-id (hash [title description (get-current-time)]))
          (deadline (add-time (get-current-time) (days voting-period))))
      (insert proposals proposal-id {
        "id": proposal-id,
        "proposer": proposer,
        "title": title,
        "description": description,
        "votes-for": 0.0,
        "votes-against": 0.0,
        "deadline": deadline,
        "executed": false,
        "proposal-type": proposal-type })
      (format "Proposal {} created by {}" [proposal-id proposer])))
      
  (defun vote:string (proposal-id:string support:bool voting-power:decimal)
    @doc "Votes on a governance proposal"
    (let ((user (at "sender" (chain-data))))
      (with-capability (VOTING user proposal-id voting-power)
        (with-read proposals proposal-id 
          { "votes-for" := votes-for, "votes-against" := votes-against, "deadline" := deadline }
          (enforce (< (get-current-time) deadline) "Voting period has ended")
          (update proposals proposal-id 
            (if support
              { "votes-for": (+ votes-for voting-power) }
              { "votes-against": (+ votes-against voting-power) }))
          (format "{} voted {} on proposal {} with {} power" [user (if support "for" "against") proposal-id voting-power])))))
          
  (defun execute-proposal:string (proposal-id:string)
    @doc "Executes a passed governance proposal"
    (with-capability (GOVERNANCE)
      (with-read proposals proposal-id 
        { "votes-for" := votes-for, "votes-against" := votes-against, "executed" := executed, "deadline" := deadline }
        (enforce (>= (get-current-time) deadline) "Voting period not ended")
        (enforce (not executed) "Proposal already executed")
        (enforce (> votes-for votes-against) "Proposal did not pass")
        (update proposals proposal-id { "executed": true })
        (format "Proposal {} executed successfully" [proposal-id]))))
        
  ;; Analytics and queries
  (defun get-pool-info:object (pool-id:string)
    @doc "Gets comprehensive pool information"
    (with-read pools pool-id pool-data
      (let* ((tvl (+ (at "reserve-a" pool-data) (at "reserve-b" pool-data)))
             (price-a-to-b (/ (at "reserve-b" pool-data) (at "reserve-a" pool-data)))
             (price-b-to-a (/ (at "reserve-a" pool-data) (at "reserve-b" pool-data))))
        (+ pool-data { "tvl": tvl, "price-a-to-b": price-a-to-b, "price-b-to-a": price-b-to-a }))))
        
  (defun get-user-portfolio:object (user:string)
    @doc "Gets user's complete DeFi portfolio"
    (let ((user-positions (select positions [] (where 'user (= user))))
          (user-orders (select orders [] (where 'user (= user)))))
      { "positions": user-positions,
        "orders": user-orders,
        "total-positions": (length user-positions),
        "active-orders": (length (filter (lambda (order) (not (at "executed" order))) user-orders)) }))
        
  (defun calculate-portfolio-value:decimal (user:string base-token:string)
    @doc "Calculates total portfolio value in base token"
    (let ((positions (select positions [] (where 'user (= user)))))
      (fold (lambda (total position)
              (+ total (calculate-position-value position base-token)))
            0.0
            positions)))
            
  (defun calculate-position-value:decimal (position:object base-token:string)
    @doc "Calculates value of a single position"
    (let ((pool-id (at "pool-id" position))
          (liquidity-tokens (at "liquidity-tokens" position)))
      (with-read pools pool-id { "total-supply" := total-supply, "reserve-a" := reserve-a, "reserve-b" := reserve-b }
        (let* ((share (/ liquidity-tokens total-supply))
               (value-a (* share reserve-a))
               (value-b (* share reserve-b)))
          (+ value-a value-b))))) ; Simplified - would need price conversion
)