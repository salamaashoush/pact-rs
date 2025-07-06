;; dex-pair.pact
(module dex-pair GOVERNANCE
  @doc "DEX trading pair with AMM functionality"
  
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
  
  (defschema lp-account
    @doc "LP token account"
    balance:decimal
    guard:guard)
  
  (defschema swap-record
    @doc "Swap transaction record"
    user:string
    token-in:module{fungible-v2}
    token-out:module{fungible-v2}
    amount-in:decimal
    amount-out:decimal
    fee:decimal
    timestamp:time)
  
  ;; Tables
  (deftable pairs:{pair-data})
  (deftable lp-accounts:{lp-account})
  (deftable swap-records:{swap-record})
  
  ;; Events
  (defcap MINT:bool (to:string amount0:decimal amount1:decimal liquidity:decimal)
    @doc "Liquidity provision event"
    @event
    true)
  
  (defcap BURN:bool (from:string amount0:decimal amount1:decimal liquidity:decimal)
    @doc "Liquidity removal event"
    @event
    true)
  
  (defcap SWAP:bool (user:string amount0-in:decimal amount1-in:decimal amount0-out:decimal amount1-out:decimal)
    @doc "Token swap event"
    @event
    true)
  
  ;; Constants
  (defconst MINIMUM_LIQUIDITY 1000)  ;; Minimum LP tokens
  (defconst FEE_RATE 0.003)          ;; 0.3% trading fee
  (defconst PRECISION 1000000000000) ;; 12 decimal precision
  
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
    (create-principal (create-capability-guard (GOVERNANCE))))
  
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
                             (- (floor (sqrt (* amount0 amount1)) 0) MINIMUM_LIQUIDITY)
                             ;; Subsequent: proportional to existing
                             (min (/ (* amount0 total-supply) reserve0)
                                  (/ (* amount1 total-supply) reserve1)))))
            
            (enforce (> liquidity 0.0) "Insufficient liquidity minted")
            
            ;; Create LP token account if needed and mint tokens
            (with-default-read lp-accounts to
              { "balance": -1.0 }
              { "balance" := existing-balance }
              (if (= existing-balance -1.0)
                (with-capability (GOVERNANCE)
                  (insert lp-accounts to {
                    "balance": liquidity,
                    "guard": (create-capability-guard (GOVERNANCE))
                  }))
                (mint-lp-tokens to liquidity)))
            
            ;; Update reserves
            (update pairs pair-id {
              "reserve0": (+ reserve0 amount0),
              "reserve1": (+ reserve1 amount1),
              "total-supply": (+ total-supply liquidity),
              "last-update": (at 'block-time (chain-data))
            })
            
            ;; Update price oracles
            (update-price-oracles pair-id)
            
            ;; Emit event
            (emit-event (MINT to amount0 amount1 liquidity))
            
            (format "Added liquidity: {} LP tokens for {} {} and {} {}" 
                    [liquidity amount0 token0 amount1 token1])))))
  
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
          (burn-lp-tokens to liquidity)
          
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
          (emit-event (BURN to amount0 amount1 liquidity))
          
          (format "Removed liquidity: {} {} and {} {} for {} LP tokens" 
                  [amount0 token0 amount1 token1 liquidity]))))
  
  ;; Swap functions
  (defun swap-exact-tokens-for-tokens:string 
    ( amount-in:decimal
      amount-out-min:decimal
      path:[module{fungible-v2}]
      to:string
      deadline:time )
    @doc "Swap exact input tokens for output tokens"
    
    (enforce (<= (at 'block-time (chain-data)) deadline) "Transaction expired")
    (enforce (= (length path) 2) "Only direct swaps supported")
    
    (let* ((token-in (at 0 path))
           (token-out (at 1 path))
           (pair-id (create-pair-id token-in token-out))
           (pair-account (get-pair-account token-in token-out)))
      
      (with-read pairs pair-id 
        { "reserve0" := reserve0
        , "reserve1" := reserve1
        , "fee-rate" := fee-rate }
        
        ;; Determine input/output reserves
        (let* ((token-in-name (format "{}" [token-in]))
               (token0-name (format "{}" [(at 'token0 (read pairs pair-id))]))
               (is-token0 (= token-in-name token0-name))
               (reserve-in (if is-token0 reserve0 reserve1))
               (reserve-out (if is-token0 reserve1 reserve0)))
          
          ;; Calculate output amount
          (let* ((amount-in-with-fee (* amount-in (- 1.0 fee-rate)))
                 (amount-out (get-amount-out amount-in-with-fee reserve-in reserve-out)))
            
            (enforce (>= amount-out amount-out-min) "Insufficient output amount")
            
            ;; Transfer input tokens from user
            (token-in::transfer to pair-account amount-in)
            
            ;; Transfer output tokens to user
            (install-capability (token-out::TRANSFER pair-account to amount-out))
            (token-out::transfer pair-account to amount-out)
            
            ;; Update reserves
            (if is-token0
              (update pairs pair-id {
                "reserve0": (+ reserve0 amount-in),
                "reserve1": (- reserve1 amount-out),
                "last-update": (at 'block-time (chain-data))
              })
              (update pairs pair-id {
                "reserve0": (- reserve0 amount-out),
                "reserve1": (+ reserve1 amount-in),
                "last-update": (at 'block-time (chain-data))
              }))
            
            ;; Record swap
            (insert swap-records (format "{}:{}:{}" [to (at 'block-time (chain-data)) amount-in]) {
              "user": to,
              "token-in": token-in,
              "token-out": token-out,
              "amount-in": amount-in,
              "amount-out": amount-out,
              "fee": (* amount-in fee-rate),
              "timestamp": (at 'block-time (chain-data))
            })
            
            ;; Update price oracles
            (update-price-oracles pair-id)
            
            ;; Emit event
            (emit-event (SWAP to 
              (if is-token0 amount-in 0.0) 
              (if is-token0 0.0 amount-in)
              (if is-token0 0.0 amount-out)
              (if is-token0 amount-out 0.0)))
            
            (format "Swapped {} {} for {} {}" [amount-in token-in amount-out token-out]))))))
  
  ;; LP token management
  (defun mint-lp-tokens:string (to:string amount:decimal)
    @doc "Mint LP tokens"
    (with-default-read lp-accounts to
      { "balance": 0.0, "guard": (read-keyset 'user-guard) }
      { "balance" := balance, "guard" := guard }
      
      (write lp-accounts to {
        "balance": (+ balance amount),
        "guard": guard
      })))
  
  (defun burn-lp-tokens:string (from:string amount:decimal)
    @doc "Burn LP tokens"
    (with-read lp-accounts from { "balance" := balance }
      (enforce (>= balance amount) "Insufficient LP balance")
      (update lp-accounts from { "balance": (- balance amount) })))
  
  (defun transfer-lp:string (from:string to:string amount:decimal)
    @doc "Transfer LP tokens between accounts"
    (enforce (> amount 0.0) "Amount must be positive")
    (enforce (!= from to) "Cannot transfer to self")
    
    (with-read lp-accounts from { "balance" := from-balance }
      (enforce (>= from-balance amount) "Insufficient LP balance")
      
      ;; Debit from sender
      (update lp-accounts from { "balance": (- from-balance amount) })
      
      ;; Credit to receiver
      (with-default-read lp-accounts to
        { "balance": 0.0, "guard": (create-capability-guard (GOVERNANCE)) }
        { "balance" := to-balance, "guard" := to-guard }
        
        (write lp-accounts to {
          "balance": (+ to-balance amount),
          "guard": to-guard
        }))))
  
  ;; Transfer capability for LP tokens
  (defcap TRANSFER:bool (from:string to:string amount:decimal)
    @doc "Transfer capability for LP tokens"
    (with-read lp-accounts from { "guard" := guard }
      (enforce-guard guard))
    (enforce (> amount 0.0) "Amount must be positive")
    (enforce (!= from to) "Cannot transfer to self"))
  
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
    
    (let* ((numerator (* amount-in reserve-out))
           (denominator (+ reserve-in amount-in)))
      (/ numerator denominator)))
  
  (defun get-amount-in:decimal 
    ( amount-out:decimal
      reserve-in:decimal
      reserve-out:decimal )
    @doc "Calculate input amount for given output"
    
    (enforce (> amount-out 0.0) "Insufficient output amount")
    (enforce (and (> reserve-in 0.0) (> reserve-out amount-out)) "Insufficient liquidity")
    
    (let* ((numerator (* reserve-in amount-out))
           (denominator (- reserve-out amount-out)))
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
  
  ;; Query functions
  (defun get-pair:object (token0:module{fungible-v2} token1:module{fungible-v2})
    @doc "Get pair information"
    (read pairs (create-pair-id token0 token1)))
  
  (defun get-reserves:[decimal] (token0:module{fungible-v2} token1:module{fungible-v2})
    @doc "Get current reserves"
    (with-read pairs (create-pair-id token0 token1) 
      { "reserve0" := r0, "reserve1" := r1 }
      [r0 r1]))
  
  (defun get-lp-balance:decimal (account:string)
    @doc "Get LP token balance"
    (with-default-read lp-accounts account
      { "balance": 0.0 }
      { "balance" := balance }
      balance))
  
  (defun quote:decimal (amount-a:decimal reserve-a:decimal reserve-b:decimal)
    @doc "Quote amount of token B for given amount of token A"
    (enforce (> amount-a 0.0) "Insufficient amount")
    (enforce (and (> reserve-a 0.0) (> reserve-b 0.0)) "Insufficient liquidity")
    (/ (* amount-a reserve-b) reserve-a))
  
  ;; Price calculation
  (defun get-price:decimal (token0:module{fungible-v2} token1:module{fungible-v2})
    @doc "Get current price of token0 in terms of token1"
    (let ((reserves (get-reserves token0 token1)))
      (if (> (at 0 reserves) 0.0)
        (/ (at 1 reserves) (at 0 reserves))
        0.0)))
  
  ;; Swap history
  (defun get-recent-swaps:[object] (limit:integer)
    @doc "Get recent swap transactions"
    (take limit (sort ['timestamp] (select swap-records (constantly true)))))
  
  ;; Analytics
  (defun calculate-volume-24h:decimal (token:module{fungible-v2})
    @doc "Calculate 24h trading volume for token"
    (let ((cutoff-time (add-time (at 'block-time (chain-data)) (days -1))))
      (fold (+) 0.0 
        (map (at 'amount-in)
          (select swap-records 
            (and? (where 'token-in (= token))
                 (where 'timestamp (>= cutoff-time))))))))
)

;; Create tables
(create-table pairs)
(create-table lp-accounts)
(create-table swap-records)