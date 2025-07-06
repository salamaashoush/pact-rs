;; defi-token.pact
;; Interest-bearing token implementation
(module defi-token GOVERNANCE
  @doc "Interest-bearing token for DeFi lending protocol"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Schemas
  (defschema balance
    @doc "User balance with interest tracking"
    balance:decimal
    principal:decimal       ;; Original deposit amount
    last-update:time)
  
  (defschema reserve
    @doc "Token reserve information"
    total-supply:decimal
    total-borrowed:decimal
    interest-index:decimal  ;; Cumulative interest multiplier
    last-update:time
    borrow-rate:decimal
    supply-rate:decimal)
  
  ;; Tables
  (deftable balances:{balance})
  (deftable reserves:{reserve})
  
  ;; Constants
  (defconst SECONDS_PER_YEAR 31536000.0)
  (defconst MIN_PRECISION 0.000000000001) ;; 12 decimals
  
  ;; Capabilities
  (defcap DEBIT (sender:string amount:decimal)
    @doc "Debit capability"
    (enforce-valid-account sender)
    (enforce-guard (account-guard sender))
    (let ((balance (get-balance sender)))
      (enforce (>= balance amount) "Insufficient balance")))
  
  (defcap CREDIT (receiver:string amount:decimal)
    @doc "Credit capability"
    (enforce-valid-account receiver))
  
  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
    @doc "Transfer capability"
    @managed amount TRANSFER-mgr
    (enforce (!= sender receiver) "Cannot transfer to self")
    (enforce-valid-amount amount)
    (compose-capability (DEBIT sender amount))
    (compose-capability (CREDIT receiver amount)))
  
  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    @doc "Transfer capability manager"
    (enforce (>= managed requested) "Transfer exceeds allowance")
    (- managed requested))
  
  ;; Account Management
  (defun create-account:string (account:string guard:guard)
    @doc "Create new account"
    (enforce-valid-account account)
    (insert balances account {
      "balance": 0.0,
      "principal": 0.0,
      "last-update": (at 'block-time (chain-data))
    })
    (format "Account {} created" [account]))
  
  (defun account-guard:guard (account:string)
    @doc "Get account guard"
    (at 'guard (coin.details account)))
  
  ;; Core Functions
  (defun mint:string (account:string guard:guard amount:decimal)
    @doc "Mint tokens (deposit underlying)"
    (with-capability (CREDIT account amount)
      (enforce-valid-amount amount)
      
      ;; Update indexes
      (update-interest-indexes)
      
      (with-read reserves "" 
        { "total-supply" := total-supply
        , "interest-index" := index }
        
        (with-default-read balances account
          { "balance": 0.0, "principal": 0.0 }
          { "balance" := current-balance, "principal" := principal }
          
          (write balances account {
            "balance": (+ current-balance amount),
            "principal": (+ principal amount),
            "last-update": (at 'block-time (chain-data))
          })
          
          (update reserves "" {
            "total-supply": (+ total-supply amount)
          })))
      
      (format "Minted {} tokens to {}" [amount account]))
  
  (defun burn:string (account:string amount:decimal)
    @doc "Burn tokens (withdraw underlying)"
    (with-capability (DEBIT account amount)
      (enforce-valid-amount amount)
      
      ;; Update indexes
      (update-interest-indexes)
      
      (with-read balances account 
        { "balance" := balance, "principal" := principal }
        
        (enforce (>= balance amount) "Insufficient balance")
        
        (update balances account {
          "balance": (- balance amount),
          "principal": (- principal (min amount principal)),
          "last-update": (at 'block-time (chain-data))
        })
        
        (with-read reserves ""
          { "total-supply" := total-supply }
          
          (update reserves "" {
            "total-supply": (- total-supply amount)
          })))
      
      (format "Burned {} tokens from {}" [amount account]))
  
  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer tokens between accounts"
    (with-capability (TRANSFER sender receiver amount)
      ;; Update sender
      (with-read balances sender { "balance" := sender-balance }
        (enforce (>= sender-balance amount) "Insufficient balance")
        (update balances sender {
          "balance": (- sender-balance amount),
          "last-update": (at 'block-time (chain-data))
        }))
      
      ;; Update receiver
      (with-default-read balances receiver
        { "balance": 0.0, "principal": 0.0 }
        { "balance" := receiver-balance }
        
        (write balances receiver {
          "balance": (+ receiver-balance amount),
          "principal": 0.0,  ;; Transfers don't track principal
          "last-update": (at 'block-time (chain-data))
        }))
      
      (format "Transferred {} from {} to {}" [amount sender receiver]))
  
  ;; Interest Rate Functions
  (defun update-interest-indexes:string ()
    @doc "Update interest indexes"
    (with-read reserves ""
      { "interest-index" := current-index
      , "last-update" := last-update
      , "borrow-rate" := borrow-rate
      , "supply-rate" := supply-rate }
      
      (let* ((current-time (at 'block-time (chain-data)))
             (time-delta (diff-time current-time last-update))
             (time-delta-seconds (/ time-delta 1.0))
             (supply-multiplier (calculate-compound-interest 
                                supply-rate time-delta-seconds)))
        
        (if (> time-delta-seconds 0.0)
          (update reserves "" {
            "interest-index": (* current-index supply-multiplier),
            "last-update": current-time
          })
          "No update needed"))))
  
  (defun calculate-compound-interest:decimal (rate:decimal seconds:decimal)
    @doc "Calculate compound interest multiplier"
    ;; Simplified: (1 + rate/secondsPerYear)^seconds
    ;; In production, use more accurate calculation
    (let ((rate-per-second (/ rate SECONDS_PER_YEAR)))
      (exp (* seconds (ln (+ 1.0 rate-per-second))))))
  
  (defun set-interest-rates:string (borrow-rate:decimal utilization:decimal)
    @doc "Update interest rates"
    (with-capability (GOVERNANCE)
      (let ((supply-rate (* borrow-rate utilization 0.9))) ;; 90% to suppliers
        (update reserves "" {
          "borrow-rate": borrow-rate,
          "supply-rate": supply-rate
        })
        (format "Rates updated: borrow={}, supply={}" 
                [borrow-rate supply-rate]))))
  
  ;; Query Functions
  (defun get-balance:decimal (account:string)
    @doc "Get account balance with interest"
    (with-default-read balances account
      { "balance": 0.0 }
      { "balance" := balance }
      balance))
  
  (defun get-supply-apy:decimal ()
    @doc "Get current supply APY"
    (at 'supply-rate (read reserves "")))
  
  ;; Utility Functions
  (defun enforce-valid-account (account:string)
    @doc "Validate account name"
    (enforce (!= account "") "Account cannot be empty")
    (enforce (< (length account) 128) "Account name too long"))
  
  (defun enforce-valid-amount (amount:decimal)
    @doc "Validate transfer amount"
    (enforce (> amount 0.0) "Amount must be positive")
    (enforce (>= amount MIN_PRECISION) "Amount too small"))
  
  (defun exp:decimal (x:decimal)
    @doc "Exponential function approximation"
    ;; Taylor series approximation for e^x
    (let ((x2 (* x x))
          (x3 (* x2 x))
          (x4 (* x3 x)))
      (+ 1.0 x (/ x2 2.0) (/ x3 6.0) (/ x4 24.0))))
  
  (defun ln:decimal (x:decimal)
    @doc "Natural logarithm approximation"
    ;; Simplified for (1 + small x)
    (let ((y (- x 1.0)))
      (- y (/ (* y y) 2.0) (/ (* y y y) 3.0))))
  
  ;; Initialize
  (defun init:string ()
    @doc "Initialize reserve"
    (insert reserves "" {
      "total-supply": 0.0,
      "total-borrowed": 0.0,
      "interest-index": 1.0,
      "last-update": (at 'block-time (chain-data)),
      "borrow-rate": 0.05,    ;; 5% APR
      "supply-rate": 0.03     ;; 3% APR
    })
    "Reserve initialized")
)

(create-table balances)
(create-table reserves)