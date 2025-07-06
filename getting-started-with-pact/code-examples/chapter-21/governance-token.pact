;; governance-token.pact
(module governance-token GOVERNANCE
  @doc "DAO governance token with voting and delegation"
  
  ;; Implements fungible-v2
  (implements fungible-v2)
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dao-admin))
  
  ;; Schemas
  (defschema account
    @doc "Account with balance and voting power"
    balance:decimal
    guard:guard
    votes:decimal           ;; Current voting power
    delegates:string        ;; Address delegated to
    checkpoint-count:integer) ;; Number of checkpoints
  
  (defschema checkpoint
    @doc "Historical balance checkpoint"
    account:string
    checkpoint-num:integer
    block-height:integer
    balance:decimal
    votes:decimal)
  
  (defschema delegate-info
    @doc "Delegation information"
    delegator:string
    delegatee:string
    amount:decimal
    timestamp:time)
  
  ;; Tables
  (deftable accounts:{account})
  (deftable checkpoints:{checkpoint})
  (deftable delegations:{delegate-info})
  
  ;; Events
  (defcap DELEGATE:bool (delegator:string delegatee:string amount:decimal)
    @doc "Delegation event"
    @event
    true)
  
  (defcap DELEGATE_VOTES_CHANGED:bool (delegate:string previous:decimal new:decimal)
    @doc "Delegate votes changed event"
    @event
    true)
  
  ;; Constants
  (defconst DECIMALS 18)
  (defconst VOTE_PRECISION 1000000) ;; 6 decimal places
  
  ;; Core token functions
  (defun create-account:string (account:string guard:guard)
    @doc "Create new account"
    (insert accounts account {
      "balance": 0.0,
      "guard": guard,
      "votes": 0.0,
      "delegates": account,  ;; Self-delegate by default
      "checkpoint-count": 0
    }))
  
  (defun get-balance:decimal (account:string)
    @doc "Get account balance"
    (at 'balance (read accounts account)))
  
  (defun get-votes:decimal (account:string)
    @doc "Get current voting power"
    (at 'votes (read accounts account)))
  
  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer tokens and update voting power"
    (enforce (!= sender receiver) "Cannot transfer to self")
    (enforce (> amount 0.0) "Amount must be positive")
    (enforce-unit amount)
    
    (with-capability (TRANSFER sender receiver amount)
      ;; Move tokens
      (debit sender amount)
      (with-read accounts receiver { "guard" := receiver-guard }
        (credit receiver receiver-guard amount))
      
      ;; Update voting power
      (move-voting-power 
        (at 'delegates (read accounts sender))
        (at 'delegates (read accounts receiver)) 
        amount)))
  
  ;; Delegation functions
  (defun delegate:string (delegator:string delegatee:string)
    @doc "Delegate voting power to another address"
    (with-read accounts delegator 
      { "balance" := balance
      , "delegates" := current-delegate }
      
      (enforce (!= delegatee current-delegate) "Already delegated to this address")
      
      ;; Update delegation
      (update accounts delegator { "delegates": delegatee })
      
      ;; Move voting power
      (if (!= current-delegate delegator)
        (move-voting-power current-delegate delegatee balance)
        (move-voting-power delegator delegatee balance))
      
      ;; Record delegation
      (insert delegations (format "{}:{}" [delegator delegatee]) {
        "delegator": delegator,
        "delegatee": delegatee,
        "amount": balance,
        "timestamp": (at 'block-time (chain-data))
      })
      
      ;; Emit event
      (emit-event (DELEGATE delegator delegatee balance))
      
      (format "Delegated {} votes from {} to {}" [balance delegator delegatee])))
  
  (defun move-voting-power:string (from:string to:string amount:decimal)
    @doc "Move voting power between addresses"
    (if (!= from to)
      (do
        (if (!= from "")
          (let ((previous-votes (get-votes from))
                (new-votes (- previous-votes amount)))
            (update accounts from { "votes": new-votes })
            (write-checkpoint from new-votes)
            (emit-event (DELEGATE_VOTES_CHANGED from previous-votes new-votes))))
        
        (if (!= to "")
          (let ((previous-votes (get-votes to))
                (new-votes (+ previous-votes amount)))
            (update accounts to { "votes": new-votes })
            (write-checkpoint to new-votes)
            (emit-event (DELEGATE_VOTES_CHANGED to previous-votes new-votes)))))
      "No delegation change"))
  
  ;; Checkpoint functions for historical voting power
  (defun write-checkpoint:string (account:string new-votes:decimal)
    @doc "Write voting power checkpoint"
    (with-read accounts account 
      { "checkpoint-count" := count
      , "balance" := balance }
      
      (let ((checkpoint-id (format "{}:{}" [account count]))
            (block-height (at 'block-height (chain-data))))
        
        (insert checkpoints checkpoint-id {
          "account": account,
          "checkpoint-num": count,
          "block-height": block-height,
          "balance": balance,
          "votes": new-votes
        })
        
        (update accounts account { "checkpoint-count": (+ count 1) }))))
  
  (defun get-prior-votes:decimal (account:string block-height:integer)
    @doc "Get voting power at specific block height"
    (with-read accounts account { "checkpoint-count" := count }
      (if (= count 0)
        0.0
        (let ((checkpoint-id (format "{}:{}" [account (- count 1)])))
          (with-read checkpoints checkpoint-id 
            { "block-height" := cp-height
            , "votes" := cp-votes }
            (if (<= cp-height block-height)
              cp-votes
              (get-checkpoint-votes account block-height 0 (- count 1))))))))
  
  (defun get-checkpoint-votes:decimal (account:string block-height:integer low:integer high:integer)
    @doc "Binary search for checkpoint votes"
    (if (> low high)
      0.0
      (let* ((mid (/ (+ low high) 2))
             (checkpoint-id (format "{}:{}" [account mid])))
        (with-read checkpoints checkpoint-id 
          { "block-height" := cp-height
          , "votes" := cp-votes }
          (if (= cp-height block-height)
            cp-votes
            (if (< cp-height block-height)
              (if (or (= mid high) 
                     (< (at 'block-height (read checkpoints (format "{}:{}" [account (+ mid 1)]))) 
                        block-height))
                cp-votes
                (get-checkpoint-votes account block-height (+ mid 1) high))
              (get-checkpoint-votes account block-height low (- mid 1))))))))
  
  ;; Additional helper functions
  (defun get-delegates:string (account:string)
    @doc "Get current delegate for account"
    (at 'delegates (read accounts account)))
  
  (defun get-delegation-info:object (delegator:string delegatee:string)
    @doc "Get delegation details"
    (read delegations (format "{}:{}" [delegator delegatee])))
  
  ;; Supply management functions (implement remaining fungible-v2 interface)
  (defschema supply
    supply:decimal)
  
  (deftable supply-table:{supply})
  
  (defun total-supply:decimal ()
    @doc "Get total supply"
    (with-default-read supply-table "total"
      { "supply": 0.0 }
      { "supply" := s }
      s))
  
  (defun mint:string (account:string amount:decimal)
    @doc "Mint new tokens (governance only)"
    (with-capability (GOVERNANCE)
      (with-capability (CREDIT account)
        (with-read accounts account { "guard" := guard }
          (credit account guard amount)))
      ;; Update voting power if self-delegated
      (with-read accounts account { "delegates" := delegates }
        (if (= delegates account)
          (move-voting-power "" account amount)
          "Delegated elsewhere"))
      (with-default-read supply-table "total"
        { "supply": 0.0 }
        { "supply" := s }
        (write supply-table "total" { "supply": (+ s amount) }))))
  
  ;; Standard fungible-v2 functions
  (defun precision:integer () DECIMALS)
  
  (defun enforce-unit:bool (amount:decimal)
    (enforce (= (floor amount DECIMALS) amount) "Precision violation"))
  
  (defun details:object{fungible-v2.account-details} (account:string)
    (with-read accounts account 
      { "balance" := bal, "guard" := g }
      { "account": account, "balance": bal, "guard": g }))
  
  (defun rotate:string (account:string new-guard:guard)
    (with-read accounts account { "guard" := old-guard }
      (enforce-guard old-guard)
      (update accounts account { "guard": new-guard })))
  
  ;; Internal functions
  (defun debit:string (account:string amount:decimal)
    (enforce (> amount 0.0) "Debit amount must be positive")
    (enforce-unit amount)
    (require-capability (DEBIT account))
    (with-read accounts account { "balance" := balance }
      (enforce (>= balance amount) "Insufficient balance")
      (update accounts account { "balance": (- balance amount) })))
  
  (defun credit:string (account:string guard:guard amount:decimal)
    (enforce (> amount 0.0) "Credit amount must be positive")
    (enforce-unit amount)
    (require-capability (CREDIT account))
    (with-default-read accounts account
      { "balance": -1.0, "guard": guard, "votes": 0.0, "delegates": account, "checkpoint-count": 0 }
      { "balance" := balance, "guard" := retg }
      (enforce (= retg guard) "Account guards do not match")
      (let ((is-new (= balance -1.0)))
        (write accounts account {
          "balance": (if is-new amount (+ balance amount)),
          "guard": retg,
          "votes": (if is-new 0.0 (at 'votes (read accounts account))),
          "delegates": (if is-new account (at 'delegates (read accounts account))),
          "checkpoint-count": (if is-new 0 (at 'checkpoint-count (read accounts account)))
        }))))
  
  (defun transfer-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
    (with-default-read accounts receiver
      { "balance": -1.0 }
      { "balance" := balance }
      (if (= balance -1.0)
        (create-account receiver receiver-guard)
        "Account exists"))
    (transfer sender receiver amount))
  
  ;; Capabilities
  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
    @doc "Transfer capability"
    @managed amount TRANSFER-mgr
    (enforce-guard (at 'guard (read accounts sender)))
    (enforce-unit amount)
    (enforce (> amount 0.0) "Amount must be positive")
    (compose-capability (DEBIT sender))
    (compose-capability (CREDIT receiver)))
  
  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    @doc "Transfer manager"
    (enforce (<= requested managed) "Transfer amount exceeds managed amount")
    (- managed requested))
  
  (defcap DEBIT:bool (account:string)
    @doc "Capability to debit account"
    (enforce-guard (at 'guard (read accounts account))))
  
  (defcap CREDIT:bool (account:string)
    @doc "Capability to credit account"
    true)
)

;; Create tables
(create-table accounts)
(create-table checkpoints)
(create-table delegations)
(create-table supply-table)