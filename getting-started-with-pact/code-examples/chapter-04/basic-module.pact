;; Basic Module Example
;; Chapter 4: Module System

(module simple-token 'token-admin
  @doc "A simple token implementation demonstrating module concepts"
  
  ;; Module metadata
  (defconst VERSION "1.0.0")
  (defconst INITIAL_SUPPLY 1000000.0)
  
  ;; Schema definitions
  (defschema account
    @doc "Token account schema"
    balance:decimal
    guard:guard
    created:time)
  
  ;; Table creation
  (deftable accounts:{account})
  
  ;; Governance capability
  (defcap GOVERNANCE ()
    @doc "Module governance capability"
    (enforce-keyset 'token-admin))
  
  ;; Transfer capability
  (defcap TRANSFER (from:string to:string amount:decimal)
    @doc "Transfer capability with managed amount"
    @managed amount TRANSFER-mgr
    (enforce-guard (at 'guard (read accounts from)))
    (enforce (> amount 0.0) "Amount must be positive"))
  
  ;; Manager function for TRANSFER capability
  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    @doc "Manages transfer capability"
    (enforce (>= managed requested) "Insufficient transfer amount")
    (- managed requested))
  
  ;; Public functions
  (defun create-account:string (account:string guard:guard)
    @doc "Create a new token account"
    (insert accounts account {
      "balance": 0.0,
      "guard": guard,
      "created": (time)
    })
    (format "Account {} created" [account]))
  
  (defun get-balance:decimal (account:string)
    @doc "Get account balance"
    (at 'balance (read accounts account)))
  
  (defun transfer:string (from:string to:string amount:decimal)
    @doc "Transfer tokens between accounts"
    (with-capability (TRANSFER from to amount)
      (with-read accounts from 
        { "balance" := from-balance }
        (enforce (>= from-balance amount) "Insufficient balance")
        (update accounts from { "balance": (- from-balance amount) })
        (with-default-read accounts to
          { "balance": 0.0, "guard": (keyset-ref-guard 'default), "created": (time) }
          { "balance" := to-balance }
          (write accounts to {
            "balance": (+ to-balance amount),
            "guard": (keyset-ref-guard 'default),
            "created": (time)
          }))))
    (format "Transferred {} from {} to {}" [amount from to]))
  
  ;; Admin functions
  (defun mint:string (account:string amount:decimal)
    @doc "Mint new tokens (admin only)"
    (with-capability (GOVERNANCE)
      (with-default-read accounts account
        { "balance": 0.0, "guard": (keyset-ref-guard 'default), "created": (time) }
        { "balance" := current-balance }
        (write accounts account {
          "balance": (+ current-balance amount),
          "guard": (keyset-ref-guard 'default),
          "created": (time)
        })))
    (format "Minted {} tokens to {}" [amount account]))
)

;; Create the table after module definition
(create-table accounts)