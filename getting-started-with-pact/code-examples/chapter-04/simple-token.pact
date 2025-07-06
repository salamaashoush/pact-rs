;; Simple Token Example - Fixed Version
;; Chapter 4: Module System

(module simple-token 'token-admin
  @doc "A simple token implementation with proper capability handling"
  
  ;; Module metadata
  (defconst VERSION "1.0.1")
  
  ;; Schema definitions
  (defschema account
    @doc "Token account schema"
    balance:decimal
    guard:guard)
  
  ;; Table creation
  (deftable accounts:{account})
  
  ;; Governance capability
  (defcap GOVERNANCE ()
    @doc "Module governance capability"
    (enforce-keyset 'token-admin))
  
  ;; Transfer capability - simplified without @managed for now
  (defcap TRANSFER (from:string to:string amount:decimal)
    @doc "Transfer capability"
    (enforce-guard (at 'guard (read accounts from)))
    (enforce (> amount 0.0) "Amount must be positive")
    (with-read accounts from { "balance" := balance }
      (enforce (>= balance amount) "Insufficient balance")))
  
  ;; Public functions
  (defun create-account:string (account:string guard:guard)
    @doc "Create a new token account"
    (insert accounts account {
      "balance": 0.0,
      "guard": guard
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
        (update accounts from { "balance": (- from-balance amount) })
        (with-read accounts to
          { "balance" := to-balance }
          (update accounts to { "balance": (+ to-balance amount) }))))
    (format "Transferred {} from {} to {}" [amount from to]))
  
  (defun transfer-create:string (from:string to:string to-guard:guard amount:decimal)
    @doc "Transfer tokens, creating the recipient account if needed"
    (with-capability (TRANSFER from to amount)
      (with-read accounts from 
        { "balance" := from-balance }
        (update accounts from { "balance": (- from-balance amount) })
        (with-default-read accounts to
          { "balance": 0.0, "guard": to-guard }
          { "balance" := to-balance }
          (write accounts to {
            "balance": (+ to-balance amount),
            "guard": to-guard
          }))))
    (format "Transferred {} from {} to {}" [amount from to]))
  
  ;; Admin functions
  (defun mint:string (account:string guard:guard amount:decimal)
    @doc "Mint new tokens to an account (admin only)"
    (with-capability (GOVERNANCE)
      (enforce (> amount 0.0) "Amount must be positive")
      (with-default-read accounts account
        { "balance": 0.0, "guard": guard }
        { "balance" := current-balance }
        (write accounts account {
          "balance": (+ current-balance amount),
          "guard": guard
        })))
    (format "Minted {} tokens to {}" [amount account]))
)

;; Create the table after module definition
(create-table accounts)