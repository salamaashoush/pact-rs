;; Debug Token Example - Chapter 4
;; Minimal working version

(module debug-token 'token-admin
  @doc "A minimal token for debugging"
  
  ;; Schema definitions
  (defschema account
    balance:decimal
    guard:guard)
  
  ;; Table creation
  (deftable accounts:{account})
  
  ;; Governance capability
  (defcap GOVERNANCE ()
    (enforce-keyset 'token-admin))
  
  ;; Public functions
  (defun create-account:string (account:string guard:guard)
    @doc "Create a new token account"
    (insert accounts account {
      "balance": 0.0,
      "guard": guard
    })
    "Account created")
  
  (defun get-balance:decimal (account:string)
    @doc "Get account balance"
    (at 'balance (read accounts account)))
  
  (defun mint:string (account:string amount:decimal)
    @doc "Mint new tokens (admin only)"
    (with-capability (GOVERNANCE)
      (with-default-read accounts account
        { "balance": 0.0, "guard": (keyset-ref-guard 'token-admin) }
        { "balance" := current-balance }
        (write accounts account {
          "balance": (+ current-balance amount),
          "guard": (keyset-ref-guard 'token-admin)
        })))
    "Tokens minted")
)

;; Create the table after module definition
(create-table accounts)