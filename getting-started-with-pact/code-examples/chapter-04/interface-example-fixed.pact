;; Interface Example - Fixed Version
;; Chapter 4: Module System - Interfaces

;; Define a fungible token interface
(interface fungible-token-v2
  @doc "Standard fungible token interface"
  
  ;; Required schemas
  (defschema account
    @doc "Account schema"
    balance:decimal
    guard:guard)
  
  ;; Function signatures
  (defun transfer:string (from:string to:string amount:decimal)
    @doc "Transfer tokens between accounts")
  
  (defun get-balance:decimal (account:string)
    @doc "Get account balance")
  
  (defun create-account:string (account:string guard:guard)
    @doc "Create a new account")
  
  ;; Capability signatures
  (defcap TRANSFER (from:string to:string amount:decimal)
    @doc "Transfer capability")
  
  ;; Properties for formal verification
  @model
  [(property conservation
     (= (fold (+) 0.0 (map (get-balance) (keys accounts)))
        (total-supply)))
   
   (property non-negative-balances
     (forall (account:string)
       (>= (get-balance account) 0.0)))]
)

;; Implementation of the interface
(module my-token 'token-admin
  @doc "Token implementing fungible-token-v2 interface"
  
  ;; Declare interface implementation
  (implements fungible-token-v2)
  
  ;; Required schema (must match interface)
  (defschema account
    @doc "Account schema - extended from interface"
    balance:decimal
    guard:guard
    created:time        ;; Additional field (allowed)
    last-tx:time)       ;; Additional field (allowed)
  
  ;; Table using the schema
  (deftable accounts:{account})
  
  ;; Constants
  (defconst INITIAL_SUPPLY 1000000.0)
  
  ;; Supply tracking
  (defschema supply
    total:decimal)
  
  (deftable supply-table:{supply})
  
  ;; Initialize supply
  (defun init-supply ()
    (insert supply-table "supply" { "total": 0.0 }))
  
  ;; Get total supply
  (defun total-supply:decimal ()
    (at 'total (read supply-table "supply")))
  
  ;; Required capability from interface
  (defcap TRANSFER (from:string to:string amount:decimal)
    @doc "Transfer capability"
    (enforce-guard (at 'guard (read accounts from)))
    (enforce (> amount 0.0) "Amount must be positive")
    (with-read accounts from { "balance" := balance }
      (enforce (>= balance amount) "Insufficient balance")))
  
  ;; Required function from interface
  (defun transfer:string (from:string to:string amount:decimal)
    @doc "Transfer tokens between accounts"
    (with-capability (TRANSFER from to amount)
      (with-read accounts from { "balance" := from-balance }
        (enforce (>= from-balance amount) "Insufficient balance")
        (update accounts from { 
          "balance": (- from-balance amount),
          "last-tx": (at 'block-time (chain-data))
        })
        (with-read accounts to
          { "balance" := to-balance }
          (update accounts to {
            "balance": (+ to-balance amount),
            "last-tx": (at 'block-time (chain-data))
          }))))
    (format "Transferred {} from {} to {}" [amount from to]))
  
  (defun transfer-create:string (from:string to:string to-guard:guard amount:decimal)
    @doc "Transfer tokens, creating recipient if needed"
    (with-capability (TRANSFER from to amount)
      (with-read accounts from { "balance" := from-balance }
        (enforce (>= from-balance amount) "Insufficient balance")
        (update accounts from { 
          "balance": (- from-balance amount),
          "last-tx": (at 'block-time (chain-data))
        })
        (with-default-read accounts to
          { "balance": 0.0, "guard": to-guard, 
            "created": (at 'block-time (chain-data)), "last-tx": (at 'block-time (chain-data)) }
          { "balance" := to-balance }
          (write accounts to {
            "balance": (+ to-balance amount),
            "guard": to-guard,
            "created": (at 'block-time (chain-data)),
            "last-tx": (at 'block-time (chain-data))
          }))))
    (format "Transferred {} from {} to {}" [amount from to]))
  
  (defun get-balance:decimal (account:string)
    @doc "Get account balance"
    (at 'balance (read accounts account)))
  
  (defun create-account:string (account:string guard:guard)
    @doc "Create a new account"
    (insert accounts account {
      "balance": 0.0,
      "guard": guard,
      "created": (at 'block-time (chain-data)),
      "last-tx": (at 'block-time (chain-data))
    })
    (format "Account {} created" [account]))
  
  ;; Additional functions not in interface (allowed)
  (defun mint:string (account:string guard:guard amount:decimal)
    @doc "Mint tokens (not in interface)"
    (enforce-keyset 'token-admin)
    (with-default-read accounts account
      { "balance": 0.0, "guard": guard,
        "created": (at 'block-time (chain-data)), "last-tx": (at 'block-time (chain-data)) }
      { "balance" := current-balance }
      (write accounts account {
        "balance": (+ current-balance amount),
        "guard": guard,
        "created": (at 'block-time (chain-data)),
        "last-tx": (at 'block-time (chain-data))
      })))
    ;; Update total supply
    (update supply-table "supply" 
      { "total": (+ (total-supply) amount) })
    (format "Minted {} to {}" [amount account]))
  
  ;; Query functions
  (defun get-account-details:object (account:string)
    @doc "Get complete account information"
    (read accounts account))
  
  (defun get-accounts:[string] ()
    @doc "Get all account names"
    (keys accounts))
)

;; Initialize tables
(create-table accounts)
(create-table supply-table)

;; Initialize supply tracker
(my-token.init-supply)