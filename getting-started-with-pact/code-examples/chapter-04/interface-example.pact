;; Interface Example
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
    created:time     ;; Additional field allowed
    last-tx:time)    ;; Additional field allowed
  
  (deftable accounts:{account})
  
  ;; Track total supply for conservation property
  (defschema supply-info
    total:decimal)
  
  (deftable supply:{supply-info})
  
  ;; Initialize total supply
  (defun initialize ()
    (insert supply "total" { "total": 0.0 }))
  
  ;; Required capability (must match interface)
  (defcap TRANSFER (from:string to:string amount:decimal)
    @doc "Transfer capability"
    @managed amount TRANSFER-mgr
    (enforce-guard (at 'guard (read accounts from)))
    (enforce (> amount 0.0) "Amount must be positive"))
  
  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    (enforce (>= managed requested) "Transfer amount exceeded")
    (- managed requested))
  
  ;; Required functions (must match interface signatures)
  (defun transfer:string (from:string to:string amount:decimal)
    @doc "Transfer tokens between accounts"
    (with-capability (TRANSFER from to amount)
      (with-read accounts from { "balance" := from-balance }
        (enforce (>= from-balance amount) "Insufficient balance")
        (update accounts from { 
          "balance": (- from-balance amount),
          "last-tx": (time)
        })
        (with-default-read accounts to
          { "balance": 0.0, "guard": (keyset-ref-guard 'default), 
            "created": (time), "last-tx": (time) }
          { "balance" := to-balance }
          (write accounts to {
            "balance": (+ to-balance amount),
            "guard": (keyset-ref-guard 'default),
            "created": (time),
            "last-tx": (time)
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
      "created": (time),
      "last-tx": (time)
    })
    (format "Account {} created" [account]))
  
  ;; Additional functions not in interface (allowed)
  (defun mint:string (account:string amount:decimal)
    @doc "Mint tokens (not in interface)"
    (enforce-keyset 'token-admin)
    (with-default-read accounts account
      { "balance": 0.0, "guard": (keyset-ref-guard 'default),
        "created": (time), "last-tx": (time) }
      { "balance" := current-balance }
      (write accounts account {
        "balance": (+ current-balance amount),
        "guard": (keyset-ref-guard 'default),
        "created": (time),
        "last-tx": (time)
      }))
    ;; Update total supply
    (with-read supply "total" { "total" := current-total }
      (update supply "total" { "total": (+ current-total amount) }))
    (format "Minted {} tokens to {}" [amount account]))
  
  (defun total-supply:decimal ()
    @doc "Get total token supply"
    (at 'total (read supply "total")))
  
  (defun burn:string (account:string amount:decimal)
    @doc "Burn tokens"
    (enforce-keyset 'token-admin)
    (with-read accounts account { "balance" := balance }
      (enforce (>= balance amount) "Insufficient balance to burn")
      (update accounts account { 
        "balance": (- balance amount),
        "last-tx": (time)
      }))
    ;; Update total supply
    (with-read supply "total" { "total" := current-total }
      (update supply "total" { "total": (- current-total amount) }))
    (format "Burned {} tokens from {}" [amount account]))
)

;; Usage example showing polymorphism
(module token-utils 'utils-admin
  @doc "Utilities that work with any fungible-token-v2"
  
  ;; This function works with any token implementing the interface
  (defun bulk-transfer (token-module:module{fungible-token-v2} 
                       transfers:[object])
    @doc "Perform bulk transfers using any fungible token"
    (map (lambda (transfer-info)
           (let ((from (at "from" transfer-info))
                 (to (at "to" transfer-info))
                 (amount (at "amount" transfer-info)))
             (token-module::transfer from to amount)))
         transfers))
  
  (defun check-balance (token-module:module{fungible-token-v2} 
                       account:string)
    @doc "Check balance in any fungible token"
    (token-module::get-balance account))
)