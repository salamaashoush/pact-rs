;; simple-coin.pact
;; Basic implementation of fungible-v2 interface

(module simple-coin GOVERNANCE
  @doc "Simple coin implementation of fungible-v2"

  ;; Implement the fungible interface
  (implements fungible-v2)

  ;; Governance capability
  (defcap GOVERNANCE ()
    @doc "Only admin can upgrade contract"
    (enforce-keyset "coin-admin"))

  ;; Account schema (required by interface)
  (defschema account
    balance:decimal
    guard:guard)

  (deftable accounts:{account})

  ;; Constants
  (defconst COIN_CHARSET CHARSET_LATIN1
    @doc "Charset for coin operations")

  (defconst MINIMUM_PRECISION 12
    @doc "Minimum precision for amounts")

  ;; Events
  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
    @doc "Transfer capability and event"
    @managed amount TRANSFER-mgr
    (enforce (!= sender receiver) "same sender and receiver")
    (enforce (> amount 0.0) "transfer amount must be positive")
    (enforce-valid-account sender)
    (enforce-valid-account receiver)
    (enforce-unit amount)
    (enforce-guard (account-guard sender)))

  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    @doc "Manages TRANSFER capability"
    (enforce (<= requested managed) "requested exceeds managed")
    (- managed requested))

  (defcap DEBIT:bool (sender:string)
    @doc "Capability to debit from account"
    (enforce-guard (account-guard sender)))

  (defcap CREDIT:bool (receiver:string)
    @doc "Capability to credit to account"
    (enforce-valid-account receiver))

  ;; Utility functions
  (defun enforce-unit:bool (amount:decimal)
    @doc "Enforce minimum precision"
    (enforce
      (= (floor amount MINIMUM_PRECISION)
         amount)
      (format "Amount violates minimum precision: {}" [amount])))

  (defun enforce-valid-account:bool (account:string)
    @doc "Enforce valid account name"
    (enforce
      (is-charset COIN_CHARSET account)
      (format "Account does not conform to charset: {}" [account])))

  (defun account-guard:guard (account:string)
    @doc "Get guard for account"
    (at 'guard (read accounts account)))

  ;; Core fungible functions
  (defun create-account:string (account:string guard:guard)
    @doc "Create new account"
    (enforce-valid-account account)
    (insert accounts account {
      "balance": 0.0,
      "guard": guard
    }))

  (defun get-balance:decimal (account:string)
    @doc "Get account balance"
    (at 'balance (read accounts account)))

  (defun details:object{fungible-v2.account} (account:string)
    @doc "Get account details"
    (read accounts account))

  (defun rotate:string (account:string new-guard:guard)
    @doc "Rotate account guard"
    (with-read accounts account
      { "guard" := old-guard }
      (enforce-guard old-guard)
      (update accounts account { "guard": new-guard })))

  (defun precision:integer ()
    @doc "Return maximum precision for coin"
    MINIMUM_PRECISION)

  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer tokens between accounts"
    (enforce (!= sender receiver) "same sender and receiver")
    (enforce (> amount 0.0) "transfer amount must be positive")
    (enforce-unit amount)
    (with-capability (TRANSFER sender receiver amount)
      (with-capability (DEBIT sender)
        (debit sender amount))
      (with-capability (CREDIT receiver)
        (credit receiver amount))))

  (defun transfer-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
    @doc "Transfer tokens, creating receiver account if necessary"
    (enforce (!= sender receiver) "same sender and receiver")
    (enforce (> amount 0.0) "transfer amount must be positive")
    (enforce-unit amount)
    (with-capability (TRANSFER sender receiver amount)
      (with-capability (DEBIT sender)
        (debit sender amount))
      (with-capability (CREDIT receiver)
        (credit-account receiver receiver-guard amount))))

  (defun debit:string (account:string amount:decimal)
    @doc "Debit amount from account"
    (require-capability (DEBIT account))
    (with-read accounts account
      { "balance" := balance }
      (enforce (<= amount balance) "Insufficient funds")
      (update accounts account
        { "balance": (- balance amount) })))

  (defun credit:string (account:string amount:decimal)
    @doc "Credit amount to existing account"
    (require-capability (CREDIT account))
    (with-read accounts account
      { "balance" := balance }
      (update accounts account
        { "balance": (+ balance amount) })))

  (defun credit-account:string (account:string guard:guard amount:decimal)
    @doc "Credit amount to account, creating if necessary"
    (require-capability (CREDIT account))
    (with-default-read accounts account
      { "balance": -1.0, "guard": guard }
      { "balance" := balance, "guard" := retg }
      (enforce (= retg guard) "account guards do not match")
      (let ((is-new (if (= balance -1.0) (enforce-valid-account account) false)))
        (write accounts account
          { "balance": (if is-new amount (+ balance amount))
          , "guard": retg
          }))))

  ;; Supply management
  (defschema supply
    @doc "Supply schema"
    supply:decimal)

  (deftable supply-table:{supply})

  (defun total-supply:decimal ()
    @doc "Get total supply"
    (with-default-read supply-table "total"
      { "supply": 0.0 }
      { "supply" := s }
      s))

  (defun get-supply:decimal ()
    @doc "Alias for total-supply (interface requirement)"
    (total-supply))

  ;; Minting (admin only)
  (defcap MINT:bool (account:string amount:decimal)
    @doc "Capability to mint tokens"
    (compose-capability (GOVERNANCE))
    (compose-capability (CREDIT account)))

  (defun mint:string (account:string guard:guard amount:decimal)
    @doc "Mint tokens to account"
    (enforce (> amount 0.0) "mint amount must be positive")
    (enforce-unit amount)
    (with-capability (MINT account amount)
      (credit-account account guard amount)
      (increase-supply amount)))

  (defun increase-supply:string (amount:decimal)
    @doc "Increase total supply"
    (require-capability (GOVERNANCE))
    (with-default-read supply-table "total"
      { "supply": 0.0 }
      { "supply" := s }
      (write supply-table "total"
        { "supply": (+ s amount) })))

  ;; Burning
  (defcap BURN:bool (account:string amount:decimal)
    @doc "Capability to burn tokens"
    (compose-capability (DEBIT account)))

  (defun burn:string (account:string amount:decimal)
    @doc "Burn tokens from account"
    (enforce (> amount 0.0) "burn amount must be positive")
    (enforce-unit amount)
    (with-capability (BURN account amount)
      (debit account amount)
      (decrease-supply amount)))

  (defun decrease-supply:string (amount:decimal)
    @doc "Decrease total supply"
    (require-capability (GOVERNANCE))
    (with-default-read supply-table "total"
      { "supply": 0.0 }
      { "supply" := s }
      (enforce (>= s amount) "insufficient supply to burn")
      (write supply-table "total"
        { "supply": (- s amount) })))
)

;; Initialize tables
(create-table accounts)
(create-table supply-table)