(module zombie-coin GOV

  (implements fungible-v2)
  
  (defcap GOV () 
    (enforce-guard (read-keyset "zombie-coin-admin")))

  ; Schemas
  (defschema account
    balance:decimal
    guard:guard)

  (deftable accounts-table:{account})

  ; Constants
  (defconst COIN_ID "zombie-coin")
  (defconst MINIMUM_PRECISION 12)
  (defconst MINIMUM_ACCOUNT_LENGTH 3)
  (defconst MAXIMUM_ACCOUNT_LENGTH 256)

  ; Capabilities
  (defcap DEBIT (sender:string)
    @doc "Capability for debiting an account"
    (enforce-guard (at "guard" (read accounts-table sender))))

  (defcap CREDIT (receiver:string) 
    @doc "Capability for crediting an account"
    true)

  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
    @doc "Capability for transferring tokens"
    @managed amount TRANSFER-mgr
    (enforce (!= sender receiver) "Sender and receiver must differ")
    (enforce (> amount 0.0) "Amount must be positive")
    (enforce-unit amount)
    (with-capability (DEBIT sender)
      (debit sender amount))
    (with-capability (CREDIT receiver)
      (credit receiver amount)))

  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    @doc "Manager for TRANSFER capability"
    (let ((balance (- managed requested)))
      (enforce (>= balance 0.0) "Transfer quantity exhausted")
      balance))

  ; Utility Functions
  (defun enforce-unit:bool (amount:decimal)
    @doc "Enforce minimum precision"
    (enforce (= (floor amount MINIMUM_PRECISION) amount) 
      "Amount precision must match token decimals")
    true)

  (defun validate-account (account:string)
    @doc "Validate account string format"
    (enforce (>= (length account) MINIMUM_ACCOUNT_LENGTH) 
      "Account name too short")
    (enforce (<= (length account) MAXIMUM_ACCOUNT_LENGTH) 
      "Account name too long"))

  ; Account Management
  (defun create-account:string (account:string guard:guard)
    @doc "Create a new ZMB account"
    (validate-account account)
    (insert accounts-table account
      { "balance": 0.0
      , "guard": guard
      })
    (format "Account {} created" [account]))

  (defun get-balance:decimal (account:string)
    @doc "Get account balance"
    (at "balance" (read accounts-table account)))

  (defun details:object{fungible-v2.account-details} (account:string)
    @doc "Get account details"
    (with-read accounts-table account 
      { "balance" := balance
      , "guard" := guard
      }
      { "account": account
      , "balance": balance
      , "guard": guard
      }))

  (defun rotate:string (account:string new-guard:guard)
    @doc "Rotate account guard"
    (with-capability (DEBIT account)
      (update accounts-table account { "guard": new-guard })
      (format "Guard rotated for account {}" [account])))

  ; Internal Functions
  (defun debit:decimal (account:string amount:decimal)
    @doc "Debit from account, requiring DEBIT capability"
    (require-capability (DEBIT account))
    (with-read accounts-table account { "balance" := balance }
      (enforce (>= balance amount) "Insufficient funds")
      (let ((new-balance (- balance amount)))
        (update accounts-table account { "balance": new-balance })
        new-balance)))

  (defun credit:decimal (account:string amount:decimal)
    @doc "Credit to account, requiring CREDIT capability"
    (require-capability (CREDIT account))
    (with-read accounts-table account { "balance" := balance }
      (let ((new-balance (+ balance amount)))
        (update accounts-table account { "balance": new-balance })
        new-balance)))

  ; Public Transfer Functions
  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer ZMB between accounts"
    (with-capability (TRANSFER sender receiver amount)
      (format "Transferred {} ZMB from {} to {}" [amount sender receiver])))

  (defun transfer-create:string 
    (sender:string receiver:string receiver-guard:guard amount:decimal)
    @doc "Transfer ZMB, creating receiver account if needed"
    (with-capability (TRANSFER sender receiver amount)
      (let ((receiver-exists (try false (let ((ok true)) 
        (with-read accounts-table receiver { "balance" := b } ok)))))
        (if (not receiver-exists)
          (create-account receiver receiver-guard)
          "Account exists"))
      (format "Transferred {} ZMB from {} to {}" [amount sender receiver])))

  ; Token Information
  (defun precision:integer ()
    @doc "Return token precision"
    MINIMUM_PRECISION)

  (defun get-coin-id:string ()
    @doc "Return token identifier"
    COIN_ID)

  ; Special Accounts Setup
  (defun init-special-accounts ()
    @doc "Initialize special system accounts"
    (with-capability (GOV)
      (create-account "battle-pool" (read-keyset "battle-pool-ks"))
      (create-account "rewards-pool" (read-keyset "rewards-pool-ks"))
      (create-account "staking-pool" (read-keyset "staking-pool-ks"))
      (create-account "fee-collector" (read-keyset "fee-collector-ks"))
      (create-account "treasury" (read-keyset "treasury-ks"))
      "Special accounts initialized"))

  ; Initial Supply Distribution
  (defun mint:string (account:string amount:decimal)
    @doc "Mint new tokens (admin only)"
    (with-capability (GOV)
      (with-capability (CREDIT account)
        (credit account amount)
        (format "Minted {} ZMB to {}" [amount account]))))
)