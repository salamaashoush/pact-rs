;; Large test contract for benchmarking
(module large-bank 'large-bank-admin
  
  ;; Schemas
  (defschema account
    balance:decimal
    guard:guard
    last-transaction:time
    status:string)
    
  (defschema transaction-log
    from:string
    to:string
    amount:decimal
    timestamp:time
    tx-id:string)
    
  (defschema exchange-rate
    from-currency:string
    to-currency:string
    rate:decimal
    updated:time)
  
  ;; Tables
  (deftable accounts:{account})
  (deftable transactions:{transaction-log})
  (deftable rates:{exchange-rate})
  
  ;; Capabilities
  (defcap GOVERNANCE ()
    (enforce-guard (keyset-ref-guard 'large-bank-admin)))
    
  (defcap TRANSFER (from:string to:string amount:decimal)
    @managed amount TRANSFER-mgr
    (enforce-valid-account from)
    (enforce-valid-account to)
    (enforce (> amount 0.0) "Amount must be positive")
    (enforce (!= from to) "Cannot transfer to self"))
    
  (defcap WITHDRAW (account:string amount:decimal)
    (enforce-valid-account account)
    (enforce (> amount 0.0) "Amount must be positive"))
    
  (defcap DEPOSIT (account:string amount:decimal)
    (enforce-valid-account account)
    (enforce (> amount 0.0) "Amount must be positive"))
    
  (defcap ADMIN ()
    (enforce-guard (keyset-ref-guard 'large-bank-admin)))
    
  (defcap EXCHANGE (from-currency:string to-currency:string amount:decimal)
    (enforce (> amount 0.0) "Amount must be positive")
    (enforce (!= from-currency to-currency) "Currencies must be different"))
  
  ;; Capability manager
  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    (enforce (>= managed requested) "Insufficient transfer allowance")
    (- managed requested))
  
  ;; Utility functions
  (defun enforce-valid-account (account:string)
    (enforce (> (length account) 0) "Account cannot be empty")
    (enforce (< (length account) 256) "Account name too long"))
    
  (defun get-time:time ()
    (at 'block-time (chain-data)))
    
  (defun generate-tx-id:string (from:string to:string amount:decimal)
    (hash [from to amount (get-time)]))
    
  (defun account-exists:bool (account:string)
    (with-default-read accounts account { "balance": -1.0 } { "balance" := balance }
      (>= balance 0.0)))
  
  ;; Core banking functions
  (defun create-account:string (account:string guard:guard)
    (enforce (not (account-exists account)) "Account already exists")
    (insert accounts account {
      "balance": 0.0,
      "guard": guard,
      "last-transaction": (get-time),
      "status": "active" })
    (format "Account {} created" [account]))
    
  (defun get-balance:decimal (account:string)
    (enforce (account-exists account) "Account does not exist")
    (at 'balance (read accounts account)))
    
  (defun update-account-status:string (account:string new-status:string)
    (with-capability (ADMIN)
      (enforce (account-exists account) "Account does not exist")
      (update accounts account { "status": new-status })
      (format "Account {} status updated to {}" [account new-status])))
      
  (defun deposit:string (account:string amount:decimal)
    (with-capability (DEPOSIT account amount)
      (enforce (account-exists account) "Account does not exist")
      (with-read accounts account { "balance" := current-balance, "guard" := guard }
        (enforce-guard guard)
        (update accounts account {
          "balance": (+ current-balance amount),
          "last-transaction": (get-time) })
        (log-transaction "" account amount)
        (format "Deposited {} to account {}" [amount account]))))
        
  (defun withdraw:string (account:string amount:decimal)
    (with-capability (WITHDRAW account amount)
      (enforce (account-exists account) "Account does not exist")
      (with-read accounts account { "balance" := current-balance, "guard" := guard }
        (enforce-guard guard)
        (enforce (>= current-balance amount) "Insufficient balance")
        (update accounts account {
          "balance": (- current-balance amount),
          "last-transaction": (get-time) })
        (log-transaction account "" amount)
        (format "Withdrew {} from account {}" [amount account]))))
        
  (defun transfer:string (from:string to:string amount:decimal)
    (with-capability (TRANSFER from to amount)
      (enforce (account-exists from) "From account does not exist")
      (enforce (account-exists to) "To account does not exist")
      (with-read accounts from { "balance" := from-balance, "guard" := from-guard }
        (enforce-guard from-guard)
        (enforce (>= from-balance amount) "Insufficient balance")
        (update accounts from {
          "balance": (- from-balance amount),
          "last-transaction": (get-time) })
        (with-read accounts to { "balance" := to-balance }
          (update accounts to {
            "balance": (+ to-balance amount),
            "last-transaction": (get-time) }))
        (log-transaction from to amount)
        (format "Transferred {} from {} to {}" [amount from to]))))
        
  ;; Exchange functionality
  (defun set-exchange-rate:string (from-currency:string to-currency:string rate:decimal)
    (with-capability (ADMIN)
      (enforce (> rate 0.0) "Rate must be positive")
      (write rates (+ from-currency ":" to-currency) {
        "from-currency": from-currency,
        "to-currency": to-currency,
        "rate": rate,
        "updated": (get-time) })
      (format "Exchange rate set: {} to {} = {}" [from-currency to-currency rate])))
      
  (defun get-exchange-rate:decimal (from-currency:string to-currency:string)
    (at 'rate (read rates (+ from-currency ":" to-currency))))
    
  (defun exchange-currency:string (account:string from-currency:string to-currency:string amount:decimal)
    (with-capability (EXCHANGE from-currency to-currency amount)
      (let ((rate (get-exchange-rate from-currency to-currency))
            (converted-amount (* amount rate)))
        (format "Exchange: {} {} = {} {} at rate {}" [amount from-currency converted-amount to-currency rate]))))
  
  ;; Transaction logging
  (defun log-transaction:string (from:string to:string amount:decimal)
    (let ((tx-id (generate-tx-id from to amount)))
      (insert transactions tx-id {
        "from": from,
        "to": to,
        "amount": amount,
        "timestamp": (get-time),
        "tx-id": tx-id })
      tx-id))
      
  (defun get-transaction-history:[object{transaction-log}] (account:string)
    (select transactions ['from 'to 'amount 'timestamp 'tx-id]
      (or (= 'from account) (= 'to account))))
      
  ;; Batch operations
  (defun batch-transfer:string (transfers:[object])
    (map (lambda (transfer)
      (bind transfer { "from" := from, "to" := to, "amount" := amount }
        (transfer from to amount)))
      transfers)
    "Batch transfer completed")
    
  ;; Interest calculation
  (defun calculate-interest:decimal (balance:decimal rate:decimal days:integer)
    (let ((daily-rate (/ rate 365.0)))
      (* balance daily-rate (dec days))))
      
  (defun apply-interest:string (account:string rate:decimal days:integer)
    (with-capability (ADMIN)
      (with-read accounts account { "balance" := balance }
        (let ((interest (calculate-interest balance rate days)))
          (update accounts account { "balance": (+ balance interest) })
          (format "Applied interest of {} to account {}" [interest account])))))
          
  ;; Complex queries and analytics
  (defun get-total-balance:decimal ()
    (fold (+) 0.0 (map (at 'balance) (select accounts [] (where 'status (= "active"))))))
    
  (defun get-account-summary:object (account:string)
    (let ((account-data (read accounts account))
          (tx-history (get-transaction-history account)))
      { "account": account-data,
        "transaction-count": (length tx-history),
        "last-transaction": (at 'last-transaction account-data) }))
        
  ;; Administrative functions
  (defun freeze-account:string (account:string)
    (with-capability (ADMIN)
      (update-account-status account "frozen")))
      
  (defun unfreeze-account:string (account:string)
    (with-capability (ADMIN)
      (update-account-status account "active")))
      
  (defun close-account:string (account:string)
    (with-capability (ADMIN)
      (with-read accounts account { "balance" := balance }
        (enforce (= balance 0.0) "Account balance must be zero")
        (update-account-status account "closed"))))
        
  ;; Multi-signature operations
  (defun multi-sig-transfer:string (from:string to:string amount:decimal signatures:[string])
    (enforce (>= (length signatures) 2) "Minimum 2 signatures required")
    (with-capability (TRANSFER from to amount)
      (transfer from to amount)))
      
  ;; Compliance and reporting
  (defun compliance-report:object (start-time:time end-time:time)
    (with-capability (ADMIN)
      (let ((transactions-in-period (select transactions []
            (and (>= 'timestamp start-time) (<= 'timestamp end-time)))))
        { "period": { "start": start-time, "end": end-time },
          "transaction-count": (length transactions-in-period),
          "total-volume": (fold (+) 0.0 (map (at 'amount) transactions-in-period)) })))
)