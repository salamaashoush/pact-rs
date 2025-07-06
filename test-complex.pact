(module my-module GOV
  @doc "A test module"
  (use coin)
  
  (defcap GOV () 
    @doc "Governance capability"
    true)
    
  (defcap TRANSFER (from:string to:string amount:decimal)
    @managed amount TRANSFER-mgr
    (enforce-guard (at 'guard (coin.details from))))
    
  (defun TRANSFER-mgr:decimal (current:decimal requested:decimal)
    (enforce (<= requested current) "Cannot increase transfer amount"))
    
  (defschema account
    @doc "Account schema"
    balance:decimal
    guard:guard)
    
  (deftable accounts:{account})
  
  (defun transfer:string (from:string to:string amount:decimal)
    @doc "Transfer tokens between accounts"
    (with-capability (TRANSFER from to amount)
      (let ((from-bal (at 'balance (read accounts from)))
            (to-bal (at 'balance (read accounts to))))
        (enforce (>= from-bal amount) "Insufficient balance")
        (update accounts from { "balance": (- from-bal amount) })
        (update accounts to { "balance": (+ to-bal amount) })
        (format "Transferred {} from {} to {}" [amount from to]))))
        
  (defpact cross-chain-transfer (from:string to:string amount:decimal target-chain:string)
    @doc "Cross chain transfer pact"
    (step (withdraw from amount))
    (step-with-rollback 
      (deposit to amount)
      (refund from amount)))
)