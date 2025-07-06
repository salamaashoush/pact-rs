;; Debug first step structure
(module debug-step GOVERNANCE
  (defcap GOVERNANCE () true)
  
  (defschema test-escrow
    buyer:string
    status:string)
  
  (deftable escrows:{test-escrow})
  
  (defpact test-step (escrow-id:string buyer:string amount:decimal)
    (step
      (do
        (enforce (> amount 0.0) "Amount must be positive")
        (insert escrows escrow-id {
          "buyer": buyer,
          "status": "active"
        })
        (format "Escrow {} created for amount {}" [escrow-id amount])))
    
    (step
      (do
        (update escrows escrow-id {"status": "completed"})
        "Final step")))
)

(create-table escrows)