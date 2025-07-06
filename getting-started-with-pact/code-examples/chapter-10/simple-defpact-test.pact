;; Simple defpact test
(module simple-defpact-test GOVERNANCE
  (defcap GOVERNANCE () true)
  
  (defschema test-data
    value:string)
  
  (deftable test-table:{test-data})
  
  (defpact test-pact (id:string)
    (step
      (do
        (insert test-table id {"value": "step0"})
        "Step 0 complete"))
    
    (step-with-rollback
      (do
        (update test-table id {"value": "step1"})
        "Step 1 complete")
      (do
        (update test-table id {"value": "rollback"})
        "Rollback complete")))
)

(create-table test-table)