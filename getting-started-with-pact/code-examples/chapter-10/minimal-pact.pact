;; Minimal pact test
(module test-pact GOVERNANCE
  (defcap GOVERNANCE () true)
  
  (defschema dummy
    data:string)
  
  (deftable dummy-table:{dummy})
  
  (defpact simple-pact (param:string)
    (step
      (do
        (enforce (!= param "") "param cannot be empty")
        "Step 0 complete")))
)

(create-table dummy-table)