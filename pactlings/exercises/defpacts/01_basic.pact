;; Exercise: Basic Defpacts
;; Learn multi-step transactions with defpacts

;; Setup
(begin-tx "setup")
(env-data { "module-keyset": ["admin-key"] })
(env-keys ["admin-key"])
(define-keyset 'module-keyset (read-keyset "module-keyset"))

(module defpact-exercise 'module-keyset
  @doc "Multi-step transaction examples"
  
  (defschema order
    buyer:string
    seller:string
    amount:decimal
    status:string)
  
  (deftable orders:{order})
  
  ;; TODO: Complete the step that creates an order
  ;; Replace "I AM NOT DONE" with: (insert orders order-id { "buyer": buyer, "seller": seller, "amount": amount, "status": "created" })
  (defpact simple-trade (order-id:string buyer:string seller:string amount:decimal)
    @doc "Simple two-step trade"
    
    ;; Step 1: Create order
    (step
      I AM NOT DONE)
    
    ;; Step 2: Complete trade
    (step
      (update orders order-id { "status": "completed" })
      "Trade completed"))
  
  ;; Helper function to get order
  (defun get-order (order-id:string)
    @doc "Get order information"
    (read orders order-id)))

;; Create the table
(create-table orders)
(commit-tx)

;; Testing
(begin-tx "test-defpacts")
(use defpact-exercise)

;; Start a pact
(simple-trade "order-1" "alice" "bob" 100.0)

;; Check that step 1 created the order
(expect "Order created" "created" (at "status" (get-order "order-1")))

;; Continue to step 2
(continue-pact 1)

;; Check that step 2 completed the order
(expect "Order completed" "completed" (at "status" (get-order "order-1")))

(commit-tx)