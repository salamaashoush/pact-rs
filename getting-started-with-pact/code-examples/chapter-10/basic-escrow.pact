;; basic-escrow.pact
;; Simple escrow service using pacts

(module basic-escrow GOVERNANCE
  @doc "Basic escrow service with timeout and dispute resolution"

  (defcap GOVERNANCE ()
    (enforce-keyset 'escrow-admin))

  ;; Escrow account schema
  (defschema escrow-account
    @doc "Escrow account information"
    buyer:string
    seller:string
    amount:decimal
    created:time
    timeout:time
    status:string
    arbitrator:string)

  (deftable escrows:{escrow-account})

  ;; Events
  (defcap ESCROW_CREATED:bool (escrow-id:string buyer:string seller:string amount:decimal)
    @doc "Escrow creation event"
    @event
    true)

  (defcap ESCROW_COMPLETED:bool (escrow-id:string)
    @doc "Escrow completion event" 
    @event
    true)

  (defcap ESCROW_REFUNDED:bool (escrow-id:string)
    @doc "Escrow refund event"
    @event
    true)

  ;; Helper functions
  (defun create-escrow-account:string (escrow-id:string)
    @doc "Create deterministic escrow account name"
    (create-principal (create-capability-guard (ESCROW_GUARD escrow-id))))

  (defcap ESCROW_GUARD:bool (escrow-id:string)
    @doc "Guard capability for escrow account"
    true)

  ;; Main escrow pact
  (defpact create-escrow (escrow-id:string buyer:string seller:string 
                         amount:decimal timeout-hours:integer arbitrator:string)
    @doc "Create escrow with timeout and arbitrator"

    ;; Step 0: Initialize escrow
    (step
      (do
        (enforce (> amount 0.0) "Amount must be positive")
        (enforce (> timeout-hours 0) "Timeout must be positive")
        (enforce (!= buyer seller) "Buyer and seller must be different")
        (enforce (!= buyer arbitrator) "Buyer and arbitrator must be different")
        (enforce (!= seller arbitrator) "Seller and arbitrator must be different")

        (let ((timeout-time (add-time (at 'block-time (chain-data)) (hours timeout-hours)))
              (escrow-account (create-escrow-account escrow-id)))

          ;; Transfer funds from buyer to escrow account
          (install-capability (coin.TRANSFER buyer escrow-account amount))
          (coin.transfer-create buyer escrow-account 
                               (create-capability-guard (ESCROW_GUARD escrow-id)) 
                               amount)

          ;; Record escrow details
          (insert escrows escrow-id {
            "buyer": buyer,
            "seller": seller,
            "amount": amount,
            "created": (at 'block-time (chain-data)),
            "timeout": timeout-time,
            "status": "active",
            "arbitrator": arbitrator
          })

          ;; Emit creation event
          (emit-event (ESCROW_CREATED escrow-id buyer seller amount))

          (format "Escrow {} created: {} from {} to {} (timeout: {})" 
                  [escrow-id amount buyer seller timeout-time])))))

    ;; Step 1: Complete or refund escrow
    (step-with-rollback
      ;; Normal completion: release to seller
      (do
        (with-read escrows escrow-id { 
          "status" := status,
          "seller" := seller-account,
          "amount" := escrow-amount
        }
          (enforce (= status "active") "Escrow not active")

          (let ((escrow-account (create-escrow-account escrow-id)))
            ;; Transfer funds to seller
            (with-capability (ESCROW_GUARD escrow-id)
              (install-capability (coin.TRANSFER escrow-account seller-account escrow-amount))
              (coin.transfer escrow-account seller-account escrow-amount))

            ;; Update status
            (update escrows escrow-id { "status": "completed" })

            ;; Emit completion event
            (emit-event (ESCROW_COMPLETED escrow-id))

            (format "Escrow {} completed: {} released to {}" 
                    [escrow-id escrow-amount seller-account]))))

      ;; Rollback: refund to buyer on timeout or dispute
      (do
        (with-read escrows escrow-id { 
          "timeout" := timeout-time,
          "buyer" := buyer-account,
          "amount" := escrow-amount,
          "status" := status
        }
          (enforce (= status "active") "Escrow not active")
          
          ;; Check if timeout reached OR arbitrator initiated rollback
          (enforce-one "Timeout reached or arbitrator decision"
            [(enforce (>= (at 'block-time (chain-data)) timeout-time) "Timeout not reached")
             (enforce (= (tx-sender) arbitrator) "Only arbitrator can force refund")])

          (let ((escrow-account (create-escrow-account escrow-id)))
            ;; Refund to buyer
            (with-capability (ESCROW_GUARD escrow-id)
              (install-capability (coin.TRANSFER escrow-account buyer-account escrow-amount))
              (coin.transfer escrow-account buyer-account escrow-amount))

            ;; Update status
            (update escrows escrow-id { "status": "refunded" })

            ;; Emit refund event
            (emit-event (ESCROW_REFUNDED escrow-id))

            (format "Escrow {} refunded: {} returned to {}" 
                    [escrow-id escrow-amount buyer-account])))))))

    ;; Step 2: Final confirmation step
    (step
      (do
        (with-read escrows escrow-id { "status" := final-status }
          (format "Escrow {} finalized with status: {}" [escrow-id final-status])))))

  ;; Query functions
  (defun get-escrow:object (escrow-id:string)
    @doc "Get escrow details"
    (read escrows escrow-id))

  (defun get-escrow-status:string (escrow-id:string)
    @doc "Get escrow status"
    (at 'status (read escrows escrow-id)))

  (defun get-active-escrows:[object] ()
    @doc "Get all active escrows"
    (select escrows (where 'status (= "active"))))

  (defun get-escrows-by-buyer:[object] (buyer:string)
    @doc "Get escrows for specific buyer"
    (select escrows (where 'buyer (= buyer))))

  (defun get-escrows-by-seller:[object] (seller:string)
    @doc "Get escrows for specific seller"
    (select escrows (where 'seller (= seller))))

  ;; Administrative functions
  (defun force-refund:string (escrow-id:string reason:string)
    @doc "Force refund by arbitrator"
    (with-read escrows escrow-id { "arbitrator" := arb }
      (enforce (= (tx-sender) arb) "Only arbitrator can force refund")
      
      ;; Continue pact with rollback
      (continue-pact (at 'pact-id (describe-pact escrow-id)) true)
      
      ;; Log the forced refund
      (insert forced-refunds escrow-id {
        "reason": reason,
        "timestamp": (at 'block-time (chain-data)),
        "arbitrator": arb
      })
      
      (format "Forced refund executed for {} by arbitrator: {}" [escrow-id reason])))

  ;; Refund log schema and table
  (defschema refund-log
    reason:string
    timestamp:time
    arbitrator:string)

  (deftable forced-refunds:{refund-log})
)

;; Initialize tables
(create-table escrows)
(create-table forced-refunds)