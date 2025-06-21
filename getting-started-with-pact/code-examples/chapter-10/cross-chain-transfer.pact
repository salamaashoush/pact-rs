;; cross-chain-transfer.pact
;; Cross-chain token transfer using yield and resume

(module cross-chain-transfer GOVERNANCE
  @doc "Cross-chain token transfer with yield/resume"

  (defcap GOVERNANCE ()
    (enforce-keyset 'xchain-admin))

  ;; Cross-chain transfer record
  (defschema xchain-transfer
    @doc "Cross-chain transfer record"
    from:string
    to:string  
    amount:decimal
    source-chain:string
    target-chain:string
    initiated:time
    completed:time
    status:string
    tx-hash:string)

  (deftable xchain-transfers:{xchain-transfer})

  ;; Burn record for source chain
  (defschema burn-record
    account:string
    amount:decimal
    target-chain:string
    burned-at:time
    pact-id:string)

  (deftable burn-records:{burn-record})

  ;; Mint record for target chain
  (defschema mint-record
    account:string
    amount:decimal
    source-chain:string
    minted-at:time
    pact-id:string
    source-tx:string)

  (deftable mint-records:{mint-record})

  ;; Events
  (defcap XCHAIN_INITIATED:bool (pact-id:string from:string to:string amount:decimal target-chain:string)
    @doc "Cross-chain transfer initiated"
    @event
    true)

  (defcap TOKENS_BURNED:bool (account:string amount:decimal target-chain:string)
    @doc "Tokens burned for cross-chain transfer"
    @event
    true)

  (defcap TOKENS_MINTED:bool (account:string amount:decimal source-chain:string)
    @doc "Tokens minted from cross-chain transfer"
    @event
    true)

  (defcap XCHAIN_COMPLETED:bool (pact-id:string)
    @doc "Cross-chain transfer completed"
    @event
    true)

  ;; Cross-chain transfer pact
  (defpact transfer-crosschain (transfer-id:string from:string to:string 
                               amount:decimal target-chain:string)
    @doc "Transfer tokens across chains using yield/resume"

    ;; Step 0: Burn tokens on source chain
    (step
      (enforce (> amount 0.0) "Amount must be positive")
      (enforce (!= target-chain (chain-data 'chain-id)) "Target must be different chain")
      (enforce (!= from "") "From account cannot be empty")
      (enforce (!= to "") "To account cannot be empty")

      (let ((source-chain (chain-data 'chain-id))
            (current-time (chain-data 'time))
            (tx-hash (hash (chain-data 'tx-hash))))

        ;; Burn tokens from source account
        ;; Note: This assumes a burnable token implementation
        (coin.transfer from "burn-account" amount)

        ;; Record the burn
        (insert burn-records transfer-id {
          "account": from,
          "amount": amount,
          "target-chain": target-chain,
          "burned-at": current-time,
          "pact-id": (pact-id)
        })

        ;; Record transfer initiation
        (insert xchain-transfers transfer-id {
          "from": from,
          "to": to,
          "amount": amount,
          "source-chain": source-chain,
          "target-chain": target-chain,
          "initiated": current-time,
          "completed": (time "1970-01-01T00:00:00Z"),
          "status": "pending",
          "tx-hash": tx-hash
        })

        ;; Emit events
        (emit-event (TOKENS_BURNED from amount target-chain))
        (emit-event (XCHAIN_INITIATED (pact-id) from to amount target-chain))

        ;; Yield data to target chain
        (yield {
          "transfer-id": transfer-id,
          "from": from,
          "to": to,
          "amount": amount,
          "source-chain": source-chain,
          "source-tx": tx-hash,
          "burned-at": current-time
        } target-chain)

        (format "Burned {} tokens for cross-chain transfer to {}" [amount target-chain])))

    ;; Step 1: Mint tokens on target chain
    (step
      ;; Resume with yielded data from source chain
      (resume { 
        "transfer-id" := xfer-id,
        "from" := source-account,
        "to" := target-account,
        "amount" := xfer-amount,
        "source-chain" := src-chain,
        "source-tx" := src-tx,
        "burned-at" := burn-time
      }
        ;; Validate the yielded data
        (enforce (= xfer-id transfer-id) "Transfer ID mismatch")
        (enforce (= src-chain (read-string "expected-source-chain")) "Invalid source chain")
        (enforce (> xfer-amount 0.0) "Invalid transfer amount")

        (let ((target-chain (chain-data 'chain-id))
              (current-time (chain-data 'time)))

          ;; Mint tokens to target account
          ;; Note: This assumes a mintable token implementation
          (coin.mint target-account xfer-amount)

          ;; Record the mint
          (insert mint-records xfer-id {
            "account": target-account,
            "amount": xfer-amount,
            "source-chain": src-chain,
            "minted-at": current-time,
            "pact-id": (pact-id),
            "source-tx": src-tx
          })

          ;; Update transfer record
          (update xchain-transfers xfer-id {
            "completed": current-time,
            "status": "completed"
          })

          ;; Emit events
          (emit-event (TOKENS_MINTED target-account xfer-amount src-chain))
          (emit-event (XCHAIN_COMPLETED (pact-id)))

          (format "Minted {} tokens to {} from cross-chain transfer" 
                  [xfer-amount target-account]))))

  ;; Query functions
  (defun get-transfer:object (transfer-id:string)
    @doc "Get transfer details"
    (read xchain-transfers transfer-id))

  (defun get-transfer-status:string (transfer-id:string)
    @doc "Get transfer status"
    (at 'status (read xchain-transfers transfer-id)))

  (defun get-pending-transfers:[object] ()
    @doc "Get all pending transfers"
    (select xchain-transfers (where 'status (= "pending"))))

  (defun get-transfers-by-account:[object] (account:string)
    @doc "Get transfers involving specific account"
    (+ (select xchain-transfers (where 'from (= account)))
       (select xchain-transfers (where 'to (= account)))))

  (defun get-burn-record:object (transfer-id:string)
    @doc "Get burn record for transfer"
    (read burn-records transfer-id))

  (defun get-mint-record:object (transfer-id:string)
    @doc "Get mint record for transfer"
    (read mint-records transfer-id))

  ;; Administrative functions
  (defun get-chain-stats:object (chain-id:string)
    @doc "Get cross-chain transfer statistics for a chain"
    (let ((outbound (select xchain-transfers (where 'target-chain (= chain-id))))
          (inbound (select xchain-transfers (where 'source-chain (= chain-id)))))
      {
        "chain-id": chain-id,
        "outbound-count": (length outbound),
        "inbound-count": (length inbound),
        "outbound-volume": (fold (+) 0.0 (map (at 'amount) outbound)),
        "inbound-volume": (fold (+) 0.0 (map (at 'amount) inbound))
      }))

  (defun list-active-pacts:[object] ()
    @doc "List all active cross-chain pacts"
    (select xchain-transfers (where 'status (= "pending"))))

  ;; Utility functions for testing
  (defun simulate-spv-proof:object (transfer-id:string)
    @doc "Create mock SPV proof for testing"
    {
      "transfer-id": transfer-id,
      "proof-type": "burn-proof",
      "chain-id": (chain-data 'chain-id),
      "block-height": (chain-data 'block-height),
      "timestamp": (chain-data 'time)
    })

  (defun validate-spv-proof:bool (proof:object expected-chain:string)
    @doc "Validate SPV proof (mock implementation)"
    (and (= (at 'chain-id proof) expected-chain)
         (> (at 'block-height proof) 0)))

  ;; Emergency functions
  (defcap EMERGENCY:bool ()
    @doc "Emergency capability"
    (enforce-keyset 'emergency-keyset))

  (defun emergency-cancel:string (transfer-id:string reason:string)
    @doc "Emergency cancel of pending transfer"
    (with-capability (EMERGENCY)
      (with-read xchain-transfers transfer-id { "status" := status }
        (enforce (= status "pending") "Transfer not pending")
        
        (update xchain-transfers transfer-id { 
          "status": "cancelled",
          "completed": (chain-data 'time)
        })
        
        ;; Log the cancellation
        (insert cancellation-log transfer-id {
          "reason": reason,
          "cancelled-at": (chain-data 'time),
          "cancelled-by": (tx-sender)
        })
        
        (format "Transfer {} cancelled: {}" [transfer-id reason]))))

  ;; Cancellation log
  (defschema cancellation
    reason:string
    cancelled-at:time
    cancelled-by:string)

  (deftable cancellation-log:{cancellation})
)

;; Initialize tables
(create-table xchain-transfers)
(create-table burn-records)
(create-table mint-records)
(create-table cancellation-log)