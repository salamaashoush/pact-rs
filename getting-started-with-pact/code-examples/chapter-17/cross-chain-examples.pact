;; Chapter 17: Cross-Chain Communication and Bridge Integration
;; Examples demonstrating Hyperlane bridge integration and cross-chain patterns

(module cross-chain-token GOVERNANCE
  @doc "Cross-chain token with Hyperlane bridge integration"
  
  ;; ============================================================================
  ;; SCHEMAS AND TABLES
  ;; ============================================================================
  
  (defschema account
    balance:decimal
    guard:guard
    locked:decimal)
  
  (defschema bridge-message
    recipient:string
    amount:decimal
    origin-chain:string
    destination-chain:string
    message-id:string
    status:string
    timestamp:time)
  
  (defschema chain-config
    chain-id:string
    bridge-contract:string
    is-native:bool
    bridge-fee:decimal
    daily-limit:decimal
    used-today:decimal
    last-reset:time)
  
  (defschema processed-message
    message-id:string
    processed-at:time
    processor:string)
  
  (deftable accounts:{account})
  (deftable bridge-messages:{bridge-message})
  (deftable chain-configs:{chain-config})
  (deftable processed-messages:{processed-message})
  
  ;; ============================================================================
  ;; CAPABILITIES
  ;; ============================================================================
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'admin-keyset))
  
  (defcap BRIDGE_OPERATOR ()
    (enforce-keyset 'bridge-operator-keyset))
  
  (defcap TRANSFER (sender:string receiver:string amount:decimal)
    @managed amount TRANSFER-mgr
    (enforce-guard (at 'guard (read accounts sender))))
  
  (defcap CROSS_CHAIN_TRANSFER (sender:string destination:string amount:decimal)
    @managed amount TRANSFER-mgr
    (enforce-guard (at 'guard (read accounts sender))))
  
  (defcap CROSS_CHAIN_SEND (sender:string destination:string amount:decimal message-id:string)
    @event
    true)
  
  (defcap CROSS_CHAIN_RECEIVE (recipient:string amount:decimal origin:string message-id:string)
    @event
    true)
  
  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    (enforce (>= managed requested) "Insufficient balance")
    (- managed requested))
  
  ;; ============================================================================
  ;; CHAIN CONFIGURATION
  ;; ============================================================================
  
  (defun configure-chain:string (chain-id:string 
                                bridge-contract:string
                                is-native:bool
                                bridge-fee:decimal
                                daily-limit:decimal)
    @doc "Configure a chain for cross-chain operations"
    (with-capability (GOVERNANCE)
      (write chain-configs chain-id {
        "chain-id": chain-id,
        "bridge-contract": bridge-contract,
        "is-native": is-native,
        "bridge-fee": bridge-fee,
        "daily-limit": daily-limit,
        "used-today": 0.0,
        "last-reset": (at 'block-time (chain-data))
      })
      "Chain configured successfully"))
  
  (defun get-chain-config:object (chain-id:string)
    @doc "Get configuration for a specific chain"
    (read chain-configs chain-id))
  
  ;; ============================================================================
  ;; ACCOUNT MANAGEMENT
  ;; ============================================================================
  
  (defun create-account:string (account:string guard:guard)
    @doc "Create a new account"
    (insert accounts account {
      "balance": 0.0,
      "guard": guard,
      "locked": 0.0
    })
    "Account created")
  
  (defun get-balance:decimal (account:string)
    @doc "Get account balance"
    (at 'balance (read accounts account)))
  
  (defun credit:string (account:string guard:guard amount:decimal)
    @doc "Credit amount to account"
    (require-capability (GOVERNANCE))
    (with-default-read accounts account
      { "balance": 0.0, "guard": guard, "locked": 0.0 }
      { "balance" := balance }
      (write accounts account {
        "balance": (+ balance amount),
        "guard": guard,
        "locked": 0.0
      }))
    "Credit successful")
  
  (defun debit:string (account:string amount:decimal)
    @doc "Debit amount from account"
    (require-capability (TRANSFER account "" amount))
    (with-read accounts account { "balance" := balance }
      (enforce (>= balance amount) "Insufficient funds")
      (update accounts account {
        "balance": (- balance amount)
      }))
    "Debit successful")
  
  ;; ============================================================================
  ;; CROSS-CHAIN TRANSFER FUNCTIONS
  ;; ============================================================================
  
  (defun send-cross-chain:string (sender:string
                                 destination-chain:string
                                 recipient:string
                                 amount:decimal)
    @doc "Send tokens to another chain via Hyperlane"
    (with-read chain-configs destination-chain {
      "bridge-fee" := fee,
      "daily-limit" := limit,
      "used-today" := used,
      "last-reset" := last-reset
    }
      ;; Check and update daily limit
      (let ((current-time (at 'block-time (chain-data))))
        (let ((reset-time (if (> (diff-time current-time last-reset) 86400)
                             current-time
                             last-reset))
              (current-used (if (> (diff-time current-time last-reset) 86400)
                               0.0
                               used)))
          
          (enforce (<= (+ current-used amount) limit)
                   (format "Daily limit exceeded. Used: {}, Limit: {}" [current-used limit]))
          
          ;; Update daily usage
          (update chain-configs destination-chain {
            "used-today": (+ current-used amount),
            "last-reset": reset-time
          })))
      
      ;; Calculate total amount including fee
      (let ((total-amount (+ amount fee)))
        (with-capability (CROSS_CHAIN_TRANSFER sender destination-chain total-amount)
          ;; Debit sender
          (debit sender total-amount)
          
          ;; Create Hyperlane message
          (let ((token-data {
                  "recipient": recipient,
                  "amount": amount,
                  "sender": sender,
                  "origin-chain": (chain-data 'chain-id),
                  "destination-chain": destination-chain
                }))
            
            ;; Encode message for Hyperlane
            (let ((encoded-message (hyperlane-encode-token-message token-data)))
              (let ((message-id (hyperlane-message-id encoded-message)))
                
                ;; Record outbound message
                (insert bridge-messages message-id {
                  "recipient": recipient,
                  "amount": amount,
                  "origin-chain": (chain-data 'chain-id),
                  "destination-chain": destination-chain,
                  "message-id": message-id,
                  "status": "pending",
                  "timestamp": (at 'block-time (chain-data))
                })
                
                ;; Emit cross-chain event
                (emit-event (CROSS_CHAIN_SEND sender destination-chain amount message-id))
                
                message-id)))))))
  
  (defun receive-cross-chain:string (encoded-message:string 
                                   proof:object
                                   recipient:string
                                   recipient-guard:guard)
    @doc "Receive tokens from another chain"
    (with-capability (BRIDGE_OPERATOR)
      ;; Get message ID first
      (let ((message-id (hyperlane-message-id encoded-message)))
        
        ;; Prevent message replay
        (with-default-read processed-messages message-id
          { "processed-at": (time "1970-01-01T00:00:00Z") }
          { "processed-at" := last-processed }
          
          (enforce (= last-processed (time "1970-01-01T00:00:00Z"))
                   "Message already processed")
          
          ;; Mark message as processed
          (write processed-messages message-id {
            "message-id": message-id,
            "processed-at": (at 'block-time (chain-data)),
            "processor": (at "sender" (chain-data))
          }))
        
        ;; Verify Hyperlane proof (simplified - would use real verification)
        (enforce (verify-hyperlane-proof encoded-message proof)
                 "Invalid cross-chain proof")
        
        ;; Decode token message
        (let ((token-data (hyperlane-decode-token-message encoded-message)))
          (bind token-data {
            "recipient" := msg-recipient,
            "amount" := amount,
            "sender" := sender,
            "origin-chain" := origin
          }
            ;; Verify recipient matches
            (enforce (= msg-recipient recipient)
                     "Recipient mismatch")
            
            ;; Credit tokens to recipient
            (credit recipient recipient-guard amount)
            
            ;; Record inbound message
            (insert bridge-messages message-id {
              "recipient": recipient,
              "amount": amount,
              "origin-chain": origin,
              "destination-chain": (chain-data 'chain-id),
              "message-id": message-id,
              "status": "completed",
              "timestamp": (at 'block-time (chain-data))
            })
            
            ;; Emit receive event
            (emit-event (CROSS_CHAIN_RECEIVE recipient amount origin message-id))
            
            "Tokens received successfully")))))
  
  (defun verify-hyperlane-proof:bool (encoded-message:string proof:object)
    @doc "Verify Hyperlane message proof (simplified mock)"
    ;; In a real implementation, this would:
    ;; 1. Verify merkle inclusion proof
    ;; 2. Validate validator signatures
    ;; 3. Check message format and integrity
    (and 
      (!= encoded-message "")
      (!= (at "signature" proof) "")))
  
  ;; ============================================================================
  ;; CROSS-CHAIN PACT EXAMPLE
  ;; ============================================================================
  
  (defpact cross-chain-atomic-swap (swap-id:string
                                   alice:string
                                   bob:string
                                   alice-chain:string
                                   bob-chain:string
                                   alice-amount:decimal
                                   bob-amount:decimal
                                   timeout-hours:integer)
    @doc "Atomic swap between two parties on different chains"
    
    ;; Step 0: Initialize swap
    (step
      (let ((timeout-time (add-time (at 'block-time (chain-data)) (hours timeout-hours))))
        ;; Create swap record
        (insert bridge-messages swap-id {
          "recipient": bob,
          "amount": alice-amount,
          "origin-chain": alice-chain,
          "destination-chain": bob-chain,
          "message-id": swap-id,
          "status": "initialized",
          "timestamp": (at 'block-time (chain-data))
        })
        
        ;; Send initialization message to both chains
        (emit-event (CROSS_CHAIN_SEND alice bob-chain alice-amount swap-id))))
    
    ;; Step 1: Lock Alice's funds
    (step-with-rollback
      (enforce (= (chain-data 'chain-id) alice-chain)
               "Must execute on Alice's chain")
      
      (with-capability (TRANSFER alice swap-id alice-amount)
        ;; Lock Alice's funds
        (with-read accounts alice { "balance" := balance, "locked" := locked }
          (enforce (>= balance alice-amount) "Alice has insufficient funds")
          (update accounts alice {
            "balance": (- balance alice-amount),
            "locked": (+ locked alice-amount)
          }))
        
        ;; Update swap status
        (update bridge-messages swap-id { "status": "alice-locked" }))
      
      ;; Rollback: unlock Alice's funds
      (with-read accounts alice { "balance" := balance, "locked" := locked }
        (update accounts alice {
          "balance": (+ balance alice-amount),
          "locked": (- locked alice-amount)
        })))
    
    ;; Step 2: Lock Bob's funds  
    (step-with-rollback
      (enforce (= (chain-data 'chain-id) bob-chain)
               "Must execute on Bob's chain")
      
      (with-capability (TRANSFER bob swap-id bob-amount)
        ;; Lock Bob's funds
        (with-read accounts bob { "balance" := balance, "locked" := locked }
          (enforce (>= balance bob-amount) "Bob has insufficient funds")
          (update accounts bob {
            "balance": (- balance bob-amount),
            "locked": (+ locked bob-amount)
          }))
        
        ;; Update swap status
        (update bridge-messages swap-id { "status": "both-locked" }))
      
      ;; Rollback: unlock Bob's funds
      (with-read accounts bob { "balance" := balance, "locked" := locked }
        (update accounts bob {
          "balance": (+ balance bob-amount),
          "locked": (- locked bob-amount)
        })))
    
    ;; Step 3: Complete swap
    (step
      ;; This step can execute on either chain to finalize
      (with-read bridge-messages swap-id { "status" := status }
        (enforce (= status "both-locked") "Funds not properly locked")
        
        ;; Execute on Alice's chain
        (if (= (chain-data 'chain-id) alice-chain)
          (with-read accounts alice { "balance" := alice-bal, "locked" := alice-locked }
            ;; Give Alice her locked amount to Bob (cross-chain)
            (update accounts alice {
              "locked": (- alice-locked alice-amount)
            })
            ;; Alice receives Bob's amount (would come via cross-chain message)
            (update accounts alice {
              "balance": (+ alice-bal bob-amount)
            }))
          "Alice's side processed")
        
        ;; Execute on Bob's chain  
        (if (= (chain-data 'chain-id) bob-chain)
          (with-read accounts bob { "balance" := bob-bal, "locked" := bob-locked }
            ;; Give Bob his locked amount to Alice (cross-chain)
            (update accounts bob {
              "locked": (- bob-locked bob-amount)
            })
            ;; Bob receives Alice's amount (would come via cross-chain message)
            (update accounts bob {
              "balance": (+ bob-bal alice-amount)
            }))
          "Bob's side processed")
        
        ;; Mark swap complete
        (update bridge-messages swap-id { "status": "completed" })))
  )
  
  ;; ============================================================================
  ;; UTILITY AND QUERY FUNCTIONS
  ;; ============================================================================
  
  (defun get-bridge-message:object (message-id:string)
    @doc "Get bridge message details"
    (read bridge-messages message-id))
  
  (defun list-pending-messages:[object] (chain-id:string)
    @doc "List pending messages for a chain"
    (select bridge-messages (where 'destination-chain (= chain-id))))
  
  (defun get-daily-usage:object (chain-id:string)
    @doc "Get daily usage statistics for a chain"
    (with-read chain-configs chain-id {
      "daily-limit" := limit,
      "used-today" := used,
      "last-reset" := reset
    }
      {
        "limit": limit,
        "used": used,
        "remaining": (- limit used),
        "last-reset": reset
      }))
  
  (defun is-message-processed:bool (message-id:string)
    @doc "Check if a message has been processed"
    (with-default-read processed-messages message-id
      { "processed-at": (time "1970-01-01T00:00:00Z") }
      { "processed-at" := processed-time }
      (!= processed-time (time "1970-01-01T00:00:00Z"))))
  
  (defun reset-daily-limits:string ()
    @doc "Reset daily limits for all chains (admin function)"
    (with-capability (GOVERNANCE)
      (let ((all-chains (keys chain-configs)))
        (map (lambda (chain-id)
               (update chain-configs chain-id {
                 "used-today": 0.0,
                 "last-reset": (at 'block-time (chain-data))
               }))
             all-chains))
      "Daily limits reset"))
)

;; Create tables
(create-table accounts)
(create-table bridge-messages)
(create-table chain-configs)
(create-table processed-messages)