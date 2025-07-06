# Chapter 17: Cross-Chain Communication and Bridge Integration

## Introduction

Modern blockchain applications often need to interact across multiple chains and protocols. Pact 5 provides specialized tools for cross-chain communication, including Hyperlane bridge integration, cross-chain message passing, and interoperability patterns. This chapter covers building applications that seamlessly work across multiple blockchain networks.

## Hyperlane Bridge Integration

### Overview of Hyperlane

Hyperlane is a modular interoperability protocol that enables secure cross-chain communication. Pact 5 includes native support for Hyperlane message processing through specialized built-in functions.

### Basic Hyperlane Operations

#### Message ID Generation

```pact
;; Generate unique message ID for cross-chain communication
(defun create-cross-chain-message (recipient:string 
                                 destination-chain:string 
                                 data:object)
  @doc "Create cross-chain message with unique ID"
  (let ((message {
          "recipient": recipient,
          "destination": destination-chain,
          "data": data,
          "timestamp": (at 'block-time (chain-data)),
          "origin": (chain-data 'chain-id)
        }))
    (let ((message-id (hyperlane-message-id message)))
      {
        "message": message,
        "id": message-id
      })))

;; Example usage
(create-cross-chain-message 
  "0x742d35Cc6634C0532925a3b8D6ac6B6bd5c9c3e6"
  "ethereum-mainnet"
  { "action": "mint", "amount": 1000.0 })
```

#### Token Message Processing

```pact
(module cross-chain-token GOVERNANCE
  @doc "Cross-chain token with Hyperlane integration"
  
  (defschema token-message
    recipient:string
    amount:decimal
    origin-chain:string
    message-id:string)
  
  (defschema bridge-state
    total-locked:decimal
    outbound-messages:[string]
    inbound-messages:[string])
  
  (deftable token-messages:{token-message})
  (deftable bridge-states:{bridge-state})
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'admin-keyset))
  
  (defcap BRIDGE_OPERATOR ()
    (enforce-keyset 'bridge-operator-keyset))
  
  (defun send-tokens-cross-chain (recipient:string 
                                destination-chain:string 
                                amount:decimal)
    @doc "Send tokens to another chain via Hyperlane"
    (with-capability (TRANSFER sender recipient amount)
      ;; Lock tokens locally
      (debit sender amount)
      
      ;; Create Hyperlane token message
      (let ((token-data {
              "recipient": recipient,
              "amount": amount,
              "sender": sender
            }))
        (let ((encoded-message (hyperlane-encode-token-message token-data))
              (message-id (hyperlane-message-id encoded-message)))
          
          ;; Record outbound message
          (insert token-messages message-id {
            "recipient": recipient,
            "amount": amount,
            "origin-chain": (chain-data 'chain-id),
            "message-id": message-id
          })
          
          ;; Update bridge state
          (update-bridge-state-outbound message-id amount)
          
          ;; Emit cross-chain event
          (emit-event (CROSS_CHAIN_SEND recipient destination-chain amount message-id))
          
          message-id))))
  
  (defun receive-tokens-cross-chain (encoded-message:string proof:object)
    @doc "Receive tokens from another chain"
    (with-capability (BRIDGE_OPERATOR)
      ;; Verify message authenticity
      (enforce (verify-hyperlane-proof encoded-message proof)
               "Invalid cross-chain proof")
      
      ;; Decode token message
      (let ((token-data (hyperlane-decode-token-message encoded-message)))
        (bind token-data {
          "recipient" := recipient,
          "amount" := amount,
          "sender" := sender,
          "origin-chain" := origin
        }
          ;; Verify message hasn't been processed
          (let ((message-id (hyperlane-message-id encoded-message)))
            (with-default-read token-messages message-id
              { "message-id": "" }
              { "message-id" := existing-id }
              
              (enforce (= existing-id "") "Message already processed")
              
              ;; Mint tokens to recipient
              (credit recipient amount)
              
              ;; Record inbound message
              (insert token-messages message-id {
                "recipient": recipient,
                "amount": amount,
                "origin-chain": origin,
                "message-id": message-id
              })
              
              ;; Update bridge state
              (update-bridge-state-inbound message-id amount)
              
              ;; Emit receive event
              (emit-event (CROSS_CHAIN_RECEIVE recipient amount origin message-id))
              
              "Tokens received successfully"))))))
  
  (defun update-bridge-state-outbound (message-id:string amount:decimal)
    @doc "Update bridge state for outbound transfer"
    (with-default-read bridge-states (chain-data 'chain-id)
      { "total-locked": 0.0, "outbound-messages": [], "inbound-messages": [] }
      { "total-locked" := locked, "outbound-messages" := outbound }
      
      (write bridge-states (chain-data 'chain-id) {
        "total-locked": (+ locked amount),
        "outbound-messages": (+ outbound [message-id]),
        "inbound-messages": (at "inbound-messages" 
                               (read bridge-states (chain-data 'chain-id)))
      })))
  
  (defun update-bridge-state-inbound (message-id:string amount:decimal)
    @doc "Update bridge state for inbound transfer"
    (with-default-read bridge-states (chain-data 'chain-id)
      { "total-locked": 0.0, "outbound-messages": [], "inbound-messages": [] }
      { "total-locked" := locked, "inbound-messages" := inbound }
      
      (write bridge-states (chain-data 'chain-id) {
        "total-locked": (- locked amount),
        "outbound-messages": (at "outbound-messages"
                               (read bridge-states (chain-data 'chain-id))),
        "inbound-messages": (+ inbound [message-id])
      })))
)
```

## Cross-Chain Pact Transactions

### Multi-Chain Pact Patterns

```pact
(module cross-chain-escrow GOVERNANCE
  @doc "Cross-chain escrow using multi-step pacts"
  
  (defschema escrow-state
    buyer:string
    seller:string
    amount:decimal
    item-id:string
    buyer-chain:string
    seller-chain:string
    status:string
    timeout:time)
  
  (deftable escrow-states:{escrow-state})
  
  (defpact cross-chain-escrow (escrow-id:string
                              buyer:string
                              seller:string
                              amount:decimal
                              item-id:string
                              buyer-chain:string
                              seller-chain:string
                              timeout-hours:integer)
    @doc "Cross-chain escrow between two parties on different chains"
    
    ;; Step 0: Initialize escrow on coordination chain
    (step
      (insert escrow-states escrow-id {
        "buyer": buyer,
        "seller": seller,
        "amount": amount,
        "item-id": item-id,
        "buyer-chain": buyer-chain,
        "seller-chain": seller-chain,
        "status": "initialized",
        "timeout": (add-time (at 'block-time (chain-data)) (hours timeout-hours))
      })
      
      ;; Send initialization message to buyer chain
      (let ((init-message {
              "escrow-id": escrow-id,
              "action": "lock-funds",
              "amount": amount,
              "seller": seller
            }))
        (send-cross-chain-message buyer-chain init-message)))
    
    ;; Step 1: Lock funds on buyer chain
    (step
      (enforce (= (chain-data 'chain-id) buyer-chain)
               "Must execute on buyer chain")
      
      ;; Lock buyer funds
      (with-capability (TRANSFER buyer escrow-id amount)
        (transfer buyer (create-escrow-account escrow-id) amount))
      
      ;; Update escrow status
      (update escrow-states escrow-id { "status": "funds-locked" })
      
      ;; Notify seller chain
      (let ((notify-message {
              "escrow-id": escrow-id,
              "action": "funds-confirmed",
              "buyer": buyer
            }))
        (send-cross-chain-message seller-chain notify-message)))
    
    ;; Step 2: Confirm delivery on seller chain  
    (step
      (enforce (= (chain-data 'chain-id) seller-chain)
               "Must execute on seller chain")
      
      ;; Verify seller has item
      (with-capability (ITEM_OWNER seller item-id)
        ;; Transfer item to escrow
        (transfer-item seller (create-escrow-account escrow-id) item-id)
        
        ;; Update status
        (update escrow-states escrow-id { "status": "item-escrowed" })
        
        ;; Notify completion
        (let ((complete-message {
                "escrow-id": escrow-id,
                "action": "delivery-confirmed",
                "item-id": item-id
              }))
          (send-cross-chain-message buyer-chain complete-message))))
    
    ;; Step 3: Release funds and item
    (step
      ;; This can execute on either chain based on confirmation
      (with-read escrow-states escrow-id { "status" := status }
        (enforce (= status "item-escrowed") "Item not confirmed")
        
        ;; Release funds to seller
        (if (= (chain-data 'chain-id) buyer-chain)
          (release-escrowed-funds escrow-id seller)
          "Funds released on buyer chain")
        
        ;; Release item to buyer  
        (if (= (chain-data 'chain-id) seller-chain)
          (release-escrowed-item escrow-id buyer)
          "Item released on seller chain")
        
        ;; Mark complete
        (update escrow-states escrow-id { "status": "completed" })))
  )
  
  (defun send-cross-chain-message (destination-chain:string data:object)
    @doc "Send message to another chain"
    (let ((message {
            "destination": destination-chain,
            "data": data,
            "timestamp": (at 'block-time (chain-data)),
            "origin": (chain-data 'chain-id)
          }))
      ;; Would integrate with actual cross-chain messaging
      (emit-event (CROSS_CHAIN_MESSAGE destination-chain message))))
)
```

## Cross-Chain Data Synchronization

### State Channel Patterns

```pact
(module state-channels GOVERNANCE
  @doc "State channels for cross-chain synchronization"
  
  (defschema channel-state
    participants:[string]
    sequence:integer
    state-hash:string
    commitments:[object]
    timeout:time
    status:string)
  
  (defschema commitment
    participant:string
    state-hash:string
    signature:string
    sequence:integer)
  
  (deftable channels:{channel-state})
  (deftable commitments:{commitment})
  
  (defun open-channel (channel-id:string 
                      participants:[string]
                      initial-state:object
                      timeout-hours:integer)
    @doc "Open new state channel"
    (let ((state-hash (hash-keccak256 (format "{}" [initial-state]))))
      (insert channels channel-id {
        "participants": participants,
        "sequence": 0,
        "state-hash": state-hash,
        "commitments": [],
        "timeout": (add-time (at 'block-time (chain-data)) (hours timeout-hours)),
        "status": "open"
      })))
  
  (defun update-channel-state (channel-id:string 
                              new-state:object
                              participant:string
                              signature:string)
    @doc "Update channel state with participant commitment"
    (with-read channels channel-id {
      "participants" := participants,
      "sequence" := current-seq,
      "commitments" := current-commitments
    }
      ;; Verify participant
      (enforce (contains participant participants)
               "Unauthorized participant")
      
      ;; Create state commitment
      (let ((new-sequence (+ current-seq 1))
            (state-hash (hash-keccak256 (format "{}" [new-state]))))
        
        ;; Verify signature
        (enforce (verify-state-signature state-hash signature participant)
                 "Invalid signature")
        
        ;; Add commitment
        (let ((commitment {
                "participant": participant,
                "state-hash": state-hash,
                "signature": signature,
                "sequence": new-sequence
              }))
          
          (insert commitments (format "{}:{}" [channel-id new-sequence]) commitment)
          
          ;; Check if all participants have committed
          (let ((new-commitments (+ current-commitments [commitment])))
            (if (= (length new-commitments) (length participants))
              ;; All committed, update channel
              (update channels channel-id {
                "sequence": new-sequence,
                "state-hash": state-hash,
                "commitments": []
              })
              ;; Waiting for more commitments
              (update channels channel-id {
                "commitments": new-commitments
              })))))))
  
  (defun close-channel (channel-id:string final-state:object signatures:[string])
    @doc "Close channel with final state"
    (with-read channels channel-id {
      "participants" := participants,
      "status" := status
    }
      (enforce (= status "open") "Channel not open")
      (enforce (= (length signatures) (length participants))
               "Missing signatures")
      
      ;; Verify all signatures
      (let ((state-hash (hash-keccak256 (format "{}" [final-state]))))
        (map (lambda (pair)
               (bind pair [participant signature]
                 (enforce (verify-state-signature state-hash signature participant)
                          (format "Invalid signature from {}" [participant]))))
             (zip participants signatures))
        
        ;; Close channel
        (update channels channel-id { "status": "closed" })
        
        ;; Apply final state
        (apply-final-state channel-id final-state))))
)
```

## Cross-Chain Asset Management

### Multi-Chain Token Standards

```pact
(module omni-token GOVERNANCE
  @doc "Omni-chain token supporting multiple networks"
  
  (implements fungible-v2)
  
  (defschema account
    balance:decimal
    guard:guard
    locked:decimal)
  
  (defschema chain-config
    chain-id:string
    bridge-contract:string
    is-native:bool
    total-supply:decimal
    bridge-fee:decimal)
  
  (deftable accounts:{account})
  (deftable chain-configs:{chain-config})
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'admin-keyset))
  
  (defcap TRANSFER (sender:string receiver:string amount:decimal)
    @managed amount TRANSFER-mgr
    (enforce-guard (at 'guard (read accounts sender))))
  
  (defcap CROSS_CHAIN_TRANSFER (sender:string 
                               destination-chain:string 
                               amount:decimal)
    @managed amount TRANSFER-mgr
    (enforce-guard (at 'guard (read accounts sender))))
  
  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    (enforce (>= managed requested) "Insufficient balance")
    (- managed requested))
  
  (defun configure-chain (chain-id:string 
                         bridge-contract:string
                         is-native:bool
                         bridge-fee:decimal)
    @doc "Configure chain for cross-chain operations"
    (with-capability (GOVERNANCE)
      (write chain-configs chain-id {
        "chain-id": chain-id,
        "bridge-contract": bridge-contract,
        "is-native": is-native,
        "total-supply": 0.0,
        "bridge-fee": bridge-fee
      })))
  
  (defun transfer-cross-chain (sender:string 
                              destination-chain:string
                              destination-address:string
                              amount:decimal)
    @doc "Transfer tokens to another chain"
    (with-read chain-configs destination-chain {
      "bridge-fee" := fee,
      "is-native" := is-native
    }
      (let ((total-amount (+ amount fee)))
        (with-capability (CROSS_CHAIN_TRANSFER sender destination-chain total-amount)
          ;; Debit sender
          (debit sender total-amount)
          
          ;; If native chain, lock tokens; otherwise burn them
          (if is-native
            (lock-tokens sender amount)
            (burn-tokens amount))
          
          ;; Create cross-chain message
          (let ((transfer-data {
                  "recipient": destination-address,
                  "amount": amount,
                  "sender": sender,
                  "origin-chain": (chain-data 'chain-id)
                }))
            (let ((message-id (send-cross-chain-transfer 
                               destination-chain 
                               transfer-data)))
              
              ;; Emit event
              (emit-event (CROSS_CHAIN_TRANSFER_INITIATED 
                          sender destination-chain amount message-id))
              
              message-id))))))
  
  (defun receive-cross-chain (encoded-message:string proof:object)
    @doc "Receive tokens from another chain"
    (with-capability (GOVERNANCE)
      ;; Verify cross-chain proof
      (enforce (verify-cross-chain-proof encoded-message proof)
               "Invalid cross-chain proof")
      
      ;; Decode transfer data
      (let ((transfer-data (decode-transfer-message encoded-message)))
        (bind transfer-data {
          "recipient" := recipient,
          "amount" := amount,
          "sender" := sender,
          "origin-chain" := origin
        }
          ;; Verify message not already processed
          (let ((message-id (hyperlane-message-id encoded-message)))
            (enforce (not (message-processed? message-id))
                     "Message already processed")
            
            ;; Credit recipient
            (with-read chain-configs (chain-data 'chain-id) { "is-native" := is-native }
              (if is-native
                (unlock-tokens recipient amount)
                (mint-tokens recipient amount)))
            
            ;; Mark message processed
            (mark-message-processed message-id)
            
            ;; Emit event
            (emit-event (CROSS_CHAIN_TRANSFER_COMPLETED 
                        recipient amount origin message-id)))))))
  
  (defun lock-tokens (account:string amount:decimal)
    @doc "Lock tokens for cross-chain transfer"
    (with-read accounts account { "locked" := current-locked }
      (update accounts account {
        "locked": (+ current-locked amount)
      })))
  
  (defun unlock-tokens (account:string amount:decimal)
    @doc "Unlock tokens from cross-chain transfer"
    (with-default-read accounts account
      { "balance": 0.0, "guard": (keyset-ref-guard 'default), "locked": 0.0 }
      { "balance" := balance, "locked" := locked }
      
      (update accounts account {
        "balance": (+ balance amount),
        "locked": (- locked amount)
      })))
  
  (defun mint-tokens (account:string amount:decimal)
    @doc "Mint tokens for cross-chain receive"
    (with-default-read accounts account
      { "balance": 0.0, "guard": (keyset-ref-guard 'default), "locked": 0.0 }
      { "balance" := balance }
      
      (update accounts account {
        "balance": (+ balance amount)
      })))
  
  (defun burn-tokens (amount:decimal)
    @doc "Burn tokens for cross-chain send"
    ;; Update total supply
    (with-read chain-configs (chain-data 'chain-id) { "total-supply" := supply }
      (update chain-configs (chain-data 'chain-id) {
        "total-supply": (- supply amount)
      })))
)
```

## Cross-Chain Governance

### Multi-Chain DAO

```pact
(module cross-chain-dao GOVERNANCE
  @doc "DAO with cross-chain governance capabilities"
  
  (defschema proposal
    title:string
    description:string
    proposer:string
    proposer-chain:string
    voting-power-required:decimal
    execution-chains:[string]
    votes:[object]
    status:string
    created:time
    voting-deadline:time)
  
  (defschema chain-voting-power
    chain-id:string
    total-power:decimal
    delegated-power:decimal
    active-voters:[string])
  
  (defschema cross-chain-vote
    voter:string
    voter-chain:string
    power:decimal
    choice:bool
    signature:string
    message-id:string)
  
  (deftable proposals:{proposal})
  (deftable chain-powers:{chain-voting-power})
  (deftable cross-votes:{cross-chain-vote})
  
  (defun create-cross-chain-proposal (proposal-id:string
                                    title:string
                                    description:string
                                    execution-chains:[string]
                                    voting-period-days:integer)
    @doc "Create proposal for cross-chain execution"
    (let ((deadline (add-time (at 'block-time (chain-data)) (days voting-period-days))))
      (insert proposals proposal-id {
        "title": title,
        "description": description,
        "proposer": (at "sender" (chain-data)),
        "proposer-chain": (chain-data 'chain-id),
        "voting-power-required": (calculate-required-power),
        "execution-chains": execution-chains,
        "votes": [],
        "status": "active",
        "created": (at 'block-time (chain-data)),
        "voting-deadline": deadline
      })
      
      ;; Notify all chains about proposal
      (map (lambda (chain)
             (send-proposal-notification chain proposal-id))
           execution-chains)))
  
  (defun submit-cross-chain-vote (proposal-id:string
                                 vote-choice:bool
                                 voter-chain:string
                                 voting-power:decimal
                                 signature:string)
    @doc "Submit vote from another chain"
    (with-read proposals proposal-id {
      "status" := status,
      "voting-deadline" := deadline,
      "votes" := current-votes
    }
      (enforce (= status "active") "Proposal not active")
      (enforce (< (at 'block-time (chain-data)) deadline) "Voting period ended")
      
      ;; Verify cross-chain voting power
      (let ((vote-message {
              "proposal-id": proposal-id,
              "voter": (at "sender" (chain-data)),
              "choice": vote-choice,
              "power": voting-power,
              "chain": voter-chain
            }))
        (enforce (verify-voting-signature vote-message signature voter-chain)
                 "Invalid voting signature")
        
        ;; Record vote
        (let ((vote-record {
                "voter": (at "sender" (chain-data)),
                "voter-chain": voter-chain,
                "power": voting-power,
                "choice": vote-choice,
                "signature": signature,
                "message-id": (hyperlane-message-id vote-message)
              }))
          
          (insert cross-votes (format "{}:{}" [proposal-id (at "voter" vote-record)]) vote-record)
          
          ;; Update proposal votes
          (update proposals proposal-id {
            "votes": (+ current-votes [vote-record])
          })
          
          ;; Check if ready for execution
          (check-proposal-ready proposal-id))))
  
  (defun check-proposal-ready (proposal-id:string)
    @doc "Check if proposal has enough votes for execution"
    (with-read proposals proposal-id {
      "votes" := votes,
      "voting-power-required" := required-power,
      "execution-chains" := execution-chains,
      "status" := status
    }
      (if (= status "active")
        (let ((total-yes-power (fold (lambda (acc vote)
                                      (if (at "choice" vote)
                                        (+ acc (at "power" vote))
                                        acc))
                                    0.0
                                    votes)))
          (if (>= total-yes-power required-power)
            (begin
              ;; Mark ready for execution
              (update proposals proposal-id { "status": "passed" })
              
              ;; Trigger execution on all chains
              (map (lambda (chain)
                     (trigger-execution chain proposal-id))
                   execution-chains))
            "Not enough voting power"))
        "Proposal not active")))
  
  (defun execute-cross-chain-proposal (proposal-id:string execution-data:object)
    @doc "Execute proposal on this chain"
    (with-read proposals proposal-id {
      "status" := status,
      "execution-chains" := execution-chains
    }
      (enforce (= status "passed") "Proposal not passed")
      (enforce (contains (chain-data 'chain-id) execution-chains)
               "Not authorized execution chain")
      
      ;; Execute proposal logic based on type
      (let ((action-type (at "type" execution-data)))
        (cond
          ((= action-type "parameter-update")
           (execute-parameter-update execution-data))
          ((= action-type "fund-transfer")
           (execute-fund-transfer execution-data))
          ((= action-type "contract-upgrade")
           (execute-contract-upgrade execution-data))
          (enforce false "Unknown action type")))
      
      ;; Mark executed on this chain
      (emit-event (PROPOSAL_EXECUTED proposal-id (chain-data 'chain-id)))))
  
  (defun verify-voting-signature (vote-message:object signature:string chain:string)
    @doc "Verify voting signature from another chain"
    (let ((message-hash (hash-keccak256 (format "{}" [vote-message]))))
      ;; Would verify signature with chain-specific verification
      (verify-chain-signature message-hash signature chain)))
  
  (defun calculate-required-power ()
    @doc "Calculate required voting power for proposal passage"
    ;; Sum power across all chains and require majority
    (let ((total-power (fold (lambda (acc chain-power)
                              (+ acc (at "total-power" chain-power)))
                            0.0
                            (select chain-powers (constantly true)))))
      (/ total-power 2.0)))
)
```

## Performance and Security Considerations

### Cross-Chain Security Best Practices

```pact
;; Message replay prevention
(defschema processed-messages
  message-id:string
  processed-at:time
  chain:string)

(deftable processed-messages-table:{processed-messages})

(defun prevent-message-replay (message-id:string origin-chain:string)
  @doc "Prevent cross-chain message replay"
  (with-default-read processed-messages-table message-id
    { "processed-at": (time "1970-01-01T00:00:00Z") }
    { "processed-at" := last-processed }
    
    (enforce (= last-processed (time "1970-01-01T00:00:00Z"))
             "Message already processed")
    
    (write processed-messages-table message-id {
      "message-id": message-id,
      "processed-at": (at 'block-time (chain-data)),
      "chain": origin-chain
    })))

;; Rate limiting for cross-chain operations
(defschema rate-limit
  chain-id:string
  operations-count:integer
  window-start:time
  limit-per-hour:integer)

(deftable rate-limits:{rate-limit})

(defun check-rate-limit (destination-chain:string)
  @doc "Check rate limit for cross-chain operations"
  (let ((current-time (at 'block-time (chain-data)))
        (hour-ago (add-time current-time (hours -1))))
    
    (with-default-read rate-limits destination-chain
      { "operations-count": 0, "window-start": current-time, "limit-per-hour": 100 }
      { "operations-count" := count, "window-start" := window-start, "limit-per-hour" := limit }
      
      ;; Reset if window expired
      (if (> (diff-time current-time window-start) 3600)
        (write rate-limits destination-chain {
          "chain-id": destination-chain,
          "operations-count": 1,
          "window-start": current-time,
          "limit-per-hour": limit
        })
        (begin
          (enforce (< count limit) "Rate limit exceeded")
          (update rate-limits destination-chain {
            "operations-count": (+ count 1)
          }))))))
```

## Summary

Cross-chain development in Pact 5 enables:

**Hyperlane Integration:**
- **Message ID generation** for unique cross-chain messages
- **Token message encoding/decoding** for asset transfers
- **Proof verification** for secure cross-chain communication

**Cross-Chain Patterns:**
- **Multi-step pacts** spanning multiple chains
- **State channels** for efficient synchronization
- **Cross-chain governance** with distributed voting

**Security Features:**
- **Message replay prevention** with unique IDs
- **Rate limiting** for cross-chain operations
- **Signature verification** across chains
- **Proof validation** for message authenticity

**Best Practices:**
1. **Always verify proofs** before processing cross-chain messages
2. **Implement replay protection** for all cross-chain operations
3. **Use rate limiting** to prevent spam attacks
4. **Validate signatures** with chain-specific methods
5. **Design for failure scenarios** with rollback mechanisms

These cross-chain capabilities enable building sophisticated multi-chain applications while maintaining security and correctness guarantees.

## Exercises

1. Build a cross-chain NFT marketplace using Hyperlane
2. Create a multi-chain lending protocol with unified governance
3. Implement cross-chain atomic swaps with escrow
4. Design a cross-chain oracle aggregation system
5. Build a multi-chain identity verification system

## References

- Hyperlane Documentation: Official Hyperlane protocol specification
- Cross-Chain Standards: IBC and other interoperability protocols
- Bridge Security: Academic papers on bridge vulnerabilities and mitigations
- Multi-Chain Architecture: Design patterns for cross-chain applications
- Pact Implementation: `/pact/Pact/Core/Builtin.hs` for Hyperlane builtins