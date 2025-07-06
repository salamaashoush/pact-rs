# Lesson 6: Zombie Apocalypse - Cross-Chain & Production Deployment 🌍

Congratulations, survivor! You've built an army of smart contract zombies. Now it's time to unleash them across the Kadena multichain network and prepare for production deployment!

## What You'll Learn

- Cross-chain zombie transfers
- Gas optimization techniques
- Production deployment strategies
- Upgradeable contracts
- Complete DApp architecture
- Security best practices

## Chapter 1: Understanding Kadena's Multichain Architecture

Kadena has multiple parallel chains that can communicate with each other. This allows for massive scalability!

### Chain Architecture

```pact
; Each chain has its own:
; - State (tables)
; - Transaction history
; - Gas costs

; But they can communicate via:
; - SPV (Simple Payment Verification) proofs
; - Cross-chain pacts
```

### Exercise 6.1

Create `zombie-apocalypse.pact` to build our cross-chain zombie empire!

## Chapter 2: Cross-Chain Zombie Transfers

Let's implement secure cross-chain zombie transfers:

```pact
(module zombie-bridge GOV

  (defcap GOV () (enforce-guard (read-keyset "zombie-bridge-admin")))

  ; Cross-chain transfer schema
  (defschema transfer-record
    zombie-id:string
    from-chain:string
    to-chain:string
    from-owner:string
    to-owner:string
    zombie-data:object
    initiated-at:time
    completed:bool
    spv-proof:string)

  (deftable transfers-table:{transfer-record})

  ; Events for tracking
  (defcap TRANSFER_INITIATED:bool 
    (transfer-id:string from-chain:string to-chain:string)
    @event true)

  (defcap TRANSFER_COMPLETED:bool 
    (transfer-id:string zombie-id:string)
    @event true)

  ; Main cross-chain transfer pact
  (defpact cross-chain-transfer 
    (zombie-id:string 
     target-chain:string 
     target-owner:string)
    @doc "Transfer zombie between chains with SPV proof"
    
    (step ; Step 1: Lock on source chain
      (let* ((source-chain (at "chain-id" (chain-data)))
             (current-owner (at "sender" (chain-data)))
             (transfer-id (format "xfer_{}_{}" [zombie-id (at "block-time" (chain-data))]))
             (zombie-data (read zombies.zombies-table zombie-id)))
        
        (enforce (!= source-chain target-chain) 
          "Source and target chains must differ")
        
        ; Verify ownership
        (enforce (= current-owner (at "owner" zombie-data)) 
          "Not your zombie!")
        
        ; Lock the zombie
        (update zombies.zombies-table zombie-id 
          { "status": "transferring"
          , "locked": true
          })
        
        ; Record transfer
        (insert transfers-table transfer-id
          { "zombie-id": zombie-id
          , "from-chain": source-chain
          , "to-chain": target-chain
          , "from-owner": current-owner
          , "to-owner": target-owner
          , "zombie-data": zombie-data
          , "initiated-at": (at "block-time" (chain-data))
          , "completed": false
          , "spv-proof": ""
          })
        
        (emit-event (TRANSFER_INITIATED transfer-id source-chain target-chain))
        
        (yield { "transfer-id": transfer-id
               , "zombie-data": zombie-data
               , "target-chain": target-chain
               , "target-owner": target-owner
               })))
    
    (step ; Step 2: Burn on source chain
      (resume { "transfer-id" := tid
              , "zombie-data" := zdata
              }
        (let ((source-chain (at "chain-id" (chain-data))))
          ; Verify we're still on source chain
          (with-read transfers-table tid 
            { "from-chain" := expected-chain }
            (enforce (= source-chain expected-chain) 
              "Must burn on source chain"))
          
          ; Burn the zombie
          (update zombies.zombies-table (at "id" zdata)
            { "status": "burned"
            , "owner": "0x0" ; Null address
            })
          
          ; Generate SPV proof (simplified for demo)
          (let ((spv-proof (hash (+ tid (at "block-height" (chain-data))))))
            (update transfers-table tid { "spv-proof": spv-proof })
            (yield { "transfer-id": tid
                   , "zombie-data": zdata
                   , "spv-proof": spv-proof
                   })))))
    
    (step ; Step 3: Mint on target chain
      (resume { "transfer-id" := tid
              , "zombie-data" := zdata
              , "spv-proof" := proof
              }
        (let ((current-chain (at "chain-id" (chain-data))))
          (with-read transfers-table tid 
            { "to-chain" := expected-chain
            , "to-owner" := new-owner
            }
            (enforce (= current-chain expected-chain) 
              "Must mint on target chain")
            
            ; Verify SPV proof (simplified)
            (enforce (!= proof "") "Invalid SPV proof")
            
            ; Mint zombie on new chain
            (let ((new-zombie-id (format "{}_{}" 
                    [(at "id" zdata) current-chain])))
              (insert zombies.zombies-table new-zombie-id
                (+ zdata 
                   { "id": new-zombie-id
                   , "owner": new-owner
                   , "status": "active"
                   , "locked": false
                   , "origin-chain": (at "from-chain" 
                       (read transfers-table tid))
                   }))
              
              (update transfers-table tid { "completed": true })
              (emit-event (TRANSFER_COMPLETED tid new-zombie-id))
              (format "Zombie {} transferred to chain {}" 
                [new-zombie-id current-chain])))))))
)
```

### Exercise 6.2

Add safety mechanisms for failed transfers and recovery options.

## Chapter 3: Gas Optimization

Production contracts need to be gas-efficient:

```pact
(module gas-optimized-zombies GOV

  ; Use constants for repeated values
  (defconst ZERO_ADDRESS "0x0")
  (defconst DEFAULT_STATUS "active")
  
  ; Batch operations to save gas
  (defun batch-create-zombies:[string] (names:[string])
    @doc "Create multiple zombies in one transaction"
    (let ((owner (at "sender" (chain-data))))
      (map (lambda (name) (create-zombie-optimized name owner)) names)))
  
  ; Optimize storage with packed data
  (defschema packed-zombie
    data:string ; Packed zombie data as string
    owner:string)
  
  (defun pack-zombie-data:string (zombie:object)
    @doc "Pack zombie data to save storage"
    (format "{}|{}|{}|{}|{}" 
      [(at "name" zombie)
       (at "dna" zombie)
       (at "level" zombie)
       (at "type" zombie)
       (at "wins" zombie)]))
  
  (defun unpack-zombie-data:object (packed:string)
    @doc "Unpack zombie data"
    ; Implementation to parse packed string
    )
  
  ; Use guards efficiently
  (defun enforce-zombie-owner-batch (zombie-ids:[string] owner:string)
    @doc "Batch ownership check"
    (let ((owned-zombies (select zombies-table 
            (and? (where "owner" (= owner))
                  (where "id" (lambda (id) (contains id zombie-ids)))))))
      (enforce (= (length owned-zombies) (length zombie-ids)) 
        "Not all zombies owned")))
)
```

### Exercise 6.3

Implement gas benchmarking for your zombie operations.

## Chapter 4: Upgradeable Contracts

Design contracts that can evolve:

```pact
(module zombie-v2 GOV

  (defcap GOV () (enforce-guard (read-keyset "zombie-admin")))
  
  ; Version management
  (defschema version-info
    version:string
    upgraded-at:time
    previous-version:string
    migration-complete:bool)
  
  (deftable versions-table:{version-info})
  (defconst CURRENT_VERSION "2.0.0")
  
  ; Migration capability
  (defcap MIGRATE () 
    @doc "Capability for migration operations"
    (with-capability (GOV) true))
  
  ; Data migration function
  (defun migrate-from-v1 ()
    @doc "Migrate data from version 1"
    (with-capability (MIGRATE)
      (let ((v1-zombies (zombies-v1.get-all-zombies)))
        (map (migrate-zombie) v1-zombies)
        (insert versions-table CURRENT_VERSION
          { "version": CURRENT_VERSION
          , "upgraded-at": (at "block-time" (chain-data))
          , "previous-version": "1.0.0"
          , "migration-complete": true
          })
        "Migration complete")))
  
  (defun migrate-zombie (v1-zombie:object)
    @doc "Migrate individual zombie"
    (let ((new-fields { "power-level": (calculate-power-level v1-zombie)
                      , "rarity": (calculate-rarity v1-zombie)
                      , "generation": 1
                      }))
      (insert zombies-table (at "id" v1-zombie)
        (+ v1-zombie new-fields))))
  
  ; Proxy pattern for gradual migration
  (defun get-zombie:object (zombie-id:string)
    @doc "Get zombie with fallback to v1"
    (with-default-read zombies-table zombie-id
      { "migrated": false }
      { "migrated" := migrated }
      (if migrated
        (read zombies-table zombie-id)
        (let ((v1-zombie (zombies-v1.get-zombie zombie-id)))
          (migrate-zombie v1-zombie)
          (read zombies-table zombie-id)))))
)
```

### Exercise 6.4

Implement a feature flag system for gradual rollouts.

## Chapter 5: Complete DApp Architecture

Let's build the full zombie apocalypse ecosystem:

```pact
(module zombie-apocalypse GOVERNANCE

  ; Comprehensive governance
  (defcap GOVERNANCE ()
    (compose-capability (ADMIN))
    (compose-capability (OPS)))
  
  (defcap ADMIN () 
    (enforce-guard (read-keyset "zombie-admin")))
  
  (defcap OPS () 
    (enforce-guard (read-keyset "zombie-ops")))
  
  ; Global game state
  (defschema apocalypse-state
    total-zombies:integer
    total-battles:integer
    total-cross-chain-transfers:integer
    economic-supply:decimal
    game-phase:string
    leaderboard:[object]
    active-events:[string])
  
  ; Advanced features
  (defschema world-event
    event-id:string
    event-type:string
    description:string
    start-time:time
    end-time:time
    rewards-multiplier:decimal
    special-rules:object)
  
  (deftable events-table:{world-event})
  
  ; Seasonal zombie mutations
  (defun apply-seasonal-mutation (zombie-id:string event-id:string)
    @doc "Apply special event mutations"
    (with-read events-table event-id 
      { "event-type" := etype
      , "special-rules" := rules
      }
      (let ((mutation-type (at "mutation" rules)))
        (update zombies-table zombie-id
          { "mutations": (+ (at "mutations" 
              (read zombies-table zombie-id)) 
              [mutation-type])
          , "power": (* (at "power" (read zombies-table zombie-id))
              (at "power-multiplier" rules))
          }))))
  
  ; Advanced marketplace with auctions
  (defpact zombie-auction 
    (zombie-id:string 
     starting-price:decimal 
     duration:decimal)
    @doc "Timed auction for rare zombies"
    
    (step ; Initialize auction
      (let ((seller (at "sender" (chain-data))))
        (with-capability (ZOMBIE_OWNER zombie-id seller)
          (insert auctions-table zombie-id
            { "zombie-id": zombie-id
            , "seller": seller
            , "starting-price": starting-price
            , "current-bid": starting-price
            , "highest-bidder": ""
            , "end-time": (add-time 
                (at "block-time" (chain-data)) duration)
            , "active": true
            })
          (yield { "auction-id": zombie-id }))))
    
    (step ; Finalize auction
      (resume { "auction-id" := aid }
        (with-read auctions-table aid 
          { "highest-bidder" := winner
          , "current-bid" := final-price
          , "seller" := seller
          , "end-time" := end
          }
          (enforce (>= (at "block-time" (chain-data)) end) 
            "Auction not ended")
          (if (!= winner "")
            (let ()
              ; Transfer zombie to winner
              (update zombies-table aid { "owner": winner })
              ; Transfer payment
              (zombie-coin.transfer winner seller final-price)
              (format "Auction won by {} for {} ZMB" 
                [winner final-price]))
            "Auction ended with no bids")))))
  
  ; Zombie breeding with genetics
  (defun advanced-breeding 
    (parent1-id:string parent2-id:string)
    @doc "Advanced genetic breeding system"
    (let* ((p1 (read zombies-table parent1-id))
           (p2 (read zombies-table parent2-id))
           (genetics (calculate-genetics p1 p2))
           (mutations (calculate-mutations p1 p2))
           (baby-stats (merge-stats p1 p2 genetics)))
      ; Create genetically superior zombie
      (create-zombie-with-genetics baby-stats genetics mutations)))
  
  ; Tournament system
  (defpact zombie-tournament (entry-fee:decimal prize-pool:decimal)
    @doc "Multi-round elimination tournament"
    ; Implementation of tournament brackets
    )
  
  ; Analytics and metrics
  (defun get-game-metrics ()
    @doc "Comprehensive game analytics"
    { "total-players": (length (distinct 
        (map (at "owner") (select zombies-table (constantly true)))))
    , "total-zombies": (length (keys zombies-table))
    , "total-volume": (zombie-coin.get-total-supply)
    , "active-auctions": (length (select auctions-table 
        (where "active" (= true))))
    , "cross-chain-activity": (get-cross-chain-stats)
    , "economic-velocity": (calculate-velocity)
    })
)
```

### Exercise 6.5

Build a zombie metaverse with land ownership and resource management.

## Chapter 6: Production Deployment Checklist

Before deploying to mainnet:

### Security Checklist
- [ ] All inputs validated
- [ ] Reentrancy protection
- [ ] Integer overflow checks
- [ ] Access control on all admin functions
- [ ] Emergency pause mechanism
- [ ] Formal verification of critical paths

### Testing Checklist
- [ ] Unit tests for all functions
- [ ] Integration tests for workflows
- [ ] Stress tests for gas usage
- [ ] Cross-chain scenario tests
- [ ] Economic simulation tests

### Deployment Steps
```bash
# 1. Deploy to testnet
pact -a deploy-testnet.yaml

# 2. Run integration tests
pact test-integration.repl

# 3. Audit results
pact verify-security.repl

# 4. Deploy to mainnet
pact -a deploy-mainnet.yaml

# 5. Verify deployment
pact verify-deployment.repl
```

## Complete Implementation

See `zombie-apocalypse-complete.pact` for the full production-ready implementation!

## Final Challenge: Build Your Zombie Empire

Create these advanced features:
1. **Zombie DAOs**: Governance for game decisions
2. **NFT Rarities**: Ultra-rare zombie variants
3. **Play-to-Earn**: Sustainable token economics
4. **Mobile Integration**: Web3 wallet support
5. **AI Opponents**: Smart contract AI battles

## Quiz

1. How do SPV proofs enable cross-chain transfers?
2. What gas optimizations can you apply to batch operations?
3. How do you handle contract upgrades safely?
4. What security considerations are unique to cross-chain operations?
5. How can you ensure economic sustainability in a game economy?

## Summary

🎉 **CONGRATULATIONS!** You've completed PactZombies!

You've mastered:
- ✅ Pact language fundamentals
- ✅ Smart contract security
- ✅ Token economics
- ✅ Multi-step transactions
- ✅ Cross-chain operations
- ✅ Production deployment

You're now ready to build production DApps on Kadena!

## What's Next?

1. Deploy your zombie game to Kadena testnet
2. Join the Kadena developer community
3. Build your own DApp ideas
4. Contribute to the ecosystem

Welcome to the Kadena developer family! 🚀

## Resources

- [Kadena Docs](https://docs.kadena.io)
- [Pact Language Reference](https://pact-language.readthedocs.io)
- [Discord Community](https://discord.gg/kadena)
- [GitHub Examples](https://github.com/kadena-io)