# Lesson 5: Multi-Step Zombie Quests - Understanding Pacts 🗺️

Welcome, zombie commander! Your zombies are battle-tested and economically savvy. Now it's time to send them on epic quests! In this lesson, you'll learn about Pact's unique feature: **pacts** (with a lowercase 'p') - multi-step transactions.

## What You'll Learn

- Understanding pacts vs defuns
- Creating multi-step transactions
- Managing pact state with `yield` and `resume`
- Building quest chains
- Cross-chain zombie transfers

## Chapter 1: What Are Pacts?

Pacts are Pact's way of handling complex, multi-step transactions that may span time and even blockchains!

### Pacts vs Functions

```pact
; Regular function - executes immediately
(defun instant-action () 
  "This happens right now")

; Pact - can pause and resume
(defpact multi-step-process ()
  (step "First, do this")
  (step "Later, do this")
  (step "Finally, do this"))
```

Key differences:
- **Functions**: Execute completely in one transaction
- **Pacts**: Can pause between steps, maintaining state
- **Steps**: Each step is a separate transaction

### Exercise 5.1

Create `zombie-quests.pact` and start building your quest system!

## Chapter 2: Your First Zombie Quest

Let's create a simple two-party trade quest:

```pact
(defpact zombie-trade-quest (zombie1-id:string zombie2-id:string 
                            party1:string party2:string)
  @doc "A two-party zombie trade requiring both confirmations"
  
  (step 
    (with-capability (ZOMBIE_OWNER zombie1-id party1)
      (update zombies-table zombie1-id { "status": "quest-locked" })
      (yield { "zombie1": zombie1-id
             , "zombie2": zombie2-id
             , "party1": party1
             , "party2": party2
             , "status": "awaiting-party2"
             })))
  
  (step 
    (resume { "zombie1" := z1
            , "zombie2" := z2 
            , "party1" := p1
            , "party2" := p2 
            }
      (with-capability (ZOMBIE_OWNER z2 p2)
        (update zombies-table z2 { "status": "quest-locked" })
        (yield { "ready": true }))))
  
  (step
    (resume { "ready" := ready }
      (enforce ready "Not ready to complete trade")
      ; Perform the actual swap
      (update zombies-table zombie1-id { "owner": party2, "status": "active" })
      (update zombies-table zombie2-id { "owner": party1, "status": "active" })
      "Trade completed!")))
```

### Key Concepts:
- `yield`: Saves data between steps
- `resume`: Retrieves data from previous step
- Each step can be executed by different parties
- State is maintained on-chain

### Exercise 5.2

Implement the zombie trade quest with proper error handling.

## Chapter 3: Epic Quest Chains

Let's build a more complex quest system:

```pact
(defschema quest-info
  quest-id:string
  zombie-id:string
  owner:string
  started-at:time
  current-stage:integer
  rewards-earned:decimal
  monsters-defeated:integer
  treasure-found:[string])

(deftable quest-table:{quest-info})

(defpact epic-dungeon-quest (zombie-id:string)
  @doc "Multi-stage dungeon quest with rewards"
  
  (step ; Stage 1: Enter the dungeon
    (let ((owner (at "sender" (chain-data))))
      (with-capability (ZOMBIE_OWNER zombie-id owner)
        (enforce-quest-requirements zombie-id)
        (let ((quest-id (generate-quest-id zombie-id)))
          (insert quest-table quest-id
            { "quest-id": quest-id
            , "zombie-id": zombie-id
            , "owner": owner
            , "started-at": (at "block-time" (chain-data))
            , "current-stage": 1
            , "rewards-earned": 0.0
            , "monsters-defeated": 0
            , "treasure-found": []
            })
          (yield { "quest-id": quest-id
                 , "zombie-id": zombie-id
                 , "owner": owner
                 , "stage": 1
                 })))))
  
  (step ; Stage 2: Fight the mini-boss
    (resume { "quest-id" := qid
            , "zombie-id" := zid
            , "owner" := owner
            }
      (enforce-ownership zid owner)
      (let* ((battle-result (simulate-battle zid "mini-boss"))
             (rewards (if battle-result 10.0 0.0)))
        (update quest-table qid 
          { "current-stage": 2
          , "rewards-earned": rewards
          , "monsters-defeated": (if battle-result 1 0)
          })
        (if battle-result
          (zombie-coin.transfer "quest-pool" owner rewards)
          (enforce false "Defeated by mini-boss! Quest failed."))
        (yield { "quest-id": qid
               , "zombie-id": zid
               , "owner": owner
               , "stage": 2
               , "mini-boss-defeated": battle-result
               }))))
  
  (step ; Stage 3: Solve the puzzle
    (resume { "quest-id" := qid
            , "zombie-id" := zid
            , "owner" := owner
            }
      (enforce-ownership zid owner)
      (let ((puzzle-solution (at "puzzle-answer" (read-msg))))
        (enforce (= puzzle-solution "BRAINS") "Wrong answer!")
        (update quest-table qid { "current-stage": 3 })
        (yield { "quest-id": qid
               , "zombie-id": zid
               , "owner": owner
               , "stage": 3
               , "puzzle-solved": true
               }))))
  
  (step ; Stage 4: Final boss and treasure
    (resume { "quest-id" := qid
            , "zombie-id" := zid
            , "owner" := owner
            }
      (enforce-ownership zid owner)
      (let* ((boss-result (simulate-battle zid "dragon-boss"))
             (treasure (if boss-result ["legendary-sword" "dragon-scale"] []))
             (final-rewards (if boss-result 100.0 25.0)))
        (with-read quest-table qid 
          { "rewards-earned" := previous-rewards
          , "monsters-defeated" := previous-monsters
          }
          (update quest-table qid 
            { "current-stage": 4
            , "rewards-earned": (+ previous-rewards final-rewards)
            , "monsters-defeated": (+ previous-monsters (if boss-result 1 0))
            , "treasure-found": treasure
            })
          (zombie-coin.transfer "quest-pool" owner final-rewards)
          (if boss-result
            (grant-quest-rewards zid treasure)
            "Defeated but gained experience!")
          (format "Quest complete! Total rewards: {} ZMB" 
            [(+ previous-rewards final-rewards)]))))))
```

### Exercise 5.3

Create helper functions for battle simulation and reward distribution.

## Chapter 4: Time-Based Quest Mechanics

Some quests should have time constraints:

```pact
(defpact timed-rescue-mission (zombie-id:string captive-id:string)
  @doc "Rescue a captive zombie within time limit"
  
  (step ; Start the mission
    (let ((owner (at "sender" (chain-data)))
          (start-time (at "block-time" (chain-data))))
      (with-capability (ZOMBIE_OWNER zombie-id owner)
        (yield { "zombie-id": zombie-id
               , "captive-id": captive-id
               , "owner": owner
               , "start-time": start-time
               , "time-limit": 3600.0 ; 1 hour
               }))))
  
  (step ; Attempt rescue
    (resume { "zombie-id" := zid
            , "captive-id" := cid
            , "owner" := owner
            , "start-time" := start
            , "time-limit" := limit
            }
      (let ((current-time (at "block-time" (chain-data)))
            (elapsed (diff-time current-time start)))
        (enforce (<= elapsed limit) "Time limit exceeded! Mission failed.")
        (enforce-ownership zid owner)
        
        ; Check rescue success based on zombie stats
        (let ((success (check-rescue-success zid)))
          (if success
            (let ()
              (update zombies-table cid { "status": "rescued" })
              (zombie-coin.transfer "quest-pool" owner 50.0)
              "Rescue successful!")
            (enforce false "Rescue attempt failed!"))))))
)
```

### Exercise 5.4

Add branching paths to quests based on player choices.

## Chapter 5: Cross-Chain Zombie Adventures

Pacts can even work across chains! Here's a cross-chain transfer:

```pact
(defpact cross-chain-zombie-transfer 
  (zombie-id:string 
   from-chain:string 
   to-chain:string 
   receiver:string)
  @doc "Transfer zombie between chains"
  
  (step ; Lock on source chain
    (enforce (= (at "chain-id" (chain-data)) from-chain) 
      "Must initiate on source chain")
    (let ((owner (at "sender" (chain-data))))
      (with-capability (ZOMBIE_OWNER zombie-id owner)
        (update zombies-table zombie-id 
          { "status": "transferring"
          , "locked": true
          })
        (let ((zombie-data (read zombies-table zombie-id)))
          (yield { "zombie": zombie-data
                 , "target-chain": to-chain
                 , "receiver": receiver
                 })))))
  
  (step ; Burn on source chain
    (resume { "zombie" := zombie-data }
      (enforce (= (at "chain-id" (chain-data)) from-chain)
        "Must burn on source chain")
      (update zombies-table (at "id" zombie-data)
        { "status": "burned"
        , "owner": "burned"
        })
      (yield zombie-data)))
  
  (step ; Mint on target chain
    (resume { "zombie" := zombie-data
            , "receiver" := receiver
            }
      (enforce (= (at "chain-id" (chain-data)) to-chain)
        "Must mint on target chain")
      (let ((new-id (format "{}_{}" 
              [(at "id" zombie-data) to-chain])))
        (insert zombies-table new-id
          (+ zombie-data 
             { "id": new-id
             , "owner": receiver
             , "status": "active"
             , "locked": false
             }))
        (format "Zombie transferred to chain {}" [to-chain])))))
```

### Exercise 5.5

Implement safety checks for cross-chain transfers.

## Chapter 6: Challenge - Quest Designer

Build an advanced quest system with:

1. **Dynamic Quests**: Generate quests based on zombie attributes
2. **Multiplayer Raids**: Coordinate multiple players in one pact
3. **Quest Chains**: Complete one quest to unlock another
4. **Seasonal Events**: Time-limited special quests
5. **Achievement System**: Track quest completions

## Quiz

1. What's the main difference between `defun` and `defpact`?
2. How does `yield` work in pacts?
3. Can different users execute different steps of a pact?
4. How do pacts maintain state between steps?
5. What makes pacts suitable for cross-chain operations?

## Complete Code

See `zombie-quests-complete.pact` for the full implementation!

## Summary

Incredible work! You've mastered multi-step transactions:
- ✅ Creating pacts with multiple steps
- ✅ Managing state with yield/resume
- ✅ Building complex quest systems
- ✅ Time-based mechanics
- ✅ Cross-chain operations
- ✅ Multi-party coordination

Next lesson: Advanced cross-chain features and deployment!