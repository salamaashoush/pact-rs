# Lesson 3: Advanced Zombie Features - Ownership & Security 🔐

Welcome back, zombie master! Your zombies can now fight and multiply. But in the blockchain world, security is paramount. In this lesson, you'll learn:

- Pact's powerful capability system
- Managing zombie ownership
- Implementing secure transfers
- Creating helper functions
- Access control patterns

## Chapter 1: Understanding Capabilities

Capabilities are Pact's way of managing permissions. Think of them as special keys that unlock certain functions.

### What are Capabilities?

```pact
(defcap ZOMBIE_OWNER (zombie-id:string owner:string)
  @doc "Capability for zombie ownership"
  (with-read zombies-table zombie-id { "owner" := zombie-owner }
    (enforce (= owner zombie-owner) "You don't own this zombie!")))
```

Capabilities can:
- Be required by functions using `with-capability`
- Be granted to others using `grant`
- Be composed together for complex permissions

### Exercise 3.1

Create a new file `zombie-ownership.pact` starting with our zombie module and add the ownership capability above.

## Chapter 2: Secure Zombie Transfers

Let's implement a secure way to transfer zombies between users:

```pact
(defcap TRANSFER (zombie-id:string sender:string receiver:string)
  @doc "Capability to transfer a zombie"
  @managed
  (enforce (!= sender receiver) "Cannot transfer to yourself")
  (enforce-guard (at "guard" (coin.details sender)))
  (with-capability (ZOMBIE_OWNER zombie-id sender)
    true))

(defun transfer-zombie (zombie-id:string receiver:string)
  @doc "Transfer a zombie to another user"
  (let ((sender (at "sender" (chain-data))))
    (with-capability (TRANSFER zombie-id sender receiver)
      (update zombies-table zombie-id { "owner": receiver })
      (emit-event (ZOMBIE_TRANSFERRED zombie-id sender receiver))
      (format "Zombie {} transferred from {} to {}" 
        [zombie-id sender receiver]))))
```

### Key Concepts:
- `@managed` - Makes the capability manageable (can be passed around)
- `enforce-guard` - Checks cryptographic signatures
- `emit-event` - Creates blockchain events for tracking

### Exercise 3.2

Add the transfer capability and function to your module. Don't forget to define the event!

## Chapter 3: Helper Functions and Views

Not all functions need to modify state. Let's add some view functions:

```pact
(defschema zombie-listing
  id:string
  name:string
  level:integer
  type:string
  price:decimal
  for-sale:bool)

(deftable market-table:{zombie-listing})

(defun list-zombie-for-sale (zombie-id:string price:decimal)
  @doc "List a zombie for sale on the market"
  (with-capability (ZOMBIE_OWNER zombie-id (at "sender" (chain-data)))
    (insert market-table zombie-id
      { "id": zombie-id
      , "name": (at "name" (read zombies-table zombie-id))
      , "level": (at "level" (read zombies-table zombie-id))
      , "type": (at "type" (read zombies-table zombie-id))
      , "price": price
      , "for-sale": true
      })
    (format "Zombie {} listed for {} KDA" [zombie-id price])))

(defun get-zombies-for-sale:[object{zombie-listing}] ()
  @doc "Get all zombies currently for sale"
  (select market-table (where "for-sale" (= true))))

(defun get-zombie-count:integer (owner:string)
  @doc "Count zombies owned by an address"
  (length (select zombies-table (where "owner" (= owner)))))
```

### Exercise 3.3

Implement the marketplace functions. Notice how some functions are read-only (no state changes).

## Chapter 4: Advanced Access Control

Let's create role-based permissions for our zombie game:

```pact
(defcap ADMIN ()
  @doc "Admin capability"
  (enforce-guard (read-keyset "zombies-admin-keyset")))

(defcap GAME_PAUSED ()
  @doc "Capability to pause game functions"
  true)

(defschema game-state
  paused:bool
  total-zombies:integer
  total-battles:integer)

(deftable game-state-table:{game-state})
(defconst GAME_STATE_KEY "SINGLETON")

(defun pause-game (paused:bool)
  @doc "Admin function to pause/unpause the game"
  (with-capability (ADMIN)
    (update game-state-table GAME_STATE_KEY { "paused": paused })
    (if paused
      "Game paused!"
      "Game resumed!")))

(defun enforce-not-paused ()
  @doc "Ensure game is not paused"
  (let ((paused (at "paused" (read game-state-table GAME_STATE_KEY))))
    (enforce (not paused) "Game is currently paused")))

; Update battle function to check pause state
(defun battle-with-pause-check (attacker-id:string defender-id:string)
  (enforce-not-paused)
  ; ... rest of battle logic
)
```

### Exercise 3.4

Add admin capabilities and pause functionality to your module.

## Chapter 5: Zombie Alliances

Let's create a system where players can form zombie alliances:

```pact
(defschema alliance
  name:string
  leader:string
  members:[string]
  created-at:time
  zombie-count:integer)

(deftable alliances-table:{alliance})

(defcap ALLIANCE_LEADER (alliance-name:string leader:string)
  @doc "Capability for alliance leadership"
  (with-read alliances-table alliance-name { "leader" := current-leader }
    (enforce (= leader current-leader) "Not the alliance leader!")))

(defun create-alliance (name:string)
  @doc "Create a new zombie alliance"
  (let ((creator (at "sender" (chain-data))))
    (insert alliances-table name
      { "name": name
      , "leader": creator
      , "members": [creator]
      , "created-at": (at "block-time" (chain-data))
      , "zombie-count": (get-zombie-count creator)
      })
    (format "Alliance {} created!" [name])))

(defun join-alliance (alliance-name:string)
  @doc "Join an existing alliance"
  (let ((joiner (at "sender" (chain-data))))
    (with-read alliances-table alliance-name 
      { "members" := current-members }
      (enforce (not (contains joiner current-members)) 
        "Already in this alliance!")
      (update alliances-table alliance-name
        { "members": (+ current-members [joiner])
        , "zombie-count": (+ (at "zombie-count" 
            (read alliances-table alliance-name))
            (get-zombie-count joiner))
        }))
    (format "{} joined alliance {}!" [joiner alliance-name])))

(defun alliance-battle-bonus:decimal (zombie-id:string)
  @doc "Get battle bonus for alliance members"
  (let ((owner (at "owner" (read zombies-table zombie-id))))
    (let ((user-alliances 
      (select alliances-table 
        (where "members" (contains owner)))))
      (if (> (length user-alliances) 0)
        1.1  ; 10% bonus for alliance members
        1.0))))
```

### Exercise 3.5

Implement the alliance system and integrate the battle bonus into your battle function.

## Chapter 6: Challenge - Zombie Breeding Program

Create an advanced breeding system with these features:

1. **Breeding Permissions**: Only zombie owners can breed their zombies
2. **Breeding Fees**: Charge KDA for breeding (integrate with coin contract)
3. **Genetics**: Implement dominant/recessive traits
4. **Breeding Cooldown**: Prevent spam breeding
5. **Rare Zombies**: Create special conditions for rare zombie types

## Quiz

1. What's the difference between `defcap` and `defun`?
2. How do you check if a user owns a zombie?
3. What does `@managed` do on a capability?
4. How can you make a function admin-only?
5. What's the purpose of `emit-event`?

## Complete Code

Check `zombie-ownership-complete.pact` for the full implementation!

## Summary

Congratulations! You've mastered advanced Pact concepts:
- ✅ Capability-based security
- ✅ Ownership management
- ✅ Secure transfers
- ✅ Admin controls
- ✅ Alliance systems
- ✅ Complex access patterns

Next lesson: We'll create a zombie economy with fungible tokens!