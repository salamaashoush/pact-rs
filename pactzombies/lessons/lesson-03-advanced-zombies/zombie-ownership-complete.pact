(module zombies GOV
  
  (defcap GOV () 
    (enforce-guard (read-keyset "zombies-admin")))

  ; Schemas
  (defschema zombie
    name:string
    dna:integer
    type:string
    level:integer
    xp:integer
    ready-time:time
    breed-count:integer
    win-count:integer
    loss-count:integer
    owner:string
    traits:[string])

  (defschema zombie-listing
    id:string
    name:string
    level:integer
    type:string
    price:decimal
    for-sale:bool)

  (defschema game-state
    paused:bool
    total-zombies:integer
    total-battles:integer
    breeding-fee:decimal)

  (defschema alliance
    name:string
    leader:string
    members:[string]
    created-at:time
    zombie-count:integer
    treasury:decimal)

  (defschema breeding-request
    zombie1-id:string
    zombie2-id:string
    owner1:string
    owner2:string
    fee:decimal
    approved:bool
    created-at:time)

  ; Tables
  (deftable zombies-table:{zombie})
  (deftable market-table:{zombie-listing})
  (deftable game-state-table:{game-state})
  (deftable alliances-table:{alliance})
  (deftable breeding-table:{breeding-request})

  ; Constants
  (defconst GAME_STATE_KEY "SINGLETON")
  (defconst BATTLE_COOLDOWN 300.0)
  (defconst BREEDING_COOLDOWN 3600.0) ; 1 hour
  (defconst MAX_BREED_COUNT 3)
  (defconst ALLIANCE_BATTLE_BONUS 1.1)
  (defconst RARE_ZOMBIE_CHANCE 0.05)

  ; Traits for genetics
  (defconst DOMINANT_TRAITS ["strong" "fast" "intelligent"])
  (defconst RECESSIVE_TRAITS ["stealthy" "regenerating" "psychic"])

  ; Events
  (defcap ZOMBIE_TRANSFERRED:bool (zombie-id:string from:string to:string)
    @event true)

  (defcap ZOMBIE_SOLD:bool (zombie-id:string from:string to:string price:decimal)
    @event true)

  (defcap ZOMBIE_BRED:bool (zombie-id:string parent1:string parent2:string)
    @event true)

  (defcap ALLIANCE_CREATED:bool (name:string leader:string)
    @event true)

  ; Capabilities
  (defcap ZOMBIE_OWNER (zombie-id:string owner:string)
    @doc "Capability for zombie ownership"
    (with-read zombies-table zombie-id { "owner" := zombie-owner }
      (enforce (= owner zombie-owner) "You don't own this zombie!")))

  (defcap TRANSFER (zombie-id:string sender:string receiver:string)
    @doc "Capability to transfer a zombie"
    @managed
    (enforce (!= sender receiver) "Cannot transfer to yourself")
    (with-capability (ZOMBIE_OWNER zombie-id sender) true))

  (defcap BREED (zombie1-id:string zombie2-id:string)
    @doc "Capability to breed zombies"
    (let ((owner (at "sender" (chain-data))))
      (with-capability (ZOMBIE_OWNER zombie1-id owner)
        (with-read zombies-table zombie1-id { "breed-count" := count1 }
          (enforce (< count1 MAX_BREED_COUNT) "Zombie 1 breeding limit reached")))
      (with-capability (ZOMBIE_OWNER zombie2-id owner)
        (with-read zombies-table zombie2-id { "breed-count" := count2 }
          (enforce (< count2 MAX_BREED_COUNT) "Zombie 2 breeding limit reached")))))

  (defcap ADMIN ()
    @doc "Admin capability"
    (enforce-guard (read-keyset "zombies-admin")))

  (defcap ALLIANCE_LEADER (alliance-name:string leader:string)
    @doc "Capability for alliance leadership"
    (with-read alliances-table alliance-name { "leader" := current-leader }
      (enforce (= leader current-leader) "Not the alliance leader!")))

  ; Helper Functions
  (defun initialize:string ()
    @doc "Initialize game state"
    (with-capability (ADMIN)
      (insert game-state-table GAME_STATE_KEY
        { "paused": false
        , "total-zombies": 0
        , "total-battles": 0
        , "breeding-fee": 10.0
        })
      "Game initialized!"))

  (defun enforce-not-paused ()
    @doc "Ensure game is not paused"
    (let ((paused (at "paused" (read game-state-table GAME_STATE_KEY))))
      (enforce (not paused) "Game is currently paused")))

  (defun generate-dna:integer (input:string)
    @doc "Generate DNA from input"
    (abs (str-to-int 64 (take 16 (hash input)))))

  (defun generate-traits:[string] (dna1:integer dna2:integer)
    @doc "Generate traits based on parent DNA"
    (let ((trait-roll (mod (+ dna1 dna2) 100)))
      (cond
        ((< trait-roll 60) [(at (mod dna1 3) DOMINANT_TRAITS)])
        ((< trait-roll 90) [(at (mod dna1 3) DOMINANT_TRAITS) 
                           (at (mod dna2 3) RECESSIVE_TRAITS)])
        [(at (mod dna1 3) DOMINANT_TRAITS) 
         (at (mod dna2 3) RECESSIVE_TRAITS)
         "rare"])))

  (defun is-rare-zombie:bool (dna:integer)
    @doc "Check if zombie is rare"
    (< (/ (mod dna 100) 100.0) RARE_ZOMBIE_CHANCE))

  ; Core Functions
  (defun create-zombie:string (name:string)
    @doc "Create a new zombie"
    (enforce-not-paused)
    (let* ((owner (at "sender" (chain-data)))
           (dna (generate-dna (+ name (format "{}" [(at "block-time" (chain-data))]))))
           (zombie-id (format "zombie_{}" [dna]))
           (zombie-type (at (mod dna 5) ["walker" "runner" "tank" "hunter" "spitter"]))
           (traits (if (is-rare-zombie dna) ["rare" "legendary"] ["common"])))
      (insert zombies-table zombie-id
        { "name": name
        , "dna": dna
        , "type": zombie-type
        , "level": 1
        , "xp": 0
        , "ready-time": (at "block-time" (chain-data))
        , "breed-count": 0
        , "win-count": 0
        , "loss-count": 0
        , "owner": owner
        , "traits": traits
        })
      (with-read game-state-table GAME_STATE_KEY { "total-zombies" := total }
        (update game-state-table GAME_STATE_KEY 
          { "total-zombies": (+ total 1) }))
      (format "Zombie {} created! Type: {}, Traits: {}" [name zombie-type traits])))

  (defun breed-zombies:string (zombie1-id:string zombie2-id:string baby-name:string)
    @doc "Breed two zombies to create a new one"
    (enforce-not-paused)
    (with-capability (BREED zombie1-id zombie2-id)
      (let* ((zombie1 (read zombies-table zombie1-id))
             (zombie2 (read zombies-table zombie2-id))
             (dna1 (at "dna" zombie1))
             (dna2 (at "dna" zombie2))
             (new-dna (/ (+ dna1 dna2 (generate-dna baby-name)) 3))
             (baby-id (format "zombie_{}" [new-dna]))
             (baby-type (at (mod new-dna 5) ["walker" "runner" "tank" "hunter" "spitter"]))
             (baby-traits (generate-traits dna1 dna2))
             (breeding-fee (at "breeding-fee" (read game-state-table GAME_STATE_KEY))))
        
        ; Check breeding cooldown
        (let ((current-time (at "block-time" (chain-data))))
          (enforce (>= (diff-time current-time (at "ready-time" zombie1)) BREEDING_COOLDOWN)
            "Zombie 1 still in breeding cooldown")
          (enforce (>= (diff-time current-time (at "ready-time" zombie2)) BREEDING_COOLDOWN)
            "Zombie 2 still in breeding cooldown"))
        
        ; In production, transfer breeding fee here
        ; (coin.transfer (at "sender" (chain-data)) GAME_TREASURY breeding-fee)
        
        ; Create baby zombie
        (insert zombies-table baby-id
          { "name": baby-name
          , "dna": new-dna
          , "type": baby-type
          , "level": 1
          , "xp": 0
          , "ready-time": (at "block-time" (chain-data))
          , "breed-count": 0
          , "win-count": 0
          , "loss-count": 0
          , "owner": (at "sender" (chain-data))
          , "traits": baby-traits
          })
        
        ; Update parent zombies
        (update zombies-table zombie1-id 
          { "breed-count": (+ (at "breed-count" zombie1) 1)
          , "ready-time": (add-time (at "block-time" (chain-data)) BREEDING_COOLDOWN)
          })
        (update zombies-table zombie2-id 
          { "breed-count": (+ (at "breed-count" zombie2) 1)
          , "ready-time": (add-time (at "block-time" (chain-data)) BREEDING_COOLDOWN)
          })
        
        (emit-event (ZOMBIE_BRED baby-id zombie1-id zombie2-id))
        (format "Baby zombie {} born! Type: {}, Traits: {}" 
          [baby-name baby-type baby-traits]))))

  ; Alliance Functions
  (defun create-alliance:string (name:string)
    @doc "Create a new zombie alliance"
    (enforce-not-paused)
    (let ((creator (at "sender" (chain-data))))
      (insert alliances-table name
        { "name": name
        , "leader": creator
        , "members": [creator]
        , "created-at": (at "block-time" (chain-data))
        , "zombie-count": (get-zombie-count creator)
        , "treasury": 0.0
        })
      (emit-event (ALLIANCE_CREATED name creator))
      (format "Alliance {} created!" [name])))

  (defun join-alliance:string (alliance-name:string)
    @doc "Join an existing alliance"
    (enforce-not-paused)
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

  (defun leave-alliance:string (alliance-name:string)
    @doc "Leave an alliance"
    (let ((leaver (at "sender" (chain-data))))
      (with-read alliances-table alliance-name 
        { "members" := members, "leader" := leader }
        (enforce (contains leaver members) "Not in this alliance!")
        (enforce (!= leaver leader) "Leader cannot leave, transfer leadership first!")
        (update alliances-table alliance-name
          { "members": (filter (!= leaver) members)
          , "zombie-count": (- (at "zombie-count" 
              (read alliances-table alliance-name))
              (get-zombie-count leaver))
          }))
      (format "{} left alliance {}" [leaver alliance-name])))

  (defun get-alliance-bonus:decimal (owner:string)
    @doc "Get battle bonus for alliance members"
    (let ((user-alliances 
      (select alliances-table 
        (where "members" (contains owner)))))
      (if (> (length user-alliances) 0)
        ALLIANCE_BATTLE_BONUS
        1.0)))

  ; Enhanced Battle Function with Alliance Bonus
  (defun battle-with-alliances:string (attacker-id:string defender-id:string)
    @doc "Battle with alliance bonuses"
    (enforce-not-paused)
    (let* ((attacker (read zombies-table attacker-id))
           (defender (read zombies-table defender-id))
           (attacker-owner (at "owner" attacker))
           (defender-owner (at "owner" defender))
           (attacker-bonus (get-alliance-bonus attacker-owner))
           (defender-bonus (get-alliance-bonus defender-owner)))
      
      (enforce (= attacker-owner (at "sender" (chain-data))) 
        "Not your zombie!")
      
      ; Calculate power with alliance bonuses
      (let ((attacker-power (* (calculate-base-power attacker) attacker-bonus))
            (defender-power (* (calculate-base-power defender) defender-bonus))
            (result (if (> attacker-power defender-power) "victory" "defeat")))
        
        (with-read game-state-table GAME_STATE_KEY { "total-battles" := total }
          (update game-state-table GAME_STATE_KEY 
            { "total-battles": (+ total 1) }))
        
        (format "{} vs {} - Result: {} (Power: {} vs {})" 
          [(at "name" attacker) (at "name" defender) result 
           attacker-power defender-power]))))

  (defun calculate-base-power:decimal (zombie:object{zombie})
    @doc "Calculate base power including traits"
    (let ((level (at "level" zombie))
          (wins (at "win-count" zombie))
          (traits (at "traits" zombie)))
      (* (+ (* level 100.0) (* wins 50.0))
         (if (contains "rare" traits) 1.5
             (if (contains "legendary" traits) 2.0 1.0)))))

  ; View Functions
  (defun get-my-zombies:[object{zombie}] ()
    @doc "Get all zombies owned by caller"
    (select zombies-table (where "owner" (= (at "sender" (chain-data))))))

  (defun get-zombie-count:integer (owner:string)
    @doc "Count zombies owned by an address"
    (length (select zombies-table (where "owner" (= owner)))))

  (defun get-alliance-info:object (alliance-name:string)
    @doc "Get detailed alliance information"
    (+ (read alliances-table alliance-name)
       { "member-count": (length (at "members" (read alliances-table alliance-name)))
       , "average-zombie-level": (/ (fold (+) 0 
           (map (lambda (m) 
             (fold (+) 0 
               (map (at "level") 
                 (select zombies-table (where "owner" (= m))))))
             (at "members" (read alliances-table alliance-name))))
           (at "zombie-count" (read alliances-table alliance-name)))
       }))

  ; Admin Functions
  (defun set-breeding-fee:string (fee:decimal)
    @doc "Admin function to set breeding fee"
    (with-capability (ADMIN)
      (enforce (> fee 0.0) "Fee must be positive")
      (update game-state-table GAME_STATE_KEY { "breeding-fee": fee })
      (format "Breeding fee set to {} KDA" [fee])))

  (defun transfer-alliance-leadership:string (alliance-name:string new-leader:string)
    @doc "Transfer alliance leadership"
    (let ((current-leader (at "sender" (chain-data))))
      (with-capability (ALLIANCE_LEADER alliance-name current-leader)
        (with-read alliances-table alliance-name { "members" := members }
          (enforce (contains new-leader members) "New leader must be alliance member!")
          (update alliances-table alliance-name { "leader": new-leader })
          (format "Leadership of {} transferred to {}" 
            [alliance-name new-leader])))))
)