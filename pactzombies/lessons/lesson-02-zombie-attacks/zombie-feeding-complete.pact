(module zombies GOV

  (defcap GOV () true)

  (defschema zombie
    name:string
    dna:integer
    type:string
    level:integer
    xp:integer
    ready-time:time
    win-count:integer
    loss-count:integer
    owner:string
    special-move:string)

  (deftable zombies-table:{zombie})

  (defconst BATTLE_COOLDOWN 300.0)
  (defconst FEED_COOLDOWN 600.0)
  (defconst ZOMBIE_TYPES ["walker" "runner" "tank" "hunter" "spitter"])
  (defconst TYPE_ADVANTAGES 
    { "walker": "spitter"    ; Walker beats Spitter
    , "runner": "walker"     ; Runner beats Walker
    , "tank": "runner"       ; Tank beats Runner
    , "hunter": "tank"       ; Hunter beats Tank
    , "spitter": "hunter"    ; Spitter beats Hunter
    })
  
  (defconst SPECIAL_MOVES
    { "bite": "Basic attack"
    , "charge": "Powerful rush attack"
    , "spit": "Ranged acid attack"
    , "stealth": "Surprise attack"
    , "rage": "Berserk mode"
    })

  (defun generate-dna:integer (name:string)
    @doc "Generate DNA from a name"
    (let ((hash-result (hash (+ name (format-time "%F %T" (at "block-time" (chain-data)))))))
      (abs (str-to-int 64 (take 16 hash-result)))))

  (defun get-zombie-type:string (dna:integer)
    @doc "Determine zombie type from DNA"
    (at (mod dna (length ZOMBIE_TYPES)) ZOMBIE_TYPES))

  (defun get-special-move:string (dna:integer)
    @doc "Determine special move from DNA"
    (let ((moves (keys SPECIAL_MOVES)))
      (at (mod (/ dna 1000) (length moves)) moves)))

  (defun create-zombie:string (name:string)
    @doc "Create a new zombie with type and special move"
    (let* ((owner (at "sender" (chain-data)))
           (dna (generate-dna name))
           (zombie-type (get-zombie-type dna))
           (special-move (get-special-move dna))
           (zombie-id (format "zombie_{}" [dna])))
      (insert zombies-table zombie-id
        { "name": name
        , "dna": dna
        , "type": zombie-type
        , "level": 1
        , "xp": 0
        , "ready-time": (at "block-time" (chain-data))
        , "win-count": 0
        , "loss-count": 0
        , "owner": owner
        , "special-move": special-move
        })
      (format "Zombie {} created! Type: {}, Special: {}" [name zombie-type special-move])))

  (defun feed-and-multiply:string (zombie-id:string victim-name:string species:string)
    @doc "Zombie feeds and creates a new zombie"
    (with-read zombies-table zombie-id
      { "owner" := owner
      , "dna" := zombie-dna
      , "name" := zombie-name
      , "level" := level
      , "xp" := xp
      , "ready-time" := ready-time
      }
      (enforce (= owner (at "sender" (chain-data))) "Not your zombie!")
      
      ; Check cooldown
      (let ((current-time (at "block-time" (chain-data))))
        (enforce (>= (diff-time current-time ready-time) FEED_COOLDOWN) 
          "Zombie needs to rest before feeding again!"))
      
      ; Special DNA modification for different species
      (let* ((victim-dna (generate-dna victim-name))
             (species-modifier (cond
               ((= species "kitty") 99)  ; CryptoKitties reference!
               ((= species "human") 0)
               1))
             (new-dna (+ (/ (+ zombie-dna victim-dna) 2) species-modifier))
             (new-zombie-name (format "Zombie {}" [victim-name]))
             (new-zombie-id (format "zombie_{}" [new-dna])))
        
        ; Create the new zombie
        (insert zombies-table new-zombie-id
          { "name": new-zombie-name
          , "dna": new-dna
          , "type": (get-zombie-type new-dna)
          , "level": 1
          , "xp": 0
          , "ready-time": (at "block-time" (chain-data))
          , "win-count": 0
          , "loss-count": 0
          , "owner": owner
          , "special-move": (get-special-move new-dna)
          })
        
        ; Update the feeding zombie
        (update zombies-table zombie-id
          { "xp": (+ xp 20)
          , "ready-time": (add-time (at "block-time" (chain-data)) FEED_COOLDOWN)
          })
        
        ; Check for level up
        (if (>= (+ xp 20) (* level 100))
          (update zombies-table zombie-id { "level": (+ level 1), "xp": 0 })
          true)
        
        (format "{} fed on {} ({}) and created {}" 
          [zombie-name victim-name species new-zombie-name]))))

  (defun calculate-battle-power:integer (zombie:object{zombie})
    @doc "Calculate zombie's battle power including type bonuses"
    (let ((dna (at "dna" zombie))
          (type (at "type" zombie))
          (level (at "level" zombie))
          (wins (at "win-count" zombie))
          (xp (at "xp" zombie)))
      (+ (* level 100) 
         (* wins 50)
         (/ xp 2)
         (mod dna 100)
         (cond 
           ((= type "tank") 50)      ; Tank bonus
           ((= type "runner") 30)    ; Speed bonus
           ((= type "hunter") 40)    ; Stealth bonus
           0))))

  (defun check-critical-hit:bool (attacker-dna:integer)
    @doc "10% chance for critical hit based on DNA"
    (= (mod attacker-dna 10) 0))

  (defun get-type-multiplier:decimal (attacker-type:string defender-type:string)
    @doc "Get damage multiplier based on type matchup"
    (if (= (at attacker-type TYPE_ADVANTAGES) defender-type)
      1.5  ; 50% bonus damage
      1.0))

  (defun battle:string (attacker-id:string defender-id:string)
    @doc "Enhanced battle system with types and critical hits"
    (let ((attacker (read zombies-table attacker-id))
          (defender (read zombies-table defender-id)))
      
      (enforce (= (at "owner" attacker) (at "sender" (chain-data))) 
        "Not your zombie!")
      
      ; Check if attacker is ready
      (let ((current-time (at "block-time" (chain-data)))
            (ready-time (at "ready-time" attacker)))
        (enforce (>= (diff-time current-time ready-time) BATTLE_COOLDOWN) 
          "Zombie is still cooling down!"))
      
      ; Calculate battle powers with all modifiers
      (let* ((base-attacker-power (calculate-battle-power attacker))
             (base-defender-power (calculate-battle-power defender))
             (type-multiplier (get-type-multiplier 
               (at "type" attacker) (at "type" defender)))
             (critical-hit (check-critical-hit (at "dna" attacker)))
             (critical-multiplier (if critical-hit 2.0 1.0))
             (random-factor (mod (at "block-height" (chain-data)) 100))
             (final-attacker-power (* base-attacker-power type-multiplier critical-multiplier))
             (final-defender-power base-defender-power))
        
        ; Update ready time
        (update zombies-table attacker-id 
          { "ready-time": (add-time (at "block-time" (chain-data)) BATTLE_COOLDOWN) })
        
        (if (> (+ final-attacker-power random-factor) 
               (+ final-defender-power (- 100 random-factor)))
          ; Attacker wins
          (let ((xp-gained 50))
            (update zombies-table attacker-id 
              { "win-count": (+ (at "win-count" attacker) 1)
              , "xp": (+ (at "xp" attacker) xp-gained)
              })
            (update zombies-table defender-id
              { "loss-count": (+ (at "loss-count" defender) 1)
              })
            
            ; Check for level up
            (if (>= (+ (at "xp" attacker) xp-gained) (* (at "level" attacker) 100))
              (update zombies-table attacker-id 
                { "level": (+ (at "level" attacker) 1), "xp": 0 })
              true)
            
            (format "{} ({}) defeated {} ({})! {} Power: {} vs {}" 
              [(at "name" attacker) (at "type" attacker)
               (at "name" defender) (at "type" defender)
               (if critical-hit "CRITICAL HIT!" "")
               final-attacker-power final-defender-power]))
          ; Defender wins
          (let ()
            (update zombies-table defender-id
              { "win-count": (+ (at "win-count" defender) 1)
              , "xp": (+ (at "xp" defender) 30)
              })
            (update zombies-table attacker-id
              { "loss-count": (+ (at "loss-count" attacker) 1)
              })
            (format "{} ({}) was defeated by {} ({})! Power: {} vs {}"
              [(at "name" attacker) (at "type" attacker)
               (at "name" defender) (at "type" defender)
               final-attacker-power final-defender-power]))))))

  (defun use-special-move:string (zombie-id:string target-id:string)
    @doc "Use zombie's special move in battle"
    (with-read zombies-table zombie-id 
      { "special-move" := move
      , "owner" := owner
      , "name" := attacker-name
      }
      (enforce (= owner (at "sender" (chain-data))) "Not your zombie!")
      
      (let ((target-name (at "name" (read zombies-table target-id))))
        (format "{} used {} on {}! {}" 
          [attacker-name move target-name (at move SPECIAL_MOVES)]))))

  (defun get-zombie-details:object (zombie-id:string)
    @doc "Get comprehensive zombie information"
    (let ((zombie (read zombies-table zombie-id)))
      { "name": (at "name" zombie)
      , "type": (at "type" zombie)
      , "level": (at "level" zombie)
      , "xp": (format "{}/{}" [(at "xp" zombie) (* (at "level" zombie) 100)])
      , "power": (calculate-battle-power zombie)
      , "special-move": (at "special-move" zombie)
      , "record": (format "{} wins / {} losses" 
          [(at "win-count" zombie) (at "loss-count" zombie)])
      , "owner": (at "owner" zombie)
      , "ready": (check-ready zombie-id)
      }))

  (defun check-ready:bool (zombie-id:string)
    @doc "Check if zombie is ready for action"
    (with-read zombies-table zombie-id { "ready-time" := ready-time }
      (let ((current-time (at "block-time" (chain-data))))
        (>= (diff-time current-time ready-time) BATTLE_COOLDOWN))))

  (defun get-leaderboard:[object] (limit:integer)
    @doc "Get top zombies by wins"
    (take limit 
      (reverse 
        (sort ["win-count"] 
          (map (lambda (z) 
            { "name": (at "name" z)
            , "type": (at "type" z)
            , "level": (at "level" z)
            , "wins": (at "win-count" z)
            , "owner": (at "owner" z)
            })
            (select zombies-table (constantly true)))))))
)