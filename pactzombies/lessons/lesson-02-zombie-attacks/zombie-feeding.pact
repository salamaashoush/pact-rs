(module zombies GOV

  (defcap GOV () true)

  (defschema zombie
    name:string
    dna:integer
    level:integer
    ready-time:time
    win-count:integer
    loss-count:integer
    owner:string)

  (deftable zombies-table:{zombie})

  (defconst BATTLE_COOLDOWN 300.0) ; 5 minutes

  (defun generate-dna:integer (name:string)
    @doc "Generate DNA from a name"
    (let ((hash-result (hash (+ name (format-time "%F %T" (at "block-time" (chain-data)))))))
      (abs (str-to-int 64 (take 16 hash-result)))))

  (defun create-zombie:string (name:string)
    @doc "Create a new zombie"
    (let* ((owner (at "sender" (chain-data)))
           (dna (generate-dna name))
           (zombie-id (format "zombie_{}" [dna])))
      (insert zombies-table zombie-id
        { "name": name
        , "dna": dna
        , "level": 1
        , "ready-time": (at "block-time" (chain-data))
        , "win-count": 0
        , "loss-count": 0
        , "owner": owner
        })
      (format "Zombie {} created with DNA: {}" [name dna])))

  (defun feed-and-multiply:string (zombie-id:string victim-name:string)
    @doc "Zombie feeds and creates a new zombie"
    (with-read zombies-table zombie-id
      { "owner" := owner
      , "dna" := zombie-dna
      , "name" := zombie-name
      , "level" := level
      }
      ; Only owner can make their zombie feed
      (enforce (= owner (at "sender" (chain-data))) "Not your zombie!")
      
      ; Generate new zombie DNA by combining
      (let* ((victim-dna (generate-dna victim-name))
             (new-dna (/ (+ zombie-dna victim-dna) 2))
             (new-zombie-name (format "Zombie {}" [victim-name]))
             (new-zombie-id (format "zombie_{}" [new-dna])))
        
        ; Create the new zombie
        (insert zombies-table new-zombie-id
          { "name": new-zombie-name
          , "dna": new-dna
          , "level": 1
          , "ready-time": (at "block-time" (chain-data))
          , "win-count": 0
          , "loss-count": 0
          , "owner": owner
          })
        
        ; Level up the feeding zombie
        (update zombies-table zombie-id
          { "level": (+ level 1)
          , "win-count": (+ (at "win-count" (read zombies-table zombie-id)) 1)
          })
        
        (format "{} fed on {} and created {}" 
          [zombie-name victim-name new-zombie-name]))))

  (defun calculate-battle-power:integer (zombie:object{zombie})
    @doc "Calculate zombie's battle power"
    (let ((dna (at "dna" zombie))
          (level (at "level" zombie))
          (wins (at "win-count" zombie)))
      (+ (* level 100) 
         (* wins 50) 
         (mod dna 100))))

  (defun check-ready:bool (zombie-id:string)
    @doc "Check if zombie is ready to battle"
    (with-read zombies-table zombie-id { "ready-time" := ready-time }
      (let ((current-time (at "block-time" (chain-data))))
        (>= (diff-time current-time ready-time) BATTLE_COOLDOWN))))

  (defun battle:string (attacker-id:string defender-id:string)
    @doc "Two zombies battle!"
    (let ((attacker (read zombies-table attacker-id))
          (defender (read zombies-table defender-id)))
      
      ; Check ownership of attacker
      (enforce (= (at "owner" attacker) (at "sender" (chain-data))) 
        "Not your zombie!")
      
      ; Check if attacker is ready
      (enforce (check-ready attacker-id) "Zombie is still cooling down!")
      
      ; Calculate battle powers
      (let ((attacker-power (calculate-battle-power attacker))
            (defender-power (calculate-battle-power defender))
            (random-factor (mod (at "block-height" (chain-data)) 100)))
        
        ; Add some randomness
        (let ((final-attacker-power (+ attacker-power random-factor))
              (final-defender-power (+ defender-power (- 100 random-factor))))
          
          ; Update ready time
          (update zombies-table attacker-id 
            { "ready-time": (add-time (at "block-time" (chain-data)) BATTLE_COOLDOWN) })
          
          (if (> final-attacker-power final-defender-power)
            ; Attacker wins
            (let ()
              (update zombies-table attacker-id 
                { "win-count": (+ (at "win-count" attacker) 1)
                , "level": (+ (at "level" attacker) 1)
                })
              (update zombies-table defender-id
                { "loss-count": (+ (at "loss-count" defender) 1)
                })
              (format "{} defeated {}! Power: {} vs {}" 
                [(at "name" attacker) (at "name" defender) 
                 final-attacker-power final-defender-power]))
            ; Defender wins
            (let ()
              (update zombies-table defender-id
                { "win-count": (+ (at "win-count" defender) 1)
                })
              (update zombies-table attacker-id
                { "loss-count": (+ (at "loss-count" attacker) 1)
                })
              (format "{} was defeated by {}! Power: {} vs {}"
                [(at "name" attacker) (at "name" defender)
                 final-attacker-power final-defender-power])))))))

  (defun get-zombie-stats:object (zombie-id:string)
    @doc "Get detailed stats for a zombie"
    (let ((zombie (read zombies-table zombie-id)))
      { "name": (at "name" zombie)
      , "level": (at "level" zombie)
      , "power": (calculate-battle-power zombie)
      , "wins": (at "win-count" zombie)
      , "losses": (at "loss-count" zombie)
      , "owner": (at "owner" zombie)
      }))

  (defun get-my-zombies:[object{zombie}] ()
    @doc "Get all zombies owned by caller"
    (select zombies-table (where "owner" (= (at "sender" (chain-data))))))
)