;; game-dungeons.pact
;; Dungeon exploration and loot system
(module game-dungeons GOVERNANCE
  @doc "Dungeon exploration with random encounters and loot"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'game-admin))
  
  ;; Schemas
  (defschema dungeon
    @doc "Dungeon definition"
    dungeon-id:string
    name:string
    description:string
    difficulty:integer      ;; 1-10
    min-level:integer
    floors:integer
    boss-monster:string
    completion-reward:object
    active:bool)
  
  (defschema dungeon-run
    @doc "Active dungeon exploration"
    run-id:string
    character-id:string
    dungeon-id:string
    current-floor:integer
    monsters-defeated:integer
    loot-collected:[string]
    status:string          ;; exploring, completed, failed
    started-at:time)
  
  (defschema monster
    @doc "Monster template"
    monster-id:string
    name:string
    level:integer
    health:integer
    attack:integer
    defense:integer
    exp-reward:integer
    loot-table:[object])   ;; Item drops with rates
  
  ;; Tables
  (deftable dungeons:{dungeon})
  (deftable dungeon-runs:{dungeon-run})
  (deftable monsters:{monster})
  
  ;; Capabilities
  (defcap EXPLORE (character-id:string dungeon-id:string)
    @doc "Dungeon exploration capability"
    (let ((character (game-characters.get-character character-id)))
      (with-read dungeons dungeon-id { "min-level" := min-level }
        (enforce (>= (at 'level character) min-level) 
                "Character level too low")
        (enforce (= (at 'status character) "active") 
                "Character must be active"))))
  
  ;; Dungeon Creation
  (defun create-dungeon:string 
    ( dungeon-id:string
      name:string
      description:string
      difficulty:integer
      min-level:integer
      floors:integer
      boss-id:string
      reward:object )
    @doc "Create new dungeon"
    (with-capability (GOVERNANCE)
      (enforce (and (>= difficulty 1) (<= difficulty 10)) 
              "Difficulty must be 1-10")
      
      (insert dungeons dungeon-id {
        "dungeon-id": dungeon-id,
        "name": name,
        "description": description,
        "difficulty": difficulty,
        "min-level": min-level,
        "floors": floors,
        "boss-monster": boss-id,
        "completion-reward": reward,
        "active": true
      })
      (format "Dungeon {} created" [name])))
  
  ;; Monster Creation
  (defun create-monster:string 
    ( monster-id:string
      name:string
      level:integer
      health:integer
      attack:integer
      defense:integer
      exp-reward:integer
      loot-table:[object] )
    @doc "Create monster template"
    (with-capability (GOVERNANCE)
      (insert monsters monster-id {
        "monster-id": monster-id,
        "name": name,
        "level": level,
        "health": health,
        "attack": attack,
        "defense": defense,
        "exp-reward": exp-reward,
        "loot-table": loot-table
      })
      (format "Monster {} created" [name])))
  
  ;; Dungeon Exploration
  (defun enter-dungeon:string (character-id:string dungeon-id:string)
    @doc "Start dungeon exploration"
    (with-capability (EXPLORE character-id dungeon-id)
      (let ((run-id (generate-run-id character-id dungeon-id)))
        
        ;; Update character status
        (game-characters.update-status character-id "in-dungeon")
        
        ;; Create dungeon run
        (insert dungeon-runs run-id {
          "run-id": run-id,
          "character-id": character-id,
          "dungeon-id": dungeon-id,
          "current-floor": 1,
          "monsters-defeated": 0,
          "loot-collected": [],
          "status": "exploring",
          "started-at": (at 'block-time (chain-data))
        })
        
        ;; Generate first encounter
        (generate-encounter run-id)
        
        run-id)))
  
  (defun explore-floor:string (run-id:string action:string)
    @doc "Explore current dungeon floor"
    (with-read dungeon-runs run-id
      { "character-id" := character-id
      , "dungeon-id" := dungeon-id
      , "current-floor" := floor
      , "status" := status }
      
      (enforce (= status "exploring") "Dungeon run not active")
      
      (cond
        ;; Fight monster
        ((= action "fight")
         (let* ((monster (generate-floor-monster dungeon-id floor))
                (battle-result (quick-battle character-id monster)))
           
           (if (at 'won battle-result)
             (progn
               ;; Victory
               (handle-victory run-id monster)
               (if (is-floor-complete run-id floor)
                 (advance-floor run-id)
                 (generate-encounter run-id)))
             ;; Defeat
             (handle-defeat run-id))))
        
        ;; Flee dungeon
        ((= action "flee")
         (flee-dungeon run-id))
        
        ;; Use item
        ((= action "rest")
         (rest-in-dungeon character-id))
        
        (true "Invalid action"))))
  
  (defun advance-floor:string (run-id:string)
    @doc "Move to next dungeon floor"
    (with-read dungeon-runs run-id
      { "dungeon-id" := dungeon-id
      , "current-floor" := floor }
      
      (with-read dungeons dungeon-id
        { "floors" := max-floors
        , "boss-monster" := boss-id }
        
        (if (>= floor max-floors)
          ;; Boss floor reached
          (initiate-boss-battle run-id boss-id)
          ;; Next floor
          (progn
            (update dungeon-runs run-id {
              "current-floor": (+ floor 1)
            })
            (generate-encounter run-id)
            (format "Advanced to floor {}" [(+ floor 1)])))))
  
  (defun complete-dungeon:string (run-id:string)
    @doc "Complete dungeon and distribute rewards"
    (with-read dungeon-runs run-id
      { "character-id" := character-id
      , "dungeon-id" := dungeon-id
      , "monsters-defeated" := kills
      , "loot-collected" := loot }
      
      (with-read dungeons dungeon-id
        { "completion-reward" := reward }
        
        ;; Update run status
        (update dungeon-runs run-id {
          "status": "completed"
        })
        
        ;; Update character status
        (game-characters.update-status character-id "active")
        
        ;; Distribute completion rewards
        (distribute-completion-rewards character-id reward)
        
        ;; Give collected loot
        (map (lambda (item-id)
               (game-items.give-item character-id item-id 1))
             loot)
        
        (format "Dungeon completed! Defeated {} monsters" [kills]))))
  
  ;; Battle System
  (defun quick-battle:object (character-id:string monster:object)
    @doc "Simplified battle calculation"
    (let* ((character (game-characters.get-character character-id))
           (char-power (+ (at 'attack-power character) 
                         (at 'level character)))
           (char-defense (at 'defense character))
           (monster-power (at 'attack monster))
           (monster-defense (at 'defense monster))
           (char-health (at 'health character))
           (monster-health (at 'health monster))
           
           ;; Simple combat simulation
           (rounds-to-kill-monster 
             (ceiling (/ monster-health 
                        (max 1 (- char-power monster-defense)))))
           (rounds-to-die 
             (ceiling (/ char-health 
                        (max 1 (- monster-power char-defense)))))
           
           (won (< rounds-to-kill-monster rounds-to-die))
           (damage-taken (if won 
                          (* monster-power (- rounds-to-kill-monster 1))
                          char-health)))
      
      { "won": won
      , "damage-taken": damage-taken
      , "rounds": (min rounds-to-kill-monster rounds-to-die) }))
  
  (defun handle-victory:string (run-id:string monster:object)
    @doc "Handle monster defeat"
    (with-read dungeon-runs run-id
      { "character-id" := character-id
      , "monsters-defeated" := kills
      , "loot-collected" := current-loot }
      
      ;; Award experience
      (game-characters.gain-experience character-id 
                                     (at 'exp-reward monster))
      
      ;; Roll for loot
      (let ((loot (roll-loot (at 'loot-table monster))))
        
        ;; Update run
        (update dungeon-runs run-id {
          "monsters-defeated": (+ kills 1),
          "loot-collected": (+ current-loot loot)
        })
        
        (format "Victory! Gained {} exp" 
                [(at 'exp-reward monster)]))))
  
  (defun handle-defeat:string (run-id:string)
    @doc "Handle character defeat"
    (with-read dungeon-runs run-id
      { "character-id" := character-id }
      
      ;; Update run status
      (update dungeon-runs run-id {
        "status": "failed"
      })
      
      ;; Update character
      (game-characters.update-status character-id "defeated")
      
      "Defeated! Dungeon run failed"))
  
  ;; Loot System
  (defun roll-loot:[string] (loot-table:[object])
    @doc "Roll for item drops"
    (let ((roll (get-random-decimal)))
      (filter (!= "") 
              (map (lambda (entry)
                    (if (<= roll (at 'drop-rate entry))
                      (at 'item-id entry)
                      ""))
                   loot-table))))
  
  ;; Helper Functions
  (defun generate-run-id:string (character-id:string dungeon-id:string)
    @doc "Generate unique run ID"
    (format "RUN-{}-{}-{}" 
            [character-id dungeon-id (at 'block-height (chain-data))]))
  
  (defun generate-encounter:string (run-id:string)
    @doc "Generate random encounter"
    ;; Simplified - would generate various encounters
    "Monster encounter!")
  
  (defun generate-floor-monster:object (dungeon-id:string floor:integer)
    @doc "Generate monster for floor"
    ;; Simplified - return a basic monster
    (with-read dungeons dungeon-id { "difficulty" := diff }
      (read monsters (format "goblin-{}" [diff]))))
  
  (defun is-floor-complete:bool (run-id:string floor:integer)
    @doc "Check if floor is cleared"
    ;; Simplified - check monster count
    (with-read dungeon-runs run-id 
      { "monsters-defeated" := kills }
      (>= kills (* floor 3))))
  
  (defun initiate-boss-battle:string (run-id:string boss-id:string)
    @doc "Start boss battle"
    (with-read dungeon-runs run-id
      { "character-id" := character-id }
      
      (let* ((boss (read monsters boss-id))
             (battle-result (quick-battle character-id boss)))
        
        (if (at 'won battle-result)
          (complete-dungeon run-id)
          (handle-defeat run-id)))))
  
  (defun flee-dungeon:string (run-id:string)
    @doc "Flee from dungeon"
    (with-read dungeon-runs run-id
      { "character-id" := character-id }
      
      (update dungeon-runs run-id {
        "status": "fled"
      })
      
      (game-characters.update-status character-id "active")
      
      "Fled from dungeon!"))
  
  (defun rest-in-dungeon:string (character-id:string)
    @doc "Rest to recover health"
    (game-characters.heal-character character-id 20)
    "Rested and recovered 20 HP")
  
  (defun distribute-completion-rewards:string 
    ( character-id:string reward:object )
    @doc "Give dungeon completion rewards"
    ;; Give items
    (if (contains "items" reward)
      (map (lambda (item)
             (game-items.give-item character-id 
                                 (at 'item-id item) 
                                 (at 'quantity item)))
           (at 'items reward))
      [])
    
    ;; Give experience
    (if (contains "experience" reward)
      (game-characters.gain-experience character-id 
                                     (at 'experience reward))
      "")
    
    "Rewards distributed")
  
  (defun get-random-decimal:decimal ()
    @doc "Get random decimal between 0 and 1"
    (/ (mod (at 'block-height (chain-data)) 100) 100.0))
  
  ;; Query Functions
  (defun get-dungeon-info:object (dungeon-id:string)
    @doc "Get dungeon information"
    (read dungeons dungeon-id))
  
  (defun get-active-run:object (character-id:string)
    @doc "Get character's active dungeon run"
    (let ((runs (select dungeon-runs 
                       (where 'character-id (= character-id)))))
      (let ((active (filter (lambda (run) 
                             (= (at 'status run) "exploring"))
                           runs)))
        (if (> (length active) 0)
          (at 0 active)
          {}))))
)

(create-table dungeons)
(create-table dungeon-runs)
(create-table monsters)