;; game-combat-fixed.pact
;; Combat system for blockchain RPG (fixed version)
(module game-combat GOVERNANCE
  @doc "Turn-based combat system"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'game-admin))
  
  ;; Schemas
  (defschema battle
    @doc "Active battle data"
    battle-id:string
    attacker-id:string
    defender-id:string
    turn:integer
    attacker-health:integer
    defender-health:integer
    status:string          ;; active, completed
    winner:string
    battle-log:[string]
    started-at:time)
  
  (defschema combat-stats
    @doc "Character combat statistics"
    character-id:string
    battles-won:integer
    battles-lost:integer
    total-damage-dealt:integer
    total-damage-taken:integer
    monsters-defeated:integer)
  
  ;; Tables
  (deftable battles:{battle})
  (deftable combat-stats-table:{combat-stats})
  
  ;; Constants
  (defconst DAMAGE_VARIANCE 0.2)  ;; ±20% damage variance
  (defconst DODGE_BASE_CHANCE 0.05)
  (defconst BLOCK_REDUCTION 0.5)
  
  ;; Capabilities
  (defcap BATTLE (character-id:string)
    @doc "Battle participation capability"
    (with-read game-characters.characters character-id 
      { "owner" := owner, "status" := status }
      (enforce (= status "active") "Character not active")
      (enforce-guard (at 'guard (coin.details owner)))))
  
  ;; Battle Management
  (defun initiate-battle:string (attacker-id:string defender-id:string)
    @doc "Start a new battle"
    (with-capability (BATTLE attacker-id)
      ;; Get character data
      (let* ((attacker (game-characters.get-character attacker-id))
             (defender (game-characters.get-character defender-id))
             (battle-id (generate-battle-id attacker-id defender-id)))
        
        ;; Update character statuses
        (game-characters.update-status attacker-id "in-combat")
        (game-characters.update-status defender-id "in-combat")
        
        ;; Create battle
        (insert battles battle-id {
          "battle-id": battle-id,
          "attacker-id": attacker-id,
          "defender-id": defender-id,
          "turn": 0,
          "attacker-health": (at 'health attacker),
          "defender-health": (at 'health defender),
          "status": "active",
          "winner": "",
          "battle-log": ["Battle started!"],
          "started-at": (at 'block-time (chain-data))
        })
        
        battle-id)))
  
  ;; Combat Actions
  (defun attack:string (battle-id:string character-id:string)
    @doc "Perform basic attack"
    (with-capability (BATTLE character-id)
      (with-read battles battle-id 
        { "status" := status
        , "attacker-id" := attacker-id
        , "defender-id" := defender-id }
        
        (enforce (= status "active") "Battle not active")
        (enforce (or (= character-id attacker-id) 
                    (= character-id defender-id)) 
                "Not a participant")
        
        (let* ((is-attacker (= character-id attacker-id))
               (target-id (if is-attacker defender-id attacker-id))
               (attacker-char (game-characters.get-character character-id))
               (defender-char (game-characters.get-character target-id))
               (damage (calculate-damage attacker-char defender-char "physical")))
          
          (apply-damage battle-id is-attacker damage)
          (add-battle-log battle-id 
            (format "{} attacks {} for {} damage!" 
                    [(at 'name attacker-char) 
                     (at 'name defender-char) 
                     damage]))
          
          (check-battle-end battle-id)))))
  
  (defun cast-spell:string 
    ( battle-id:string 
      character-id:string 
      spell-name:string )
    @doc "Cast a spell"
    (with-capability (BATTLE character-id)
      (with-read battles battle-id 
        { "status" := status
        , "attacker-id" := attacker-id
        , "defender-id" := defender-id }
        
        (enforce (= status "active") "Battle not active")
        
        (let* ((is-attacker (= character-id attacker-id))
               (target-id (if is-attacker defender-id attacker-id))
               (caster (game-characters.get-character character-id))
               (target (game-characters.get-character target-id))
               (spell-cost (get-spell-cost spell-name))
               (current-mana (at 'mana caster)))
          
          (enforce (>= current-mana spell-cost) "Insufficient mana")
          
          ;; Deduct mana
          (game-characters.restore-mana character-id (- spell-cost))
          
          (let ((damage (calculate-damage caster target "magical")))
            (apply-damage battle-id is-attacker damage)
            (add-battle-log battle-id 
              (format "{} casts {} on {} for {} damage!" 
                      [(at 'name caster) 
                       spell-name 
                       (at 'name target) 
                       damage]))
            
            (check-battle-end battle-id))))))
  
  ;; Damage Calculation
  (defun calculate-damage:integer 
    ( attacker:object 
      defender:object 
      damage-type:string )
    @doc "Calculate damage dealt"
    (let* ((base-damage (if (= damage-type "physical")
                          (at 'attack-power attacker)
                          (at 'magic-power attacker)))
           (defense (at 'defense defender))
           (level-diff (- (at 'level attacker) (at 'level defender)))
           (level-modifier (+ 1.0 (* level-diff 0.05)))
           ;; Random variance using block height
           (seed-value (get-pseudo-random))
           (variance (* DAMAGE_VARIANCE (- (* seed-value 2.0) 1.0)))
           (damage-with-variance (* base-damage (+ 1.0 variance)))
           ;; Apply defense
           (damage-after-defense (max 1 
             (floor (* damage-with-variance 
                      (/ 100.0 (+ 100.0 defense))
                      level-modifier))))
           ;; Critical hit check
           (is-crit (< seed-value (at 'critical-chance attacker)))
           (final-damage (if is-crit 
                          (floor (* damage-after-defense 2.0))
                          damage-after-defense)))
      
      final-damage))
  
  (defun apply-damage:string (battle-id:string to-defender:bool damage:integer)
    @doc "Apply damage to combatant"
    (with-read battles battle-id 
      { "attacker-health" := attacker-hp
      , "defender-health" := defender-hp }
      
      (if to-defender
        (update battles battle-id {
          "defender-health": (max 0 (- defender-hp damage))
        })
        (update battles battle-id {
          "attacker-health": (max 0 (- attacker-hp damage))
        }))
      
      "Damage applied"))
  
  ;; Battle Flow
  (defun check-battle-end:string (battle-id:string)
    @doc "Check if battle has ended"
    (with-read battles battle-id 
      { "attacker-id" := attacker-id
      , "defender-id" := defender-id
      , "attacker-health" := attacker-hp
      , "defender-health" := defender-hp }
      
      (cond
        ;; Defender defeated
        ((<= defender-hp 0)
         (end-battle battle-id attacker-id defender-id))
        ;; Attacker defeated  
        ((<= attacker-hp 0)
         (end-battle battle-id defender-id attacker-id))
        ;; Battle continues
        (true "Battle continues"))))
  
  (defun end-battle:string (battle-id:string winner-id:string loser-id:string)
    @doc "End battle and distribute rewards"
    ;; Update battle status
    (update battles battle-id {
      "status": "completed",
      "winner": winner-id
    })
    
    ;; Update character statuses
    (game-characters.update-status winner-id "active")
    (game-characters.update-status loser-id "defeated")
    
    ;; Award experience
    (let* ((winner (game-characters.get-character winner-id))
           (loser (game-characters.get-character loser-id))
           (level-diff (- (at 'level loser) (at 'level winner)))
           (base-exp 100)
           (exp-modifier (max 0.5 (+ 1.0 (* level-diff 0.2))))
           (exp-reward (floor (* base-exp exp-modifier))))
      
      (game-characters.gain-experience winner-id exp-reward)
      
      ;; Update combat stats
      (update-combat-stats winner-id true)
      (update-combat-stats loser-id false)
      
      (add-battle-log battle-id 
        (format "{} wins! Gained {} experience." 
                [(at 'name winner) exp-reward]))
      
      (format "Battle ended. {} wins!" [(at 'name winner)])))
  
  ;; Combat Statistics
  (defun update-combat-stats:string (character-id:string won:bool)
    @doc "Update character combat statistics"
    (with-default-read combat-stats-table character-id
      { "battles-won": 0
      , "battles-lost": 0
      , "total-damage-dealt": 0
      , "total-damage-taken": 0
      , "monsters-defeated": 0 }
      { "battles-won" := wins
      , "battles-lost" := losses }
      
      (write combat-stats-table character-id {
        "character-id": character-id,
        "battles-won": (if won (+ wins 1) wins),
        "battles-lost": (if won losses (+ losses 1)),
        "total-damage-dealt": 0,  ;; Would track in real implementation
        "total-damage-taken": 0,
        "monsters-defeated": 0
      }))
    
    "Stats updated")
  
  ;; Helper Functions
  (defun generate-battle-id:string (attacker:string defender:string)
    @doc "Generate unique battle ID"
    (format "BATTLE-{}-{}-{}" 
            [attacker defender (at 'block-height (chain-data))]))
  
  (defun add-battle-log:string (battle-id:string message:string)
    @doc "Add message to battle log"
    (with-read battles battle-id { "battle-log" := log }
      (update battles battle-id {
        "battle-log": (+ log [message])
      }))
    "Log updated")
  
  (defun get-spell-cost:integer (spell-name:string)
    @doc "Get mana cost for spell"
    ;; Simplified spell costs
    (cond
      ((= spell-name "fireball") 10)
      ((= spell-name "heal") 15)
      ((= spell-name "lightning") 20)
      (true 5)))
  
  (defun get-pseudo-random:decimal ()
    @doc "Generate pseudo-random number between 0 and 1"
    ;; Use block height and convert to a decimal between 0 and 1
    (let* ((height (at 'block-height (chain-data)))
           ;; Take modulo to get a number between 0-999
           (seed-mod (mod height 1000)))
      ;; Convert to decimal between 0 and 1
      (/ seed-mod 1000.0)))
  
  ;; Query Functions
  (defun get-battle:object (battle-id:string)
    @doc "Get battle details"
    (read battles battle-id))
  
  (defun get-active-battles:[string] (character-id:string)
    @doc "Get active battles for character"
    (keys battles))  ;; Simplified - would filter properly
)

(create-table battles)
(create-table combat-stats-table)