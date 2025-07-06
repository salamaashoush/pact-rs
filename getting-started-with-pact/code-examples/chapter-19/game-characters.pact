;; game-characters.pact
;; Character system for blockchain RPG
(module game-characters GOVERNANCE
  @doc "Character creation and management for blockchain RPG"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'game-admin))
  
  ;; Schemas
  (defschema character
    @doc "Player character data"
    character-id:string
    owner:string
    name:string
    class:string          ;; warrior, mage, rogue, cleric
    level:integer
    experience:integer
    ;; Base stats
    strength:integer
    intelligence:integer
    agility:integer
    vitality:integer
    ;; Combat stats
    health:integer
    max-health:integer
    mana:integer
    max-mana:integer
    attack-power:integer
    magic-power:integer
    defense:integer
    speed:integer
    critical-chance:decimal
    ;; Status
    location:string
    status:string         ;; active, resting, in-combat, defeated
    created-at:time
    last-action:time)
  
  ;; Tables
  (deftable characters:{character})
  
  ;; Constants
  (defconst CLASSES ["warrior" "mage" "rogue" "cleric"])
  
  (defconst CLASS_BASE_STATS {
    "warrior": {
      "strength": 15,
      "intelligence": 8,
      "agility": 10,
      "vitality": 12,
      "health-per-vitality": 10,
      "mana-per-intelligence": 2
    },
    "mage": {
      "strength": 6,
      "intelligence": 15,
      "agility": 9,
      "vitality": 8,
      "health-per-vitality": 6,
      "mana-per-intelligence": 8
    },
    "rogue": {
      "strength": 10,
      "intelligence": 10,
      "agility": 15,
      "vitality": 9,
      "health-per-vitality": 7,
      "mana-per-intelligence": 3
    },
    "cleric": {
      "strength": 8,
      "intelligence": 12,
      "agility": 8,
      "vitality": 13,
      "health-per-vitality": 8,
      "mana-per-intelligence": 6
    }
  })
  
  ;; Capabilities
  (defcap OWNER (character-id:string owner:string)
    @doc "Character ownership capability"
    (with-read characters character-id { "owner" := current-owner }
      (enforce (= current-owner owner) "Not character owner")))
  
  ;; Character Creation
  (defun create-character:string 
    ( owner:string 
      name:string 
      class:string )
    @doc "Create new character"
    (enforce (contains class CLASSES) "Invalid class")
    
    (let* ((character-id (generate-character-id owner))
           (base-stats (at class CLASS_BASE_STATS))
           (strength (at "strength" base-stats))
           (intelligence (at "intelligence" base-stats))
           (agility (at "agility" base-stats))
           (vitality (at "vitality" base-stats))
           (max-health (* vitality (at "health-per-vitality" base-stats)))
           (max-mana (* intelligence (at "mana-per-intelligence" base-stats))))
      
      (insert characters character-id {
        "character-id": character-id,
        "owner": owner,
        "name": name,
        "class": class,
        "level": 1,
        "experience": 0,
        "strength": strength,
        "intelligence": intelligence,
        "agility": agility,
        "vitality": vitality,
        "health": max-health,
        "max-health": max-health,
        "mana": max-mana,
        "max-mana": max-mana,
        "attack-power": (calculate-attack-power strength agility class),
        "magic-power": (calculate-magic-power intelligence class),
        "defense": (calculate-defense vitality agility class),
        "speed": (calculate-speed agility class),
        "critical-chance": (calculate-crit-chance agility class),
        "location": "town",
        "status": "active",
        "created-at": (at 'block-time (chain-data)),
        "last-action": (at 'block-time (chain-data))
      })
      
      character-id))
  
  ;; Stat Calculations
  (defun calculate-attack-power:integer (strength:integer agility:integer class:string)
    @doc "Calculate physical attack power"
    (cond
      ((= class "warrior") (+ strength (floor (* agility 0.5))))
      ((= class "rogue") (+ (floor (* strength 0.7)) agility))
      (true (floor (* strength 0.5)))))
  
  (defun calculate-magic-power:integer (intelligence:integer class:string)
    @doc "Calculate magical attack power"
    (cond
      ((= class "mage") (floor (* intelligence 1.5)))
      ((= class "cleric") intelligence)
      (true (floor (* intelligence 0.5)))))
  
  (defun calculate-defense:integer (vitality:integer agility:integer class:string)
    @doc "Calculate defense"
    (cond
      ((= class "warrior") (+ vitality (floor (* agility 0.3))))
      (true (+ (floor (* vitality 0.7)) (floor (* agility 0.3))))))
  
  (defun calculate-speed:integer (agility:integer class:string)
    @doc "Calculate speed"
    (cond
      ((= class "rogue") (floor (* agility 1.2)))
      ((= class "warrior") (floor (* agility 0.8)))
      (true agility)))
  
  (defun calculate-crit-chance:decimal (agility:integer class:string)
    @doc "Calculate critical hit chance"
    (let ((base-crit (* agility 0.005)))
      (cond
        ((= class "rogue") (min 0.5 (* base-crit 1.5)))
        (true (min 0.3 base-crit)))))
  
  ;; Experience and Leveling
  (defun gain-experience:string (character-id:string amount:integer)
    @doc "Award experience points"
    (with-read characters character-id 
      { "experience" := current-exp
      , "level" := current-level }
      
      (let* ((new-exp (+ current-exp amount))
             (exp-needed (exp-for-next-level current-level))
             (should-level-up (>= new-exp exp-needed)))
        
        (if should-level-up
          (level-up character-id new-exp)
          (update characters character-id {
            "experience": new-exp
          }))
        
        (format "Gained {} experience" [amount]))))
  
  (defun level-up:string (character-id:string new-exp:integer)
    @doc "Level up character"
    (with-read characters character-id 
      { "level" := current-level
      , "class" := class
      , "strength" := str
      , "intelligence" := int
      , "agility" := agi
      , "vitality" := vit }
      
      (let* ((new-level (+ current-level 1))
             (remaining-exp (- new-exp (exp-for-next-level current-level)))
             ;; Stat gains per level
             (str-gain (if (= class "warrior") 3 1))
             (int-gain (if (= class "mage") 3 1))
             (agi-gain (if (= class "rogue") 3 1))
             (vit-gain (if (= class "cleric") 3 2))
             ;; New stats
             (new-str (+ str str-gain))
             (new-int (+ int int-gain))
             (new-agi (+ agi agi-gain))
             (new-vit (+ vit vit-gain))
             ;; Recalculate combat stats
             (base-stats (at class CLASS_BASE_STATS))
             (new-max-health (* new-vit (at "health-per-vitality" base-stats)))
             (new-max-mana (* new-int (at "mana-per-intelligence" base-stats))))
        
        (update characters character-id {
          "level": new-level,
          "experience": remaining-exp,
          "strength": new-str,
          "intelligence": new-int,
          "agility": new-agi,
          "vitality": new-vit,
          "max-health": new-max-health,
          "health": new-max-health,  ;; Full heal on level up
          "max-mana": new-max-mana,
          "mana": new-max-mana,      ;; Full mana on level up
          "attack-power": (calculate-attack-power new-str new-agi class),
          "magic-power": (calculate-magic-power new-int class),
          "defense": (calculate-defense new-vit new-agi class),
          "speed": (calculate-speed new-agi class),
          "critical-chance": (calculate-crit-chance new-agi class)
        })
        
        (format "Level up! Now level {}" [new-level]))))
  
  ;; Helper Functions
  (defun generate-character-id:string (owner:string)
    @doc "Generate unique character ID"
    (format "CHAR-{}-{}" 
            [owner (at 'block-height (chain-data))]))
  
  (defun exp-for-next-level:integer (level:integer)
    @doc "Calculate experience needed for next level"
    (* level level 100))
  
  ;; Status Management
  (defun update-status:string (character-id:string new-status:string)
    @doc "Update character status"
    (update characters character-id {
      "status": new-status,
      "last-action": (at 'block-time (chain-data))
    })
    (format "Status updated to {}" [new-status]))
  
  (defun heal-character:string (character-id:string amount:integer)
    @doc "Heal character"
    (with-read characters character-id 
      { "health" := current-health
      , "max-health" := max-health }
      
      (let ((new-health (min max-health (+ current-health amount))))
        (update characters character-id {
          "health": new-health
        })
        (format "Healed for {} HP" [amount]))))
  
  (defun restore-mana:string (character-id:string amount:integer)
    @doc "Restore mana"
    (with-read characters character-id 
      { "mana" := current-mana
      , "max-mana" := max-mana }
      
      (let ((new-mana (min max-mana (+ current-mana amount))))
        (update characters character-id {
          "mana": new-mana
        })
        (format "Restored {} mana" [amount]))))
  
  ;; Query Functions
  (defun get-character:object (character-id:string)
    @doc "Get character details"
    (read characters character-id))
  
  (defun get-characters-by-owner:[object] (owner:string)
    @doc "Get all characters owned by account"
    (select characters (where 'owner (= owner))))
)

(create-table characters)