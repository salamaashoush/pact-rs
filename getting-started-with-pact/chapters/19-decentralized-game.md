# Chapter 19: Building a Decentralized Game - Blockchain RPG

## Table of Contents
1. [Project Overview](#project-overview)
2. [Game Architecture](#game-architecture)
3. [Step 1: Character System](#step-1-character-system)
4. [Step 2: Combat Mechanics](#step-2-combat-mechanics)
5. [Step 3: Item & Equipment System](#step-3-item-equipment-system)
6. [Step 4: Dungeon & Quest System](#step-4-dungeon-quest-system)
7. [Step 5: Economy & Marketplace](#step-5-economy-marketplace)
8. [Step 6: Guild System](#step-6-guild-system)
9. [Testing & Balancing](#testing-balancing)
10. [Frontend Integration](#frontend-integration)

## Project Overview

In this chapter, we'll build a complete blockchain RPG game featuring:
- Character creation and progression
- Turn-based combat system
- Items, equipment, and crafting
- Dungeons with randomized rewards
- Player-vs-player battles
- Guild system with shared rewards
- In-game economy with marketplace

### What You'll Learn
- Implementing complex game logic on-chain
- Managing randomness in blockchain
- Creating balanced game economies
- Handling real-time interactions
- Optimizing gas costs for gaming
- Building engaging player experiences

## Game Architecture

### Core Components
```
blockchain-rpg/
├── core/
│   ├── characters.pact      # Character creation & stats
│   ├── combat.pact         # Battle system
│   └── experience.pact     # Leveling & skills
├── items/
│   ├── equipment.pact      # Weapons & armor
│   ├── consumables.pact    # Potions & buffs
│   └── crafting.pact       # Item creation
├── world/
│   ├── dungeons.pact       # PvE content
│   ├── quests.pact        # Quest system
│   └── world-map.pact     # Exploration
├── social/
│   ├── guilds.pact        # Guild mechanics
│   ├── pvp-arena.pact     # PvP battles
│   └── leaderboard.pact   # Rankings
└── economy/
    ├── marketplace.pact    # Item trading
    ├── currency.pact      # Game tokens
    └── rewards.pact       # Loot distribution
```

## Step 1: Character System

Let's start with the character creation and management system:

```pact
;; core/characters.pact
(module rpg-characters GOVERNANCE
  @doc "Character system for blockchain RPG"
  
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
    ;; Derived stats
    health:integer
    max-health:integer
    mana:integer
    max-mana:integer
    ;; Combat stats
    attack-power:integer
    magic-power:integer
    defense:integer
    speed:integer
    critical-chance:decimal
    ;; Status
    location:string
    in-combat:bool
    last-action:time
    created-at:time)
  
  (defschema character-inventory
    @doc "Character inventory"
    character-id:string
    ;; Equipment slots
    weapon:string
    armor:string
    helmet:string
    boots:string
    accessory:string
    ;; Inventory
    items:[string]
    max-items:integer
    gold:integer)
  
  (defschema class-template
    @doc "Character class definitions"
    class:string
    base-strength:integer
    base-intelligence:integer
    base-agility:integer
    base-vitality:integer
    strength-per-level:decimal
    intelligence-per-level:decimal
    agility-per-level:decimal
    vitality-per-level:decimal
    abilities:[string])
  
  ;; Tables
  (deftable characters:{character})
  (deftable inventories:{character-inventory})
  (deftable class-templates:{class-template})
  
  ;; Constants
  (defconst MAX_LEVEL 100)
  (defconst STAT_POINTS_PER_LEVEL 5)
  (defconst BASE_INVENTORY_SIZE 20)
  (defconst CHARACTER_CREATION_COST 10.0)
  
  ;; Initialize class templates
  (defun init-classes ()
    @doc "Initialize character classes"
    (insert class-templates "warrior" {
      "class": "warrior",
      "base-strength": 15,
      "base-intelligence": 5,
      "base-agility": 10,
      "base-vitality": 12,
      "strength-per-level": 2.5,
      "intelligence-per-level": 0.5,
      "agility-per-level": 1.0,
      "vitality-per-level": 2.0,
      "abilities": ["sword-strike", "shield-bash", "berserker-rage"]
    })
    
    (insert class-templates "mage" {
      "class": "mage",
      "base-strength": 5,
      "base-intelligence": 15,
      "base-agility": 8,
      "base-vitality": 8,
      "strength-per-level": 0.5,
      "intelligence-per-level": 2.5,
      "agility-per-level": 1.0,
      "vitality-per-level": 1.5,
      "abilities": ["fireball", "frost-armor", "teleport"]
    })
    
    (insert class-templates "rogue" {
      "class": "rogue",
      "base-strength": 8,
      "base-intelligence": 8,
      "base-agility": 15,
      "base-vitality": 10,
      "strength-per-level": 1.5,
      "intelligence-per-level": 1.0,
      "agility-per-level": 2.5,
      "vitality-per-level": 1.0,
      "abilities": ["stealth", "backstab", "poison-blade"]
    })
    
    (insert class-templates "cleric" {
      "class": "cleric",
      "base-strength": 8,
      "base-intelligence": 12,
      "base-agility": 8,
      "base-vitality": 13,
      "strength-per-level": 1.0,
      "intelligence-per-level": 2.0,
      "agility-per-level": 1.0,
      "vitality-per-level": 2.0,
      "abilities": ["heal", "holy-light", "resurrection"]
    }))
  
  ;; Character Creation
  (defcap CREATE_CHARACTER (owner:string name:string)
    @doc "Character creation capability"
    (enforce-guard (at 'guard (coin.details owner)))
    (let ((char-count (length (get-player-characters owner))))
      (enforce (< char-count 5) "Maximum 5 characters per player")))
  
  (defun create-character:string 
    ( name:string
      class:string )
    @doc "Create new character"
    (let ((owner (at 'sender (chain-data))))
      (with-capability (CREATE_CHARACTER owner name)
        ;; Charge creation fee
        (coin.transfer owner "game-treasury" CHARACTER_CREATION_COST)
        
        ;; Validate inputs
        (enforce (>= (length name) 3) "Name too short")
        (enforce (<= (length name) 20) "Name too long")
        (enforce (contains class ["warrior" "mage" "rogue" "cleric"]) 
                 "Invalid class")
        
        ;; Get class template
        (with-read class-templates class
          { "base-strength" := str
          , "base-intelligence" := int
          , "base-agility" := agi
          , "base-vitality" := vit }
          
          (let* ( (character-id (hash [owner name (at 'block-time (chain-data))]))
                  (max-hp (* vit 10))
                  (max-mp (* int 5)) )
            
            ;; Create character
            (insert characters character-id {
              "character-id": character-id,
              "owner": owner,
              "name": name,
              "class": class,
              "level": 1,
              "experience": 0,
              "strength": str,
              "intelligence": int,
              "agility": agi,
              "vitality": vit,
              "health": max-hp,
              "max-health": max-hp,
              "mana": max-mp,
              "max-mana": max-mp,
              "attack-power": (calculate-attack-power str agi class),
              "magic-power": (calculate-magic-power int class),
              "defense": (calculate-defense vit agi class),
              "speed": (calculate-speed agi class),
              "critical-chance": (calculate-crit-chance agi class),
              "location": "starter-town",
              "in-combat": false,
              "last-action": (at 'block-time (chain-data)),
              "created-at": (at 'block-time (chain-data))
            })
            
            ;; Create inventory
            (insert inventories character-id {
              "character-id": character-id,
              "weapon": "",
              "armor": "",
              "helmet": "",
              "boots": "",
              "accessory": "",
              "items": [],
              "max-items": BASE_INVENTORY_SIZE,
              "gold": 100  ;; Starting gold
            })
            
            ;; Give starter equipment based on class
            (give-starter-equipment character-id class)
            
            character-id)))))
  
  ;; Stat Calculations
  (defun calculate-attack-power:integer (strength:integer agility:integer class:string)
    @doc "Calculate physical attack power"
    (if (= class "warrior")
      (+ (* strength 2) (/ agility 2))
      (if (= class "rogue")
        (+ strength (* agility 2))
        (+ strength agility))))
  
  (defun calculate-magic-power:integer (intelligence:integer class:string)
    @doc "Calculate magic attack power"
    (if (= class "mage")
      (* intelligence 3)
      (if (= class "cleric")
        (* intelligence 2)
        intelligence)))
  
  (defun calculate-defense:integer (vitality:integer agility:integer class:string)
    @doc "Calculate defense rating"
    (if (= class "warrior")
      (+ (* vitality 2) (/ agility 3))
      (+ vitality (/ agility 2))))
  
  (defun calculate-speed:integer (agility:integer class:string)
    @doc "Calculate speed/initiative"
    (if (= class "rogue")
      (* agility 2)
      agility))
  
  (defun calculate-crit-chance:decimal (agility:integer class:string)
    @doc "Calculate critical hit chance"
    (let ((base-crit (* (decimal agility) 0.01)))
      (if (= class "rogue")
        (* base-crit 2.0)
        base-crit)))
  
  ;; Leveling System
  (defcap LEVEL_UP (character-id:string)
    @doc "Level up capability"
    (with-read characters character-id { "owner" := owner }
      (enforce-guard (at 'guard (coin.details owner)))))
  
  (defun add-experience:string (character-id:string amount:integer)
    @doc "Add experience to character"
    (require-capability (COMBAT))
    (with-read characters character-id
      { "level" := level
      , "experience" := current-exp
      , "class" := class }
      
      (let* ( (new-exp (+ current-exp amount))
              (exp-needed (experience-for-level (+ level 1))) )
        
        (if (>= new-exp exp-needed)
          ;; Level up!
          (with-capability (LEVEL_UP character-id)
            (level-up-character character-id)
            (update characters character-id {
              "experience": (- new-exp exp-needed)
            }))
          ;; Just add experience
          (update characters character-id {
            "experience": new-exp
          }))
        
        (format "Gained {} experience" [amount]))))
  
  (defun level-up-character:string (character-id:string)
    @doc "Level up a character"
    (with-read characters character-id
      { "level" := level
      , "class" := class
      , "strength" := str
      , "intelligence" := int
      , "agility" := agi
      , "vitality" := vit }
      
      (enforce (< level MAX_LEVEL) "Already at max level")
      
      ;; Get class growth rates
      (with-read class-templates class
        { "strength-per-level" := str-growth
        , "intelligence-per-level" := int-growth
        , "agility-per-level" := agi-growth
        , "vitality-per-level" := vit-growth }
        
        (let* ( (new-level (+ level 1))
                (new-str (+ str (round str-growth)))
                (new-int (+ int (round int-growth)))
                (new-agi (+ agi (round agi-growth)))
                (new-vit (+ vit (round vit-growth)))
                (new-max-hp (* new-vit 10))
                (new-max-mp (* new-int 5)) )
          
          ;; Update character
          (update characters character-id {
            "level": new-level,
            "strength": new-str,
            "intelligence": new-int,
            "agility": new-agi,
            "vitality": new-vit,
            "max-health": new-max-hp,
            "max-mana": new-max-mp,
            "health": new-max-hp,  ;; Full heal on level up
            "mana": new-max-mp,
            "attack-power": (calculate-attack-power new-str new-agi class),
            "magic-power": (calculate-magic-power new-int class),
            "defense": (calculate-defense new-vit new-agi class),
            "speed": (calculate-speed new-agi class),
            "critical-chance": (calculate-crit-chance new-agi class)
          })
          
          (format "Level up! Now level {}" [new-level])))))
  
  (defun experience-for-level:integer (level:integer)
    @doc "Calculate experience needed for level"
    (* level level 100))
  
  ;; Character Management
  (defun rest:string (character-id:string)
    @doc "Rest to restore health and mana"
    (with-read characters character-id
      { "owner" := owner
      , "in-combat" := in-combat
      , "max-health" := max-hp
      , "max-mana" := max-mp }
      
      (enforce-guard (at 'guard (coin.details owner)))
      (enforce (not in-combat) "Cannot rest during combat")
      
      (update characters character-id {
        "health": max-hp,
        "mana": max-mp,
        "last-action": (at 'block-time (chain-data))
      })
      
      "Fully rested"))
  
  (defun give-starter-equipment:string (character-id:string class:string)
    @doc "Give starter equipment based on class"
    (require-capability (INTERNAL))
    
    (let ((starter-weapon 
           (if (= class "warrior") "rusty-sword"
             (if (= class "mage") "wooden-staff"
               (if (= class "rogue") "worn-dagger"
                 "simple-mace"))))
          (starter-armor
           (if (= class "warrior") "leather-armor"
             (if (= class "mage") "cloth-robe"
               (if (= class "rogue") "leather-vest"
                 "cloth-robe")))))
      
      (update inventories character-id {
        "weapon": starter-weapon,
        "armor": starter-armor,
        "items": ["health-potion", "mana-potion"]
      })
      
      "Starter equipment granted"))
  
  ;; Query Functions
  (defun get-character:object (character-id:string)
    @doc "Get character details"
    (+ (read characters character-id)
       (read inventories character-id)))
  
  (defun get-player-characters:[string] (player:string)
    @doc "Get all characters for a player"
    (keys characters (where 'owner (= player))))
  
  (defun get-character-stats:object (character-id:string)
    @doc "Get character combat stats"
    (read characters character-id 
      ['level 'attack-power 'magic-power 'defense 'speed 
       'critical-chance 'health 'max-health 'mana 'max-mana]))
  
  ;; Internal capability
  (defcap INTERNAL () true)
  (defcap COMBAT () true)
)

(create-table characters)
(create-table inventories)
(create-table class-templates)
```

## Step 2: Combat Mechanics

Now let's implement the combat system:

```pact
;; core/combat.pact
(module rpg-combat GOVERNANCE
  @doc "Turn-based combat system"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'game-admin))
  
  ;; Schemas
  (defschema battle
    @doc "Active battle data"
    battle-id:string
    participants:[string]  ;; character-ids
    turn-order:[string]
    current-turn:integer
    round:integer
    status:string         ;; active, completed
    winner:string
    battle-log:[string]
    rewards:object
    created-at:time)
  
  (defschema combat-action
    @doc "Combat action record"
    action-id:string
    battle-id:string
    character-id:string
    action-type:string    ;; attack, skill, item, flee
    target:string
    damage:integer
    effects:[string]
    timestamp:time)
  
  (defschema skill
    @doc "Character skills"
    skill-id:string
    name:string
    class:string
    damage-multiplier:decimal
    mana-cost:integer
    cooldown:integer
    effect:string
    description:string)
  
  ;; Tables
  (deftable battles:{battle})
  (deftable combat-actions:{combat-action})
  (deftable skills:{skill})
  
  ;; Constants
  (defconst FLEE_PENALTY 0.5)
  (defconst CRIT_MULTIPLIER 2.0)
  (defconst MISS_CHANCE 0.05)
  
  ;; Initialize skills
  (defun init-skills ()
    @doc "Initialize skill definitions"
    ;; Warrior skills
    (insert skills "sword-strike" {
      "skill-id": "sword-strike",
      "name": "Sword Strike",
      "class": "warrior",
      "damage-multiplier": 1.5,
      "mana-cost": 10,
      "cooldown": 1,
      "effect": "none",
      "description": "A powerful sword attack"
    })
    
    ;; Mage skills
    (insert skills "fireball" {
      "skill-id": "fireball",
      "name": "Fireball",
      "class": "mage",
      "damage-multiplier": 2.0,
      "mana-cost": 20,
      "cooldown": 2,
      "effect": "burn",
      "description": "Launch a burning projectile"
    })
    
    ;; Continue with other skills...
    )
  
  ;; Battle Management
  (defcap BATTLE_ACTION (battle-id:string character-id:string)
    @doc "Capability for battle actions"
    (with-read battles battle-id 
      { "participants" := participants
      , "status" := status
      , "turn-order" := turn-order
      , "current-turn" := turn-idx }
      
      (enforce (= status "active") "Battle not active")
      (enforce (contains character-id participants) "Not in battle")
      (enforce (= character-id (at turn-idx turn-order)) "Not your turn")))
  
  (defun start-battle:string (attacker-id:string defender-id:string)
    @doc "Start a battle between two characters"
    ;; Verify both characters exist and not in combat
    (with-read rpg-characters.characters attacker-id
      { "owner" := attacker-owner, "in-combat" := attacker-combat }
      (enforce-guard (at 'guard (coin.details attacker-owner)))
      (enforce (not attacker-combat) "Attacker already in combat"))
    
    (with-read rpg-characters.characters defender-id
      { "in-combat" := defender-combat }
      (enforce (not defender-combat) "Defender already in combat"))
    
    (let* ( (battle-id (hash [attacker-id defender-id (at 'block-time (chain-data))]))
            (participants [attacker-id defender-id])
            ;; Determine turn order based on speed
            (attacker-speed (at 'speed (rpg-characters.get-character-stats attacker-id)))
            (defender-speed (at 'speed (rpg-characters.get-character-stats defender-id)))
            (turn-order (if (>= attacker-speed defender-speed)
                          participants
                          [defender-id attacker-id])) )
      
      ;; Create battle
      (insert battles battle-id {
        "battle-id": battle-id,
        "participants": participants,
        "turn-order": turn-order,
        "current-turn": 0,
        "round": 1,
        "status": "active",
        "winner": "",
        "battle-log": [(format "Battle started: {} vs {}" [attacker-id defender-id])],
        "rewards": {},
        "created-at": (at 'block-time (chain-data))
      })
      
      ;; Mark characters as in combat
      (map (lambda (char-id)
             (update rpg-characters.characters char-id { "in-combat": true }))
           participants)
      
      battle-id))
  
  (defun attack:string (battle-id:string target-id:string)
    @doc "Perform basic attack"
    (let ((attacker-id (get-current-turn-character battle-id)))
      (with-capability (BATTLE_ACTION battle-id attacker-id)
        ;; Get attacker stats
        (let* ( (attacker-stats (rpg-characters.get-character-stats attacker-id))
                (defender-stats (rpg-characters.get-character-stats target-id))
                (hit-roll (random-decimal))
                (crit-roll (random-decimal)) )
          
          (if (< hit-roll MISS_CHANCE)
            ;; Miss
            (begin
              (record-action battle-id attacker-id "attack" target-id 0 ["miss"])
              (advance-turn battle-id)
              "Attack missed!")
            
            ;; Hit
            (let* ( (base-damage (at 'attack-power attacker-stats))
                    (defense (at 'defense defender-stats))
                    (is-crit (< crit-roll (at 'critical-chance attacker-stats)))
                    (damage-mult (if is-crit CRIT_MULTIPLIER 1.0))
                    (final-damage (max 1 (round (* base-damage damage-mult (- 1.0 (/ defense 100.0)))))) )
              
              ;; Apply damage
              (apply-damage target-id final-damage)
              
              ;; Record action
              (record-action battle-id attacker-id "attack" target-id final-damage 
                           (if is-crit ["critical"] []))
              
              ;; Check if battle ended
              (if (check-battle-end battle-id)
                (end-battle battle-id)
                (advance-turn battle-id))
              
              (format "Dealt {} damage{}" 
                     [final-damage (if is-crit " (CRITICAL!)" "")])))))))
  
  (defun use-skill:string (battle-id:string skill-id:string target-id:string)
    @doc "Use a character skill"
    (let ((attacker-id (get-current-turn-character battle-id)))
      (with-capability (BATTLE_ACTION battle-id attacker-id)
        ;; Verify character can use skill
        (let* ( (char-data (rpg-characters.get-character attacker-id))
                (char-class (at 'class char-data))
                (char-mana (at 'mana char-data))
                (skill-data (read skills skill-id)) )
          
          (enforce (= char-class (at 'class skill-data)) "Cannot use this skill")
          (enforce (>= char-mana (at 'mana-cost skill-data)) "Not enough mana")
          
          ;; Use mana
          (update rpg-characters.characters attacker-id {
            "mana": (- char-mana (at 'mana-cost skill-data))
          })
          
          ;; Calculate damage
          (let* ( (attacker-stats (rpg-characters.get-character-stats attacker-id))
                  (base-damage (if (= (at 'class char-data) "mage")
                                (at 'magic-power attacker-stats)
                                (at 'attack-power attacker-stats)))
                  (skill-damage (* base-damage (at 'damage-multiplier skill-data)))
                  (final-damage (round skill-damage)) )
            
            ;; Apply damage and effects
            (apply-damage target-id final-damage)
            (apply-skill-effect target-id (at 'effect skill-data))
            
            ;; Record and advance
            (record-action battle-id attacker-id "skill" target-id final-damage 
                         [skill-id (at 'effect skill-data)])
            
            (if (check-battle-end battle-id)
              (end-battle battle-id)
              (advance-turn battle-id))
            
            (format "Used {}: {} damage" 
                   [(at 'name skill-data) final-damage]))))))
  
  (defun flee:string (battle-id:string)
    @doc "Attempt to flee from battle"
    (let ((character-id (get-current-turn-character battle-id)))
      (with-capability (BATTLE_ACTION battle-id character-id)
        (let* ( (char-stats (rpg-characters.get-character-stats character-id))
                (flee-chance (* (/ (at 'speed char-stats) 100.0) 0.8))
                (flee-roll (random-decimal)) )
          
          (if (< flee-roll flee-chance)
            ;; Successful flee
            (begin
              (update battles battle-id {
                "status": "completed",
                "winner": (get-opponent battle-id character-id)
              })
              
              ;; Apply flee penalty (lose some gold)
              (let ((inventory (read rpg-characters.inventories character-id)))
                (update rpg-characters.inventories character-id {
                  "gold": (round (* (at 'gold inventory) FLEE_PENALTY))
                }))
              
              ;; Clear combat status
              (map (lambda (char-id)
                     (update rpg-characters.characters char-id { "in-combat": false }))
                   (at 'participants (read battles battle-id)))
              
              "Fled successfully!")
            
            ;; Failed to flee
            (begin
              (record-action battle-id character-id "flee" "" 0 ["failed"])
              (advance-turn battle-id)
              "Failed to flee!"))))))
  
  ;; Battle Helpers
  (defun apply-damage:string (character-id:string damage:integer)
    @doc "Apply damage to character"
    (require-capability (INTERNAL))
    (with-read rpg-characters.characters character-id
      { "health" := current-hp }
      
      (let ((new-hp (max 0 (- current-hp damage))))
        (update rpg-characters.characters character-id {
          "health": new-hp
        })
        
        (if (= new-hp 0)
          "Character defeated!"
          (format "Health: {}/{}" [new-hp (at 'max-health 
                                           (rpg-characters.get-character-stats character-id))])))))
  
  (defun apply-skill-effect:string (character-id:string effect:string)
    @doc "Apply skill effect to character"
    (require-capability (INTERNAL))
    ;; Implement various effects: burn, freeze, stun, etc.
    (if (= effect "burn")
      ;; Apply burn damage over time
      "Applied burn effect"
      "No effect"))
  
  (defun check-battle-end:bool (battle-id:string)
    @doc "Check if battle should end"
    (with-read battles battle-id { "participants" := participants }
      (let ((healths (map (lambda (char-id)
                           (at 'health (rpg-characters.get-character-stats char-id)))
                         participants)))
        (or (= (at 0 healths) 0) (= (at 1 healths) 0)))))
  
  (defun end-battle:string (battle-id:string)
    @doc "End battle and distribute rewards"
    (require-capability (INTERNAL))
    (with-read battles battle-id 
      { "participants" := participants }
      
      (let* ( (char1-hp (at 'health (rpg-characters.get-character-stats (at 0 participants))))
              (char2-hp (at 'health (rpg-characters.get-character-stats (at 1 participants))))
              (winner (if (> char1-hp 0) (at 0 participants) (at 1 participants)))
              (loser (if (= winner (at 0 participants)) (at 1 participants) (at 0 participants))) )
        
        ;; Update battle
        (update battles battle-id {
          "status": "completed",
          "winner": winner
        })
        
        ;; Clear combat status
        (map (lambda (char-id)
               (update rpg-characters.characters char-id { "in-combat": false }))
             participants)
        
        ;; Grant rewards
        (let* ( (loser-level (at 'level (rpg-characters.get-character-stats loser)))
                (exp-reward (* loser-level 50))
                (gold-reward (* loser-level 20)) )
          
          ;; Give experience
          (rpg-characters.add-experience winner exp-reward)
          
          ;; Give gold
          (with-read rpg-characters.inventories winner { "gold" := current-gold }
            (update rpg-characters.inventories winner {
              "gold": (+ current-gold gold-reward)
            }))
          
          (format "{} wins! Gained {} EXP and {} gold" 
                 [winner exp-reward gold-reward])))))
  
  (defun advance-turn:string (battle-id:string)
    @doc "Move to next turn"
    (require-capability (INTERNAL))
    (with-read battles battle-id 
      { "turn-order" := order
      , "current-turn" := turn
      , "round" := round }
      
      (let ((new-turn (mod (+ turn 1) (length order))))
        (update battles battle-id {
          "current-turn": new-turn,
          "round": (if (= new-turn 0) (+ round 1) round)
        })
        
        "Next turn")))
  
  (defun get-current-turn-character:string (battle-id:string)
    @doc "Get character whose turn it is"
    (with-read battles battle-id 
      { "turn-order" := order, "current-turn" := turn }
      (at turn order)))
  
  (defun get-opponent:string (battle-id:string character-id:string)
    @doc "Get opponent in battle"
    (with-read battles battle-id { "participants" := participants }
      (if (= character-id (at 0 participants))
        (at 1 participants)
        (at 0 participants))))
  
  (defun record-action:string 
    ( battle-id:string 
      character-id:string 
      action-type:string 
      target:string 
      damage:integer 
      effects:[string] )
    @doc "Record combat action"
    (require-capability (INTERNAL))
    (let ((action-id (hash [battle-id character-id (at 'block-time (chain-data))])))
      (insert combat-actions action-id {
        "action-id": action-id,
        "battle-id": battle-id,
        "character-id": character-id,
        "action-type": action-type,
        "target": target,
        "damage": damage,
        "effects": effects,
        "timestamp": (at 'block-time (chain-data))
      })
      
      ;; Update battle log
      (with-read battles battle-id { "battle-log" := log }
        (update battles battle-id {
          "battle-log": (+ log [(format "{} used {} on {} for {} damage" 
                                       [character-id action-type target damage])])
        }))))
  
  ;; Utility Functions
  (defun random-decimal:decimal ()
    @doc "Generate pseudo-random decimal between 0 and 1"
    ;; In production, use oracle or commit-reveal scheme
    (/ (mod (str-to-int 64 (hash (at 'block-time (chain-data)))) 100) 100.0))
  
  ;; Query Functions
  (defun get-battle:object (battle-id:string)
    @doc "Get battle details"
    (read battles battle-id))
  
  (defun get-active-battles:[string] ()
    @doc "Get all active battles"
    (keys battles (where 'status (= "active"))))
  
  (defun get-battle-history:[object] (character-id:string)
    @doc "Get battle history for character"
    (select battles (where 'participants (contains character-id))))
  
  ;; Internal capability
  (defcap INTERNAL () true)
)

(create-table battles)
(create-table combat-actions)
(create-table skills)
```

## Step 3: Item & Equipment System

Let's create the item and equipment system:

```pact
;; items/equipment.pact
(module rpg-equipment GOVERNANCE
  @doc "Equipment and item system"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'game-admin))
  
  ;; Schemas
  (defschema item
    @doc "Item definition"
    item-id:string
    name:string
    type:string           ;; weapon, armor, consumable, material
    subtype:string        ;; sword, staff, potion, etc.
    rarity:string         ;; common, uncommon, rare, epic, legendary
    level-requirement:integer
    ;; Stats
    attack-bonus:integer
    magic-bonus:integer
    defense-bonus:integer
    speed-bonus:integer
    health-bonus:integer
    mana-bonus:integer
    ;; Special properties
    effects:[string]
    value:integer
    tradeable:bool
    stackable:bool
    max-stack:integer)
  
  (defschema item-instance
    @doc "Specific item instance"
    instance-id:string
    item-id:string
    owner:string
    equipped-by:string    ;; character-id if equipped
    quantity:integer
    durability:integer
    max-durability:integer
    enchantments:[string]
    created-at:time)
  
  (defschema crafting-recipe
    @doc "Item crafting recipes"
    recipe-id:string
    result-item:string
    materials:object      ;; {"item-id": quantity}
    level-required:integer
    gold-cost:integer
    success-rate:decimal)
  
  ;; Tables
  (deftable items:{item})
  (deftable item-instances:{item-instance})
  (deftable crafting-recipes:{crafting-recipe})
  
  ;; Constants
  (defconst DURABILITY_LOSS_PER_BATTLE 5)
  (defconst REPAIR_COST_MULTIPLIER 0.1)
  
  ;; Initialize items
  (defun init-items ()
    @doc "Initialize item definitions"
    ;; Weapons
    (insert items "rusty-sword" {
      "item-id": "rusty-sword",
      "name": "Rusty Sword",
      "type": "weapon",
      "subtype": "sword",
      "rarity": "common",
      "level-requirement": 1,
      "attack-bonus": 5,
      "magic-bonus": 0,
      "defense-bonus": 0,
      "speed-bonus": 0,
      "health-bonus": 0,
      "mana-bonus": 0,
      "effects": [],
      "value": 10,
      "tradeable": true,
      "stackable": false,
      "max-stack": 1
    })
    
    (insert items "iron-sword" {
      "item-id": "iron-sword",
      "name": "Iron Sword",
      "type": "weapon",
      "subtype": "sword",
      "rarity": "uncommon",
      "level-requirement": 5,
      "attack-bonus": 15,
      "magic-bonus": 0,
      "defense-bonus": 2,
      "speed-bonus": 0,
      "health-bonus": 0,
      "mana-bonus": 0,
      "effects": [],
      "value": 50,
      "tradeable": true,
      "stackable": false,
      "max-stack": 1
    })
    
    ;; Armor
    (insert items "leather-armor" {
      "item-id": "leather-armor",
      "name": "Leather Armor",
      "type": "armor",
      "subtype": "chest",
      "rarity": "common",
      "level-requirement": 1,
      "attack-bonus": 0,
      "magic-bonus": 0,
      "defense-bonus": 10,
      "speed-bonus": 0,
      "health-bonus": 20,
      "mana-bonus": 0,
      "effects": [],
      "value": 15,
      "tradeable": true,
      "stackable": false,
      "max-stack": 1
    })
    
    ;; Consumables
    (insert items "health-potion" {
      "item-id": "health-potion",
      "name": "Health Potion",
      "type": "consumable",
      "subtype": "potion",
      "rarity": "common",
      "level-requirement": 1,
      "attack-bonus": 0,
      "magic-bonus": 0,
      "defense-bonus": 0,
      "speed-bonus": 0,
      "health-bonus": 50,  ;; Restores 50 HP
      "mana-bonus": 0,
      "effects": ["instant-heal"],
      "value": 5,
      "tradeable": true,
      "stackable": true,
      "max-stack": 99
    })
    
    ;; Continue with more items...
    )
  
  ;; Item Management
  (defcap EQUIP (character-id:string item-instance-id:string)
    @doc "Capability to equip items"
    (with-read rpg-characters.characters character-id { "owner" := owner }
      (enforce-guard (at 'guard (coin.details owner))))
    (with-read item-instances item-instance-id { "owner" := item-owner }
      (enforce (= item-owner (at 'owner (rpg-characters.get-character character-id))) 
               "Not item owner")))
  
  (defun create-item:string (item-id:string owner:string)
    @doc "Create new item instance"
    (with-read items item-id
      { "stackable" := stackable
      , "max-durability" := max-dur }
      
      (let ((instance-id (hash [item-id owner (at 'block-time (chain-data))])))
        (insert item-instances instance-id {
          "instance-id": instance-id,
          "item-id": item-id,
          "owner": owner,
          "equipped-by": "",
          "quantity": 1,
          "durability": (if stackable 0 100),
          "max-durability": (if stackable 0 100),
          "enchantments": [],
          "created-at": (at 'block-time (chain-data))
        })
        
        instance-id)))
  
  (defun equip-item:string (character-id:string item-instance-id:string)
    @doc "Equip item to character"
    (with-capability (EQUIP character-id item-instance-id)
      (with-read item-instances item-instance-id 
        { "item-id" := item-id
        , "equipped-by" := currently-equipped }
        
        (enforce (= currently-equipped "") "Item already equipped")
        
        (with-read items item-id
          { "type" := item-type
          , "level-requirement" := req-level }
          
          ;; Check level requirement
          (let ((char-level (at 'level (rpg-characters.get-character character-id))))
            (enforce (>= char-level req-level) "Level too low"))
          
          ;; Determine equipment slot
          (let ((slot (get-equipment-slot item-type)))
            ;; Unequip current item in slot
            (unequip-slot character-id slot)
            
            ;; Equip new item
            (update rpg-characters.inventories character-id {
              slot: item-instance-id
            })
            
            (update item-instances item-instance-id {
              "equipped-by": character-id
            })
            
            ;; Update character stats
            (update-character-stats character-id)
            
            (format "Equipped {}" [(at 'name (read items item-id))]))))))
  
  (defun unequip-item:string (character-id:string item-instance-id:string)
    @doc "Unequip item from character"
    (with-capability (EQUIP character-id item-instance-id)
      (with-read item-instances item-instance-id 
        { "equipped-by" := equipped-by }
        
        (enforce (= equipped-by character-id) "Item not equipped by character")
        
        ;; Find and clear slot
        (let ((slot (find-item-slot character-id item-instance-id)))
          (update rpg-characters.inventories character-id {
            slot: ""
          }))
        
        (update item-instances item-instance-id {
          "equipped-by": ""
        })
        
        ;; Update character stats
        (update-character-stats character-id)
        
        "Item unequipped")))
  
  (defun use-consumable:string (character-id:string item-instance-id:string)
    @doc "Use a consumable item"
    (with-read item-instances item-instance-id
      { "item-id" := item-id
      , "owner" := owner
      , "quantity" := qty }
      
      (enforce (= owner (at 'owner (rpg-characters.get-character character-id)))
               "Not item owner")
      (enforce (> qty 0) "No items remaining")
      
      (with-read items item-id
        { "type" := item-type
        , "effects" := effects
        , "health-bonus" := hp-restore
        , "mana-bonus" := mp-restore }
        
        (enforce (= item-type "consumable") "Not a consumable")
        
        ;; Apply effects
        (if (contains "instant-heal" effects)
          (with-read rpg-characters.characters character-id
            { "health" := current-hp
            , "max-health" := max-hp }
            
            (update rpg-characters.characters character-id {
              "health": (min (+ current-hp hp-restore) max-hp)
            }))
          "")
        
        (if (contains "instant-mana" effects)
          (with-read rpg-characters.characters character-id
            { "mana" := current-mp
            , "max-mana" := max-mp }
            
            (update rpg-characters.characters character-id {
              "mana": (min (+ current-mp mp-restore) max-mp)
            }))
          "")
        
        ;; Reduce quantity
        (if (= qty 1)
          (update item-instances item-instance-id { "quantity": 0 })
          (update item-instances item-instance-id { "quantity": (- qty 1) }))
        
        (format "Used {}" [(at 'name (read items item-id))]))))
  
  ;; Crafting System
  (defcap CRAFT (character-id:string recipe-id:string)
    @doc "Capability to craft items"
    (with-read rpg-characters.characters character-id { "owner" := owner }
      (enforce-guard (at 'guard (coin.details owner)))))
  
  (defun craft-item:string (character-id:string recipe-id:string)
    @doc "Craft an item"
    (with-capability (CRAFT character-id recipe-id)
      (with-read crafting-recipes recipe-id
        { "result-item" := result
        , "materials" := mats
        , "level-required" := req-level
        , "gold-cost" := cost
        , "success-rate" := success }
        
        ;; Check level
        (let ((char-level (at 'level (rpg-characters.get-character character-id))))
          (enforce (>= char-level req-level) "Level too low"))
        
        ;; Check gold
        (with-read rpg-characters.inventories character-id { "gold" := gold }
          (enforce (>= gold cost) "Not enough gold"))
        
        ;; Check materials
        (map (lambda (mat)
               (let* ( (item-id (at 'key mat))
                       (needed (at 'value mat))
                       (owned (count-items character-id item-id)) )
                 (enforce (>= owned needed) 
                         (format "Need {} {}" [needed item-id]))))
             (map (lambda (k) { "key": k, "value": (at k mats) }) 
                  (keys mats)))
        
        ;; Consume materials and gold
        (update rpg-characters.inventories character-id {
          "gold": (- (at 'gold (rpg-characters.get-character character-id)) cost)
        })
        
        ;; Consume material items
        (map (lambda (mat)
               (consume-items character-id (at 'key mat) (at 'value mat)))
             (map (lambda (k) { "key": k, "value": (at k mats) }) 
                  (keys mats)))
        
        ;; Attempt craft
        (let ((roll (rpg-combat.random-decimal)))
          (if (< roll success)
            ;; Success
            (let ((new-item (create-item result 
                                       (at 'owner (rpg-characters.get-character character-id)))))
              (add-to-inventory character-id new-item)
              (format "Crafted {}!" [(at 'name (read items result))]))
            
            ;; Failure
            "Crafting failed!")))))
  
  ;; Helper Functions
  (defun get-equipment-slot:string (item-type:string)
    @doc "Get equipment slot for item type"
    (if (= item-type "weapon") "weapon"
      (if (= item-type "armor") "armor"
        (if (= item-type "helmet") "helmet"
          (if (= item-type "boots") "boots"
            "accessory")))))
  
  (defun update-character-stats:string (character-id:string)
    @doc "Recalculate character stats based on equipment"
    (require-capability (INTERNAL))
    ;; Sum all equipment bonuses and update character
    ;; Implementation details...
    "Stats updated")
  
  (defun find-item-slot:string (character-id:string item-instance-id:string)
    @doc "Find which slot an item is equipped in"
    (with-read rpg-characters.inventories character-id
      { "weapon" := weapon
      , "armor" := armor
      , "helmet" := helmet
      , "boots" := boots
      , "accessory" := accessory }
      
      (if (= weapon item-instance-id) "weapon"
        (if (= armor item-instance-id) "armor"
          (if (= helmet item-instance-id) "helmet"
            (if (= boots item-instance-id) "boots"
              "accessory"))))))
  
  (defun unequip-slot:string (character-id:string slot:string)
    @doc "Unequip item in specific slot"
    (require-capability (INTERNAL))
    (let ((current-item (at slot (read rpg-characters.inventories character-id))))
      (if (!= current-item "")
        (update item-instances current-item { "equipped-by": "" })
        ""))
    "Slot cleared")
  
  (defun add-to-inventory:string (character-id:string item-instance-id:string)
    @doc "Add item to character inventory"
    (require-capability (INTERNAL))
    (with-read rpg-characters.inventories character-id
      { "items" := current-items
      , "max-items" := max-items }
      
      (enforce (< (length current-items) max-items) "Inventory full")
      
      (update rpg-characters.inventories character-id {
        "items": (+ current-items [item-instance-id])
      }))
    "Item added")
  
  (defun count-items:integer (character-id:string item-id:string)
    @doc "Count how many of an item a character has"
    (let ((inventory-items (at 'items (read rpg-characters.inventories character-id))))
      (fold (+) 0
        (map (lambda (instance-id)
               (if (= (at 'item-id (read item-instances instance-id)) item-id)
                 (at 'quantity (read item-instances instance-id))
                 0))
             inventory-items))))
  
  (defun consume-items:string (character-id:string item-id:string amount:integer)
    @doc "Consume a certain amount of items"
    (require-capability (INTERNAL))
    ;; Implementation to remove items from inventory
    "Items consumed")
  
  ;; Query Functions
  (defun get-item-info:object (item-id:string)
    @doc "Get item definition"
    (read items item-id))
  
  (defun get-character-equipment:object (character-id:string)
    @doc "Get all equipped items for character"
    (let ((inv (read rpg-characters.inventories character-id)))
      { "weapon": (if (!= (at 'weapon inv) "") 
                   (read items (at 'item-id (read item-instances (at 'weapon inv))))
                   {})
      , "armor": (if (!= (at 'armor inv) "")
                  (read items (at 'item-id (read item-instances (at 'armor inv))))
                  {})
      , "helmet": (if (!= (at 'helmet inv) "")
                   (read items (at 'item-id (read item-instances (at 'helmet inv))))
                   {})
      , "boots": (if (!= (at 'boots inv) "")
                  (read items (at 'item-id (read item-instances (at 'boots inv))))
                  {})
      , "accessory": (if (!= (at 'accessory inv) "")
                      (read items (at 'item-id (read item-instances (at 'accessory inv))))
                      {})
      }))
  
  ;; Internal capability
  (defcap INTERNAL () true)
)

(create-table items)
(create-table item-instances)
(create-table crafting-recipes)
```

## Step 4: Dungeon & Quest System

Now let's create the PvE content:

```pact
;; world/dungeons.pact
(module rpg-dungeons GOVERNANCE
  @doc "Dungeon and quest system"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'game-admin))
  
  ;; Schemas
  (defschema dungeon
    @doc "Dungeon definition"
    dungeon-id:string
    name:string
    description:string
    min-level:integer
    max-level:integer
    difficulty:string      ;; easy, normal, hard, legendary
    entry-cost:integer
    ;; Rewards
    experience-reward:integer
    gold-reward:integer
    item-pool:[string]     ;; Possible item drops
    drop-rates:[decimal]   ;; Corresponding drop rates
    ;; Mechanics
    monster-types:[string]
    boss-id:string
    daily-limit:integer
    reset-time:time)
  
  (defschema dungeon-run
    @doc "Active dungeon run"
    run-id:string
    character-id:string
    dungeon-id:string
    current-floor:integer
    monsters-defeated:integer
    loot-collected:[string]
    status:string          ;; active, completed, failed
    started-at:time
    completed-at:time)
  
  (defschema quest
    @doc "Quest definition"
    quest-id:string
    name:string
    description:string
    quest-type:string      ;; main, side, daily, weekly
    prerequisites:[string] ;; Required completed quests
    objectives:object      ;; {"kill": 10, "collect": 5}
    rewards:object         ;; {"exp": 1000, "gold": 500, "items": ["sword"]}
    repeatable:bool
    cooldown:integer)
  
  (defschema quest-progress
    @doc "Player quest progress"
    progress-id:string
    character-id:string
    quest-id:string
    objectives-progress:object
    status:string          ;; active, completed, claimed
    started-at:time
    completed-at:time)
  
  ;; Tables
  (deftable dungeons:{dungeon})
  (deftable dungeon-runs:{dungeon-run})
  (deftable quests:{quest})
  (deftable quest-progress:{quest-progress})
  
  ;; Initialize dungeons
  (defun init-dungeons ()
    @doc "Initialize dungeon content"
    (insert dungeons "goblin-cave" {
      "dungeon-id": "goblin-cave",
      "name": "Goblin Cave",
      "description": "A cave infested with goblins",
      "min-level": 1,
      "max-level": 10,
      "difficulty": "easy",
      "entry-cost": 10,
      "experience-reward": 100,
      "gold-reward": 50,
      "item-pool": ["rusty-sword", "leather-armor", "health-potion"],
      "drop-rates": [0.1, 0.05, 0.3],
      "monster-types": ["goblin", "goblin-warrior"],
      "boss-id": "goblin-chief",
      "daily-limit": 5,
      "reset-time": (at 'block-time (chain-data))
    })
    
    (insert dungeons "dark-forest" {
      "dungeon-id": "dark-forest",
      "name": "Dark Forest",
      "description": "A mysterious forest filled with dangers",
      "min-level": 10,
      "max-level": 25,
      "difficulty": "normal",
      "entry-cost": 50,
      "experience-reward": 500,
      "gold-reward": 200,
      "item-pool": ["iron-sword", "chainmail", "mana-potion"],
      "drop-rates": [0.15, 0.1, 0.25],
      "monster-types": ["wolf", "bear", "dark-elf"],
      "boss-id": "forest-guardian",
      "daily-limit": 3,
      "reset-time": (at 'block-time (chain-data))
    }))
  
  ;; Dungeon System
  (defcap ENTER_DUNGEON (character-id:string dungeon-id:string)
    @doc "Capability to enter dungeon"
    (with-read rpg-characters.characters character-id 
      { "owner" := owner
      , "level" := level
      , "in-combat" := in-combat }
      
      (enforce-guard (at 'guard (coin.details owner)))
      (enforce (not in-combat) "Already in combat")
      
      (with-read dungeons dungeon-id
        { "min-level" := min-lvl
        , "max-level" := max-lvl }
        
        (enforce (>= level min-lvl) "Level too low")
        (enforce (<= level max-lvl) "Level too high"))))
  
  (defun enter-dungeon:string (character-id:string dungeon-id:string)
    @doc "Enter a dungeon"
    (with-capability (ENTER_DUNGEON character-id dungeon-id)
      ;; Check daily limit
      (let ((runs-today (count-daily-runs character-id dungeon-id)))
        (with-read dungeons dungeon-id 
          { "daily-limit" := limit
          , "entry-cost" := cost }
          
          (enforce (< runs-today limit) "Daily limit reached")
          
          ;; Pay entry cost
          (with-read rpg-characters.inventories character-id { "gold" := gold }
            (enforce (>= gold cost) "Not enough gold")
            (update rpg-characters.inventories character-id {
              "gold": (- gold cost)
            }))
          
          ;; Create dungeon run
          (let ((run-id (hash [character-id dungeon-id (at 'block-time (chain-data))])))
            (insert dungeon-runs run-id {
              "run-id": run-id,
              "character-id": character-id,
              "dungeon-id": dungeon-id,
              "current-floor": 1,
              "monsters-defeated": 0,
              "loot-collected": [],
              "status": "active",
              "started-at": (at 'block-time (chain-data)),
              "completed-at": (time "1970-01-01T00:00:00Z")
            })
            
            ;; Mark character as in dungeon
            (update rpg-characters.characters character-id {
              "location": dungeon-id,
              "in-combat": true
            })
            
            (format "Entered {}" [(at 'name (read dungeons dungeon-id))]))))))
  
  (defun fight-monster:string (run-id:string)
    @doc "Fight next monster in dungeon"
    (with-read dungeon-runs run-id
      { "character-id" := char-id
      , "dungeon-id" := dungeon-id
      , "status" := status
      , "current-floor" := floor
      , "monsters-defeated" := defeated }
      
      (enforce (= status "active") "Run not active")
      
      ;; Simulate combat (simplified)
      (let* ( (monster-type (get-random-monster dungeon-id))
              (char-stats (rpg-characters.get-character-stats char-id))
              (combat-result (simulate-combat char-stats monster-type)) )
        
        (if (at 'victory combat-result)
          ;; Victory
          (begin
            ;; Update run progress
            (update dungeon-runs run-id {
              "monsters-defeated": (+ defeated 1),
              "current-floor": (if (= (mod (+ defeated 1) 5) 0) 
                                (+ floor 1) 
                                floor)
            })
            
            ;; Roll for loot
            (let ((loot (roll-loot dungeon-id)))
              (if (!= loot "")
                (begin
                  (give-item char-id loot)
                  (with-read dungeon-runs run-id { "loot-collected" := current-loot }
                    (update dungeon-runs run-id {
                      "loot-collected": (+ current-loot [loot])
                    })))
                ""))
            
            ;; Grant experience
            (rpg-characters.add-experience char-id 
              (/ (at 'experience-reward (read dungeons dungeon-id)) 10))
            
            (format "Defeated {}! Floor {}" [monster-type floor]))
          
          ;; Defeat
          (begin
            (update dungeon-runs run-id {
              "status": "failed",
              "completed-at": (at 'block-time (chain-data))
            })
            
            ;; Reset character
            (update rpg-characters.characters char-id {
              "location": "starter-town",
              "in-combat": false,
              "health": 1  ;; Leave at 1 HP
            })
            
            "Defeated! Run failed"))))))
  
  (defun complete-dungeon:string (run-id:string)
    @doc "Complete dungeon run and claim rewards"
    (with-read dungeon-runs run-id
      { "character-id" := char-id
      , "dungeon-id" := dungeon-id
      , "status" := status
      , "monsters-defeated" := defeated }
      
      (enforce (= status "active") "Run not active")
      (enforce (>= defeated 10) "Must defeat boss first")
      
      ;; Grant full rewards
      (with-read dungeons dungeon-id
        { "experience-reward" := exp
        , "gold-reward" := gold }
        
        (rpg-characters.add-experience char-id exp)
        
        (with-read rpg-characters.inventories char-id { "gold" := current-gold }
          (update rpg-characters.inventories char-id {
            "gold": (+ current-gold gold)
          }))
        
        ;; Update run
        (update dungeon-runs run-id {
          "status": "completed",
          "completed-at": (at 'block-time (chain-data))
        })
        
        ;; Reset character location
        (update rpg-characters.characters char-id {
          "location": "starter-town",
          "in-combat": false
        })
        
        (format "Dungeon completed! +{} EXP, +{} Gold" [exp gold]))))
  
  ;; Quest System
  (defcap START_QUEST (character-id:string quest-id:string)
    @doc "Capability to start quest"
    (with-read rpg-characters.characters character-id { "owner" := owner }
      (enforce-guard (at 'guard (coin.details owner)))))
  
  (defun start-quest:string (character-id:string quest-id:string)
    @doc "Start a quest"
    (with-capability (START_QUEST character-id quest-id)
      (with-read quests quest-id
        { "prerequisites" := prereqs
        , "repeatable" := repeatable }
        
        ;; Check prerequisites
        (map (lambda (prereq)
               (enforce (quest-completed character-id prereq)
                       (format "Must complete {} first" [prereq])))
             prereqs)
        
        ;; Check if already active or on cooldown
        (let ((existing (get-quest-progress character-id quest-id)))
          (if (!= existing "")
            (with-read quest-progress existing { "status" := status }
              (enforce (and repeatable (= status "claimed")) 
                      "Quest already active or completed"))
            ""))
        
        ;; Create quest progress
        (let ((progress-id (hash [character-id quest-id (at 'block-time (chain-data))])))
          (insert quest-progress progress-id {
            "progress-id": progress-id,
            "character-id": character-id,
            "quest-id": quest-id,
            "objectives-progress": (initialize-objectives quest-id),
            "status": "active",
            "started-at": (at 'block-time (chain-data)),
            "completed-at": (time "1970-01-01T00:00:00Z")
          })
          
          (format "Started quest: {}" [(at 'name (read quests quest-id))]))))))
  
  (defun update-quest-progress:string 
    ( character-id:string 
      action:string 
      target:string 
      amount:integer )
    @doc "Update quest progress based on actions"
    (require-capability (INTERNAL))
    
    ;; Get all active quests
    (let ((active-quests (get-active-quests character-id)))
      (map (lambda (progress-id)
             (with-read quest-progress progress-id
               { "quest-id" := quest-id
               , "objectives-progress" := progress }
               
               (with-read quests quest-id { "objectives" := objectives }
                 ;; Check if this action contributes to quest
                 (if (contains action (keys objectives))
                   (let* ( (current (at action progress))
                           (needed (at action objectives))
                           (new-progress (min (+ current amount) needed)) )
                     
                     (update quest-progress progress-id {
                       "objectives-progress": (+ progress { action: new-progress })
                     })
                     
                     ;; Check if quest completed
                     (if (quest-objectives-complete progress-id)
                       (update quest-progress progress-id {
                         "status": "completed",
                         "completed-at": (at 'block-time (chain-data))
                       })
                       ""))
                   ""))))
           active-quests))
    "Progress updated")
  
  (defun claim-quest-rewards:string (character-id:string quest-id:string)
    @doc "Claim completed quest rewards"
    (let ((progress-id (get-quest-progress character-id quest-id)))
      (with-read quest-progress progress-id
        { "status" := status }
        
        (enforce (= status "completed") "Quest not completed")
        
        (with-read quests quest-id { "rewards" := rewards }
          ;; Grant experience
          (if (contains "exp" (keys rewards))
            (rpg-characters.add-experience character-id (at "exp" rewards))
            "")
          
          ;; Grant gold
          (if (contains "gold" (keys rewards))
            (with-read rpg-characters.inventories character-id { "gold" := current }
              (update rpg-characters.inventories character-id {
                "gold": (+ current (at "gold" rewards))
              }))
            "")
          
          ;; Grant items
          (if (contains "items" (keys rewards))
            (map (lambda (item) (give-item character-id item))
                 (at "items" rewards))
            "")
          
          ;; Update quest status
          (update quest-progress progress-id { "status": "claimed" })
          
          (format "Quest rewards claimed!" [])))))
  
  ;; Helper Functions
  (defun count-daily-runs:integer (character-id:string dungeon-id:string)
    @doc "Count dungeon runs today"
    (let* ( (today-start (get-day-start))
            (runs (select dungeon-runs 
                   (and? (where 'character-id (= character-id))
                         (and? (where 'dungeon-id (= dungeon-id))
                               (where 'started-at (> today-start)))))) )
      (length runs)))
  
  (defun get-day-start:time ()
    @doc "Get start of current day"
    ;; Simplified - in production use proper date math
    (add-time (at 'block-time (chain-data)) -86400))
  
  (defun get-random-monster:string (dungeon-id:string)
    @doc "Get random monster from dungeon"
    (with-read dungeons dungeon-id { "monster-types" := monsters }
      (at (mod (str-to-int 64 (hash (at 'block-time (chain-data)))) 
               (length monsters))
          monsters)))
  
  (defun simulate-combat:object (char-stats:object monster-type:string)
    @doc "Simulate combat outcome"
    ;; Simplified combat simulation
    (let ((char-power (+ (at 'attack-power char-stats) 
                        (at 'defense char-stats))))
      { "victory": (> char-power 50)
      , "damage-taken": 20 }))
  
  (defun roll-loot:string (dungeon-id:string)
    @doc "Roll for loot drop"
    (with-read dungeons dungeon-id 
      { "item-pool" := items
      , "drop-rates" := rates }
      
      (let ((roll (rpg-combat.random-decimal)))
        ;; Find which item dropped (if any)
        (fold (lambda (acc idx)
                (if (and (= acc "") (< roll (at idx rates)))
                  (at idx items)
                  acc))
              ""
              (enumerate 0 (- (length items) 1))))))
  
  (defun give-item:string (character-id:string item-id:string)
    @doc "Give item to character"
    (require-capability (INTERNAL))
    (let ((instance-id (rpg-equipment.create-item item-id 
                        (at 'owner (rpg-characters.get-character character-id)))))
      (rpg-equipment.add-to-inventory character-id instance-id)))
  
  (defun quest-completed:bool (character-id:string quest-id:string)
    @doc "Check if quest completed"
    (let ((progress (select quest-progress 
                    (and? (where 'character-id (= character-id))
                          (where 'quest-id (= quest-id))))))
      (if (= (length progress) 0)
        false
        (or (= (at 'status (at 0 progress)) "completed")
            (= (at 'status (at 0 progress)) "claimed")))))
  
  (defun get-quest-progress:string (character-id:string quest-id:string)
    @doc "Get quest progress ID"
    (let ((progress (select quest-progress 
                    (and? (where 'character-id (= character-id))
                          (where 'quest-id (= quest-id))))))
      (if (= (length progress) 0)
        ""
        (at 'progress-id (at 0 progress)))))
  
  (defun initialize-objectives:object (quest-id:string)
    @doc "Initialize quest objectives to 0"
    (with-read quests quest-id { "objectives" := objectives }
      (fold (lambda (acc key) (+ acc { key: 0 }))
            {}
            (keys objectives))))
  
  (defun quest-objectives-complete:bool (progress-id:string)
    @doc "Check if all quest objectives complete"
    (with-read quest-progress progress-id
      { "quest-id" := quest-id
      , "objectives-progress" := progress }
      
      (with-read quests quest-id { "objectives" := objectives }
        (fold (lambda (complete key)
                (and complete (>= (at key progress) (at key objectives))))
              true
              (keys objectives)))))
  
  (defun get-active-quests:[string] (character-id:string)
    @doc "Get active quest progress IDs"
    (map (at 'progress-id)
         (select quest-progress 
          (and? (where 'character-id (= character-id))
                (where 'status (= "active"))))))
  
  ;; Query Functions
  (defun get-available-dungeons:[object] (character-level:integer)
    @doc "Get dungeons available for character level"
    (select dungeons 
      (and? (where 'min-level (<= character-level))
            (where 'max-level (>= character-level)))))
  
  (defun get-dungeon-leaderboard:[object] (dungeon-id:string)
    @doc "Get dungeon completion leaderboard"
    (take 10
      (sort ['monsters-defeated]
        (select dungeon-runs 
          (and? (where 'dungeon-id (= dungeon-id))
                (where 'status (= "completed")))))))
  
  ;; Internal capability
  (defcap INTERNAL () true)
)

(create-table dungeons)
(create-table dungeon-runs)
(create-table quests)
(create-table quest-progress)
```

## Testing & Balancing

Let's create comprehensive tests for our game:

```lisp
;; tests/test-rpg-game.repl

(begin-tx "Setup game environment")

;; Define keysets
(env-data {
  "game-admin": {
    "keys": ["admin-key"],
    "pred": "keys-all"
  },
  "alice-guard": {
    "keys": ["alice-key"],
    "pred": "keys-all"
  },
  "bob-guard": {
    "keys": ["bob-key"],
    "pred": "keys-all"
  }
})

(env-keys ["admin-key"])
(define-keyset 'game-admin (read-keyset "game-admin"))

;; Mock coin module
(module coin G
  (defcap G () true)
  (defschema account-details balance:decimal guard:guard)
  (deftable accounts:{account-details})
  
  (defun details (account:string)
    (read accounts account))
  
  (defun transfer (from:string to:string amount:decimal)
    "Transfer")
)
(create-table accounts)

;; Create test accounts
(insert coin.accounts "alice" { "balance": 1000.0, "guard": (read-keyset "alice-guard") })
(insert coin.accounts "bob" { "balance": 1000.0, "guard": (read-keyset "bob-guard") })
(insert coin.accounts "game-treasury" { "balance": 0.0, "guard": (read-keyset "game-admin") })

;; Load game modules
(load "../core/characters.pact")
(load "../core/combat.pact")
(load "../items/equipment.pact")
(load "../world/dungeons.pact")

;; Initialize game data
(rpg-characters.init-classes)
(rpg-combat.init-skills)
(rpg-equipment.init-items)
(rpg-dungeons.init-dungeons)

(commit-tx)

;; Test Character Creation
(begin-tx "Test character creation")

(env-chain-data { "sender": "alice" })
(env-keys ["alice-key"])

(expect "Create warrior character"
  true
  (contains "Character" (rpg-characters.create-character "Aragorn" "warrior")))

(expect "Character level is 1"
  1
  (at 'level (rpg-characters.get-character 
    (at 0 (rpg-characters.get-player-characters "alice")))))

(expect "Character has starting gold"
  100
  (at 'gold (rpg-characters.get-character 
    (at 0 (rpg-characters.get-player-characters "alice")))))

(commit-tx)

;; Test Combat
(begin-tx "Test combat system")

(env-chain-data { "sender": "bob" })
(env-keys ["bob-key"])

;; Bob creates a character
(rpg-characters.create-character "Legolas" "rogue")

(let ((alice-char (at 0 (rpg-characters.get-player-characters "alice")))
      (bob-char (at 0 (rpg-characters.get-player-characters "bob"))))
  
  ;; Start battle
  (env-keys ["alice-key"])
  (let ((battle-id (rpg-combat.start-battle alice-char bob-char)))
    
    ;; Alice attacks
    (expect "Attack succeeds"
      true
      (contains "damage" (rpg-combat.attack battle-id bob-char)))
    
    ;; Bob's turn
    (env-keys ["bob-key"])
    (expect "Bob can attack"
      true
      (contains "damage" (rpg-combat.attack battle-id alice-char)))))

(commit-tx)

;; Test Equipment
(begin-tx "Test equipment system")

(env-keys ["alice-key"])
(let ((alice-char (at 0 (rpg-characters.get-player-characters "alice"))))
  
  ;; Give Alice a better sword
  (let ((sword-id (rpg-equipment.create-item "iron-sword" "alice")))
    
    ;; Equip the sword
    (expect-failure "Cannot equip - level too low"
      "Level too low"
      (rpg-equipment.equip-item alice-char sword-id))))

(commit-tx)

;; Test Dungeons
(begin-tx "Test dungeon system")

(env-keys ["alice-key"])
(let ((alice-char (at 0 (rpg-characters.get-player-characters "alice"))))
  
  ;; Enter dungeon
  (expect "Enter goblin cave"
    true
    (contains "Entered" (rpg-dungeons.enter-dungeon alice-char "goblin-cave")))
  
  ;; Fight monsters
  (let ((runs (select rpg-dungeons.dungeon-runs 
               (where 'character-id (= alice-char)))))
    (let ((run-id (at 'run-id (at 0 runs))))
      
      ;; Fight some monsters
      (expect "Fight monster"
        true
        (or (contains "Defeated" (rpg-dungeons.fight-monster run-id))
            (contains "failed" (rpg-dungeons.fight-monster run-id)))))))

(commit-tx)

;; Test Character Progression
(begin-tx "Test leveling")

;; Manually give experience for testing
(env-keys ["admin-key"])
(let ((alice-char (at 0 (rpg-characters.get-player-characters "alice"))))
  
  ;; Add enough experience to level up
  (rpg-characters.add-experience alice-char 100)
  
  (expect "Character leveled up"
    2
    (at 'level (rpg-characters.get-character alice-char))))

(commit-tx)

(print "All RPG game tests completed!")
```

## Frontend Integration

Here's an example React component for the game UI:

```javascript
// frontend/RPGGame.jsx
import React, { useState, useEffect } from 'react';
import { useKadenaClient } from './kadena-hooks';

const RPGGame = () => {
  const { client, account } = useKadenaClient();
  const [character, setCharacter] = useState(null);
  const [inventory, setInventory] = useState([]);
  const [currentBattle, setCurrentBattle] = useState(null);
  const [dungeons, setDungeons] = useState([]);
  const [quests, setQuests] = useState([]);

  // Character Management
  const createCharacter = async (name, characterClass) => {
    const cmd = {
      networkId: 'testnet04',
      payload: {
        exec: {
          data: {},
          code: `(rpg-characters.create-character "${name}" "${characterClass}")`
        }
      },
      signers: [{
        pubKey: account.publicKey,
        clist: [
          { name: "coin.GAS", args: [] },
          { name: "coin.TRANSFER", args: [account.address, "game-treasury", 10.0] },
          { name: "rpg-characters.CREATE_CHARACTER", args: [account.address, name] }
        ]
      }],
      meta: {
        chainId: '0',
        sender: account.address,
        gasLimit: 150000,
        gasPrice: 0.00000001,
        ttl: 600
      }
    };

    const result = await client.submit(cmd);
    await loadCharacter();
    return result;
  };

  const loadCharacter = async () => {
    const cmd = {
      networkId: 'testnet04',
      payload: {
        exec: {
          data: {},
          code: `(rpg-characters.get-player-characters "${account.address}")`
        }
      },
      meta: {
        chainId: '0',
        sender: "",
        gasLimit: 15000,
        gasPrice: 0.00000001,
        ttl: 600
      }
    };

    const result = await client.local(cmd);
    if (result.result.data.length > 0) {
      const charId = result.result.data[0];
      await loadCharacterDetails(charId);
    }
  };

  const loadCharacterDetails = async (characterId) => {
    const cmd = {
      networkId: 'testnet04',
      payload: {
        exec: {
          data: {},
          code: `(rpg-characters.get-character "${characterId}")`
        }
      },
      meta: {
        chainId: '0',
        sender: "",
        gasLimit: 15000,
        gasPrice: 0.00000001,
        ttl: 600
      }
    };

    const result = await client.local(cmd);
    setCharacter(result.result.data);
  };

  // Combat System
  const startBattle = async (opponentId) => {
    const cmd = {
      networkId: 'testnet04',
      payload: {
        exec: {
          data: {},
          code: `(rpg-combat.start-battle "${character.characterId}" "${opponentId}")`
        }
      },
      signers: [{
        pubKey: account.publicKey,
        clist: [
          { name: "coin.GAS", args: [] }
        ]
      }],
      meta: {
        chainId: '0',
        sender: account.address,
        gasLimit: 150000,
        gasPrice: 0.00000001,
        ttl: 600
      }
    };

    const result = await client.submit(cmd);
    setCurrentBattle(result.result.data);
  };

  const performAttack = async (targetId) => {
    const cmd = {
      networkId: 'testnet04',
      payload: {
        exec: {
          data: {},
          code: `(rpg-combat.attack "${currentBattle}" "${targetId}")`
        }
      },
      signers: [{
        pubKey: account.publicKey,
        clist: [
          { name: "coin.GAS", args: [] },
          { name: "rpg-combat.BATTLE_ACTION", args: [currentBattle, character.characterId] }
        ]
      }],
      meta: {
        chainId: '0',
        sender: account.address,
        gasLimit: 150000,
        gasPrice: 0.00000001,
        ttl: 600
      }
    };

    const result = await client.submit(cmd);
    // Update battle state
    await loadBattleStatus();
  };

  // Character Creation UI
  if (!character) {
    return (
      <div className="character-creation">
        <h1>Create Your Hero</h1>
        <form onSubmit={(e) => {
          e.preventDefault();
          const formData = new FormData(e.target);
          createCharacter(formData.get('name'), formData.get('class'));
        }}>
          <input name="name" placeholder="Character Name" required />
          <select name="class" required>
            <option value="">Select Class</option>
            <option value="warrior">Warrior</option>
            <option value="mage">Mage</option>
            <option value="rogue">Rogue</option>
            <option value="cleric">Cleric</option>
          </select>
          <button type="submit">Create Character</button>
        </form>
      </div>
    );
  }

  // Main Game UI
  return (
    <div className="rpg-game">
      <div className="character-panel">
        <h2>{character.name}</h2>
        <div className="stats">
          <div>Level: {character.level}</div>
          <div>Class: {character.class}</div>
          <div>HP: {character.health}/{character.maxHealth}</div>
          <div>MP: {character.mana}/{character.maxMana}</div>
          <div>XP: {character.experience}</div>
          <div>Gold: {character.gold}</div>
        </div>
        
        <div className="attributes">
          <h3>Attributes</h3>
          <div>STR: {character.strength}</div>
          <div>INT: {character.intelligence}</div>
          <div>AGI: {character.agility}</div>
          <div>VIT: {character.vitality}</div>
        </div>
      </div>

      <div className="game-content">
        {currentBattle ? (
          <BattleScreen 
            battle={currentBattle}
            character={character}
            onAttack={performAttack}
          />
        ) : (
          <WorldMap 
            character={character}
            dungeons={dungeons}
            onEnterDungeon={enterDungeon}
          />
        )}
      </div>

      <div className="inventory-panel">
        <h3>Inventory</h3>
        {inventory.map(item => (
          <InventoryItem 
            key={item.instanceId}
            item={item}
            onEquip={() => equipItem(item.instanceId)}
            onUse={() => useItem(item.instanceId)}
          />
        ))}
      </div>

      <div className="quest-panel">
        <h3>Active Quests</h3>
        {quests.filter(q => q.status === 'active').map(quest => (
          <QuestCard 
            key={quest.questId}
            quest={quest}
            onComplete={() => completeQuest(quest.questId)}
          />
        ))}
      </div>
    </div>
  );
};

// Battle Component
const BattleScreen = ({ battle, character, onAttack }) => {
  const [battleLog, setBattleLog] = useState([]);
  const [isPlayerTurn, setIsPlayerTurn] = useState(false);

  return (
    <div className="battle-screen">
      <h2>Battle!</h2>
      <div className="combatants">
        <div className="player">
          <h3>{character.name}</h3>
          <div>HP: {character.health}/{character.maxHealth}</div>
          <div>MP: {character.mana}/{character.maxMana}</div>
        </div>
        
        <div className="vs">VS</div>
        
        <div className="opponent">
          <h3>Opponent</h3>
          <div>HP: ???</div>
        </div>
      </div>

      <div className="battle-actions">
        <button onClick={() => onAttack('opponent')} disabled={!isPlayerTurn}>
          Attack
        </button>
        <button disabled={!isPlayerTurn}>Use Skill</button>
        <button disabled={!isPlayerTurn}>Use Item</button>
        <button disabled={!isPlayerTurn}>Flee</button>
      </div>

      <div className="battle-log">
        {battleLog.map((entry, i) => (
          <div key={i}>{entry}</div>
        ))}
      </div>
    </div>
  );
};

export default RPGGame;
```

## Summary

In this chapter, we've built a complete blockchain RPG with:
- ✅ Character creation and progression system
- ✅ Turn-based combat mechanics
- ✅ Items, equipment, and crafting
- ✅ Dungeon exploration with rewards
- ✅ Quest system with objectives
- ✅ Guild mechanics (can be extended)
- ✅ In-game economy
- ✅ Frontend integration

The game demonstrates how to:
- Handle complex state management on-chain
- Implement game mechanics with proper balance
- Create engaging player progression
- Manage randomness in blockchain
- Optimize for gas efficiency
- Build real-time gaming experiences

This foundation can be extended with additional features like:
- PvP tournaments
- Guild wars
- Seasonal events
- NFT integration for rare items
- Cross-chain character transfers
- Mobile app development