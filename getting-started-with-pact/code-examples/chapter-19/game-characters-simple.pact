;; game-characters-simple.pact
;; Simplified character system for testing
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
    class:string
    level:integer
    experience:integer
    health:integer
    max-health:integer
    mana:integer
    max-mana:integer
    strength:integer
    intelligence:integer
    agility:integer
    created-at:time)
  
  ;; Tables
  (deftable characters:{character})
  
  ;; Constants
  (defconst CLASSES ["warrior" "mage" "rogue"])
  
  ;; Functions
  (defun create-character:string (owner:string name:string class:string)
    @doc "Create new character"
    (enforce (contains class CLASSES) "Invalid class")
    
    (let* ((character-id (format "CHAR-{}-{}" [owner (at 'block-height (chain-data))]))
           (base-stats (get-base-stats class)))
      
      (insert characters character-id 
        (+ base-stats {
          "character-id": character-id,
          "owner": owner,
          "name": name,
          "class": class,
          "level": 1,
          "experience": 0,
          "created-at": (at 'block-time (chain-data))
        }))
      
      character-id))
  
  (defun get-base-stats:object (class:string)
    @doc "Get base stats for class"
    (cond
      ((= class "warrior") {
        "strength": 15,
        "intelligence": 8,
        "agility": 10,
        "health": 120,
        "max-health": 120,
        "mana": 20,
        "max-mana": 20
      })
      ((= class "mage") {
        "strength": 6,
        "intelligence": 15,
        "agility": 9,
        "health": 60,
        "max-health": 60,
        "mana": 100,
        "max-mana": 100
      })
      ((= class "rogue") {
        "strength": 10,
        "intelligence": 10,
        "agility": 15,
        "health": 80,
        "max-health": 80,
        "mana": 40,
        "max-mana": 40
      })))
  
  (defun gain-experience:string (character-id:string amount:integer)
    @doc "Award experience points"
    (with-read characters character-id 
      { "experience" := current-exp, "level" := level }
      
      (let ((new-exp (+ current-exp amount)))
        (update characters character-id {
          "experience": new-exp
        })
        (format "Gained {} experience" [amount]))))
  
  (defun get-character:object (character-id:string)
    @doc "Get character details"
    (read characters character-id))
)

(create-table characters)