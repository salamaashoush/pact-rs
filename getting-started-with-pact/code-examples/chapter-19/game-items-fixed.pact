;; game-items-fixed.pact
;; Item and equipment system (fixed version)
(module game-items GOVERNANCE
  @doc "Items, equipment, and inventory management"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'game-admin))
  
  ;; Schemas
  (defschema item-template
    @doc "Item definition"
    item-id:string
    name:string
    description:string
    item-type:string       ;; weapon, armor, consumable, material
    rarity:string         ;; common, uncommon, rare, epic, legendary
    level-requirement:integer
    ;; Stats
    strength-bonus:integer
    intelligence-bonus:integer
    agility-bonus:integer
    vitality-bonus:integer
    attack-bonus:integer
    defense-bonus:integer
    ;; Special properties
    special-effects:[string]
    consumable:bool
    stackable:bool
    max-stack:integer
    value:integer)        ;; Base shop value
  
  (defschema inventory-slot
    @doc "Character inventory slot"
    character-id:string
    slot-id:integer
    item-id:string
    quantity:integer
    equipped:bool
    equipment-slot:string) ;; weapon, armor, accessory, none
  
  (defschema equipment
    @doc "Character equipped items"
    character-id:string
    weapon:string
    armor:string
    accessory:string)
  
  ;; Tables
  (deftable item-templates:{item-template})
  (deftable inventory:{inventory-slot})
  (deftable equipment-table:{equipment})
  
  ;; Constants
  (defconst MAX_INVENTORY_SLOTS 20)
  (defconst EQUIPMENT_SLOTS ["weapon" "armor" "accessory"])
  
  (defconst RARITY_DROP_RATES {
    "common": 0.60,
    "uncommon": 0.25,
    "rare": 0.10,
    "epic": 0.04,
    "legendary": 0.01
  })
  
  ;; Capabilities
  (defcap ITEM_OWNER (character-id:string)
    @doc "Item ownership capability"
    (let ((character (game-characters.get-character character-id)))
      (enforce-guard (at 'guard (coin.details (at 'owner character))))))
  
  ;; Item Creation (Admin)
  (defun create-item-template:string 
    ( item-id:string
      name:string
      description:string
      item-type:string
      rarity:string
      level-req:integer
      stats:object )
    @doc "Create new item template"
    (with-capability (GOVERNANCE)
      (insert item-templates item-id {
        "item-id": item-id,
        "name": name,
        "description": description,
        "item-type": item-type,
        "rarity": rarity,
        "level-requirement": level-req,
        "strength-bonus": (try 0 (at 'strength stats)),
        "intelligence-bonus": (try 0 (at 'intelligence stats)),
        "agility-bonus": (try 0 (at 'agility stats)),
        "vitality-bonus": (try 0 (at 'vitality stats)),
        "attack-bonus": (try 0 (at 'attack stats)),
        "defense-bonus": (try 0 (at 'defense stats)),
        "special-effects": (try [] (at 'effects stats)),
        "consumable": (= item-type "consumable"),
        "stackable": (or (= item-type "consumable") 
                        (= item-type "material")),
        "max-stack": (if (or (= item-type "consumable") 
                           (= item-type "material")) 99 1),
        "value": (calculate-item-value rarity level-req)
      })
      (format "Item template {} created" [item-id])))
  
  ;; Inventory Management
  (defun give-item:string 
    ( character-id:string 
      item-id:string 
      quantity:integer )
    @doc "Give item to character"
    (with-capability (ITEM_OWNER character-id)
      ;; Check item exists
      (with-read item-templates item-id 
        { "stackable" := stackable
        , "max-stack" := max-stack }
        
        (if stackable
          (add-stackable-item character-id item-id quantity max-stack)
          (add-non-stackable-items character-id item-id quantity)))))
  
  (defun add-stackable-item:string 
    ( character-id:string 
      item-id:string 
      quantity:integer 
      max-stack:integer )
    @doc "Add stackable item to inventory"
    ;; Find existing stack
    (let ((existing-slots (filter (lambda (key) 
                                   (with-read inventory key
                                     { "character-id" := char-id
                                     , "item-id" := i-id }
                                     (and (= char-id character-id)
                                          (= i-id item-id))))
                                 (keys inventory))))
      
      (if (> (length existing-slots) 0)
        ;; Add to existing stack
        (let* ((slot-key (at 0 existing-slots))
               (slot-data (read inventory slot-key))
               (current-qty (at 'quantity slot-data))
               (new-qty (min max-stack (+ current-qty quantity))))
          
          (update inventory slot-key {
            "quantity": new-qty
          })
          
          (if (> (+ current-qty quantity) max-stack)
            ;; Create new stack with overflow
            (add-stackable-item character-id item-id 
                               (- (+ current-qty quantity) max-stack) 
                               max-stack)
            (format "Added {} {} to inventory" [quantity item-id])))
        ;; Create new stack
        (create-inventory-slot character-id item-id quantity))))
  
  (defun add-non-stackable-items:string 
    ( character-id:string 
      item-id:string 
      quantity:integer )
    @doc "Add non-stackable items"
    (let ((slots-needed quantity))
      (enforce (<= slots-needed (get-free-slots character-id)) 
              "Not enough inventory space")
      
      (map (lambda (n) 
             (create-inventory-slot character-id item-id 1))
           (enumerate 0 (- quantity 1)))
      
      (format "Added {} {} to inventory" [quantity item-id])))
  
  (defun create-inventory-slot:string 
    ( character-id:string 
      item-id:string 
      quantity:integer )
    @doc "Create new inventory slot"
    (let ((slot-id (get-next-slot-id character-id)))
      (enforce (< slot-id MAX_INVENTORY_SLOTS) "Inventory full")
      
      (insert inventory (format "{}:{}" [character-id slot-id]) {
        "character-id": character-id,
        "slot-id": slot-id,
        "item-id": item-id,
        "quantity": quantity,
        "equipped": false,
        "equipment-slot": "none"
      })
      
      (format "Item added to slot {}" [slot-id])))
  
  ;; Equipment System
  (defun equip-item:string 
    ( character-id:string 
      slot-id:integer )
    @doc "Equip item from inventory"
    (with-capability (ITEM_OWNER character-id)
      (with-read inventory (format "{}:{}" [character-id slot-id])
        { "item-id" := item-id
        , "equipped" := already-equipped }
        
        (enforce (not already-equipped) "Item already equipped")
        
        (with-read item-templates item-id
          { "item-type" := item-type
          , "level-requirement" := level-req }
          
          ;; Check level requirement
          (let ((character (game-characters.get-character character-id)))
            (enforce (>= (at 'level character) level-req) 
                    "Level requirement not met"))
          
          ;; Determine equipment slot
          (let ((equip-slot (cond
                             ((= item-type "weapon") "weapon")
                             ((= item-type "armor") "armor")
                             ((= item-type "accessory") "accessory")
                             (true ""))))
            
            (enforce (!= equip-slot "") "Item cannot be equipped")
            
            ;; Unequip current item in slot
            (unequip-slot character-id equip-slot)
            
            ;; Equip new item
            (update inventory (format "{}:{}" [character-id slot-id]) {
              "equipped": true,
              "equipment-slot": equip-slot
            })
            
            ;; Update equipment table
            (with-default-read equipment-table character-id
              { "weapon": "", "armor": "", "accessory": "" }
              { "weapon" := current-weapon
              , "armor" := current-armor
              , "accessory" := current-accessory }
              
              (write equipment-table character-id {
                "character-id": character-id,
                "weapon": (if (= equip-slot "weapon") item-id current-weapon),
                "armor": (if (= equip-slot "armor") item-id current-armor),
                "accessory": (if (= equip-slot "accessory") item-id current-accessory)
              }))
            
            ;; Apply stat bonuses
            (apply-equipment-stats character-id)
            
            (format "Equipped {} in {} slot" [item-id equip-slot]))))))
  
  (defun unequip-slot:string 
    ( character-id:string 
      equipment-slot:string )
    @doc "Unequip item in specific slot"
    ;; Find equipped item in slot
    (let ((equipped-keys (filter (lambda (key)
                                  (with-read inventory key
                                    { "character-id" := char-id
                                    , "equipped" := equipped
                                    , "equipment-slot" := slot }
                                    (and (= char-id character-id)
                                         (= equipped true)
                                         (= slot equipment-slot))))
                                (keys inventory))))
      
      (if (> (length equipped-keys) 0)
        (let ((slot-key (at 0 equipped-keys)))
          (update inventory slot-key {
            "equipped": false,
            "equipment-slot": "none"
          })
          "Item unequipped")
        "No item to unequip")))
  
  (defun apply-equipment-stats:string (character-id:string)
    @doc "Recalculate character stats with equipment"
    ;; This would recalculate all character stats based on equipped items
    ;; For simplicity, returning success
    "Stats updated")
  
  ;; Item Usage
  (defun use-item:string 
    ( character-id:string 
      slot-id:integer )
    @doc "Use consumable item"
    (with-capability (ITEM_OWNER character-id)
      (with-read inventory (format "{}:{}" [character-id slot-id])
        { "item-id" := item-id
        , "quantity" := qty }
        
        (with-read item-templates item-id
          { "consumable" := consumable
          , "special-effects" := effects }
          
          (enforce consumable "Item is not consumable")
          (enforce (> qty 0) "No items to use")
          
          ;; Apply item effects
          (map (apply-item-effect character-id) effects)
          
          ;; Reduce quantity
          (if (= qty 1)
            ;; Remove item
            (update inventory (format "{}:{}" [character-id slot-id]) {
              "item-id": "",
              "quantity": 0
            })
            ;; Reduce stack
            (update inventory (format "{}:{}" [character-id slot-id]) {
              "quantity": (- qty 1)
            }))
          
          (format "Used {}" [item-id])))))
  
  (defun apply-item-effect:string (character-id:string effect:string)
    @doc "Apply consumable effect"
    (cond
      ((= effect "heal-50")
       (game-characters.heal-character character-id 50))
      ((= effect "mana-30")
       (game-characters.restore-mana character-id 30))
      ((= effect "heal-full")
       (let ((char (game-characters.get-character character-id)))
         (game-characters.heal-character character-id 
           (- (at 'max-health char) (at 'health char)))))
      (true "Unknown effect")))
  
  ;; Helper Functions
  (defun calculate-item-value:integer (rarity:string level:integer)
    @doc "Calculate base item value"
    (let ((rarity-mult (cond
                        ((= rarity "common") 1)
                        ((= rarity "uncommon") 3)
                        ((= rarity "rare") 10)
                        ((= rarity "epic") 30)
                        ((= rarity "legendary") 100)
                        (true 1))))
      (* level rarity-mult 10)))
  
  (defun get-free-slots:integer (character-id:string)
    @doc "Get number of free inventory slots"
    (let ((used-slots (length (filter (lambda (key)
                                       (with-read inventory key
                                         { "character-id" := char-id }
                                         (= char-id character-id)))
                                     (keys inventory)))))
      (- MAX_INVENTORY_SLOTS used-slots)))
  
  (defun get-next-slot-id:integer (character-id:string)
    @doc "Get next available slot ID"
    (let ((slots (filter (lambda (key)
                          (with-read inventory key
                            { "character-id" := char-id }
                            (= char-id character-id)))
                        (keys inventory))))
      (if (= (length slots) 0)
        0
        (+ 1 (fold (max) 0 (map (lambda (key)
                                 (with-read inventory key
                                   { "slot-id" := id }
                                   id))
                               slots))))))
  
  ;; Query Functions
  (defun get-inventory:[object] (character-id:string)
    @doc "Get character inventory"
    (let ((inv-keys (filter (lambda (key)
                             (with-read inventory key
                               { "character-id" := char-id }
                               (= char-id character-id)))
                           (keys inventory))))
      (map (lambda (key) (read inventory key)) inv-keys)))
  
  (defun get-equipped-items:object (character-id:string)
    @doc "Get equipped items"
    (with-default-read equipment-table character-id
      { "weapon": "", "armor": "", "accessory": "" }
      { "weapon" := weapon, "armor" := armor, "accessory" := accessory }
      
      { "weapon": (if (!= weapon "") (try {} (read item-templates weapon)) {})
      , "armor": (if (!= armor "") (try {} (read item-templates armor)) {})
      , "accessory": (if (!= accessory "") (try {} (read item-templates accessory)) {}) }))
  
  ;; Utility function to help with list operations
  (defun enumerate:list (start:integer end:integer)
    @doc "Generate list of integers from start to end"
    (if (> start end)
      []
      (if (= start end)
        [start]
        (+ [start] (enumerate (+ start 1) end)))))
)

(create-table item-templates)
(create-table inventory)
(create-table equipment-table)