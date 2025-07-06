;; game-quests.pact
;; Quest system for blockchain RPG
(module game-quests GOVERNANCE
  @doc "Quest system with objectives and rewards"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'game-admin))
  
  ;; Schemas
  (defschema quest
    @doc "Quest definition"
    quest-id:string
    name:string
    description:string
    quest-type:string      ;; main, side, daily
    objectives:[object]    ;; List of objectives
    requirements:object    ;; Level, items, etc
    rewards:object        ;; XP, items, gold
    repeatable:bool
    active:bool)
  
  (defschema quest-progress
    @doc "Player quest progress"
    progress-id:string
    character-id:string
    quest-id:string
    status:string         ;; active, completed, abandoned
    objectives-complete:[bool]
    started-at:time
    completed-at:time)
  
  (defschema achievement
    @doc "Achievement tracking"
    achievement-id:string
    character-id:string
    name:string
    description:string
    unlocked-at:time)
  
  ;; Tables
  (deftable quests:{quest})
  (deftable quest-progress-table:{quest-progress})
  (deftable achievements:{achievement})
  
  ;; Capabilities
  (defcap ACCEPT_QUEST (character-id:string quest-id:string)
    @doc "Quest acceptance capability"
    (let ((character (game-characters.get-character character-id)))
      (with-read quests quest-id { "requirements" := reqs }
        (enforce (check-requirements character reqs) 
                "Requirements not met"))))
  
  ;; Quest Management
  (defun create-quest:string 
    ( quest-id:string
      name:string
      description:string
      quest-type:string
      objectives:[object]
      requirements:object
      rewards:object
      repeatable:bool )
    @doc "Create new quest"
    (with-capability (GOVERNANCE)
      (insert quests quest-id {
        "quest-id": quest-id,
        "name": name,
        "description": description,
        "quest-type": quest-type,
        "objectives": objectives,
        "requirements": requirements,
        "rewards": rewards,
        "repeatable": repeatable,
        "active": true
      })
      (format "Quest {} created" [name])))
  
  ;; Quest Interaction
  (defun accept-quest:string (character-id:string quest-id:string)
    @doc "Accept a quest"
    (with-capability (ACCEPT_QUEST character-id quest-id)
      (with-read quests quest-id
        { "objectives" := objectives
        , "repeatable" := repeatable }
        
        ;; Check if already has quest
        (let ((existing (get-quest-progress character-id quest-id)))
          (if (and (!= existing {})
                   (not repeatable)
                   (= (at 'status existing) "completed"))
            (enforce false "Quest already completed")
            true))
        
        ;; Create progress entry
        (let ((progress-id (format "{}:{}" [character-id quest-id])))
          (write quest-progress-table progress-id {
            "progress-id": progress-id,
            "character-id": character-id,
            "quest-id": quest-id,
            "status": "active",
            "objectives-complete": (make-list (length objectives) false),
            "started-at": (at 'block-time (chain-data)),
            "completed-at": (parse-time "%F" "1970-01-01")
          })
          
          (format "Quest {} accepted" [quest-id]))))
  
  (defun update-objective:string 
    ( character-id:string 
      quest-id:string 
      objective-index:integer )
    @doc "Mark objective as complete"
    (let ((progress-id (format "{}:{}" [character-id quest-id])))
      (with-read quest-progress-table progress-id
        { "status" := status
        , "objectives-complete" := objectives }
        
        (enforce (= status "active") "Quest not active")
        
        ;; Update objective
        (let ((new-objectives 
               (update-list objectives objective-index true)))
          
          (update quest-progress-table progress-id {
            "objectives-complete": new-objectives
          })
          
          ;; Check if quest complete
          (if (fold (and) true new-objectives)
            (complete-quest character-id quest-id)
            (format "Objective {} completed" [objective-index]))))))
  
  (defun complete-quest:string (character-id:string quest-id:string)
    @doc "Complete quest and give rewards"
    (let ((progress-id (format "{}:{}" [character-id quest-id])))
      (with-read quest-progress-table progress-id
        { "status" := status }
        
        (enforce (= status "active") "Quest not active")
        
        (with-read quests quest-id
          { "rewards" := rewards
          , "name" := quest-name }
          
          ;; Update quest status
          (update quest-progress-table progress-id {
            "status": "completed",
            "completed-at": (at 'block-time (chain-data))
          })
          
          ;; Distribute rewards
          (distribute-rewards character-id rewards)
          
          ;; Check for achievement
          (check-quest-achievements character-id quest-id)
          
          (format "Quest {} completed!" [quest-name])))))
  
  (defun abandon-quest:string (character-id:string quest-id:string)
    @doc "Abandon active quest"
    (let ((progress-id (format "{}:{}" [character-id quest-id])))
      (with-read quest-progress-table progress-id
        { "status" := status }
        
        (enforce (= status "active") "Quest not active")
        
        (update quest-progress-table progress-id {
          "status": "abandoned"
        })
        
        "Quest abandoned")))
  
  ;; Reward Distribution
  (defun distribute-rewards:string (character-id:string rewards:object)
    @doc "Give quest rewards to character"
    ;; Experience reward
    (if (contains "experience" rewards)
      (game-characters.gain-experience character-id 
                                     (at 'experience rewards))
      "")
    
    ;; Item rewards
    (if (contains "items" rewards)
      (map (lambda (item)
             (game-items.give-item character-id 
                                 (at 'item-id item)
                                 (at 'quantity item)))
           (at 'items rewards))
      [])
    
    ;; Special rewards (titles, etc)
    (if (contains "title" rewards)
      (grant-title character-id (at 'title rewards))
      "")
    
    "Rewards distributed")
  
  ;; Achievement System
  (defun unlock-achievement:string 
    ( character-id:string 
      achievement-name:string 
      description:string )
    @doc "Unlock achievement for character"
    (let ((achievement-id (format "{}:{}" 
                                 [character-id achievement-name])))
      (insert achievements achievement-id {
        "achievement-id": achievement-id,
        "character-id": character-id,
        "name": achievement-name,
        "description": description,
        "unlocked-at": (at 'block-time (chain-data))
      })
      (format "Achievement unlocked: {}" [achievement-name])))
  
  (defun check-quest-achievements:string 
    ( character-id:string quest-id:string )
    @doc "Check for quest-related achievements"
    ;; Check quest completion count
    (let ((completed-quests 
           (select quest-progress-table 
                  (and? (where 'character-id (= character-id))
                       (where 'status (= "completed"))))))
      
      (let ((count (length completed-quests)))
        (cond
          ((= count 1)
           (unlock-achievement character-id 
                             "First Quest" 
                             "Complete your first quest"))
          ((= count 10)
           (unlock-achievement character-id 
                             "Quester" 
                             "Complete 10 quests"))
          ((= count 50)
           (unlock-achievement character-id 
                             "Quest Master" 
                             "Complete 50 quests"))
          (true ""))))
    
    ;; Check specific quest achievements
    (cond
      ((= quest-id "main-1")
       (unlock-achievement character-id 
                         "Hero's Journey" 
                         "Complete the first main quest"))
      (true "")))
  
  ;; Helper Functions
  (defun check-requirements:bool (character:object requirements:object)
    @doc "Check if character meets quest requirements"
    (let ((level-ok (if (contains "min-level" requirements)
                      (>= (at 'level character) 
                          (at 'min-level requirements))
                      true))
          (class-ok (if (contains "class" requirements)
                      (= (at 'class character)
                         (at 'class requirements))
                      true)))
      (and level-ok class-ok)))
  
  (defun update-list:[bool] (lst:[bool] index:integer value:bool)
    @doc "Update list element at index"
    (+ (take index lst) 
       [value] 
       (drop (+ index 1) lst)))
  
  (defun grant-title:string (character-id:string title:string)
    @doc "Grant title to character"
    ;; Would update character with title
    (format "Title '{}' granted" [title]))
  
  ;; Query Functions
  (defun get-available-quests:[object] (character-id:string)
    @doc "Get quests available to character"
    (let ((character (game-characters.get-character character-id))
          (all-quests (select quests (where 'active (= true)))))
      (filter (lambda (quest)
                (check-requirements character 
                                  (at 'requirements quest)))
              all-quests)))
  
  (defun get-active-quests:[object] (character-id:string)
    @doc "Get character's active quests"
    (select quest-progress-table 
           (and? (where 'character-id (= character-id))
                (where 'status (= "active")))))
  
  (defun get-quest-progress:object (character-id:string quest-id:string)
    @doc "Get specific quest progress"
    (let ((progress-id (format "{}:{}" [character-id quest-id])))
      (try {} (read quest-progress-table progress-id))))
  
  (defun get-achievements:[object] (character-id:string)
    @doc "Get character achievements"
    (select achievements (where 'character-id (= character-id))))
  
  ;; Daily Quest System
  (defun get-daily-quests:[string] ()
    @doc "Get today's daily quests"
    ;; Would rotate based on date
    ["daily-monster-hunt" "daily-dungeon-clear" "daily-pvp-victory"])
  
  (defun reset-daily-quests:string ()
    @doc "Reset daily quests (admin only)"
    (with-capability (GOVERNANCE)
      ;; Would reset daily quest progress
      "Daily quests reset"))
)

(create-table quests)
(create-table quest-progress-table)
(create-table achievements)