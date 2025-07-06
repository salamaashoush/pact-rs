(module zombie-quests GOV

  (defcap GOV () 
    (enforce-guard (read-keyset "zombie-quest-admin")))

  ; Schemas
  (defschema quest-info
    quest-id:string
    zombie-id:string
    owner:string
    quest-type:string
    started-at:time
    current-stage:integer
    rewards-earned:decimal
    status:string)

  (defschema quest-progress
    monsters-defeated:integer
    puzzles-solved:integer
    treasure-found:[string]
    experience-gained:integer)

  ; Tables
  (deftable quest-table:{quest-info})
  (deftable quest-progress-table:{quest-progress})

  ; Constants
  (defconst QUEST_TIMEOUT 86400.0) ; 24 hours
  (defconst MIN_LEVEL_FOR_QUEST 3)

  ; Events
  (defcap QUEST_STARTED:bool (quest-id:string zombie-id:string owner:string)
    @event true)

  (defcap QUEST_COMPLETED:bool (quest-id:string rewards:decimal)
    @event true)

  ; Capabilities
  (defcap QUEST_PARTICIPANT (zombie-id:string owner:string)
    @doc "Capability for quest participation"
    (enforce (= owner (at "sender" (chain-data))) "Not authorized")
    (with-read zombies.zombies-table zombie-id { "owner" := zombie-owner }
      (enforce (= owner zombie-owner) "Not your zombie!")))

  ; Helper Functions
  (defun generate-quest-id:string (zombie-id:string)
    @doc "Generate unique quest ID"
    (format "quest_{}_{}" [zombie-id (at "block-time" (chain-data))]))

  (defun enforce-quest-requirements (zombie-id:string)
    @doc "Check if zombie meets quest requirements"
    (with-read zombies.zombies-table zombie-id 
      { "level" := level
      , "status" := status
      }
      (enforce (>= level MIN_LEVEL_FOR_QUEST) 
        "Zombie must be at least level 3")
      (enforce (= status "active") 
        "Zombie must be active")))

  (defun simulate-battle:bool (zombie-id:string enemy:string)
    @doc "Simulate battle outcome"
    (with-read zombies.zombies-table zombie-id 
      { "level" := level
      , "win-count" := wins
      }
      (let ((power (+ (* level 10) (* wins 5)))
            (enemy-power (cond 
              ((= enemy "mini-boss") 50)
              ((= enemy "dragon-boss") 100)
              ((= enemy "skeleton") 20)
              30)))
        (> (+ power (mod (at "block-height" (chain-data)) 100)) 
           enemy-power))))

  ; Simple Quest - Single Step
  (defun start-training-quest (zombie-id:string)
    @doc "Simple single-transaction training quest"
    (let ((owner (at "sender" (chain-data))))
      (with-capability (QUEST_PARTICIPANT zombie-id owner)
        (enforce-quest-requirements zombie-id)
        (let ((quest-id (generate-quest-id zombie-id))
              (reward 5.0))
          (insert quest-table quest-id
            { "quest-id": quest-id
            , "zombie-id": zombie-id
            , "owner": owner
            , "quest-type": "training"
            , "started-at": (at "block-time" (chain-data))
            , "current-stage": 1
            , "rewards-earned": reward
            , "status": "completed"
            })
          (zombie-coin.transfer "quest-pool" owner reward)
          (emit-event (QUEST_COMPLETED quest-id reward))
          (format "Training complete! Earned {} ZMB" [reward])))))

  ; Two-Party Trade Pact
  (defpact zombie-trade-quest (zombie1-id:string zombie2-id:string 
                               party1:string party2:string)
    @doc "Two-party zombie trade requiring both confirmations"
    
    (step 
      (let ()
        (with-capability (QUEST_PARTICIPANT zombie1-id party1)
          (update zombies.zombies-table zombie1-id 
            { "status": "quest-locked" })
          (yield { "zombie1": zombie1-id
                 , "zombie2": zombie2-id
                 , "party1": party1
                 , "party2": party2
                 , "status": "awaiting-party2"
                 }))))
    
    (step 
      (resume { "zombie1" := z1
              , "zombie2" := z2 
              , "party1" := p1
              , "party2" := p2 
              }
        (with-capability (QUEST_PARTICIPANT z2 p2)
          (update zombies.zombies-table z2 
            { "status": "quest-locked" })
          (yield { "zombie1": z1
                 , "zombie2": z2
                 , "party1": p1
                 , "party2": p2
                 , "ready": true
                 }))))
    
    (step
      (resume { "zombie1" := z1
              , "zombie2" := z2
              , "party1" := p1
              , "party2" := p2
              , "ready" := ready
              }
        (enforce ready "Not ready to complete trade")
        ; Perform the swap
        (update zombies.zombies-table z1 
          { "owner": p2, "status": "active" })
        (update zombies.zombies-table z2 
          { "owner": p1, "status": "active" })
        "Trade completed!")))

  ; Multi-Stage Dungeon Quest
  (defpact dungeon-quest (zombie-id:string)
    @doc "Multi-stage dungeon exploration"
    
    (step ; Enter dungeon
      (let ((owner (at "sender" (chain-data))))
        (with-capability (QUEST_PARTICIPANT zombie-id owner)
          (enforce-quest-requirements zombie-id)
          (let ((quest-id (generate-quest-id zombie-id)))
            (insert quest-table quest-id
              { "quest-id": quest-id
              , "zombie-id": zombie-id
              , "owner": owner
              , "quest-type": "dungeon"
              , "started-at": (at "block-time" (chain-data))
              , "current-stage": 1
              , "rewards-earned": 0.0
              , "status": "in-progress"
              })
            (insert quest-progress-table quest-id
              { "monsters-defeated": 0
              , "puzzles-solved": 0
              , "treasure-found": []
              , "experience-gained": 0
              })
            (emit-event (QUEST_STARTED quest-id zombie-id owner))
            (yield { "quest-id": quest-id
                   , "zombie-id": zombie-id
                   , "owner": owner
                   })))))
    
    (step ; Battle monsters
      (resume { "quest-id" := qid
              , "zombie-id" := zid
              , "owner" := owner
              }
        (with-capability (QUEST_PARTICIPANT zid owner)
          (let ((battle1 (simulate-battle zid "skeleton"))
                (battle2 (simulate-battle zid "skeleton"))
                (monsters-defeated (+ (if battle1 1 0) (if battle2 1 0))))
            (update quest-progress-table qid
              { "monsters-defeated": monsters-defeated
              , "experience-gained": (* monsters-defeated 10)
              })
            (update quest-table qid 
              { "current-stage": 2
              , "rewards-earned": (* monsters-defeated 5.0)
              })
            (yield { "quest-id": qid
                   , "zombie-id": zid
                   , "owner": owner
                   , "battles-won": monsters-defeated
                   })))))
    
    (step ; Boss battle
      (resume { "quest-id" := qid
              , "zombie-id" := zid
              , "owner" := owner
              , "battles-won" := prev-battles
              }
        (with-capability (QUEST_PARTICIPANT zid owner)
          (enforce (> prev-battles 0) "Must defeat minions first")
          (let ((boss-result (simulate-battle zid "mini-boss")))
            (if boss-result
              (let ((reward 25.0))
                (with-read quest-table qid { "rewards-earned" := prev-rewards }
                  (update quest-table qid 
                    { "current-stage": 3
                    , "rewards-earned": (+ prev-rewards reward)
                    , "status": "completed"
                    })
                  (update quest-progress-table qid
                    { "monsters-defeated": (+ prev-battles 1)
                    , "treasure-found": ["boss-key"]
                    , "experience-gained": 50
                    })
                  (zombie-coin.transfer "quest-pool" owner 
                    (+ prev-rewards reward))
                  (emit-event (QUEST_COMPLETED qid (+ prev-rewards reward)))
                  "Quest completed! Boss defeated!"))
              (let ()
                (update quest-table qid { "status": "failed" })
                "Quest failed! Boss was too strong."))))))

  ; Timed Rescue Mission
  (defpact rescue-mission (hero-zombie:string captive-zombie:string)
    @doc "Time-sensitive rescue mission"
    
    (step ; Start mission
      (let ((owner (at "sender" (chain-data)))
            (start-time (at "block-time" (chain-data))))
        (with-capability (QUEST_PARTICIPANT hero-zombie owner)
          (enforce-quest-requirements hero-zombie)
          (update zombies.zombies-table captive-zombie 
            { "status": "captured" })
          (yield { "hero": hero-zombie
                 , "captive": captive-zombie
                 , "owner": owner
                 , "start-time": start-time
                 , "time-limit": 3600.0 ; 1 hour
                 }))))
    
    (step ; Attempt rescue
      (resume { "hero" := hero
              , "captive" := captive
              , "owner" := owner
              , "start-time" := start
              , "time-limit" := limit
              }
        (let ((current-time (at "block-time" (chain-data)))
              (elapsed (diff-time current-time start)))
          (enforce (<= elapsed limit) "Time limit exceeded!")
          (with-capability (QUEST_PARTICIPANT hero owner)
            (let ((success (simulate-battle hero "guards")))
              (if success
                (let ()
                  (update zombies.zombies-table captive 
                    { "status": "active" })
                  (zombie-coin.transfer "quest-pool" owner 50.0)
                  "Rescue successful!")
                "Rescue failed!")))))))

  ; View Functions
  (defun get-active-quests (owner:string)
    @doc "Get all active quests for an owner"
    (select quest-table 
      (and? (where "owner" (= owner))
            (where "status" (= "in-progress")))))

  (defun get-quest-details (quest-id:string)
    @doc "Get detailed quest information"
    (+ (read quest-table quest-id)
       (try {} (read quest-progress-table quest-id))))

  (defun get-zombie-quest-history (zombie-id:string)
    @doc "Get quest history for a zombie"
    (select quest-table (where "zombie-id" (= zombie-id))))
)