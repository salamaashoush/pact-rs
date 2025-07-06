; Simple Zombie Game - All-in-One Example
; ========================================
; A simplified version that works without module dependencies

(module simple-zombies GOV
  
  (defcap GOV () true)
  
  ; === SCHEMAS ===
  
  (defschema zombie
    name:string
    dna:integer
    level:integer
    power:integer
    owner:string
    wins:integer
    losses:integer)
  
  (defschema player
    account:string
    zombie-count:integer
    total-wins:integer
    total-losses:integer
    coins:decimal)
  
  ; === TABLES ===
  
  (deftable zombies:{zombie})
  (deftable players:{player})
  
  ; === CAPABILITIES ===
  
  (defcap ZOMBIE_OWNER (zombie-id:string owner:string)
    @doc "Ownership capability"
    (with-read zombies zombie-id { "owner" := zombie-owner }
      (enforce (= owner zombie-owner) "Not the zombie owner")))
  
  (defcap ACCOUNT_OWNER (account:string)
    @doc "Account ownership"
    (enforce (= account (at "sender" (chain-data))) "Not the account owner"))
  
  ; === HELPER FUNCTIONS ===
  
  (defun create-zombie-id:string (name:string owner:string)
    @doc "Generate unique zombie ID"
    (hash (+ name owner (format "{}" [(at "block-time" (chain-data))]))))
  
  (defun calculate-power:integer (level:integer wins:integer)
    @doc "Calculate zombie power"
    (+ (* level 10) (* wins 5)))
  
  ; === CORE FUNCTIONS ===
  
  (defun create-account:string (account:string)
    @doc "Create a new player account"
    (insert players account
      { "account": account
      , "zombie-count": 0
      , "total-wins": 0
      , "total-losses": 0
      , "coins": 100.0  ; Starting coins
      })
    "Account created!")
  
  (defun create-zombie:string (name:string)
    @doc "Create a new zombie"
    (let* ((owner (at "sender" (chain-data)))
           (zombie-id (create-zombie-id name owner))
           (dna (abs (str-to-int 64 (take 8 (hash name))))))
      
      ; Ensure player exists
      (with-default-read players owner
        { "zombie-count": 0, "coins": 100.0 }
        { "zombie-count" := count, "coins" := coins }
        
        ; Charge creation fee
        (enforce (>= coins 10.0) "Insufficient coins")
        
        ; Create zombie
        (insert zombies zombie-id
          { "name": name
          , "dna": dna
          , "level": 1
          , "power": 10
          , "owner": owner
          , "wins": 0
          , "losses": 0
          })
        
        ; Update player
        (write players owner
          { "account": owner
          , "zombie-count": (+ count 1)
          , "total-wins": 0
          , "total-losses": 0
          , "coins": (- coins 10.0)
          }))
      
      (format "Zombie {} created with ID: {}" [name zombie-id])))
  
  (defun battle:string (attacker-id:string defender-id:string)
    @doc "Battle between two zombies"
    (with-capability (ZOMBIE_OWNER attacker-id (at "sender" (chain-data)))
      (let ((attacker (read zombies attacker-id))
            (defender (read zombies defender-id)))
        
        ; Calculate battle outcome
        (let ((attacker-power (calculate-power 
                (at "level" attacker) 
                (at "wins" attacker)))
              (defender-power (calculate-power 
                (at "level" defender) 
                (at "losses" defender)))
              (random-factor (mod (at "block-height" (chain-data)) 20)))
          
          (if (> (+ attacker-power random-factor) defender-power)
            ; Attacker wins
            (let ()
              (update zombies attacker-id 
                { "wins": (+ (at "wins" attacker) 1)
                , "level": (+ (at "level" attacker) 1)
                , "power": (+ (at "power" attacker) 5)
                })
              (update zombies defender-id
                { "losses": (+ (at "losses" defender) 1) })
              
              ; Reward winner
              (with-read players (at "owner" attacker) { "coins" := coins }
                (update players (at "owner" attacker) 
                  { "coins": (+ coins 5.0) }))
              
              "Victory!")
            ; Defender wins  
            (let ()
              (update zombies defender-id
                { "wins": (+ (at "wins" defender) 1) })
              (update zombies attacker-id
                { "losses": (+ (at "losses" attacker) 1) })
              "Defeated!"))))))
  
  (defun transfer-zombie:string (zombie-id:string new-owner:string)
    @doc "Transfer zombie ownership"
    (with-capability (ZOMBIE_OWNER zombie-id (at "sender" (chain-data)))
      (update zombies zombie-id { "owner": new-owner })
      "Zombie transferred!"))
  
  ; === VIEW FUNCTIONS ===
  
  (defun get-zombie:object (zombie-id:string)
    @doc "Get zombie details"
    (read zombies zombie-id))
  
  (defun get-player-zombies:[object] (owner:string)
    @doc "Get all zombies owned by player"
    (select zombies (where "owner" (= owner))))
  
  (defun get-player-info:object (account:string)
    @doc "Get player information"
    (read players account))
  
  (defun get-top-zombies:[object] (limit:integer)
    @doc "Get top zombies by wins"
    (take limit 
      (reverse 
        (sort ["wins"] 
          (select zombies (constantly true))))))
)