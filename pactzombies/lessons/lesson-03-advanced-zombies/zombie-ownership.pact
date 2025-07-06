(module zombies GOV
  
  (defcap GOV () 
    (enforce-guard (read-keyset "zombies-admin")))

  ; Schemas
  (defschema zombie
    name:string
    dna:integer
    type:string
    level:integer
    ready-time:time
    win-count:integer
    loss-count:integer
    owner:string)

  (defschema zombie-listing
    id:string
    name:string
    level:integer
    type:string
    price:decimal
    for-sale:bool)

  (defschema game-state
    paused:bool
    total-zombies:integer
    total-battles:integer)

  ; Tables
  (deftable zombies-table:{zombie})
  (deftable market-table:{zombie-listing})
  (deftable game-state-table:{game-state})

  ; Constants
  (defconst GAME_STATE_KEY "SINGLETON")
  (defconst BATTLE_COOLDOWN 300.0)

  ; Events
  (defcap ZOMBIE_TRANSFERRED:bool (zombie-id:string from:string to:string)
    @event
    true)

  (defcap ZOMBIE_SOLD:bool (zombie-id:string from:string to:string price:decimal)
    @event
    true)

  ; Capabilities
  (defcap ZOMBIE_OWNER (zombie-id:string owner:string)
    @doc "Capability for zombie ownership"
    (with-read zombies-table zombie-id { "owner" := zombie-owner }
      (enforce (= owner zombie-owner) "You don't own this zombie!")))

  (defcap TRANSFER (zombie-id:string sender:string receiver:string)
    @doc "Capability to transfer a zombie"
    @managed
    (enforce (!= sender receiver) "Cannot transfer to yourself")
    (with-capability (ZOMBIE_OWNER zombie-id sender)
      true))

  (defcap ADMIN ()
    @doc "Admin capability"
    (enforce-guard (read-keyset "zombies-admin")))

  ; Helper Functions
  (defun initialize ()
    @doc "Initialize game state"
    (with-capability (ADMIN)
      (insert game-state-table GAME_STATE_KEY
        { "paused": false
        , "total-zombies": 0
        , "total-battles": 0
        })))

  (defun enforce-not-paused ()
    @doc "Ensure game is not paused"
    (let ((paused (at "paused" (read game-state-table GAME_STATE_KEY))))
      (enforce (not paused) "Game is currently paused")))

  (defun generate-dna:integer (name:string)
    @doc "Generate DNA from name"
    (abs (str-to-int 64 (take 16 (hash name)))))

  ; Core Functions
  (defun create-zombie:string (name:string)
    @doc "Create a new zombie"
    (enforce-not-paused)
    (let* ((owner (at "sender" (chain-data)))
           (dna (generate-dna name))
           (zombie-id (format "zombie_{}" [dna]))
           (zombie-type (at (mod dna 5) ["walker" "runner" "tank" "hunter" "spitter"])))
      (insert zombies-table zombie-id
        { "name": name
        , "dna": dna
        , "type": zombie-type
        , "level": 1
        , "ready-time": (at "block-time" (chain-data))
        , "win-count": 0
        , "loss-count": 0
        , "owner": owner
        })
      ; Update total zombies
      (with-read game-state-table GAME_STATE_KEY { "total-zombies" := total }
        (update game-state-table GAME_STATE_KEY 
          { "total-zombies": (+ total 1) }))
      (format "Zombie {} created! Type: {}" [name zombie-type])))

  (defun transfer-zombie:string (zombie-id:string receiver:string)
    @doc "Transfer a zombie to another user"
    (enforce-not-paused)
    (let ((sender (at "sender" (chain-data))))
      (with-capability (TRANSFER zombie-id sender receiver)
        ; Remove from market if listed
        (if (try false (at "for-sale" (read market-table zombie-id)))
          (update market-table zombie-id { "for-sale": false })
          true)
        ; Transfer ownership
        (update zombies-table zombie-id { "owner": receiver })
        (emit-event (ZOMBIE_TRANSFERRED zombie-id sender receiver))
        (format "Zombie {} transferred from {} to {}" 
          [zombie-id sender receiver]))))

  (defun list-zombie-for-sale:string (zombie-id:string price:decimal)
    @doc "List a zombie for sale on the market"
    (enforce-not-paused)
    (enforce (> price 0.0) "Price must be positive")
    (with-capability (ZOMBIE_OWNER zombie-id (at "sender" (chain-data)))
      (with-read zombies-table zombie-id 
        { "name" := name, "level" := level, "type" := type }
        (write market-table zombie-id
          { "id": zombie-id
          , "name": name
          , "level": level
          , "type": type
          , "price": price
          , "for-sale": true
          }))
      (format "Zombie {} listed for {} KDA" [zombie-id price])))

  (defun buy-zombie:string (zombie-id:string)
    @doc "Buy a zombie from the market"
    (enforce-not-paused)
    (with-read market-table zombie-id 
      { "price" := price
      , "for-sale" := for-sale
      }
      (enforce for-sale "Zombie is not for sale")
      (let ((buyer (at "sender" (chain-data)))
            (seller (at "owner" (read zombies-table zombie-id))))
        (enforce (!= buyer seller) "Cannot buy your own zombie")
        ; In real implementation, transfer KDA here
        ; (coin.transfer buyer seller price)
        (update zombies-table zombie-id { "owner": buyer })
        (update market-table zombie-id { "for-sale": false })
        (emit-event (ZOMBIE_SOLD zombie-id seller buyer price))
        (format "Zombie {} sold to {} for {} KDA" [zombie-id buyer price]))))

  (defun cancel-listing:string (zombie-id:string)
    @doc "Cancel a market listing"
    (with-capability (ZOMBIE_OWNER zombie-id (at "sender" (chain-data)))
      (update market-table zombie-id { "for-sale": false })
      (format "Listing cancelled for zombie {}" [zombie-id])))

  ; View Functions
  (defun get-zombies-for-sale:[object{zombie-listing}] ()
    @doc "Get all zombies currently for sale"
    (select market-table (where "for-sale" (= true))))

  (defun get-zombie-count:integer (owner:string)
    @doc "Count zombies owned by an address"
    (length (select zombies-table (where "owner" (= owner)))))

  (defun get-my-zombies:[object{zombie}] ()
    @doc "Get all zombies owned by caller"
    (select zombies-table (where "owner" (= (at "sender" (chain-data))))))

  (defun get-zombie-details:object (zombie-id:string)
    @doc "Get detailed zombie information"
    (+ (read zombies-table zombie-id)
       { "for-sale": (try false (at "for-sale" (read market-table zombie-id)))
       , "price": (try 0.0 (at "price" (read market-table zombie-id)))
       }))

  ; Admin Functions
  (defun pause-game:string (paused:bool)
    @doc "Admin function to pause/unpause the game"
    (with-capability (ADMIN)
      (update game-state-table GAME_STATE_KEY { "paused": paused })
      (if paused "Game paused!" "Game resumed!")))

  (defun get-game-stats:object ()
    @doc "Get game statistics"
    (read game-state-table GAME_STATE_KEY))
)