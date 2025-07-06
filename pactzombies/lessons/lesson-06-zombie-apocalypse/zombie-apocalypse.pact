(module zombie-apocalypse GOVERNANCE

  ; Governance
  (defcap GOVERNANCE ()
    (compose-capability (ADMIN))
    (compose-capability (OPS)))
  
  (defcap ADMIN () 
    (enforce-guard (read-keyset "zombie-admin")))
  
  (defcap OPS () 
    (enforce-guard (read-keyset "zombie-ops")))

  ; Schemas
  (defschema apocalypse-state
    total-zombies:integer
    total-battles:integer
    total-transfers:integer
    economic-supply:decimal
    game-phase:string)

  (defschema cross-chain-transfer
    transfer-id:string
    zombie-id:string
    from-chain:string
    to-chain:string
    from-owner:string
    to-owner:string
    zombie-data:object
    initiated-at:time
    completed:bool
    spv-proof:string)

  (defschema auction-info
    zombie-id:string
    seller:string
    starting-price:decimal
    current-bid:decimal
    highest-bidder:string
    end-time:time
    active:bool)

  (defschema tournament-entry
    player:string
    zombie-id:string
    entry-time:time
    eliminated:bool
    final-rank:integer)

  (defschema chain-stats
    chain-id:string
    zombie-count:integer
    transfer-in:integer
    transfer-out:integer
    last-update:time)

  ; Tables
  (deftable apocalypse-table:{apocalypse-state})
  (deftable transfers-table:{cross-chain-transfer})
  (deftable auctions-table:{auction-info})
  (deftable tournament-table:{tournament-entry})
  (deftable chain-stats-table:{chain-stats})

  ; Constants
  (defconst APOCALYPSE_KEY "GLOBAL_STATE")
  (defconst MIN_AUCTION_DURATION 3600.0) ; 1 hour
  (defconst AUCTION_FEE_PERCENT 0.025)
  (defconst TRANSFER_FEE 10.0)

  ; Events
  (defcap CROSS_CHAIN_INITIATED:bool 
    (transfer-id:string from:string to:string)
    @event true)

  (defcap AUCTION_CREATED:bool 
    (zombie-id:string starting-price:decimal)
    @event true)

  (defcap TOURNAMENT_STARTED:bool 
    (tournament-id:string prize-pool:decimal)
    @event true)

  ; Capabilities
  (defcap TRANSFER_XCHAIN (zombie-id:string owner:string)
    @doc "Capability for cross-chain transfers"
    (with-read zombies.zombies-table zombie-id { "owner" := zombie-owner }
      (enforce (= owner zombie-owner) "Not zombie owner")))

  (defcap AUCTION_OPERATOR (zombie-id:string seller:string)
    @doc "Capability for auction operations"
    (enforce (= seller (at "sender" (chain-data))) "Not authorized"))

  ; Initialization
  (defun initialize ()
    @doc "Initialize apocalypse state"
    (with-capability (GOVERNANCE)
      (insert apocalypse-table APOCALYPSE_KEY
        { "total-zombies": 0
        , "total-battles": 0
        , "total-transfers": 0
        , "economic-supply": 0.0
        , "game-phase": "genesis"
        })))

  ; Cross-chain Transfer Implementation
  (defpact cross-chain-zombie-transfer 
    (zombie-id:string target-chain:string target-owner:string)
    @doc "Secure cross-chain zombie transfer"
    
    (step ; Lock on source
      (let* ((source-chain (at "chain-id" (chain-data)))
             (current-owner (at "sender" (chain-data)))
             (transfer-id (format "xfer_{}_{}_{}" 
               [zombie-id source-chain (at "block-time" (chain-data))]))
             (zombie-data (zombies.read-zombie zombie-id)))
        
        (enforce (!= source-chain target-chain) 
          "Cannot transfer to same chain")
        
        (with-capability (TRANSFER_XCHAIN zombie-id current-owner)
          ; Lock zombie
          (zombies.update-zombie-status zombie-id "locked-transfer")
          
          ; Charge transfer fee
          (zombie-coin.transfer current-owner "transfer-pool" TRANSFER_FEE)
          
          ; Record transfer
          (insert transfers-table transfer-id
            { "transfer-id": transfer-id
            , "zombie-id": zombie-id
            , "from-chain": source-chain
            , "to-chain": target-chain
            , "from-owner": current-owner
            , "to-owner": target-owner
            , "zombie-data": zombie-data
            , "initiated-at": (at "block-time" (chain-data))
            , "completed": false
            , "spv-proof": ""
            })
          
          ; Update stats
          (update-chain-stats source-chain 0 1)
          
          (emit-event (CROSS_CHAIN_INITIATED transfer-id source-chain target-chain))
          
          (yield { "transfer-id": transfer-id
                 , "zombie-data": zombie-data
                 , "target-chain": target-chain
                 , "target-owner": target-owner
                 }))))
    
    (step ; Burn on source
      (resume { "transfer-id" := tid
              , "zombie-data" := zdata
              }
        (with-read transfers-table tid 
          { "from-chain" := from-chain }
          (enforce (= (at "chain-id" (chain-data)) from-chain) 
            "Must execute on source chain")
          
          ; Burn zombie
          (zombies.burn-zombie (at "id" zdata))
          
          ; Generate proof
          (let ((proof (generate-spv-proof tid)))
            (update transfers-table tid { "spv-proof": proof })
            (yield { "transfer-id": tid
                   , "zombie-data": zdata
                   , "spv-proof": proof
                   })))))
    
    (step ; Mint on target
      (resume { "transfer-id" := tid
              , "zombie-data" := zdata
              , "spv-proof" := proof
              }
        (with-read transfers-table tid 
          { "to-chain" := to-chain
          , "to-owner" := new-owner
          }
          (enforce (= (at "chain-id" (chain-data)) to-chain) 
            "Must execute on target chain")
          
          ; Verify proof
          (enforce (verify-spv-proof proof) "Invalid SPV proof")
          
          ; Mint on target chain
          (let ((new-id (format "{}_{}" [(at "id" zdata) to-chain])))
            (zombies.mint-zombie new-id (+ zdata { "owner": new-owner }))
            
            ; Update transfer record
            (update transfers-table tid { "completed": true })
            
            ; Update stats
            (update-chain-stats to-chain 1 0)
            
            (format "Zombie {} transferred to chain {}" 
              [new-id to-chain]))))))

  ; Auction System
  (defun create-auction 
    (zombie-id:string starting-price:decimal duration:decimal)
    @doc "Create a zombie auction"
    (let ((seller (at "sender" (chain-data))))
      (enforce (>= duration MIN_AUCTION_DURATION) 
        "Auction duration too short")
      (enforce (> starting-price 0.0) "Invalid starting price")
      
      (with-capability (AUCTION_OPERATOR zombie-id seller)
        ; Lock zombie for auction
        (zombies.update-zombie-status zombie-id "auctioned")
        
        (insert auctions-table zombie-id
          { "zombie-id": zombie-id
          , "seller": seller
          , "starting-price": starting-price
          , "current-bid": starting-price
          , "highest-bidder": ""
          , "end-time": (add-time (at "block-time" (chain-data)) duration)
          , "active": true
          })
        
        (emit-event (AUCTION_CREATED zombie-id starting-price))
        (format "Auction created for zombie {}" [zombie-id]))))

  (defun place-bid (zombie-id:string bid-amount:decimal)
    @doc "Place bid on zombie auction"
    (let ((bidder (at "sender" (chain-data))))
      (with-read auctions-table zombie-id 
        { "current-bid" := current
        , "highest-bidder" := prev-bidder
        , "end-time" := end
        , "active" := active
        }
        (enforce active "Auction not active")
        (enforce (< (at "block-time" (chain-data)) end) 
          "Auction ended")
        (enforce (> bid-amount current) 
          "Bid too low")
        
        ; Return previous bid
        (if (!= prev-bidder "")
          (zombie-coin.transfer "auction-escrow" prev-bidder current)
          true)
        
        ; Escrow new bid
        (zombie-coin.transfer bidder "auction-escrow" bid-amount)
        
        ; Update auction
        (update auctions-table zombie-id
          { "current-bid": bid-amount
          , "highest-bidder": bidder
          })
        
        (format "Bid of {} ZMB placed" [bid-amount]))))

  (defun finalize-auction (zombie-id:string)
    @doc "Complete auction and transfer zombie"
    (with-read auctions-table zombie-id 
      { "seller" := seller
      , "highest-bidder" := winner
      , "current-bid" := final-price
      , "end-time" := end
      , "active" := active
      }
      (enforce active "Auction not active")
      (enforce (>= (at "block-time" (chain-data)) end) 
        "Auction not ended")
      
      (if (!= winner "")
        (let ((fee (* final-price AUCTION_FEE_PERCENT))
              (seller-amount (- final-price fee)))
          ; Transfer payment
          (zombie-coin.transfer "auction-escrow" seller seller-amount)
          (zombie-coin.transfer "auction-escrow" "fee-collector" fee)
          
          ; Transfer zombie
          (zombies.transfer-zombie zombie-id winner)
          (zombies.update-zombie-status zombie-id "active")
          
          ; Close auction
          (update auctions-table zombie-id { "active": false })
          
          (format "Auction won by {} for {} ZMB" [winner final-price]))
        (let ()
          ; No bids - return zombie
          (zombies.update-zombie-status zombie-id "active")
          (update auctions-table zombie-id { "active": false })
          "Auction ended with no bids"))))

  ; Helper Functions
  (defun generate-spv-proof:string (transfer-id:string)
    @doc "Generate SPV proof for cross-chain transfer"
    ; Simplified - real implementation would use merkle proofs
    (hash (+ transfer-id (format "{}" [(at "block-height" (chain-data))]))))

  (defun verify-spv-proof:bool (proof:string)
    @doc "Verify SPV proof"
    ; Simplified - real implementation would verify merkle proof
    (!= proof ""))

  (defun update-chain-stats (chain:string in:integer out:integer)
    @doc "Update cross-chain statistics"
    (with-default-read chain-stats-table chain
      { "zombie-count": 0, "transfer-in": 0, "transfer-out": 0 }
      { "zombie-count" := zcount
      , "transfer-in" := tin
      , "transfer-out" := tout
      }
      (write chain-stats-table chain
        { "chain-id": chain
        , "zombie-count": (+ zcount (- in out))
        , "transfer-in": (+ tin in)
        , "transfer-out": (+ tout out)
        , "last-update": (at "block-time" (chain-data))
        })))

  ; View Functions
  (defun get-active-auctions ()
    @doc "Get all active auctions"
    (select auctions-table (where "active" (= true))))

  (defun get-transfer-status (transfer-id:string)
    @doc "Check cross-chain transfer status"
    (read transfers-table transfer-id))

  (defun get-chain-statistics ()
    @doc "Get cross-chain statistics"
    (select chain-stats-table (constantly true)))

  (defun get-apocalypse-stats ()
    @doc "Get global game statistics"
    (+ (read apocalypse-table APOCALYPSE_KEY)
       { "total-chains": (length (keys chain-stats-table))
       , "active-transfers": (length (select transfers-table 
           (where "completed" (= false))))
       , "active-auctions": (length (get-active-auctions))
       }))
)