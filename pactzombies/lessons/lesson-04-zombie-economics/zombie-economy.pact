(module zombie-economy GOV

  (defcap GOV () 
    (enforce-guard (read-keyset "zombie-admin")))

  ; Schemas
  (defschema reward-config
    battle-win:decimal
    battle-loss:decimal
    daily-bonus:decimal
    breeding-fee:decimal
    market-fee-percent:decimal
    staking-base-rate:decimal)

  (defschema player-economy
    account:string
    total-earned:decimal
    total-spent:decimal
    last-daily-claim:time
    battle-wins:integer
    battle-losses:integer)

  (defschema staking-pool
    zombie-id:string
    owner:string
    staked-at:time
    last-claim:time
    reward-rate:decimal
    total-earned:decimal
    locked:bool)

  (defschema zombie-listing-v2
    zombie-id:string
    seller:string
    price:decimal
    currency:string ; "ZMB" or "KDA"
    listed-at:time
    active:bool)

  ; Tables
  (deftable rewards-table:{reward-config})
  (deftable player-economy-table:{player-economy})
  (deftable staking-table:{staking-pool})
  (deftable market-v2-table:{zombie-listing-v2})

  ; Constants
  (defconst REWARDS_KEY "REWARDS")
  (defconst SECONDS_PER_HOUR 3600.0)
  (defconst SECONDS_PER_DAY 86400.0)

  ; Events
  (defcap REWARD_EARNED:bool (account:string amount:decimal reason:string)
    @event true)

  (defcap ZOMBIE_STAKED:bool (zombie-id:string owner:string rate:decimal)
    @event true)

  (defcap MARKET_SALE:bool (zombie-id:string seller:string buyer:string price:decimal)
    @event true)

  ; Capabilities
  (defcap REWARD_BATTLE (winner:string loser:string)
    @doc "Capability to reward battle participants"
    true)

  (defcap STAKE (zombie-id:string owner:string)
    @doc "Capability to stake a zombie"
    (enforce (= owner (at "sender" (chain-data))) "Not the owner")
    (with-read zombies.zombies-table zombie-id { "owner" := zombie-owner }
      (enforce (= owner zombie-owner) "Not your zombie!")))

  (defcap CLAIM_REWARDS (zombie-id:string owner:string)
    @doc "Capability to claim staking rewards"
    (with-read staking-table zombie-id { "owner" := staked-owner }
      (enforce (= owner staked-owner) "Not your staked zombie!")))

  ; Initialization
  (defun init-rewards ()
    @doc "Initialize reward configuration"
    (with-capability (GOV)
      (insert rewards-table REWARDS_KEY
        { "battle-win": 10.0
        , "battle-loss": 2.0
        , "daily-bonus": 50.0
        , "breeding-fee": 100.0
        , "market-fee-percent": 0.025
        , "staking-base-rate": 1.0
        })
      "Rewards initialized"))

  ; Player Economy Management
  (defun init-player-economy (account:string)
    @doc "Initialize player economy record"
    (insert player-economy-table account
      { "account": account
      , "total-earned": 0.0
      , "total-spent": 0.0
      , "last-daily-claim": (parse-time "%F" "1970-01-01")
      , "battle-wins": 0
      , "battle-losses": 0
      }))

  (defun ensure-player-economy (account:string)
    @doc "Ensure player has economy record"
    (with-default-read player-economy-table account
      { "account": account
      , "total-earned": 0.0
      , "total-spent": 0.0
      , "last-daily-claim": (parse-time "%F" "1970-01-01")
      , "battle-wins": 0
      , "battle-losses": 0
      }
      { "account" := existing }
      (if (= existing "")
        (init-player-economy account)
        true)))

  ; Battle Rewards System
  (defun reward-battle (winner:string loser:string)
    @doc "Distribute rewards after battle"
    (with-capability (REWARD_BATTLE winner loser)
      (ensure-player-economy winner)
      (ensure-player-economy loser)
      
      (with-read rewards-table REWARDS_KEY 
        { "battle-win" := win-reward
        , "battle-loss" := loss-reward
        }
        ; Transfer rewards
        (zombie-coin.transfer "battle-pool" winner win-reward)
        (zombie-coin.transfer "battle-pool" loser loss-reward)
        
        ; Update player stats
        (with-read player-economy-table winner 
          { "total-earned" := winner-earned
          , "battle-wins" := wins
          }
          (update player-economy-table winner
            { "total-earned": (+ winner-earned win-reward)
            , "battle-wins": (+ wins 1)
            }))
        
        (with-read player-economy-table loser 
          { "total-earned" := loser-earned
          , "battle-losses" := losses
          }
          (update player-economy-table loser
            { "total-earned": (+ loser-earned loss-reward)
            , "battle-losses": (+ losses 1)
            }))
        
        ; Emit events
        (emit-event (REWARD_EARNED winner win-reward "battle-win"))
        (emit-event (REWARD_EARNED loser loss-reward "battle-loss"))
        
        (format "Battle rewards: {} ZMB to {}, {} ZMB to {}" 
          [win-reward winner loss-reward loser]))))

  ; Daily Rewards
  (defun claim-daily-bonus ()
    @doc "Claim daily ZMB bonus"
    (let ((account (at "sender" (chain-data)))
          (current-time (at "block-time" (chain-data))))
      
      (ensure-player-economy account)
      
      (with-read player-economy-table account
        { "last-daily-claim" := last-claim
        , "total-earned" := total-earned
        }
        (enforce (>= (diff-time current-time last-claim) SECONDS_PER_DAY)
          "Daily bonus already claimed")
        
        (with-read rewards-table REWARDS_KEY { "daily-bonus" := bonus }
          (zombie-coin.transfer "rewards-pool" account bonus)
          
          (update player-economy-table account
            { "last-daily-claim": current-time
            , "total-earned": (+ total-earned bonus)
            })
          
          (emit-event (REWARD_EARNED account bonus "daily-bonus"))
          (format "Daily bonus of {} ZMB claimed!" [bonus])))))

  ; Staking System
  (defun calculate-staking-rate:decimal (zombie:object)
    @doc "Calculate staking rate based on zombie attributes"
    (let ((base-rate (at "staking-base-rate" (read rewards-table REWARDS_KEY)))
          (level (at "level" zombie))
          (type (at "type" zombie))
          (wins (at "win-count" zombie)))
      (* base-rate
         (+ 1.0 
            (* level 0.1)           ; 10% per level
            (* wins 0.05)           ; 5% per win
            (cond
              ((= type "legendary") 1.0)   ; 100% bonus
              ((= type "rare") 0.5)        ; 50% bonus
              ((= type "epic") 0.25)       ; 25% bonus
              0.0)))))

  (defun stake-zombie (zombie-id:string)
    @doc "Stake a zombie to earn passive ZMB"
    (let ((owner (at "sender" (chain-data))))
      (with-capability (STAKE zombie-id owner)
        (let* ((zombie (zombies.read-zombie zombie-id))
               (reward-rate (calculate-staking-rate zombie))
               (current-time (at "block-time" (chain-data))))
          
          (insert staking-table zombie-id
            { "zombie-id": zombie-id
            , "owner": owner
            , "staked-at": current-time
            , "last-claim": current-time
            , "reward-rate": reward-rate
            , "total-earned": 0.0
            , "locked": true
            })
          
          ; Lock zombie from battles/transfers
          (zombies.lock-zombie zombie-id)
          
          (emit-event (ZOMBIE_STAKED zombie-id owner reward-rate))
          (format "Zombie staked! Earning {} ZMB per hour" [reward-rate])))))

  (defun calculate-rewards:decimal (zombie-id:string)
    @doc "Calculate pending staking rewards"
    (with-read staking-table zombie-id 
      { "last-claim" := last-claim
      , "reward-rate" := rate
      }
      (let* ((current-time (at "block-time" (chain-data)))
             (hours-elapsed (/ (diff-time current-time last-claim) SECONDS_PER_HOUR)))
        (* hours-elapsed rate))))

  (defun claim-staking-rewards (zombie-id:string)
    @doc "Claim accumulated staking rewards"
    (let ((owner (at "sender" (chain-data))))
      (with-capability (CLAIM_REWARDS zombie-id owner)
        (let ((rewards (calculate-rewards zombie-id)))
          (enforce (> rewards 0.0) "No rewards to claim")
          
          (zombie-coin.transfer "staking-pool" owner rewards)
          
          (with-read staking-table zombie-id { "total-earned" := previous-earned }
            (update staking-table zombie-id 
              { "last-claim": (at "block-time" (chain-data))
              , "total-earned": (+ previous-earned rewards)
              }))
          
          (emit-event (REWARD_EARNED owner rewards "staking"))
          (format "Claimed {} ZMB in staking rewards!" [rewards])))))

  (defun unstake-zombie (zombie-id:string)
    @doc "Unstake a zombie"
    (let ((owner (at "sender" (chain-data))))
      (with-capability (CLAIM_REWARDS zombie-id owner)
        ; Claim final rewards
        (claim-staking-rewards zombie-id)
        
        ; Unlock zombie
        (zombies.unlock-zombie zombie-id)
        
        ; Remove from staking
        (update staking-table zombie-id { "locked": false })
        
        "Zombie unstaked successfully!")))

  ; Enhanced Marketplace
  (defun list-zombie-for-zmb (zombie-id:string price:decimal)
    @doc "List zombie for sale in ZMB tokens"
    (let ((seller (at "sender" (chain-data))))
      (enforce (> price 0.0) "Price must be positive")
      (zombies.enforce-zombie-owner zombie-id seller)
      
      (insert market-v2-table zombie-id
        { "zombie-id": zombie-id
        , "seller": seller
        , "price": price
        , "currency": "ZMB"
        , "listed-at": (at "block-time" (chain-data))
        , "active": true
        })
      
      (format "Zombie listed for {} ZMB" [price])))

  (defun buy-zombie-with-zmb (zombie-id:string)
    @doc "Buy zombie using ZMB tokens"
    (with-read market-v2-table zombie-id 
      { "seller" := seller
      , "price" := price
      , "currency" := currency
      , "active" := active
      }
      (enforce active "Listing not active")
      (enforce (= currency "ZMB") "Listing not in ZMB")
      
      (let* ((buyer (at "sender" (chain-data)))
             (market-fee (* price (at "market-fee-percent" 
               (read rewards-table REWARDS_KEY))))
             (seller-amount (- price market-fee)))
        
        (enforce (!= buyer seller) "Cannot buy your own zombie")
        
        ; Transfer tokens
        (zombie-coin.transfer buyer seller seller-amount)
        (zombie-coin.transfer buyer "fee-collector" market-fee)
        
        ; Transfer zombie
        (zombies.transfer-zombie zombie-id buyer)
        
        ; Update listing
        (update market-v2-table zombie-id { "active": false })
        
        ; Update economy stats
        (with-read player-economy-table seller { "total-earned" := earned }
          (update player-economy-table seller 
            { "total-earned": (+ earned seller-amount) }))
        
        (with-read player-economy-table buyer { "total-spent" := spent }
          (update player-economy-table buyer 
            { "total-spent": (+ spent price) }))
        
        (emit-event (MARKET_SALE zombie-id seller buyer price))
        (format "Zombie purchased for {} ZMB (fee: {} ZMB)" 
          [price market-fee]))))

  ; View Functions
  (defun get-player-stats (account:string)
    @doc "Get player economy statistics"
    (+ (read player-economy-table account)
       { "current-balance": (zombie-coin.get-balance account)
       , "zombies-owned": (zombies.get-zombie-count account)
       , "zombies-staked": (length (select staking-table 
           (where "owner" (= account))))
       }))

  (defun get-staking-info (zombie-id:string)
    @doc "Get staking information for a zombie"
    (+ (read staking-table zombie-id)
       { "pending-rewards": (calculate-rewards zombie-id) }))

  (defun get-market-listings ()
    @doc "Get all active market listings"
    (select market-v2-table (where "active" (= true))))

  (defun get-economy-stats ()
    @doc "Get overall economy statistics"
    (let ((total-staked (length (select staking-table (where "locked" (= true)))))
          (total-listings (length (get-market-listings)))
          (rewards-config (read rewards-table REWARDS_KEY)))
      { "total-zombies-staked": total-staked
      , "active-market-listings": total-listings
      , "current-rewards": rewards-config
      , "zmb-total-supply": (zombie-coin.total-supply)
      }))
)