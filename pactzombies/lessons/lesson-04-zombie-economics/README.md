# Lesson 4: Zombie Economics - Creating a Token Economy 💰

Welcome to the world of zombie economics! Your zombies need resources to thrive. In this lesson, you'll learn:

- Creating fungible tokens (Zombie Coins)
- Implementing the Kadena token standard
- Building a marketplace economy
- Reward systems and staking
- DeFi basics in Pact

## Chapter 1: Understanding Fungible Tokens

Fungible tokens are interchangeable units of value, like currency. Let's create Zombie Coins (ZMB)!

### The Fungible Token Interface

Kadena has a standard interface for fungible tokens:

```pact
(interface fungible-v2

  (defschema account-details
    @doc "Schema for account details"
    account:string
    balance:decimal
    guard:guard)

  (defun transfer:string (sender:string receiver:string amount:decimal))
  (defun get-balance:decimal (account:string))
  (defun details:object{account-details} (account:string))
  ; ... more functions
)
```

### Exercise 4.1

Create `zombie-coin.pact` and start implementing a basic token that follows the fungible-v2 interface.

## Chapter 2: Creating Zombie Coin (ZMB)

Let's build our zombie economy token:

```pact
(module zombie-coin GOV

  (implements fungible-v2)
  
  (defcap GOV () 
    (enforce-guard (read-keyset "zombie-coin-admin")))

  ; Token Schema
  (defschema account
    balance:decimal
    guard:guard)

  (deftable accounts-table:{account})

  ; Constants
  (defconst COIN_ID "zombie-coin")
  (defconst INITIAL_SUPPLY 1000000.0)
  (defconst DECIMALS 12)

  ; Capabilities
  (defcap DEBIT (sender:string)
    @doc "Capability for debiting an account"
    (enforce-guard (at "guard" (read accounts-table sender))))

  (defcap CREDIT (receiver:string) 
    @doc "Capability for crediting an account"
    true)

  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
    @doc "Capability for transferring tokens"
    @managed amount TRANSFER-mgr
    (enforce (!= sender receiver) "Sender and receiver must differ")
    (enforce (> amount 0.0) "Amount must be positive")
    (enforce-unit amount)
    (with-capability (DEBIT sender)
      (debit sender amount))
    (with-capability (CREDIT receiver)
      (credit receiver amount)))

  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    @doc "Manager for TRANSFER capability"
    (let ((balance (- managed requested)))
      (enforce (>= balance 0.0) "Transfer quantity exhausted")
      balance))

  ; Core Functions
  (defun create-account:string (account:string guard:guard)
    @doc "Create a new ZMB account"
    (insert accounts-table account
      { "balance": 0.0
      , "guard": guard
      })
    (format "Account {} created" [account]))

  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer ZMB between accounts"
    (with-capability (TRANSFER sender receiver amount)
      (format "Transferred {} ZMB from {} to {}" 
        [amount sender receiver])))

  (defun debit:decimal (account:string amount:decimal)
    @doc "Debit ZMB from an account"
    (require-capability (DEBIT account))
    (with-read accounts-table account { "balance" := balance }
      (enforce (>= balance amount) "Insufficient funds")
      (update accounts-table account 
        { "balance": (- balance amount) })
      (- balance amount)))

  (defun credit:decimal (account:string amount:decimal)
    @doc "Credit ZMB to an account"
    (require-capability (CREDIT account))
    (with-read accounts-table account { "balance" := balance }
      (update accounts-table account 
        { "balance": (+ balance amount) })
      (+ balance amount)))

  (defun get-balance:decimal (account:string)
    @doc "Get account balance"
    (at "balance" (read accounts-table account)))

  (defun enforce-unit:bool (amount:decimal)
    @doc "Enforce decimal precision"
    (enforce (= (floor amount DECIMALS) amount) 
      "Amount precision must match token decimals"))
)
```

### Exercise 4.2

Implement the complete fungible-v2 interface for ZMB token.

## Chapter 3: Integrating Tokens with Zombies

Now let's connect our token to the zombie game:

```pact
(module zombie-economy GOV

  (defcap GOV () (enforce-guard (read-keyset "zombie-admin")))

  ; Reward Configuration
  (defschema reward-config
    battle-win:decimal
    battle-loss:decimal
    daily-bonus:decimal
    breeding-fee:decimal
    market-fee-percent:decimal)

  (deftable rewards-table:{reward-config})
  (defconst REWARDS_KEY "REWARDS")

  ; Player Economy Schema
  (defschema player-economy
    account:string
    total-earned:decimal
    total-spent:decimal
    last-daily-claim:time
    staked-zombies:[string])

  (deftable player-economy-table:{player-economy})

  ; Initialize rewards
  (defun init-rewards ()
    (insert rewards-table REWARDS_KEY
      { "battle-win": 10.0
      , "battle-loss": 2.0
      , "daily-bonus": 50.0
      , "breeding-fee": 100.0
      , "market-fee-percent": 0.025
      }))

  ; Battle Rewards
  (defun reward-battle (winner:string loser:string)
    @doc "Distribute rewards after battle"
    (with-read rewards-table REWARDS_KEY 
      { "battle-win" := win-reward
      , "battle-loss" := loss-reward
      }
      ; Credit winner
      (zombie-coin.transfer "battle-pool" winner win-reward)
      ; Give consolation to loser
      (zombie-coin.transfer "battle-pool" loser loss-reward)
      (format "Battle rewards distributed: {} to winner, {} to loser" 
        [win-reward loss-reward])))

  ; Daily Rewards
  (defun claim-daily-bonus (account:string)
    @doc "Claim daily ZMB bonus"
    (let ((current-time (at "block-time" (chain-data))))
      (with-default-read player-economy-table account
        { "last-daily-claim": (parse-time "%F" "1970-01-01") }
        { "last-daily-claim" := last-claim }
        (enforce (>= (diff-time current-time last-claim) 86400.0)
          "Daily bonus already claimed")
        (with-read rewards-table REWARDS_KEY { "daily-bonus" := bonus }
          (zombie-coin.transfer "rewards-pool" account bonus)
          (write player-economy-table account
            { "account": account
            , "last-daily-claim": current-time
            })
          (format "Daily bonus of {} ZMB claimed!" [bonus])))))

  ; Marketplace with Token Integration
  (defun buy-zombie-with-tokens (zombie-id:string)
    @doc "Buy zombie using ZMB tokens"
    (with-read market-table zombie-id 
      { "price" := price
      , "for-sale" := for-sale
      }
      (enforce for-sale "Zombie not for sale")
      (let ((buyer (at "sender" (chain-data)))
            (seller (at "owner" (read zombies-table zombie-id)))
            (market-fee (* price (at "market-fee-percent" 
              (read rewards-table REWARDS_KEY))))
            (seller-amount (- price market-fee)))
        ; Transfer tokens
        (zombie-coin.transfer buyer seller seller-amount)
        (zombie-coin.transfer buyer "fee-collector" market-fee)
        ; Transfer zombie
        (update zombies-table zombie-id { "owner": buyer })
        (update market-table zombie-id { "for-sale": false })
        (format "Zombie purchased for {} ZMB (fee: {} ZMB)" 
          [price market-fee]))))
)
```

### Exercise 4.3

Add token rewards to your zombie battle system.

## Chapter 4: Staking Zombies for Passive Income

Let's create a staking system where zombies can earn passive ZMB:

```pact
(defschema staking-pool
  zombie-id:string
  owner:string
  staked-at:time
  reward-rate:decimal
  total-earned:decimal)

(deftable staking-table:{staking-pool})

(defcap STAKE (zombie-id:string owner:string)
  @doc "Capability to stake a zombie"
  (with-read zombies-table zombie-id { "owner" := zombie-owner }
    (enforce (= owner zombie-owner) "Not your zombie!")))

(defun stake-zombie (zombie-id:string)
  @doc "Stake a zombie to earn passive ZMB"
  (let ((owner (at "sender" (chain-data))))
    (with-capability (STAKE zombie-id owner)
      (with-read zombies-table zombie-id 
        { "level" := level
        , "type" := type
        }
        ; Calculate reward rate based on zombie stats
        (let ((base-rate 1.0)
              (level-bonus (* level 0.5))
              (type-bonus (cond 
                ((= type "legendary") 2.0)
                ((= type "rare") 1.5)
                1.0))
              (reward-rate (* base-rate level-bonus type-bonus)))
          (insert staking-table zombie-id
            { "zombie-id": zombie-id
            , "owner": owner
            , "staked-at": (at "block-time" (chain-data))
            , "reward-rate": reward-rate
            , "total-earned": 0.0
            })
          (format "Zombie staked! Earning {} ZMB per hour" [reward-rate])))))

(defun claim-staking-rewards (zombie-id:string)
  @doc "Claim accumulated staking rewards"
  (with-read staking-table zombie-id 
    { "owner" := owner
    , "staked-at" := staked-at
    , "reward-rate" := rate
    , "total-earned" := previous-earned
    }
    (enforce (= owner (at "sender" (chain-data))) "Not your staked zombie!")
    (let* ((current-time (at "block-time" (chain-data)))
           (hours-staked (/ (diff-time current-time staked-at) 3600.0))
           (rewards (* hours-staked rate)))
      (zombie-coin.transfer "staking-pool" owner rewards)
      (update staking-table zombie-id 
        { "staked-at": current-time
        , "total-earned": (+ previous-earned rewards)
        })
      (format "Claimed {} ZMB in staking rewards!" [rewards]))))

(defun unstake-zombie (zombie-id:string)
  @doc "Unstake a zombie"
  (with-read staking-table zombie-id { "owner" := owner }
    (enforce (= owner (at "sender" (chain-data))) "Not your staked zombie!")
    ; Claim final rewards
    (claim-staking-rewards zombie-id)
    ; Remove from staking
    (drop-table staking-table zombie-id)
    "Zombie unstaked successfully!"))
```

### Exercise 4.4

Implement the complete staking system with proper reward calculations.

## Chapter 5: Advanced DeFi Features

Let's add more advanced economic features:

```pact
; Liquidity Pools for Zombie Trading
(defschema liquidity-pool
  zmb-balance:decimal
  kda-balance:decimal
  lp-tokens:decimal
  fee-percent:decimal)

(deftable pools-table:{liquidity-pool})

; Zombie Loans
(defschema zombie-loan
  zombie-id:string
  borrower:string
  lender:string
  collateral-amount:decimal
  loan-amount:decimal
  interest-rate:decimal
  due-date:time
  repaid:bool)

(deftable loans-table:{zombie-loan})

(defun create-zombie-loan (zombie-id:string loan-amount:decimal days:integer)
  @doc "Lend your zombie for ZMB tokens"
  (let ((lender (at "sender" (chain-data)))
        (interest-rate 0.05) ; 5% interest
        (collateral (* loan-amount 1.5))) ; 150% collateralization
    (with-capability (ZOMBIE_OWNER zombie-id lender)
      ; Borrower deposits collateral
      (zombie-coin.transfer lender "loan-escrow" collateral)
      ; Lender receives loan amount
      (zombie-coin.transfer "loan-pool" lender loan-amount)
      ; Create loan record
      (let ((loan-id (format "loan_{}_{}" [zombie-id (at "block-time" (chain-data))]))
            (due-date (add-time (at "block-time" (chain-data)) 
              (* days 86400.0))))
        (insert loans-table loan-id
          { "zombie-id": zombie-id
          , "borrower": lender ; They borrow against their zombie
          , "lender": "loan-pool"
          , "collateral-amount": collateral
          , "loan-amount": loan-amount
          , "interest-rate": interest-rate
          , "due-date": due-date
          , "repaid": false
          })
        (format "Loan created: {} ZMB for {} days" [loan-amount days])))))

; Zombie NFT Fractionalization
(defschema fractional-zombie
  zombie-id:string
  total-shares:integer
  available-shares:integer
  price-per-share:decimal)

(defun fractionalize-zombie (zombie-id:string shares:integer price-per-share:decimal)
  @doc "Split zombie ownership into tradeable shares"
  ; Implementation here
)
```

### Exercise 4.5

Design and implement a zombie rental system where players can rent powerful zombies for battles.

## Chapter 6: Challenge - Complete DeFi Ecosystem

Build these advanced features:

1. **Automated Market Maker (AMM)** for ZMB/KDA trading
2. **Yield Farming** with zombie-themed pools
3. **Governance Token** for game decisions
4. **Insurance Pool** for protecting rare zombies
5. **Prediction Markets** for battle outcomes

## Quiz

1. What's the difference between fungible and non-fungible tokens?
2. How do capabilities ensure secure token transfers?
3. What's the purpose of the TRANSFER-mgr function?
4. How can staking incentivize long-term gameplay?
5. What are the risks in DeFi lending?

## Complete Code

See `zombie-economy-complete.pact` for the full implementation!

## Summary

Amazing work! You've built a complete token economy:
- ✅ Fungible token implementation
- ✅ Reward and incentive systems
- ✅ Marketplace with fees
- ✅ Staking mechanisms
- ✅ DeFi primitives
- ✅ Advanced economic features

Next lesson: Multi-step transactions with Pacts!