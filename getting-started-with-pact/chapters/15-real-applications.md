# Chapter 15: Building Real-World Applications

## Introduction

This chapter demonstrates how to build complete, production-ready applications using Pact. We'll explore real-world patterns, architecture decisions, and implementation details through comprehensive examples including a DeFi lending protocol, NFT marketplace, and governance system.

## Application Architecture Patterns

### Modular Architecture

Real-world Pact applications benefit from modular architecture that separates concerns:

```
defi-protocol/
├── interfaces/
│   ├── fungible-v2.pact          # Token standard interface
│   ├── lending-v1.pact           # Lending protocol interface
│   └── oracle-v1.pact            # Price oracle interface
├── core/
│   ├── token.pact                # Protocol token implementation
│   ├── lending-pool.pact         # Core lending logic
│   └── liquidation.pact          # Liquidation engine
├── periphery/
│   ├── router.pact               # User-facing router
│   ├── price-oracle.pact         # Price feed aggregator
│   └── governance.pact           # Protocol governance
├── libraries/
│   ├── math.pact                 # Mathematical utilities
│   ├── security.pact             # Security utilities
│   └── events.pact               # Event management
└── deployment/
    ├── setup.repl                # Deployment scripts
    └── initial-data.json         # Initial configuration
```

## Case Study 1: DeFi Lending Protocol

### Protocol Overview

Let's build a comprehensive lending protocol with the following features:
- Multiple asset pools
- Variable interest rates
- Collateralized borrowing
- Liquidation mechanism
- Governance token rewards

### Core Interfaces

```pact
;; interfaces/lending-v1.pact
(interface lending-v1
  @doc "Lending protocol interface"
  
  ;; Pool management
  (defun create-pool:string (asset:string reserve-factor:decimal))
  (defun supply:string (user:string asset:string amount:decimal))
  (defun withdraw:string (user:string asset:string amount:decimal))
  (defun borrow:string (user:string asset:string amount:decimal))
  (defun repay:string (user:string asset:string amount:decimal))
  
  ;; Interest rate model
  (defun calculate-supply-rate:decimal (asset:string))
  (defun calculate-borrow-rate:decimal (asset:string))
  (defun update-interest-rates:string (asset:string))
  
  ;; Liquidation
  (defun liquidate:string (borrower:string collateral-asset:string debt-asset:string amount:decimal))
  (defun get-account-health:decimal (user:string))
  
  ;; Oracle integration
  (defun update-asset-price:string (asset:string price:decimal))
  (defun get-asset-price:decimal (asset:string))
  
  ;; Schemas
  (defschema pool-info
    asset:string
    total-supplied:decimal
    total-borrowed:decimal
    supply-rate:decimal
    borrow-rate:decimal
    utilization-rate:decimal
    reserve-factor:decimal
    liquidation-threshold:decimal
    last-update:time)
  
  (defschema user-position
    user:string
    supplied:[object]      ;; [{ asset: string, amount: decimal, index: decimal }]
    borrowed:[object]      ;; [{ asset: string, amount: decimal, index: decimal }]
    last-update:time)
)
```

### Core Lending Implementation

```pact
;; core/lending-pool.pact
(module lending-pool GOVERNANCE
  @doc "Core lending pool implementation"
  
  (implements lending-v1)
  (use math [safe-add safe-subtract safe-multiply safe-divide])
  (use security [validate-user validate-amount])
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'protocol-admin))
  
  ;; Tables
  (deftable pools:{pool-info})
  (deftable user-positions:{user-position})
  (deftable asset-prices:{asset-price})
  
  ;; Constants
  (defconst SECONDS_PER_YEAR:integer 31536000)
  (defconst MIN_HEALTH_FACTOR:decimal 1.0)
  (defconst LIQUIDATION_BONUS:decimal 0.05)
  
  ;; Interest rate model
  (defun calculate-utilization:decimal (total-supplied:decimal total-borrowed:decimal)
    @doc "Calculate pool utilization rate"
    (if (= total-supplied 0.0)
        0.0
        (safe-divide total-borrowed total-supplied)))
  
  (defun calculate-borrow-rate:decimal (asset:string)
    @doc "Calculate borrowing interest rate"
    (with-read pools asset { 
      "total-supplied" := supplied,
      "total-borrowed" := borrowed 
    }
      (let ((utilization (calculate-utilization supplied borrowed))
            (base-rate 0.02)      ;; 2% base rate
            (slope1 0.10)         ;; 10% slope for optimal utilization
            (slope2 0.50)         ;; 50% slope above optimal
            (optimal-util 0.80))  ;; 80% optimal utilization
        
        (if (<= utilization optimal-util)
            (safe-add base-rate (safe-multiply utilization slope1))
            (safe-add base-rate
                     (safe-add (safe-multiply optimal-util slope1)
                              (safe-multiply (safe-subtract utilization optimal-util) slope2)))))))
  
  (defun calculate-supply-rate:decimal (asset:string)
    @doc "Calculate supply interest rate"
    (with-read pools asset { 
      "total-supplied" := supplied,
      "total-borrowed" := borrowed,
      "reserve-factor" := reserve-factor
    }
      (let ((borrow-rate (calculate-borrow-rate asset))
            (utilization (calculate-utilization supplied borrowed)))
        (safe-multiply borrow-rate 
                      (safe-multiply utilization (safe-subtract 1.0 reserve-factor))))))
  
  ;; Pool management
  (defun create-pool:string (asset:string reserve-factor:decimal)
    @doc "Create new lending pool"
    (with-capability (GOVERNANCE)
      (insert pools asset {
        "asset": asset,
        "total-supplied": 0.0,
        "total-borrowed": 0.0,
        "supply-rate": 0.0,
        "borrow-rate": 0.0,
        "utilization-rate": 0.0,
        "reserve-factor": reserve-factor,
        "liquidation-threshold": 0.75,
        "last-update": (at 'block-time (chain-data))
      }))
    (format "Pool created for asset {}" [asset]))
  
  ;; Supply functionality
  (defun supply:string (user:string asset:string amount:decimal)
    @doc "Supply assets to lending pool"
    (validate-user user)
    (validate-amount amount)
    
    ;; Update interest rates before supply
    (update-interest-rates asset)
    
    ;; Update pool state
    (with-read pools asset { "total-supplied" := supplied }
      (update pools asset { 
        "total-supplied": (safe-add supplied amount),
        "last-update": (at 'block-time (chain-data))
      }))
    
    ;; Update user position
    (with-default-read user-positions user
      { "supplied": [], "borrowed": [], "last-update": (at 'block-time (chain-data)) }
      { "supplied" := user-supplied }
      
      (let ((updated-supplied (update-user-supplied user-supplied asset amount)))
        (write user-positions user {
          "user": user,
          "supplied": updated-supplied,
          "borrowed": user-supplied, ;; Keep existing borrowed
          "last-update": (at 'block-time (chain-data))
        })))
    
    ;; Transfer tokens to pool
    (let ((token-contract (get-asset-contract asset)))
      (token-contract::transfer user (get-pool-account asset) amount))
    
    (format "Supplied {} {} to pool" [amount asset]))
  
  ;; Borrow functionality
  (defun borrow:string (user:string asset:string amount:decimal)
    @doc "Borrow assets from lending pool"
    (validate-user user)
    (validate-amount amount)
    
    ;; Update interest rates
    (update-interest-rates asset)
    
    ;; Check collateral and health factor
    (let ((health-factor (get-account-health user)))
      (enforce (> health-factor MIN_HEALTH_FACTOR) "Insufficient collateral"))
    
    ;; Check pool liquidity
    (with-read pools asset { 
      "total-supplied" := supplied,
      "total-borrowed" := borrowed 
    }
      (let ((available-liquidity (safe-subtract supplied borrowed)))
        (enforce (>= available-liquidity amount) "Insufficient pool liquidity")
        
        ;; Update pool state
        (update pools asset {
          "total-borrowed": (safe-add borrowed amount),
          "last-update": (at 'block-time (chain-data))
        })))
    
    ;; Update user position
    (with-default-read user-positions user
      { "supplied": [], "borrowed": [], "last-update": (at 'block-time (chain-data)) }
      { "borrowed" := user-borrowed }
      
      (let ((updated-borrowed (update-user-borrowed user-borrowed asset amount)))
        (update user-positions user {
          "borrowed": updated-borrowed,
          "last-update": (at 'block-time (chain-data))
        })))
    
    ;; Transfer tokens to user
    (let ((token-contract (get-asset-contract asset)))
      (token-contract::transfer (get-pool-account asset) user amount))
    
    (format "Borrowed {} {} from pool" [amount asset]))
  
  ;; Health factor calculation
  (defun get-account-health:decimal (user:string)
    @doc "Calculate user's account health factor"
    (with-default-read user-positions user
      { "supplied": [], "borrowed": [] }
      { "supplied" := supplied, "borrowed" := borrowed }
      
      (let ((total-collateral-value (calculate-collateral-value supplied))
            (total-debt-value (calculate-debt-value borrowed)))
        
        (if (= total-debt-value 0.0)
            999.0  ;; No debt = excellent health
            (safe-divide total-collateral-value total-debt-value)))))
  
  (defun calculate-collateral-value:[decimal] (supplied:[object])
    @doc "Calculate total collateral value"
    (fold (lambda (acc position)
            (bind position { "asset" := asset, "amount" := amount }
              (let ((price (get-asset-price asset))
                    (threshold (get-liquidation-threshold asset)))
                (safe-add acc (safe-multiply (safe-multiply amount price) threshold)))))
          0.0
          supplied))
  
  (defun calculate-debt-value:[decimal] (borrowed:[object])
    @doc "Calculate total debt value"
    (fold (lambda (acc position)
            (bind position { "asset" := asset, "amount" := amount }
              (let ((price (get-asset-price asset)))
                (safe-add acc (safe-multiply amount price)))))
          0.0
          borrowed))
  
  ;; Liquidation functionality
  (defun liquidate:string (borrower:string collateral-asset:string debt-asset:string amount:decimal)
    @doc "Liquidate undercollateralized position"
    
    ;; Check if borrower is liquidatable
    (let ((health-factor (get-account-health borrower)))
      (enforce (< health-factor MIN_HEALTH_FACTOR) "Account is healthy"))
    
    ;; Calculate liquidation amounts
    (let ((debt-price (get-asset-price debt-asset))
          (collateral-price (get-asset-price collateral-asset))
          (liquidation-value (safe-multiply amount debt-price))
          (collateral-amount (safe-divide liquidation-value collateral-price))
          (bonus-amount (safe-multiply collateral-amount LIQUIDATION_BONUS))
          (total-collateral-seized (safe-add collateral-amount bonus-amount)))
      
      ;; Transfer debt asset from liquidator to pool
      (let ((debt-contract (get-asset-contract debt-asset)))
        (debt-contract::transfer (tx-sender) (get-pool-account debt-asset) amount))
      
      ;; Transfer collateral from pool to liquidator  
      (let ((collateral-contract (get-asset-contract collateral-asset)))
        (collateral-contract::transfer (get-pool-account collateral-asset) 
                                      (tx-sender) 
                                      total-collateral-seized))
      
      ;; Update borrower positions
      (update-borrower-position borrower debt-asset (- amount))
      (update-borrower-position borrower collateral-asset (- total-collateral-seized))
      
      (format "Liquidated {} {} debt, seized {} {} collateral" 
              [amount debt-asset total-collateral-seized collateral-asset])))
  
  ;; Utility functions
  (defun update-interest-rates:string (asset:string)
    @doc "Update pool interest rates"
    (let ((new-borrow-rate (calculate-borrow-rate asset))
          (new-supply-rate (calculate-supply-rate asset)))
      (update pools asset {
        "borrow-rate": new-borrow-rate,
        "supply-rate": new-supply-rate,
        "last-update": (at 'block-time (chain-data))
      })))
  
  (defun get-pool-account:string (asset:string)
    @doc "Get pool account for asset"
    (create-principal (create-capability-guard (POOL_GUARD asset))))
  
  (defcap POOL_GUARD:bool (asset:string)
    @doc "Pool account guard"
    true)
  
  ;; Integration helpers
  (defun get-asset-contract:module{fungible-v2} (asset:string)
    @doc "Get token contract for asset"
    ;; This would typically read from a registry
    (if (= asset "KDA") coin 
        (if (= asset "USDC") usdc-token
            (read token-registry asset))))
)
```

### Frontend Integration

```typescript
// DeFi Protocol Frontend Integration
class DeFiProtocolClient {
  private pactClient: PactAPIClient;
  private walletConnector: WalletConnector;

  constructor(apiUrl: string, networkId: string, chainId: string) {
    this.pactClient = new PactAPIClient(apiUrl, networkId, chainId);
    this.walletConnector = new WalletConnector();
  }

  async supply(asset: string, amount: number, userAccount: string): Promise<string> {
    const transaction = Pact.builder
      .execution(`
        (lending-pool.supply "${userAccount}" "${asset}" ${amount.toFixed(12)})
      `)
      .addSigner(userAccount, (withCapability) => [
        withCapability('lending-pool.SUPPLY', userAccount, asset, amount),
        withCapability(`${asset}.TRANSFER`, userAccount, `lending-pool.${asset}-pool`, amount)
      ])
      .setMeta({
        chainId: '0',
        gasLimit: 3000,
        gasPrice: 0.00000001,
        ttl: 28800,
        sender: userAccount
      })
      .createTransaction();

    const signedTx = await this.walletConnector.signTransaction(transaction);
    const result = await this.pactClient.sendTransaction(signedTx);
    
    return result.requestKeys[0];
  }

  async borrow(asset: string, amount: number, userAccount: string): Promise<string> {
    // Check health factor first
    const healthFactor = await this.getHealthFactor(userAccount);
    if (healthFactor <= 1.0) {
      throw new Error('Insufficient collateral for borrowing');
    }

    const transaction = Pact.builder
      .execution(`
        (lending-pool.borrow "${userAccount}" "${asset}" ${amount.toFixed(12)})
      `)
      .addSigner(userAccount, (withCapability) => [
        withCapability('lending-pool.BORROW', userAccount, asset, amount)
      ])
      .setMeta({
        chainId: '0',
        gasLimit: 4000,
        gasPrice: 0.00000001,
        ttl: 28800,
        sender: userAccount
      })
      .createTransaction();

    const signedTx = await this.walletConnector.signTransaction(transaction);
    const result = await this.pactClient.sendTransaction(signedTx);
    
    return result.requestKeys[0];
  }

  async getHealthFactor(userAccount: string): Promise<number> {
    const query = Pact.builder
      .execution(`(lending-pool.get-account-health "${userAccount}")`)
      .setMeta({ chainId: '0' })
      .createTransaction();

    const result = await this.pactClient.localQuery(query);
    return result.result.data;
  }

  async getUserPosition(userAccount: string): Promise<any> {
    const query = Pact.builder
      .execution(`(read lending-pool.user-positions "${userAccount}")`)
      .setMeta({ chainId: '0' })
      .createTransaction();

    const result = await this.pactClient.localQuery(query);
    return result.result.data;
  }

  async getPoolInfo(asset: string): Promise<any> {
    const query = Pact.builder
      .execution(`(read lending-pool.pools "${asset}")`)
      .setMeta({ chainId: '0' })
      .createTransaction();

    const result = await this.pactClient.localQuery(query);
    return result.result.data;
  }
}
```

## Case Study 2: NFT Marketplace

### NFT Marketplace Architecture

```pact
;; interfaces/nft-marketplace-v1.pact
(interface nft-marketplace-v1
  @doc "NFT marketplace interface"
  
  ;; Listing management
  (defun list-nft:string (token-id:string collection:string price:decimal))
  (defun delist-nft:string (listing-id:string))
  (defun buy-nft:string (listing-id:string buyer:string))
  (defun make-offer:string (token-id:string collection:string amount:decimal expires:time))
  (defun accept-offer:string (offer-id:string))
  
  ;; Auction functionality
  (defun create-auction:string (token-id:string collection:string starting-price:decimal duration:integer))
  (defun place-bid:string (auction-id:string bidder:string amount:decimal))
  (defun finalize-auction:string (auction-id:string))
  
  ;; Royalty management
  (defun set-royalty:string (collection:string recipient:string percentage:decimal))
  (defun calculate-royalty:decimal (collection:string sale-price:decimal))
  
  ;; Collection management
  (defun register-collection:string (collection:string metadata:object))
  (defun verify-collection:string (collection:string))
  
  ;; Schemas
  (defschema listing
    listing-id:string
    token-id:string
    collection:string
    seller:string
    price:decimal
    currency:string
    created:time
    active:bool)
  
  (defschema offer
    offer-id:string
    token-id:string
    collection:string
    buyer:string
    amount:decimal
    currency:string
    expires:time
    active:bool)
  
  (defschema auction
    auction-id:string
    token-id:string
    collection:string
    seller:string
    starting-price:decimal
    current-bid:decimal
    highest-bidder:string
    end-time:time
    finalized:bool)
)
```

### Marketplace Implementation

```pact
;; core/nft-marketplace.pact
(module nft-marketplace GOVERNANCE
  @doc "Comprehensive NFT marketplace implementation"
  
  (implements nft-marketplace-v1)
  (use math [safe-add safe-subtract safe-multiply safe-divide])
  (use security [validate-user validate-amount])
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'marketplace-admin))
  
  ;; Tables
  (deftable listings:{listing})
  (deftable offers:{offer})
  (deftable auctions:{auction})
  (deftable collections:{collection-info})
  (deftable royalties:{royalty-info})
  (deftable sales-history:{sale-record})
  
  ;; Constants
  (defconst MARKETPLACE_FEE:decimal 0.025)  ;; 2.5% marketplace fee
  (defconst MIN_AUCTION_DURATION:integer 3600)  ;; 1 hour minimum
  (defconst MAX_AUCTION_DURATION:integer 604800)  ;; 7 days maximum
  (defconst MIN_BID_INCREASE:decimal 0.05)  ;; 5% minimum bid increase
  
  ;; Events
  (defcap NFT_LISTED:bool (listing-id:string token-id:string seller:string price:decimal)
    @doc "NFT listing event"
    @event
    true)
  
  (defcap NFT_SOLD:bool (listing-id:string token-id:string seller:string buyer:string price:decimal)
    @doc "NFT sale event"
    @event
    true)
  
  (defcap AUCTION_CREATED:bool (auction-id:string token-id:string seller:string starting-price:decimal)
    @doc "Auction creation event"
    @event
    true)
  
  (defcap BID_PLACED:bool (auction-id:string bidder:string amount:decimal)
    @doc "Auction bid event"
    @event
    true)
  
  ;; Listing functionality
  (defun list-nft:string (token-id:string collection:string price:decimal)
    @doc "List NFT for sale"
    (validate-amount price)
    
    (let ((seller (tx-sender))
          (listing-id (create-listing-id token-id collection seller)))
      
      ;; Verify ownership
      (verify-nft-ownership token-id collection seller)
      
      ;; Create listing
      (insert listings listing-id {
        "listing-id": listing-id,
        "token-id": token-id,
        "collection": collection,
        "seller": seller,
        "price": price,
        "currency": "KDA",
        "created": (at 'block-time (chain-data)),
        "active": true
      })
      
      ;; Transfer NFT to marketplace escrow
      (transfer-nft-to-escrow token-id collection seller)
      
      ;; Emit event
      (emit-event (NFT_LISTED listing-id token-id seller price))
      
      (format "NFT {} listed for {} KDA" [token-id price])))
  
  (defun buy-nft:string (listing-id:string buyer:string)
    @doc "Purchase listed NFT"
    (with-read listings listing-id {
      "token-id" := token-id,
      "collection" := collection,
      "seller" := seller,
      "price" := price,
      "active" := is-active
    }
      (enforce is-active "Listing not active")
      (enforce (!= buyer seller) "Cannot buy own NFT")
      
      ;; Calculate fees and payments
      (let ((marketplace-fee (safe-multiply price MARKETPLACE_FEE))
            (royalty-amount (calculate-royalty collection price))
            (seller-amount (safe-subtract price (safe-add marketplace-fee royalty-amount))))
        
        ;; Transfer payment from buyer
        (coin.transfer buyer seller seller-amount)
        (coin.transfer buyer (get-marketplace-account) marketplace-fee)
        
        ;; Pay royalties if applicable
        (if (> royalty-amount 0.0)
            (let ((royalty-recipient (get-royalty-recipient collection)))
              (coin.transfer buyer royalty-recipient royalty-amount))
            "No royalties")
        
        ;; Transfer NFT to buyer
        (transfer-nft-from-escrow token-id collection buyer)
        
        ;; Update listing status
        (update listings listing-id { "active": false })
        
        ;; Record sale
        (record-sale listing-id token-id collection seller buyer price)
        
        ;; Emit event
        (emit-event (NFT_SOLD listing-id token-id seller buyer price))
        
        (format "NFT {} sold to {} for {} KDA" [token-id buyer price]))))
  
  ;; Auction functionality
  (defun create-auction:string (token-id:string collection:string starting-price:decimal duration:integer)
    @doc "Create NFT auction"
    (validate-amount starting-price)
    (enforce (and (>= duration MIN_AUCTION_DURATION) (<= duration MAX_AUCTION_DURATION))
             "Invalid auction duration")
    
    (let ((seller (tx-sender))
          (auction-id (create-auction-id token-id collection seller))
          (end-time (add-time (at 'block-time (chain-data)) (seconds duration))))
      
      ;; Verify ownership
      (verify-nft-ownership token-id collection seller)
      
      ;; Create auction
      (insert auctions auction-id {
        "auction-id": auction-id,
        "token-id": token-id,
        "collection": collection,
        "seller": seller,
        "starting-price": starting-price,
        "current-bid": 0.0,
        "highest-bidder": "",
        "end-time": end-time,
        "finalized": false
      })
      
      ;; Transfer NFT to auction escrow
      (transfer-nft-to-escrow token-id collection seller)
      
      ;; Emit event
      (emit-event (AUCTION_CREATED auction-id token-id seller starting-price))
      
      (format "Auction {} created for NFT {}" [auction-id token-id])))
  
  (defun place-bid:string (auction-id:string bidder:string amount:decimal)
    @doc "Place bid on auction"
    (validate-amount amount)
    
    (with-read auctions auction-id {
      "seller" := seller,
      "starting-price" := starting-price,
      "current-bid" := current-bid,
      "highest-bidder" := current-bidder,
      "end-time" := end-time,
      "finalized" := is-finalized
    }
      (enforce (not is-finalized) "Auction already finalized")
      (enforce (< (at 'block-time (chain-data)) end-time) "Auction has ended")
      (enforce (!= bidder seller) "Seller cannot bid on own auction")
      
      ;; Validate bid amount
      (let ((minimum-bid (if (= current-bid 0.0)
                            starting-price
                            (safe-multiply current-bid (safe-add 1.0 MIN_BID_INCREASE)))))
        (enforce (>= amount minimum-bid) 
                 (format "Bid must be at least {}" [minimum-bid]))
        
        ;; Return previous bid if exists
        (if (!= current-bidder "")
            (coin.transfer (get-auction-escrow-account auction-id) current-bidder current-bid)
            "No previous bid")
        
        ;; Transfer new bid to escrow
        (coin.transfer bidder (get-auction-escrow-account auction-id) amount)
        
        ;; Update auction
        (update auctions auction-id {
          "current-bid": amount,
          "highest-bidder": bidder
        })
        
        ;; Emit event
        (emit-event (BID_PLACED auction-id bidder amount))
        
        (format "Bid of {} KDA placed on auction {}" [amount auction-id]))))
  
  (defun finalize-auction:string (auction-id:string)
    @doc "Finalize completed auction"
    (with-read auctions auction-id {
      "token-id" := token-id,
      "collection" := collection,
      "seller" := seller,
      "current-bid" := final-bid,
      "highest-bidder" := winner,
      "end-time" := end-time,
      "finalized" := is-finalized
    }
      (enforce (not is-finalized) "Auction already finalized")
      (enforce (>= (at 'block-time (chain-data)) end-time) "Auction not yet ended")
      
      (if (= winner "")
          ;; No bids - return NFT to seller
          (progn
            (transfer-nft-from-escrow token-id collection seller)
            (update auctions auction-id { "finalized": true })
            "Auction ended with no bids")
          
          ;; Process winning bid
          (let ((marketplace-fee (safe-multiply final-bid MARKETPLACE_FEE))
                (royalty-amount (calculate-royalty collection final-bid))
                (seller-amount (safe-subtract final-bid (safe-add marketplace-fee royalty-amount))))
            
            ;; Transfer payments
            (coin.transfer (get-auction-escrow-account auction-id) seller seller-amount)
            (coin.transfer (get-auction-escrow-account auction-id) (get-marketplace-account) marketplace-fee)
            
            ;; Pay royalties
            (if (> royalty-amount 0.0)
                (let ((royalty-recipient (get-royalty-recipient collection)))
                  (coin.transfer (get-auction-escrow-account auction-id) royalty-recipient royalty-amount))
                "No royalties")
            
            ;; Transfer NFT to winner
            (transfer-nft-from-escrow token-id collection winner)
            
            ;; Update auction
            (update auctions auction-id { "finalized": true })
            
            ;; Record sale
            (record-sale auction-id token-id collection seller winner final-bid)
            
            (format "Auction {} won by {} for {} KDA" [auction-id winner final-bid])))))
  
  ;; Royalty management
  (defun set-royalty:string (collection:string recipient:string percentage:decimal)
    @doc "Set collection royalty"
    (with-capability (GOVERNANCE)
      (enforce (and (>= percentage 0.0) (<= percentage 0.10)) "Invalid royalty percentage")
      
      (write royalties collection {
        "collection": collection,
        "recipient": recipient,
        "percentage": percentage
      }))
    
    (format "Royalty set for collection {}: {}% to {}" [collection (* percentage 100.0) recipient]))
  
  (defun calculate-royalty:decimal (collection:string sale-price:decimal)
    @doc "Calculate royalty amount for sale"
    (with-default-read royalties collection
      { "percentage": 0.0 }
      { "percentage" := royalty-rate }
      (safe-multiply sale-price royalty-rate)))
  
  ;; Utility functions
  (defun create-listing-id:string (token-id:string collection:string seller:string)
    @doc "Create unique listing ID"
    (hash [token-id collection seller (at 'block-time (chain-data))]))
  
  (defun create-auction-id:string (token-id:string collection:string seller:string)
    @doc "Create unique auction ID"
    (hash ["auction" token-id collection seller (at 'block-time (chain-data))]))
  
  (defun verify-nft-ownership:bool (token-id:string collection:string owner:string)
    @doc "Verify NFT ownership"
    ;; This would integrate with the specific NFT contract
    (let ((nft-contract (get-nft-contract collection)))
      (= owner (nft-contract::get-owner token-id))))
  
  (defun get-marketplace-account:string ()
    @doc "Get marketplace fee collection account"
    (create-principal (create-capability-guard (MARKETPLACE_GUARD))))
  
  (defcap MARKETPLACE_GUARD:bool ()
    @doc "Marketplace account guard"
    true)
  
  ;; Query functions
  (defun get-active-listings:[object] (collection:string)
    @doc "Get active listings for collection"
    (select listings 
            (and? (where 'collection (= collection))
                  (where 'active (= true)))))
  
  (defun get-user-listings:[object] (user:string)
    @doc "Get user's active listings"
    (select listings
            (and? (where 'seller (= user))
                  (where 'active (= true)))))
  
  (defun get-active-auctions:[object] (collection:string)
    @doc "Get active auctions for collection"
    (select auctions
            (and? (where 'collection (= collection))
                  (where 'finalized (= false)))))
)
```

## Case Study 3: Governance System

### DAO Governance Implementation

```pact
;; core/governance.pact
(module dao-governance GOVERNANCE
  @doc "Decentralized Autonomous Organization governance system"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'dao-admin))
  
  ;; Schemas
  (defschema proposal
    proposal-id:string
    title:string
    description:string
    proposer:string
    proposal-type:string
    target-contract:string
    target-function:string
    parameters:object
    voting-start:time
    voting-end:time
    execution-delay:time
    votes-for:decimal
    votes-against:decimal
    votes-abstain:decimal
    status:string
    executed:bool
    quorum-threshold:decimal
    approval-threshold:decimal)
  
  (defschema vote-record
    proposal-id:string
    voter:string
    choice:string
    voting-power:decimal
    timestamp:time)
  
  (defschema voting-power
    user:string
    power:decimal
    delegated-to:string
    delegated-power:decimal
    last-update:time)
  
  ;; Tables
  (deftable proposals:{proposal})
  (deftable vote-records:{vote-record})
  (deftable voting-powers:{voting-power})
  (deftable delegation-history:{delegation-record})
  
  ;; Constants
  (defconst VOTING_PERIOD_DAYS:integer 7)
  (defconst EXECUTION_DELAY_DAYS:integer 2)
  (defconst MIN_PROPOSAL_THRESHOLD:decimal 10000.0)
  (defconst QUORUM_THRESHOLD:decimal 0.04)      ;; 4% quorum
  (defconst APPROVAL_THRESHOLD:decimal 0.60)    ;; 60% approval
  
  ;; Proposal management
  (defun create-proposal:string (
    proposal-id:string
    title:string
    description:string
    proposal-type:string
    target-contract:string
    target-function:string
    parameters:object
  )
    @doc "Create governance proposal"
    (let ((proposer (tx-sender))
          (user-power (get-user-voting-power proposer))
          (voting-start (add-time (at 'block-time (chain-data)) (days 1)))
          (voting-end (add-time voting-start (days VOTING_PERIOD_DAYS)))
          (execution-delay (add-time voting-end (days EXECUTION_DELAY_DAYS))))
      
      ;; Check proposer has sufficient voting power
      (enforce (>= user-power MIN_PROPOSAL_THRESHOLD)
               (format "Insufficient voting power: {} required" [MIN_PROPOSAL_THRESHOLD]))
      
      ;; Create proposal
      (insert proposals proposal-id {
        "proposal-id": proposal-id,
        "title": title,
        "description": description,
        "proposer": proposer,
        "proposal-type": proposal-type,
        "target-contract": target-contract,
        "target-function": target-function,
        "parameters": parameters,
        "voting-start": voting-start,
        "voting-end": voting-end,
        "execution-delay": execution-delay,
        "votes-for": 0.0,
        "votes-against": 0.0,
        "votes-abstain": 0.0,
        "status": "pending",
        "executed": false,
        "quorum-threshold": QUORUM_THRESHOLD,
        "approval-threshold": APPROVAL_THRESHOLD
      })
      
      (format "Proposal {} created by {}" [proposal-id proposer])))
  
  (defun vote:string (proposal-id:string choice:string)
    @doc "Vote on governance proposal"
    (let ((voter (tx-sender))
          (voting-power (get-user-voting-power voter)))
      
      (with-read proposals proposal-id {
        "voting-start" := start-time,
        "voting-end" := end-time,
        "status" := status,
        "votes-for" := current-for,
        "votes-against" := current-against,
        "votes-abstain" := current-abstain
      }
        ;; Validate voting conditions
        (enforce (= status "active") "Proposal not active for voting")
        (enforce (>= (at 'block-time (chain-data)) start-time) "Voting not yet started")
        (enforce (< (at 'block-time (chain-data)) end-time) "Voting period ended")
        (enforce (> voting-power 0.0) "No voting power")
        (enforce (contains choice ["FOR" "AGAINST" "ABSTAIN"]) "Invalid vote choice")
        
        ;; Check if already voted
        (with-default-read vote-records (format "{}:{}" [proposal-id voter])
          { "choice": "NONE" }
          { "choice" := existing-choice }
          (enforce (= existing-choice "NONE") "Already voted on this proposal"))
        
        ;; Record vote
        (insert vote-records (format "{}:{}" [proposal-id voter]) {
          "proposal-id": proposal-id,
          "voter": voter,
          "choice": choice,
          "voting-power": voting-power,
          "timestamp": (at 'block-time (chain-data))
        })
        
        ;; Update proposal vote counts
        (let ((updated-for (if (= choice "FOR") (+ current-for voting-power) current-for))
              (updated-against (if (= choice "AGAINST") (+ current-against voting-power) current-against))
              (updated-abstain (if (= choice "ABSTAIN") (+ current-abstain voting-power) current-abstain)))
          
          (update proposals proposal-id {
            "votes-for": updated-for,
            "votes-against": updated-against,
            "votes-abstain": updated-abstain
          }))
        
        (format "Vote {} cast for proposal {} with power {}" [choice proposal-id voting-power]))))
  
  (defun finalize-proposal:string (proposal-id:string)
    @doc "Finalize proposal after voting period"
    (with-read proposals proposal-id {
      "voting-end" := end-time,
      "votes-for" := for-votes,
      "votes-against" := against-votes,
      "votes-abstain" := abstain-votes,
      "quorum-threshold" := quorum-req,
      "approval-threshold" := approval-req,
      "status" := status
    }
      (enforce (= status "active") "Proposal not active")
      (enforce (>= (at 'block-time (chain-data)) end-time) "Voting period not ended")
      
      (let ((total-votes (+ (+ for-votes against-votes) abstain-votes))
            (total-voting-power (get-total-voting-power))
            (quorum-met (>= total-votes (* total-voting-power quorum-req)))
            (approval-rate (if (= (+ for-votes against-votes) 0.0)
                              0.0
                              (/ for-votes (+ for-votes against-votes))))
            (approved (and quorum-met (>= approval-rate approval-req))))
        
        (update proposals proposal-id {
          "status": (if approved "approved" "rejected")
        })
        
        (format "Proposal {} {}: quorum {}, approval {}" 
                [proposal-id 
                 (if approved "APPROVED" "REJECTED")
                 quorum-met
                 approval-rate]))))
  
  (defun execute-proposal:string (proposal-id:string)
    @doc "Execute approved proposal"
    (with-read proposals proposal-id {
      "status" := status,
      "execution-delay" := delay-time,
      "executed" := is-executed,
      "target-contract" := contract,
      "target-function" := function,
      "parameters" := params
    }
      (enforce (= status "approved") "Proposal not approved")
      (enforce (not is-executed) "Proposal already executed")
      (enforce (>= (at 'block-time (chain-data)) delay-time) "Execution delay not passed")
      
      ;; Execute the governance action
      (execute-governance-action contract function params)
      
      ;; Mark as executed
      (update proposals proposal-id { "executed": true })
      
      (format "Proposal {} executed successfully" [proposal-id])))
  
  ;; Voting power and delegation
  (defun delegate-voting-power:string (delegatee:string amount:decimal)
    @doc "Delegate voting power to another user"
    (let ((delegator (tx-sender)))
      (enforce (!= delegator delegatee) "Cannot delegate to self")
      (enforce (> amount 0.0) "Delegation amount must be positive")
      
      (with-default-read voting-powers delegator
        { "power": 0.0, "delegated-power": 0.0 }
        { "power" := current-power, "delegated-power" := current-delegated }
        
        (let ((available-power (- current-power current-delegated)))
          (enforce (>= available-power amount) "Insufficient voting power to delegate")
          
          ;; Update delegator
          (update voting-powers delegator {
            "delegated-to": delegatee,
            "delegated-power": (+ current-delegated amount),
            "last-update": (at 'block-time (chain-data))
          })
          
          ;; Update delegatee
          (with-default-read voting-powers delegatee
            { "power": 0.0 }
            { "power" := delegatee-power }
            (update voting-powers delegatee {
              "power": (+ delegatee-power amount),
              "last-update": (at 'block-time (chain-data))
            }))
          
          (format "Delegated {} voting power from {} to {}" [amount delegator delegatee])))))
  
  ;; Utility functions
  (defun get-user-voting-power:decimal (user:string)
    @doc "Get user's current voting power"
    (with-default-read voting-powers user
      { "power": 0.0 }
      { "power" := power }
      power))
  
  (defun get-total-voting-power:decimal ()
    @doc "Get total voting power in system"
    (fold (+) 0.0 
          (map (at 'power) 
               (select voting-powers (constantly true)))))
  
  (defun execute-governance-action:string (contract:string function:string params:object)
    @doc "Execute governance action on target contract"
    ;; This would call the target contract's governance function
    ;; Implementation depends on specific governance actions
    (format "Executed {}::{} with params {}" [contract function params]))
)
```

## Deployment and Operations

### Deployment Scripts

```pact
;; deployment/deploy-all.repl
(begin-tx "Deploy complete DeFi ecosystem")

;; Set deployment environment
(env-data {
  "protocol-admin": ["protocol-admin-key"],
  "marketplace-admin": ["marketplace-admin-key"],
  "dao-admin": ["dao-admin-key"],
  "upgrade-keyset": ["upgrade-key-1", "upgrade-key-2"],
  "initial-config": {
    "marketplace-fee": 0.025,
    "min-auction-duration": 3600,
    "max-auction-duration": 604800
  }
})

(env-keys ["protocol-admin-key", "marketplace-admin-key", "dao-admin-key"])

;; Deploy interfaces first
(load "../interfaces/fungible-v2.pact")
(load "../interfaces/lending-v1.pact") 
(load "../interfaces/nft-marketplace-v1.pact")

;; Deploy utility libraries
(load "../libraries/math.pact")
(load "../libraries/security.pact")
(load "../libraries/events.pact")

;; Deploy core contracts
(load "../core/lending-pool.pact")
(load "../core/nft-marketplace.pact")
(load "../core/governance.pact")

;; Initialize with configuration
(lending-pool.create-pool "KDA" 0.10)
(lending-pool.create-pool "USDC" 0.15)

(nft-marketplace.register-collection "crypto-punks" {
  "name": "Crypto Punks",
  "description": "Original NFT collection",
  "verified": true
})

;; Set up initial governance
(dao-governance.initialize-governance (read-msg "initial-config"))

(commit-tx)

(print "DeFi ecosystem deployed successfully!")
```

### Monitoring and Analytics

```typescript
// Real-time protocol monitoring
class ProtocolMonitor {
  private eventSubscribers: Map<string, Function[]> = new Map();

  async startMonitoring(): Promise<void> {
    // Monitor lending pool events
    this.subscribeToEvents('lending-pool', [
      'SUPPLY',
      'WITHDRAW', 
      'BORROW',
      'REPAY',
      'LIQUIDATION'
    ]);

    // Monitor marketplace events
    this.subscribeToEvents('nft-marketplace', [
      'NFT_LISTED',
      'NFT_SOLD',
      'AUCTION_CREATED',
      'BID_PLACED'
    ]);

    // Monitor governance events
    this.subscribeToEvents('dao-governance', [
      'PROPOSAL_CREATED',
      'VOTE_CAST',
      'PROPOSAL_EXECUTED'
    ]);
  }

  async getProtocolMetrics(): Promise<any> {
    const [
      lendingMetrics,
      marketplaceMetrics,
      governanceMetrics
    ] = await Promise.all([
      this.getLendingMetrics(),
      this.getMarketplaceMetrics(),
      this.getGovernanceMetrics()
    ]);

    return {
      lending: lendingMetrics,
      marketplace: marketplaceMetrics,
      governance: governanceMetrics,
      timestamp: new Date().toISOString()
    };
  }

  private async getLendingMetrics(): Promise<any> {
    const pools = ['KDA', 'USDC'];
    const poolData = await Promise.all(
      pools.map(asset => this.getPoolInfo(asset))
    );

    return {
      totalValueLocked: poolData.reduce((sum, pool) => 
        sum + (pool.totalSupplied * pool.price), 0),
      totalBorrowed: poolData.reduce((sum, pool) => 
        sum + (pool.totalBorrowed * pool.price), 0),
      averageUtilization: poolData.reduce((sum, pool) => 
        sum + pool.utilizationRate, 0) / pools.length,
      pools: poolData
    };
  }

  private async getMarketplaceMetrics(): Promise<any> {
    // Implementation for marketplace metrics
    return {
      totalListings: await this.getTotalListings(),
      totalVolume24h: await this.getVolume24h(),
      averagePrice: await this.getAveragePrice(),
      activeAuctions: await this.getActiveAuctions()
    };
  }

  private async getGovernanceMetrics(): Promise<any> {
    return {
      totalProposals: await this.getTotalProposals(),
      activeProposals: await this.getActiveProposals(),
      participationRate: await this.getParticipationRate(),
      totalVotingPower: await this.getTotalVotingPower()
    };
  }
}
```

## Summary

Building real-world Pact applications requires:

**Architecture Principles:**
- **Modular design** with clear separation of concerns
- **Interface-based** contracts for interoperability
- **Comprehensive testing** at all levels
- **Event-driven** architecture for monitoring

**Production Considerations:**
- **Gas optimization** for cost-effective operations
- **Security audits** and formal verification
- **Upgrade mechanisms** for protocol evolution
- **Monitoring systems** for real-time insights

**Integration Patterns:**
- **Client libraries** for seamless frontend integration
- **Error handling** with retry mechanisms
- **Event monitoring** for real-time updates
- **Performance optimization** for user experience

**Deployment Strategy:**
- **Staged deployment** with thorough testing
- **Configuration management** for different environments
- **Monitoring and alerting** for operational excellence
- **Documentation** for user and developer adoption

These patterns enable building sophisticated, production-ready applications that leverage Pact's unique capabilities while maintaining security, performance, and user experience standards.

## Exercises

1. Extend the lending protocol with flash loans and yield farming
2. Add Dutch auction support to the NFT marketplace
3. Implement a cross-chain governance system
4. Build a comprehensive analytics dashboard
5. Create an automated liquidation bot for the lending protocol

## References

- Real World Pact: https://github.com/thomashoneyman/real-world-pact
- DeFi patterns: Compound, Aave protocol documentation
- NFT standards: OpenSea, LooksRare marketplace architecture
- Governance systems: Compound governance, Snapshot voting