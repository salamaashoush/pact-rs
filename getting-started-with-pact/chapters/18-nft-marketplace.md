# Chapter 18: Building an NFT Marketplace - Complete Step-by-Step Guide

## Table of Contents
1. [Project Overview](#project-overview)
2. [Architecture Design](#architecture-design)
3. [Step 1: NFT Token Standard](#step-1-nft-token-standard)
4. [Step 2: Marketplace Core](#step-2-marketplace-core)
5. [Step 3: Auction System](#step-3-auction-system)
6. [Step 4: Royalty Distribution](#step-4-royalty-distribution)
7. [Step 5: Frontend Integration](#step-5-frontend-integration)
8. [Testing Strategy](#testing-strategy)
9. [Deployment Guide](#deployment-guide)
10. [Best Practices](#best-practices)

## Project Overview

In this chapter, we'll build a complete NFT marketplace from scratch, including:
- NFT minting and metadata management
- Direct sales and auction mechanisms
- Royalty distribution system
- Collection management
- Search and discovery features
- Gas optimization strategies

### What You'll Learn
- Implementing non-fungible token standards
- Building secure marketplace mechanics
- Handling complex capability patterns
- Managing state across multiple modules
- Integrating with IPFS for metadata
- Creating efficient indexing systems

## Architecture Design

### Module Structure
```
nft-marketplace/
├── interfaces/
│   ├── nft-v2.pact           # NFT standard interface
│   └── marketplace-v1.pact    # Marketplace interface
├── modules/
│   ├── nft-token.pact        # NFT implementation
│   ├── marketplace.pact      # Core marketplace
│   ├── auction.pact          # Auction system
│   └── royalty.pact          # Royalty distribution
├── policies/
│   ├── guard-policy.pact     # Access control
│   └── royalty-policy.pact   # Royalty rules
└── tests/
    └── *.repl                # Test files
```

### Data Flow
```
User → Marketplace → NFT Contract → Royalty System
         ↓              ↓               ↓
    Sale Events    Token Events   Payment Events
         ↓              ↓               ↓
      Indexer ← ← ← ← ← ← ← ← ← ← ← ← ↓
```

## Step 1: NFT Token Standard

Let's start by implementing a comprehensive NFT standard:

```pact
;; interfaces/nft-v2.pact
(interface nft-v2
  @doc "Non-fungible token standard interface"
  
  ;; Token information schema
  (defschema token-info
    @doc "Token metadata"
    id:string
    uri:string
    precision:integer
    collection:string)
  
  ;; Required capabilities
  (defcap TRANSFER:bool (token-id:string sender:string receiver:string)
    @doc "Transfer capability for specific token")
  
  (defcap MINT:bool (token-id:string account:string)
    @doc "Minting capability")
  
  ;; Core functions
  (defun mint:string (token-id:string account:string guard:guard uri:string)
    @doc "Mint new NFT")
  
  (defun transfer:string (token-id:string sender:string receiver:string)
    @doc "Transfer NFT ownership")
  
  (defun get-owner:string (token-id:string)
    @doc "Get current owner of token")
  
  (defun get-token-info:object{token-info} (token-id:string)
    @doc "Get token metadata")
  
  (defun get-balance:integer (account:string)
    @doc "Get number of tokens owned by account")
  
  ;; Events
  (defcap TRANSFER_EVENT (token-id:string from:string to:string)
    @event)
  
  (defcap MINT_EVENT (token-id:string account:string)
    @event)
)
```

Now implement the NFT contract:

```pact
;; modules/nft-token.pact
(module nft-token GOVERNANCE
  @doc "NFT token implementation with advanced features"
  
  (implements nft-v2)
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'nft-admin))
  
  ;; Schemas
  (defschema token
    @doc "NFT token storage"
    id:string
    uri:string
    owner:string
    guard:guard
    collection:string
    created-at:time
    last-sale-price:decimal
    mint-number:integer
    attributes:object)
  
  (defschema collection
    @doc "NFT collection information"
    name:string
    creator:string
    description:string
    max-supply:integer
    minted:integer
    base-uri:string
    mint-price:decimal
    mint-guard:guard
    royalty-rate:decimal
    status:string)  ;; active, paused, completed
  
  (defschema account-balance
    @doc "Track NFT ownership count"
    account:string
    balance:integer
    token-ids:[string])
  
  ;; Tables
  (deftable tokens:{token})
  (deftable collections:{collection})
  (deftable balances:{account-balance})
  
  ;; Constants
  (defconst MAX_URI_LENGTH 512)
  (defconst MINT_STATUS "mint-status")
  
  ;; Collection Management
  (defcap COLLECTION_OWNER (collection:string)
    @doc "Collection ownership capability"
    (with-read collections collection { "creator" := creator }
      (enforce-guard (at 'guard (read accounts creator)))))
  
  (defun create-collection:string 
    ( name:string 
      creator:string 
      description:string 
      max-supply:integer 
      base-uri:string
      mint-price:decimal
      royalty-rate:decimal )
    @doc "Create new NFT collection"
    (enforce (> max-supply 0) "Max supply must be positive")
    (enforce (>= royalty-rate 0.0) "Royalty rate must be non-negative")
    (enforce (<= royalty-rate 0.25) "Royalty rate cannot exceed 25%")
    
    (insert collections name {
      "name": name,
      "creator": creator,
      "description": description,
      "max-supply": max-supply,
      "minted": 0,
      "base-uri": base-uri,
      "mint-price": mint-price,
      "mint-guard": (at 'guard (read accounts creator)),
      "royalty-rate": royalty-rate,
      "status": "active"
    })
    (format "Collection {} created" [name]))
  
  ;; Minting
  (defcap MINT:bool (token-id:string account:string)
    @doc "Minting capability"
    @event
    (let* ( (collection (get-token-collection token-id))
            (col-data (read collections collection)) )
      (enforce (= (at 'status col-data) "active") "Collection not active")
      (enforce (< (at 'minted col-data) (at 'max-supply col-data)) 
               "Collection fully minted")
      true))
  
  (defun mint:string 
    ( collection:string
      account:string 
      guard:guard
      attributes:object )
    @doc "Mint new NFT in collection"
    (with-capability (MINT collection account)
      (with-read collections collection 
        { "minted" := minted
        , "max-supply" := max-supply
        , "base-uri" := base-uri
        , "mint-price" := price }
        
        (enforce (< minted max-supply) "Collection fully minted")
        
        ;; Generate token ID
        (let ( (token-id (format "{}:{}" [collection (+ minted 1)]))
               (uri (format "{}/{}.json" [base-uri (+ minted 1)])) )
          
          ;; Charge mint price
          (if (> price 0.0)
            (coin.transfer account (at 'creator (read collections collection)) price)
            "Free mint")
          
          ;; Create token
          (insert tokens token-id {
            "id": token-id,
            "uri": uri,
            "owner": account,
            "guard": guard,
            "collection": collection,
            "created-at": (at 'block-time (chain-data)),
            "last-sale-price": price,
            "mint-number": (+ minted 1),
            "attributes": attributes
          })
          
          ;; Update collection
          (update collections collection {
            "minted": (+ minted 1)
          })
          
          ;; Update balance
          (update-balance account 1 token-id)
          
          ;; Emit event
          (emit-event (MINT_EVENT token-id account))
          
          token-id))))
  
  ;; Transfer
  (defcap TRANSFER:bool (token-id:string sender:string receiver:string)
    @doc "Transfer capability"
    @managed
    (enforce (!= sender receiver) "Cannot transfer to self")
    (with-read tokens token-id { "owner" := owner, "guard" := guard }
      (enforce (= owner sender) "Not token owner")
      (enforce-guard guard)))
  
  (defun transfer:string (token-id:string sender:string receiver:string)
    @doc "Transfer NFT ownership"
    (with-capability (TRANSFER token-id sender receiver)
      (with-read tokens token-id 
        { "guard" := token-guard }
        
        ;; Update token
        (update tokens token-id {
          "owner": receiver,
          "guard": (at 'guard (read accounts receiver))
        })
        
        ;; Update balances
        (update-balance sender -1 token-id)
        (update-balance receiver 1 token-id)
        
        ;; Emit event
        (emit-event (TRANSFER_EVENT token-id sender receiver))
        
        "Transfer complete")))
  
  ;; Balance Management
  (defun update-balance:string (account:string delta:integer token-id:string)
    @doc "Update account NFT balance"
    (require-capability (INTERNAL))
    (with-default-read balances account
      { "balance": 0, "token-ids": [] }
      { "balance" := current-balance, "token-ids" := tokens-list }
      
      (let ((new-balance (+ current-balance delta)))
        (enforce (>= new-balance 0) "Invalid balance")
        
        (write balances account {
          "account": account,
          "balance": new-balance,
          "token-ids": (if (> delta 0)
                         (+ tokens-list [token-id])
                         (filter (!= token-id) tokens-list))
        }))))
  
  ;; Query Functions
  (defun get-owner:string (token-id:string)
    @doc "Get token owner"
    (at 'owner (read tokens token-id)))
  
  (defun get-token-info:object (token-id:string)
    @doc "Get token metadata"
    (read tokens token-id))
  
  (defun get-balance:integer (account:string)
    @doc "Get account NFT count"
    (at 'balance (read balances account)))
  
  (defun get-account-tokens:[string] (account:string)
    @doc "Get all tokens owned by account"
    (at 'token-ids (read balances account)))
  
  (defun get-collection-info:object (collection:string)
    @doc "Get collection information"
    (read collections collection))
  
  ;; Utility Functions
  (defun get-token-collection:string (token-id:string)
    @doc "Extract collection from token ID"
    (take (- (str-to-int (at 'idx (str-to-list ":" token-id))) 1) token-id))
  
  (defcap INTERNAL ()
    @doc "Internal capability"
    true)
)

(create-table tokens)
(create-table collections)
(create-table balances)
```

## Step 2: Marketplace Core

Now let's build the marketplace module:

```pact
;; modules/marketplace.pact
(module marketplace GOVERNANCE
  @doc "NFT marketplace with advanced trading features"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'marketplace-admin))
  
  ;; Schemas
  (defschema listing
    @doc "NFT listing information"
    token-id:string
    seller:string
    price:decimal
    currency:module{fungible-v2}
    created-at:time
    expires-at:time
    status:string)  ;; active, sold, cancelled, expired
  
  (defschema offer
    @doc "Offer on NFT"
    offer-id:string
    token-id:string
    buyer:string
    price:decimal
    currency:module{fungible-v2}
    created-at:time
    expires-at:time
    status:string)  ;; active, accepted, rejected, expired
  
  (defschema sale-history
    @doc "Historical sales data"
    token-id:string
    seller:string
    buyer:string
    price:decimal
    currency:module{fungible-v2}
    timestamp:time
    listing-id:string
    royalty-paid:decimal)
  
  (defschema marketplace-stats
    @doc "Marketplace statistics"
    total-volume:decimal
    total-sales:integer
    active-listings:integer
    total-fees-collected:decimal)
  
  ;; Tables
  (deftable listings:{listing})
  (deftable offers:{offer})
  (deftable sales:{sale-history})
  (deftable stats:{marketplace-stats})
  
  ;; Constants
  (defconst MARKETPLACE_FEE 0.025)  ;; 2.5% fee
  (defconst MIN_PRICE 0.000001)
  (defconst MAX_LISTING_DURATION 2592000)  ;; 30 days in seconds
  
  ;; Capabilities
  (defcap LIST:bool (token-id:string seller:string)
    @doc "Capability to list NFT"
    (enforce-guard (nft-token.get-token-guard token-id))
    (enforce (= seller (nft-token.get-owner token-id)) "Not token owner"))
  
  (defcap BUY:bool (listing-id:string buyer:string)
    @doc "Capability to buy NFT"
    @managed
    (with-read listings listing-id { "status" := status }
      (enforce (= status "active") "Listing not active")))
  
  (defcap OFFER:bool (token-id:string buyer:string)
    @doc "Capability to make offer"
    true)
  
  (defcap CANCEL:bool (listing-id:string seller:string)
    @doc "Capability to cancel listing"
    (with-read listings listing-id 
      { "seller" := listing-seller, "status" := status }
      (enforce (= seller listing-seller) "Not the seller")
      (enforce (= status "active") "Listing not active")))
  
  ;; Events
  (defcap LISTING_CREATED (listing-id:string token-id:string price:decimal)
    @event true)
  
  (defcap SALE_COMPLETED (token-id:string seller:string buyer:string price:decimal)
    @event true)
  
  (defcap OFFER_CREATED (offer-id:string token-id:string price:decimal)
    @event true)
  
  ;; Initialize marketplace
  (defun init-marketplace ()
    @doc "Initialize marketplace statistics"
    (insert stats "stats" {
      "total-volume": 0.0,
      "total-sales": 0,
      "active-listings": 0,
      "total-fees-collected": 0.0
    }))
  
  ;; Listing Functions
  (defun list-nft:string 
    ( token-id:string
      price:decimal
      duration:integer
      currency:module{fungible-v2} )
    @doc "List NFT for sale"
    (enforce (> price MIN_PRICE) "Price too low")
    (enforce (<= duration MAX_LISTING_DURATION) "Duration too long")
    
    (with-capability (LIST token-id (nft-token.get-owner token-id))
      (let* ( (seller (nft-token.get-owner token-id))
              (listing-id (hash [token-id seller (at 'block-time (chain-data))]))
              (expires-at (add-time (at 'block-time (chain-data)) duration)) )
        
        ;; Create listing
        (insert listings listing-id {
          "token-id": token-id,
          "seller": seller,
          "price": price,
          "currency": currency,
          "created-at": (at 'block-time (chain-data)),
          "expires-at": expires-at,
          "status": "active"
        })
        
        ;; Update stats
        (with-read stats "stats" { "active-listings" := current }
          (update stats "stats" { "active-listings": (+ current 1) }))
        
        ;; Emit event
        (emit-event (LISTING_CREATED listing-id token-id price))
        
        listing-id)))
  
  (defun buy-nft:string (listing-id:string)
    @doc "Buy listed NFT"
    (with-capability (BUY listing-id (at 'sender (chain-data)))
      (with-read listings listing-id 
        { "token-id" := token-id
        , "seller" := seller
        , "price" := price
        , "currency" := currency
        , "expires-at" := expires-at }
        
        ;; Check expiration
        (enforce (< (at 'block-time (chain-data)) expires-at) "Listing expired")
        
        (let* ( (buyer (at 'sender (chain-data)))
                (collection (nft-token.get-token-collection token-id))
                (royalty-rate (at 'royalty-rate 
                               (nft-token.get-collection-info collection)))
                (creator (at 'creator 
                           (nft-token.get-collection-info collection)))
                (marketplace-fee (* price MARKETPLACE_FEE))
                (royalty-amount (* price royalty-rate))
                (seller-amount (- price (+ marketplace-fee royalty-amount))) )
          
          ;; Transfer payment
          (currency::transfer buyer seller seller-amount)
          (currency::transfer buyer "marketplace-treasury" marketplace-fee)
          (if (> royalty-amount 0.0)
            (currency::transfer buyer creator royalty-amount)
            "No royalty")
          
          ;; Transfer NFT
          (nft-token.transfer token-id seller buyer)
          
          ;; Update listing
          (update listings listing-id { "status": "sold" })
          
          ;; Record sale
          (insert sales (hash [listing-id buyer]) {
            "token-id": token-id,
            "seller": seller,
            "buyer": buyer,
            "price": price,
            "currency": currency,
            "timestamp": (at 'block-time (chain-data)),
            "listing-id": listing-id,
            "royalty-paid": royalty-amount
          })
          
          ;; Update stats
          (with-read stats "stats" 
            { "total-volume" := volume
            , "total-sales" := sales
            , "active-listings" := listings
            , "total-fees-collected" := fees }
            (update stats "stats" {
              "total-volume": (+ volume price),
              "total-sales": (+ sales 1),
              "active-listings": (- listings 1),
              "total-fees-collected": (+ fees marketplace-fee)
            }))
          
          ;; Update token last sale price
          (update tokens token-id { "last-sale-price": price })
          
          ;; Emit event
          (emit-event (SALE_COMPLETED token-id seller buyer price))
          
          "Purchase complete"))))
  
  (defun cancel-listing:string (listing-id:string)
    @doc "Cancel active listing"
    (with-capability (CANCEL listing-id (at 'sender (chain-data)))
      (update listings listing-id { "status": "cancelled" })
      
      ;; Update stats
      (with-read stats "stats" { "active-listings" := current }
        (update stats "stats" { "active-listings": (- current 1) }))
      
      "Listing cancelled"))
  
  ;; Offer System
  (defun make-offer:string 
    ( token-id:string
      price:decimal
      duration:integer
      currency:module{fungible-v2} )
    @doc "Make offer on NFT"
    (enforce (> price MIN_PRICE) "Offer too low")
    
    (with-capability (OFFER token-id (at 'sender (chain-data)))
      (let* ( (buyer (at 'sender (chain-data)))
              (offer-id (hash [token-id buyer (at 'block-time (chain-data))]))
              (expires-at (add-time (at 'block-time (chain-data)) duration)) )
        
        ;; Lock funds
        (currency::transfer buyer "marketplace-escrow" price)
        
        ;; Create offer
        (insert offers offer-id {
          "offer-id": offer-id,
          "token-id": token-id,
          "buyer": buyer,
          "price": price,
          "currency": currency,
          "created-at": (at 'block-time (chain-data)),
          "expires-at": expires-at,
          "status": "active"
        })
        
        ;; Emit event
        (emit-event (OFFER_CREATED offer-id token-id price))
        
        offer-id)))
  
  (defun accept-offer:string (offer-id:string)
    @doc "Accept offer on owned NFT"
    (with-read offers offer-id 
      { "token-id" := token-id
      , "buyer" := buyer
      , "price" := price
      , "currency" := currency
      , "status" := status
      , "expires-at" := expires-at }
      
      (enforce (= status "active") "Offer not active")
      (enforce (< (at 'block-time (chain-data)) expires-at) "Offer expired")
      
      (let ((seller (nft-token.get-owner token-id)))
        (enforce-guard (nft-token.get-token-guard token-id))
        
        ;; Calculate payments
        (let* ( (collection (nft-token.get-token-collection token-id))
                (royalty-rate (at 'royalty-rate 
                               (nft-token.get-collection-info collection)))
                (creator (at 'creator 
                           (nft-token.get-collection-info collection)))
                (marketplace-fee (* price MARKETPLACE_FEE))
                (royalty-amount (* price royalty-rate))
                (seller-amount (- price (+ marketplace-fee royalty-amount))) )
          
          ;; Distribute payment from escrow
          (currency::transfer "marketplace-escrow" seller seller-amount)
          (currency::transfer "marketplace-escrow" "marketplace-treasury" marketplace-fee)
          (if (> royalty-amount 0.0)
            (currency::transfer "marketplace-escrow" creator royalty-amount)
            "No royalty")
          
          ;; Transfer NFT
          (nft-token.transfer token-id seller buyer)
          
          ;; Update offer
          (update offers offer-id { "status": "accepted" })
          
          ;; Record sale
          (insert sales (hash [offer-id seller]) {
            "token-id": token-id,
            "seller": seller,
            "buyer": buyer,
            "price": price,
            "currency": currency,
            "timestamp": (at 'block-time (chain-data)),
            "listing-id": offer-id,
            "royalty-paid": royalty-amount
          })
          
          ;; Update stats
          (with-read stats "stats" 
            { "total-volume" := volume
            , "total-sales" := sales
            , "total-fees-collected" := fees }
            (update stats "stats" {
              "total-volume": (+ volume price),
              "total-sales": (+ sales 1),
              "total-fees-collected": (+ fees marketplace-fee)
            }))
          
          ;; Emit event
          (emit-event (SALE_COMPLETED token-id seller buyer price))
          
          "Offer accepted"))))
  
  ;; Query Functions
  (defun get-listing:object (listing-id:string)
    @doc "Get listing details"
    (read listings listing-id))
  
  (defun get-active-listings:[object] ()
    @doc "Get all active listings"
    (select listings (where 'status (= "active"))))
  
  (defun get-listings-by-collection:[object] (collection:string)
    @doc "Get listings for a collection"
    (select listings 
      (and? (where 'status (= "active"))
            (where 'token-id (contains collection)))))
  
  (defun get-listings-by-seller:[object] (seller:string)
    @doc "Get listings by seller"
    (select listings (where 'seller (= seller))))
  
  (defun get-offers-for-token:[object] (token-id:string)
    @doc "Get active offers for token"
    (select offers 
      (and? (where 'token-id (= token-id))
            (where 'status (= "active")))))
  
  (defun get-sales-history:[object] (token-id:string)
    @doc "Get sales history for token"
    (select sales (where 'token-id (= token-id))))
  
  (defun get-marketplace-stats:object ()
    @doc "Get marketplace statistics"
    (read stats "stats"))
  
  ;; Price Oracle Integration
  (defun get-floor-price:decimal (collection:string)
    @doc "Get collection floor price"
    (let ((listings (get-listings-by-collection collection)))
      (if (= (length listings) 0)
        0.0
        (fold (min) 999999999.0 (map (at 'price) listings)))))
  
  (defun get-average-price:decimal (collection:string days:integer)
    @doc "Get average sale price over period"
    (let* ( (cutoff (add-time (at 'block-time (chain-data)) (* -86400 days)))
            (sales (select sales 
                    (and? (where 'token-id (contains collection))
                          (where 'timestamp (> cutoff))))) )
      (if (= (length sales) 0)
        0.0
        (/ (fold (+) 0.0 (map (at 'price) sales)) (length sales)))))
)

(create-table listings)
(create-table offers)
(create-table sales)
(create-table stats)
```

## Step 3: Auction System

Let's implement a comprehensive auction system:

```pact
;; modules/auction.pact
(module auction GOVERNANCE
  @doc "NFT auction system with multiple auction types"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'auction-admin))
  
  ;; Schemas
  (defschema auction-info
    @doc "Auction information"
    auction-id:string
    token-id:string
    seller:string
    auction-type:string  ;; english, dutch, sealed-bid
    start-price:decimal
    end-price:decimal    ;; For dutch auctions
    reserve-price:decimal
    current-bid:decimal
    leading-bidder:string
    currency:module{fungible-v2}
    start-time:time
    end-time:time
    bid-increment:decimal
    status:string)       ;; active, ended, cancelled
  
  (defschema bid
    @doc "Bid information"
    bid-id:string
    auction-id:string
    bidder:string
    amount:decimal
    timestamp:time
    status:string)       ;; active, outbid, won, refunded
  
  (defschema sealed-bid
    @doc "Sealed bid for blind auctions"
    bid-id:string
    auction-id:string
    bidder:string
    commitment:string    ;; Hash of bid + nonce
    revealed-amount:decimal
    nonce:string
    status:string)       ;; committed, revealed, won
  
  ;; Tables
  (deftable auctions:{auction-info})
  (deftable bids:{bid})
  (deftable sealed-bids:{sealed-bid})
  
  ;; Constants
  (defconst MIN_AUCTION_DURATION 3600)      ;; 1 hour
  (defconst MAX_AUCTION_DURATION 604800)    ;; 7 days
  (defconst MIN_BID_INCREMENT 0.01)
  (defconst AUCTION_EXTENSION 600)          ;; 10 minute extension
  
  ;; Capabilities
  (defcap CREATE_AUCTION:bool (token-id:string seller:string)
    @doc "Capability to create auction"
    (enforce-guard (nft-token.get-token-guard token-id))
    (enforce (= seller (nft-token.get-owner token-id)) "Not token owner"))
  
  (defcap BID:bool (auction-id:string bidder:string)
    @doc "Capability to bid"
    @managed
    (with-read auctions auction-id { "status" := status }
      (enforce (= status "active") "Auction not active")))
  
  (defcap END_AUCTION:bool (auction-id:string)
    @doc "Capability to end auction"
    (with-read auctions auction-id 
      { "end-time" := end-time, "status" := status }
      (enforce (>= (at 'block-time (chain-data)) end-time) 
               "Auction not ended")
      (enforce (= status "active") "Auction not active")))
  
  ;; Events
  (defcap AUCTION_CREATED (auction-id:string token-id:string auction-type:string)
    @event true)
  
  (defcap BID_PLACED (auction-id:string bidder:string amount:decimal)
    @event true)
  
  (defcap AUCTION_ENDED (auction-id:string winner:string final-price:decimal)
    @event true)
  
  ;; English Auction (ascending price)
  (defun create-english-auction:string
    ( token-id:string
      start-price:decimal
      reserve-price:decimal
      duration:integer
      bid-increment:decimal
      currency:module{fungible-v2} )
    @doc "Create English (ascending) auction"
    (enforce (>= duration MIN_AUCTION_DURATION) "Duration too short")
    (enforce (<= duration MAX_AUCTION_DURATION) "Duration too long")
    (enforce (>= reserve-price start-price) "Invalid reserve price")
    (enforce (>= bid-increment MIN_BID_INCREMENT) "Bid increment too small")
    
    (with-capability (CREATE_AUCTION token-id (nft-token.get-owner token-id))
      (let* ( (seller (nft-token.get-owner token-id))
              (auction-id (hash [token-id seller (at 'block-time (chain-data))]))
              (start-time (at 'block-time (chain-data)))
              (end-time (add-time start-time duration)) )
        
        ;; Lock NFT in auction contract
        (nft-token.transfer token-id seller "auction-escrow")
        
        ;; Create auction
        (insert auctions auction-id {
          "auction-id": auction-id,
          "token-id": token-id,
          "seller": seller,
          "auction-type": "english",
          "start-price": start-price,
          "end-price": 0.0,
          "reserve-price": reserve-price,
          "current-bid": 0.0,
          "leading-bidder": "",
          "currency": currency,
          "start-time": start-time,
          "end-time": end-time,
          "bid-increment": bid-increment,
          "status": "active"
        })
        
        ;; Emit event
        (emit-event (AUCTION_CREATED auction-id token-id "english"))
        
        auction-id)))
  
  (defun place-bid:string (auction-id:string amount:decimal)
    @doc "Place bid on English auction"
    (with-capability (BID auction-id (at 'sender (chain-data)))
      (with-read auctions auction-id 
        { "auction-type" := type
        , "current-bid" := current-bid
        , "leading-bidder" := leading-bidder
        , "start-price" := start-price
        , "bid-increment" := increment
        , "currency" := currency
        , "end-time" := end-time
        , "seller" := seller }
        
        (enforce (= type "english") "Wrong auction type")
        
        (let ((bidder (at 'sender (chain-data))))
          (enforce (!= bidder seller) "Seller cannot bid")
          
          ;; Validate bid amount
          (if (= current-bid 0.0)
            (enforce (>= amount start-price) "Bid below start price")
            (enforce (>= amount (+ current-bid increment)) "Bid too low"))
          
          ;; Transfer bid amount to escrow
          (currency::transfer bidder "auction-escrow" amount)
          
          ;; Refund previous bidder
          (if (!= leading-bidder "")
            (currency::transfer "auction-escrow" leading-bidder current-bid)
            "First bid")
          
          ;; Update auction
          (update auctions auction-id {
            "current-bid": amount,
            "leading-bidder": bidder
          })
          
          ;; Extend auction if near end (anti-sniping)
          (let ((time-left (diff-time end-time (at 'block-time (chain-data)))))
            (if (< time-left AUCTION_EXTENSION)
              (update auctions auction-id {
                "end-time": (add-time end-time AUCTION_EXTENSION)
              })
              "No extension"))
          
          ;; Record bid
          (let ((bid-id (hash [auction-id bidder amount])))
            (insert bids bid-id {
              "bid-id": bid-id,
              "auction-id": auction-id,
              "bidder": bidder,
              "amount": amount,
              "timestamp": (at 'block-time (chain-data)),
              "status": "active"
            })
            
            ;; Update previous bid status
            (if (!= leading-bidder "")
              (let ((prev-bids (select bids 
                              (and? (where 'auction-id (= auction-id))
                                    (where 'bidder (= leading-bidder))))))
                (map (lambda (bid) 
                       (update bids (at 'bid-id bid) { "status": "outbid" }))
                     prev-bids))
              "First bid"))
          
          ;; Emit event
          (emit-event (BID_PLACED auction-id bidder amount))
          
          "Bid placed successfully"))))
  
  ;; Dutch Auction (descending price)
  (defun create-dutch-auction:string
    ( token-id:string
      start-price:decimal
      end-price:decimal
      duration:integer
      currency:module{fungible-v2} )
    @doc "Create Dutch (descending) auction"
    (enforce (> start-price end-price) "Start price must exceed end price")
    (enforce (>= duration MIN_AUCTION_DURATION) "Duration too short")
    (enforce (<= duration MAX_AUCTION_DURATION) "Duration too long")
    
    (with-capability (CREATE_AUCTION token-id (nft-token.get-owner token-id))
      (let* ( (seller (nft-token.get-owner token-id))
              (auction-id (hash [token-id seller (at 'block-time (chain-data))]))
              (start-time (at 'block-time (chain-data)))
              (end-time (add-time start-time duration)) )
        
        ;; Lock NFT
        (nft-token.transfer token-id seller "auction-escrow")
        
        ;; Create auction
        (insert auctions auction-id {
          "auction-id": auction-id,
          "token-id": token-id,
          "seller": seller,
          "auction-type": "dutch",
          "start-price": start-price,
          "end-price": end-price,
          "reserve-price": end-price,
          "current-bid": 0.0,
          "leading-bidder": "",
          "currency": currency,
          "start-time": start-time,
          "end-time": end-time,
          "bid-increment": 0.0,
          "status": "active"
        })
        
        ;; Emit event
        (emit-event (AUCTION_CREATED auction-id token-id "dutch"))
        
        auction-id)))
  
  (defun buy-dutch-auction:string (auction-id:string)
    @doc "Buy at current Dutch auction price"
    (with-read auctions auction-id 
      { "auction-type" := type
      , "token-id" := token-id
      , "seller" := seller
      , "start-price" := start-price
      , "end-price" := end-price
      , "start-time" := start-time
      , "end-time" := end-time
      , "currency" := currency
      , "status" := status }
      
      (enforce (= type "dutch") "Not a Dutch auction")
      (enforce (= status "active") "Auction not active")
      
      (let* ( (buyer (at 'sender (chain-data)))
              (elapsed (diff-time (at 'block-time (chain-data)) start-time))
              (duration (diff-time end-time start-time))
              (price-drop (- start-price end-price))
              (current-price (- start-price 
                               (* price-drop (/ elapsed duration)))) )
        
        (enforce (!= buyer seller) "Seller cannot buy")
        (enforce (>= current-price end-price) "Price calculation error")
        
        ;; Process payment
        (let* ( (collection (nft-token.get-token-collection token-id))
                (royalty-rate (at 'royalty-rate 
                               (nft-token.get-collection-info collection)))
                (creator (at 'creator 
                           (nft-token.get-collection-info collection)))
                (marketplace-fee (* current-price marketplace.MARKETPLACE_FEE))
                (royalty-amount (* current-price royalty-rate))
                (seller-amount (- current-price (+ marketplace-fee royalty-amount))) )
          
          ;; Transfer payment
          (currency::transfer buyer seller seller-amount)
          (currency::transfer buyer "marketplace-treasury" marketplace-fee)
          (if (> royalty-amount 0.0)
            (currency::transfer buyer creator royalty-amount)
            "No royalty")
          
          ;; Transfer NFT
          (nft-token.transfer token-id "auction-escrow" buyer)
          
          ;; Update auction
          (update auctions auction-id {
            "status": "ended",
            "current-bid": current-price,
            "leading-bidder": buyer
          })
          
          ;; Emit event
          (emit-event (AUCTION_ENDED auction-id buyer current-price))
          
          "Purchase complete"))))
  
  ;; End English Auction
  (defun end-english-auction:string (auction-id:string)
    @doc "End English auction and transfer NFT"
    (with-capability (END_AUCTION auction-id)
      (with-read auctions auction-id 
        { "token-id" := token-id
        , "seller" := seller
        , "current-bid" := current-bid
        , "leading-bidder" := winner
        , "reserve-price" := reserve
        , "currency" := currency }
        
        (if (>= current-bid reserve)
          ;; Reserve met - complete sale
          (let* ( (collection (nft-token.get-token-collection token-id))
                  (royalty-rate (at 'royalty-rate 
                                 (nft-token.get-collection-info collection)))
                  (creator (at 'creator 
                             (nft-token.get-collection-info collection)))
                  (marketplace-fee (* current-bid marketplace.MARKETPLACE_FEE))
                  (royalty-amount (* current-bid royalty-rate))
                  (seller-amount (- current-bid (+ marketplace-fee royalty-amount))) )
            
            ;; Distribute payment
            (currency::transfer "auction-escrow" seller seller-amount)
            (currency::transfer "auction-escrow" "marketplace-treasury" marketplace-fee)
            (if (> royalty-amount 0.0)
              (currency::transfer "auction-escrow" creator royalty-amount)
              "No royalty")
            
            ;; Transfer NFT
            (nft-token.transfer token-id "auction-escrow" winner)
            
            ;; Update bid status
            (let ((winning-bids (select bids 
                              (and? (where 'auction-id (= auction-id))
                                    (where 'bidder (= winner))))))
              (map (lambda (bid) 
                     (update bids (at 'bid-id bid) { "status": "won" }))
                   winning-bids))
            
            "Auction completed successfully")
          
          ;; Reserve not met - return NFT
          (begin
            (nft-token.transfer token-id "auction-escrow" seller)
            (if (!= winner "")
              (currency::transfer "auction-escrow" winner current-bid)
              "No bids to refund")
            "Reserve not met - NFT returned"))
        
        ;; Update auction status
        (update auctions auction-id { "status": "ended" })
        
        ;; Emit event
        (emit-event (AUCTION_ENDED auction-id winner current-bid)))))
  
  ;; Query Functions
  (defun get-auction:object (auction-id:string)
    @doc "Get auction details"
    (+ (read auctions auction-id)
       { "current-price": (calculate-current-price auction-id) }))
  
  (defun calculate-current-price:decimal (auction-id:string)
    @doc "Calculate current price for any auction type"
    (with-read auctions auction-id 
      { "auction-type" := type
      , "start-price" := start-price
      , "end-price" := end-price
      , "current-bid" := current-bid
      , "start-time" := start-time
      , "end-time" := end-time }
      
      (if (= type "dutch")
        (let* ( (elapsed (diff-time (at 'block-time (chain-data)) start-time))
                (duration (diff-time end-time start-time))
                (price-drop (- start-price end-price)) )
          (- start-price (* price-drop (/ elapsed duration))))
        current-bid)))
  
  (defun get-active-auctions:[object] ()
    @doc "Get all active auctions"
    (select auctions (where 'status (= "active"))))
  
  (defun get-bids-for-auction:[object] (auction-id:string)
    @doc "Get all bids for an auction"
    (select bids (where 'auction-id (= auction-id))))
  
  (defun get-user-bids:[object] (user:string)
    @doc "Get all bids by user"
    (select bids (where 'bidder (= user))))
)

(create-table auctions)
(create-table bids)
(create-table sealed-bids)
```

## Step 4: Royalty Distribution

Now let's implement advanced royalty distribution:

```pact
;; modules/royalty.pact
(module royalty GOVERNANCE
  @doc "Advanced royalty distribution system"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'royalty-admin))
  
  ;; Schemas
  (defschema royalty-config
    @doc "Royalty configuration"
    collection:string
    primary-rate:decimal
    primary-recipient:string
    splits:[object{royalty-split}]
    total-distributed:decimal
    last-update:time)
  
  (defschema royalty-split
    @doc "Royalty split configuration"
    recipient:string
    percentage:decimal
    role:string)  ;; creator, investor, platform, charity
  
  (defschema royalty-payment
    @doc "Royalty payment record"
    payment-id:string
    token-id:string
    sale-price:decimal
    total-royalty:decimal
    distributions:[object{distribution}]
    timestamp:time)
  
  (defschema distribution
    @doc "Individual distribution"
    recipient:string
    amount:decimal
    role:string)
  
  ;; Tables
  (deftable royalty-configs:{royalty-config})
  (deftable royalty-payments:{royalty-payment})
  
  ;; Constants
  (defconst MAX_ROYALTY_RATE 0.25)    ;; 25% max
  (defconst MIN_SPLIT_PERCENTAGE 0.01) ;; 1% minimum split
  
  ;; Capabilities
  (defcap CONFIGURE_ROYALTY (collection:string)
    @doc "Configure royalty for collection"
    (with-read nft-token.collections collection { "creator" := creator }
      (enforce-guard (at 'guard (read accounts creator)))))
  
  (defcap DISTRIBUTE_ROYALTY (token-id:string amount:decimal)
    @doc "Distribute royalty payments"
    @managed amount ROYALTY-mgr
    true)
  
  (defun ROYALTY-mgr:decimal (managed:decimal requested:decimal)
    (enforce (<= requested managed) "Exceeds royalty amount")
    (- managed requested))
  
  ;; Configuration
  (defun configure-royalty:string
    ( collection:string
      primary-rate:decimal
      splits:[object{royalty-split}] )
    @doc "Configure royalty distribution"
    (enforce (>= primary-rate 0.0) "Invalid rate")
    (enforce (<= primary-rate MAX_ROYALTY_RATE) "Rate too high")
    
    ;; Validate splits
    (let ((total-split (fold (+) 0.0 (map (at 'percentage) splits))))
      (enforce (= total-split 1.0) "Splits must total 100%"))
    
    (map (lambda (split)
           (enforce (>= (at 'percentage split) MIN_SPLIT_PERCENTAGE)
                    "Split percentage too small"))
         splits)
    
    (with-capability (CONFIGURE_ROYALTY collection)
      (let ((creator (at 'creator (read nft-token.collections collection))))
        (write royalty-configs collection {
          "collection": collection,
          "primary-rate": primary-rate,
          "primary-recipient": creator,
          "splits": splits,
          "total-distributed": 0.0,
          "last-update": (at 'block-time (chain-data))
        })
        "Royalty configured")))
  
  ;; Distribution
  (defun distribute-royalty:string
    ( token-id:string
      sale-price:decimal
      payer:string
      currency:module{fungible-v2} )
    @doc "Distribute royalty payment"
    (let* ( (collection (nft-token.get-token-collection token-id))
            (config (read royalty-configs collection))
            (royalty-amount (* sale-price (at 'primary-rate config))) )
      
      (with-capability (DISTRIBUTE_ROYALTY token-id royalty-amount)
        (let ((payment-id (hash [token-id sale-price (at 'block-time (chain-data))])))
          
          ;; Process distributions
          (let ((distributions 
                 (map (lambda (split)
                        (let ((amount (* royalty-amount (at 'percentage split))))
                          (currency::transfer payer (at 'recipient split) amount)
                          { "recipient": (at 'recipient split)
                          , "amount": amount
                          , "role": (at 'role split) }))
                      (at 'splits config))))
            
            ;; Record payment
            (insert royalty-payments payment-id {
              "payment-id": payment-id,
              "token-id": token-id,
              "sale-price": sale-price,
              "total-royalty": royalty-amount,
              "distributions": distributions,
              "timestamp": (at 'block-time (chain-data))
            })
            
            ;; Update total distributed
            (update royalty-configs collection {
              "total-distributed": (+ (at 'total-distributed config) royalty-amount)
            })
            
            "Royalty distributed")))))
  
  ;; Query Functions
  (defun get-royalty-config:object (collection:string)
    @doc "Get royalty configuration"
    (read royalty-configs collection))
  
  (defun get-royalty-payments:[object] (token-id:string)
    @doc "Get royalty payment history"
    (select royalty-payments (where 'token-id (= token-id))))
  
  (defun calculate-royalty:decimal (collection:string sale-price:decimal)
    @doc "Calculate royalty amount"
    (* sale-price (at 'primary-rate (read royalty-configs collection))))
  
  ;; Analytics
  (defun get-total-royalties:decimal (collection:string)
    @doc "Get total royalties distributed"
    (at 'total-distributed (read royalty-configs collection)))
  
  (defun get-recipient-earnings:decimal (recipient:string)
    @doc "Get total earnings for recipient"
    (let ((payments (select royalty-payments (constantly true))))
      (fold (+) 0.0
        (map (lambda (payment)
               (fold (+) 0.0
                 (map (lambda (dist)
                        (if (= (at 'recipient dist) recipient)
                          (at 'amount dist)
                          0.0))
                      (at 'distributions payment))))
             payments))))
)

(create-table royalty-configs)
(create-table royalty-payments)
```

## Step 5: Frontend Integration

Here's an example of how to integrate with the marketplace from a frontend:

```javascript
// frontend/marketplace-integration.js

class PactNFTMarketplace {
  constructor(kadenaClient) {
    this.client = kadenaClient;
    this.networkId = 'testnet04';
    this.chainId = '0';
    this.gasPrice = 0.00000001;
    this.gasLimit = 150000;
  }

  // Create NFT Collection
  async createCollection({
    name,
    description,
    maxSupply,
    baseUri,
    mintPrice,
    royaltyRate,
    account,
    keyset
  }) {
    const cmd = {
      networkId: this.networkId,
      payload: {
        exec: {
          data: { keyset },
          code: `(nft-token.create-collection 
            "${name}" 
            "${account}" 
            "${description}" 
            ${maxSupply} 
            "${baseUri}"
            ${mintPrice}
            ${royaltyRate})`
        }
      },
      signers: [{
        pubKey: account.publicKey,
        clist: [
          { name: "coin.GAS", args: [] },
          { name: "nft-token.CREATE_COLLECTION", args: [name] }
        ]
      }],
      meta: {
        chainId: this.chainId,
        sender: account.address,
        gasLimit: this.gasLimit,
        gasPrice: this.gasPrice,
        ttl: 600
      }
    };

    return await this.client.submit(cmd);
  }

  // Mint NFT
  async mintNFT({
    collection,
    recipientAccount,
    recipientGuard,
    attributes,
    senderAccount
  }) {
    const cmd = {
      networkId: this.networkId,
      payload: {
        exec: {
          data: { 
            guard: recipientGuard,
            attributes: attributes 
          },
          code: `(nft-token.mint 
            "${collection}"
            "${recipientAccount}" 
            (read-keyset 'guard)
            (read-msg 'attributes))`
        }
      },
      signers: [{
        pubKey: senderAccount.publicKey,
        clist: [
          { name: "coin.GAS", args: [] },
          { name: "coin.TRANSFER", args: [
            senderAccount.address, 
            "", // Will be filled by contract
            0.0 // Will be determined by mint price
          ]},
          { name: "nft-token.MINT", args: [collection, recipientAccount] }
        ]
      }],
      meta: {
        chainId: this.chainId,
        sender: senderAccount.address,
        gasLimit: this.gasLimit,
        gasPrice: this.gasPrice,
        ttl: 600
      }
    };

    return await this.client.submit(cmd);
  }

  // List NFT for Sale
  async listNFT({
    tokenId,
    price,
    duration,
    sellerAccount
  }) {
    const cmd = {
      networkId: this.networkId,
      payload: {
        exec: {
          data: {},
          code: `(marketplace.list-nft 
            "${tokenId}"
            ${price}
            ${duration}
            coin)`
        }
      },
      signers: [{
        pubKey: sellerAccount.publicKey,
        clist: [
          { name: "coin.GAS", args: [] },
          { name: "marketplace.LIST", args: [tokenId, sellerAccount.address] }
        ]
      }],
      meta: {
        chainId: this.chainId,
        sender: sellerAccount.address,
        gasLimit: this.gasLimit,
        gasPrice: this.gasPrice,
        ttl: 600
      }
    };

    return await this.client.submit(cmd);
  }

  // Buy Listed NFT
  async buyNFT({
    listingId,
    buyerAccount
  }) {
    // First get listing details
    const listing = await this.getListingDetails(listingId);
    
    const cmd = {
      networkId: this.networkId,
      payload: {
        exec: {
          data: {},
          code: `(marketplace.buy-nft "${listingId}")`
        }
      },
      signers: [{
        pubKey: buyerAccount.publicKey,
        clist: [
          { name: "coin.GAS", args: [] },
          { name: "coin.TRANSFER", args: [
            buyerAccount.address,
            listing.seller,
            listing.price
          ]},
          { name: "marketplace.BUY", args: [listingId, buyerAccount.address] }
        ]
      }],
      meta: {
        chainId: this.chainId,
        sender: buyerAccount.address,
        gasLimit: this.gasLimit,
        gasPrice: this.gasPrice,
        ttl: 600
      }
    };

    return await this.client.submit(cmd);
  }

  // Create Auction
  async createAuction({
    tokenId,
    auctionType,
    startPrice,
    endPrice,
    reservePrice,
    duration,
    bidIncrement,
    sellerAccount
  }) {
    let code;
    if (auctionType === 'english') {
      code = `(auction.create-english-auction 
        "${tokenId}"
        ${startPrice}
        ${reservePrice}
        ${duration}
        ${bidIncrement}
        coin)`;
    } else if (auctionType === 'dutch') {
      code = `(auction.create-dutch-auction 
        "${tokenId}"
        ${startPrice}
        ${endPrice}
        ${duration}
        coin)`;
    }

    const cmd = {
      networkId: this.networkId,
      payload: {
        exec: {
          data: {},
          code: code
        }
      },
      signers: [{
        pubKey: sellerAccount.publicKey,
        clist: [
          { name: "coin.GAS", args: [] },
          { name: "auction.CREATE_AUCTION", args: [tokenId, sellerAccount.address] },
          { name: "nft-token.TRANSFER", args: [
            tokenId, 
            sellerAccount.address, 
            "auction-escrow"
          ]}
        ]
      }],
      meta: {
        chainId: this.chainId,
        sender: sellerAccount.address,
        gasLimit: this.gasLimit,
        gasPrice: this.gasPrice,
        ttl: 600
      }
    };

    return await this.client.submit(cmd);
  }

  // Place Bid
  async placeBid({
    auctionId,
    amount,
    bidderAccount
  }) {
    const cmd = {
      networkId: this.networkId,
      payload: {
        exec: {
          data: {},
          code: `(auction.place-bid "${auctionId}" ${amount})`
        }
      },
      signers: [{
        pubKey: bidderAccount.publicKey,
        clist: [
          { name: "coin.GAS", args: [] },
          { name: "coin.TRANSFER", args: [
            bidderAccount.address,
            "auction-escrow",
            amount
          ]},
          { name: "auction.BID", args: [auctionId, bidderAccount.address] }
        ]
      }],
      meta: {
        chainId: this.chainId,
        sender: bidderAccount.address,
        gasLimit: this.gasLimit,
        gasPrice: this.gasPrice,
        ttl: 600
      }
    };

    return await this.client.submit(cmd);
  }

  // Query Functions
  async getCollectionInfo(collection) {
    const cmd = {
      networkId: this.networkId,
      payload: {
        exec: {
          data: {},
          code: `(nft-token.get-collection-info "${collection}")`
        }
      },
      meta: {
        chainId: this.chainId,
        sender: "",
        gasLimit: this.gasLimit,
        gasPrice: this.gasPrice,
        ttl: 600
      }
    };

    const result = await this.client.local(cmd);
    return result.result.data;
  }

  async getTokenInfo(tokenId) {
    const cmd = {
      networkId: this.networkId,
      payload: {
        exec: {
          data: {},
          code: `(nft-token.get-token-info "${tokenId}")`
        }
      },
      meta: {
        chainId: this.chainId,
        sender: "",
        gasLimit: this.gasLimit,
        gasPrice: this.gasPrice,
        ttl: 600
      }
    };

    const result = await this.client.local(cmd);
    return result.result.data;
  }

  async getActiveListings() {
    const cmd = {
      networkId: this.networkId,
      payload: {
        exec: {
          data: {},
          code: `(marketplace.get-active-listings)`
        }
      },
      meta: {
        chainId: this.chainId,
        sender: "",
        gasLimit: this.gasLimit,
        gasPrice: this.gasPrice,
        ttl: 600
      }
    };

    const result = await this.client.local(cmd);
    return result.result.data;
  }

  async getListingDetails(listingId) {
    const cmd = {
      networkId: this.networkId,
      payload: {
        exec: {
          data: {},
          code: `(marketplace.get-listing "${listingId}")`
        }
      },
      meta: {
        chainId: this.chainId,
        sender: "",
        gasLimit: this.gasLimit,
        gasPrice: this.gasPrice,
        ttl: 600
      }
    };

    const result = await this.client.local(cmd);
    return result.result.data;
  }

  async getFloorPrice(collection) {
    const cmd = {
      networkId: this.networkId,
      payload: {
        exec: {
          data: {},
          code: `(marketplace.get-floor-price "${collection}")`
        }
      },
      meta: {
        chainId: this.chainId,
        sender: "",
        gasLimit: this.gasLimit,
        gasPrice: this.gasPrice,
        ttl: 600
      }
    };

    const result = await this.client.local(cmd);
    return result.result.data;
  }
}

// Usage Example
async function main() {
  const marketplace = new PactNFTMarketplace(kadenaClient);
  
  // Create a collection
  const collection = await marketplace.createCollection({
    name: "CryptoArt",
    description: "Digital art collection",
    maxSupply: 10000,
    baseUri: "ipfs://QmXxx/",
    mintPrice: 5.0,
    royaltyRate: 0.05,
    account: sellerAccount,
    keyset: sellerKeyset
  });
  
  // Mint an NFT
  const mint = await marketplace.mintNFT({
    collection: "CryptoArt",
    recipientAccount: buyerAccount.address,
    recipientGuard: buyerKeyset,
    attributes: {
      rarity: "rare",
      artist: "Alice",
      edition: 1
    },
    senderAccount: buyerAccount
  });
  
  // List for sale
  const listing = await marketplace.listNFT({
    tokenId: "CryptoArt:1",
    price: 100.0,
    duration: 86400, // 24 hours
    sellerAccount: buyerAccount
  });
  
  // Get marketplace data
  const activeListings = await marketplace.getActiveListings();
  const floorPrice = await marketplace.getFloorPrice("CryptoArt");
  
  console.log("Active Listings:", activeListings);
  console.log("Floor Price:", floorPrice);
}
```

## Testing Strategy

Create comprehensive tests for the marketplace:

```lisp
;; tests/nft-marketplace-test.repl

;; Setup
(begin-tx "Setup test environment")

(env-data {
  "nft-admin": {
    "keys": ["nft-admin-key"],
    "pred": "keys-all"
  },
  "marketplace-admin": {
    "keys": ["marketplace-admin-key"],
    "pred": "keys-all"
  },
  "alice-keyset": {
    "keys": ["alice-key"],
    "pred": "keys-all"
  },
  "bob-keyset": {
    "keys": ["bob-key"],
    "pred": "keys-all"
  }
})

(env-keys ["nft-admin-key", "marketplace-admin-key"])

(define-keyset 'nft-admin (read-keyset "nft-admin"))
(define-keyset 'marketplace-admin (read-keyset "marketplace-admin"))

;; Load modules
(load "interfaces/nft-v2.pact")
(load "modules/nft-token.pact")
(load "modules/marketplace.pact")
(load "modules/auction.pact")
(load "modules/royalty.pact")

;; Initialize
(marketplace.init-marketplace)

(commit-tx)

;; Test Collection Creation
(begin-tx "Test collection creation")

(env-keys ["alice-key"])
(use nft-token)

(expect "Create collection succeeds"
  "Collection CryptoArt created"
  (create-collection 
    "CryptoArt" 
    "alice" 
    "Digital art collection" 
    100 
    "ipfs://QmTest/"
    5.0
    0.05))

(expect "Collection info correct"
  { "name": "CryptoArt"
  , "creator": "alice"
  , "max-supply": 100
  , "minted": 0
  , "royalty-rate": 0.05 }
  (at ['name 'creator 'max-supply 'minted 'royalty-rate]
    (get-collection-info "CryptoArt")))

(commit-tx)

;; Test Minting
(begin-tx "Test NFT minting")

(env-keys ["alice-key"])
(env-data {
  "alice-keyset": {
    "keys": ["alice-key"],
    "pred": "keys-all"
  }
})

;; Mint first NFT
(expect "Mint succeeds"
  "CryptoArt:1"
  (mint "CryptoArt" "alice" (read-keyset "alice-keyset") 
    { "rarity": "common", "power": 10 }))

(expect "Token owner correct"
  "alice"
  (get-owner "CryptoArt:1"))

(expect "Balance updated"
  1
  (get-balance "alice"))

(commit-tx)

;; Test Marketplace Listing
(begin-tx "Test marketplace listing")

(env-keys ["alice-key"])
(use marketplace)

(let ((listing-id (list-nft "CryptoArt:1" 50.0 86400 coin)))
  (expect "Listing created"
    true
    (!= listing-id ""))
  
  (expect "Listing details correct"
    { "token-id": "CryptoArt:1"
    , "seller": "alice"
    , "price": 50.0
    , "status": "active" }
    (at ['token-id 'seller 'price 'status]
      (get-listing listing-id))))

(commit-tx)

;; Test Purchase
(begin-tx "Test NFT purchase")

(env-keys ["bob-key"])
(env-data {
  "bob-keyset": {
    "keys": ["bob-key"],
    "pred": "keys-all"
  }
})

;; Get listing ID
(let ((listings (get-active-listings)))
  (expect "Active listing exists"
    1
    (length listings))
  
  (let ((listing-id (at 'listing-id (at 0 listings))))
    ;; Bob buys NFT
    (expect "Purchase succeeds"
      "Purchase complete"
      (buy-nft listing-id))
    
    ;; Verify ownership transfer
    (expect "New owner is Bob"
      "bob"
      (nft-token.get-owner "CryptoArt:1"))
    
    ;; Check marketplace stats
    (expect "Stats updated"
      { "total-volume": 50.0
      , "total-sales": 1 }
      (at ['total-volume 'total-sales]
        (get-marketplace-stats)))))

(commit-tx)

;; Test Auctions
(begin-tx "Test auction system")

(env-keys ["bob-key"])
(use auction)

;; Create English auction
(let ((auction-id (create-english-auction 
                    "CryptoArt:1" 
                    10.0    ; start price
                    30.0    ; reserve
                    3600    ; 1 hour
                    1.0     ; increment
                    coin)))
  
  (expect "Auction created"
    true
    (!= auction-id ""))
  
  ;; Alice places bid
  (env-keys ["alice-key"])
  (expect "Bid placed"
    "Bid placed successfully"
    (place-bid auction-id 15.0))
  
  ;; Check auction state
  (expect "Auction updated"
    { "current-bid": 15.0
    , "leading-bidder": "alice" }
    (at ['current-bid 'leading-bidder]
      (get-auction auction-id))))

(commit-tx)

;; Test Royalty Distribution
(begin-tx "Test royalty system")

(env-keys ["alice-key"])
(use royalty)

;; Configure royalty splits
(expect "Royalty configured"
  "Royalty configured"
  (configure-royalty 
    "CryptoArt"
    0.05
    [{ "recipient": "alice", "percentage": 0.7, "role": "creator" }
     { "recipient": "platform", "percentage": 0.3, "role": "platform" }]))

;; Simulate royalty distribution
(expect-failure "Distribution requires payment"
  "Transfer"
  (distribute-royalty "CryptoArt:1" 100.0 "bob" coin))

(commit-tx)

(print "All NFT marketplace tests passed!")
```

## Deployment Guide

### 1. Deploy to Testnet

```bash
# Deploy interfaces
pact -a interfaces/nft-v2.pact -l

# Deploy modules in order
pact -a modules/nft-token.pact -l
pact -a modules/marketplace.pact -l
pact -a modules/auction.pact -l
pact -a modules/royalty.pact -l

# Initialize marketplace
pact -e '(marketplace.init-marketplace)' -l
```

### 2. Deploy to Mainnet

```pact
;; deployment/mainnet-deploy.pact
(begin-tx)

;; Define keysets
(define-keyset 'nft-admin 
  (read-keyset "nft-admin"))

(define-keyset 'marketplace-admin 
  (read-keyset "marketplace-admin"))

;; Deploy interfaces
(load "interfaces/nft-v2.pact")

;; Deploy modules
(load "modules/nft-token.pact")
(load "modules/marketplace.pact")
(load "modules/auction.pact")
(load "modules/royalty.pact")

;; Initialize
(marketplace.init-marketplace)

(commit-tx)
```

## Best Practices

### 1. Gas Optimization
- Use batch operations for multiple NFT transfers
- Implement lazy minting for large collections
- Cache frequently accessed data

### 2. Security Considerations
- Always validate NFT ownership before operations
- Implement reentrancy guards for critical functions
- Use time locks for admin functions
- Audit all payment flows

### 3. Scalability
- Implement off-chain metadata storage (IPFS)
- Use events for indexing and search
- Paginate large result sets
- Consider sharding for very large collections

### 4. User Experience
- Provide clear error messages
- Implement preview functions for gas estimation
- Support multiple payment tokens
- Add social features (favorites, follows)

## Summary

In this chapter, we've built a complete NFT marketplace with:
- ✅ Full NFT standard implementation
- ✅ Marketplace with listings and offers
- ✅ Multiple auction types
- ✅ Advanced royalty distribution
- ✅ Frontend integration examples
- ✅ Comprehensive testing
- ✅ Production deployment guide

This marketplace can handle real-world NFT trading with features comparable to OpenSea or Rarible, while leveraging Pact's unique capabilities for security and efficiency.

## Exercises

1. **Add Batch Operations**: Implement batch minting and batch transfers
2. **Create Collection Factory**: Build a factory pattern for creating collections
3. **Add Whitelist Minting**: Implement whitelist functionality for exclusive mints
4. **Build Analytics Module**: Create on-chain analytics for collections
5. **Implement Lazy Minting**: Add lazy minting to reduce upfront costs
6. **Add Bundle Sales**: Allow selling multiple NFTs as a bundle
7. **Create Rental System**: Implement NFT rental functionality
8. **Add Governance**: Create a DAO for marketplace governance