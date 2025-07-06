;; modules/nft-token-simplified.pact
;; Simplified version for testing
(module nft-token GOVERNANCE
  @doc "NFT token implementation"
  
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
    mint-number:integer)
  
  (defschema collection
    @doc "NFT collection information"
    name:string
    creator:string
    description:string
    max-supply:integer
    minted:integer
    base-uri:string
    mint-price:decimal
    royalty-rate:decimal
    status:string)
  
  (defschema account-balance
    @doc "Track NFT ownership count"
    account:string
    balance:integer
    token-ids:[string])
  
  ;; Tables
  (deftable tokens:{token})
  (deftable collections:{collection})
  (deftable balances:{account-balance})
  
  ;; Collection Management
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
      "royalty-rate": royalty-rate,
      "status": "active"
    })
    (format "Collection {} created" [name]))
  
  ;; Minting
  (defcap MINT:bool (token-id:string account:string)
    @doc "Minting capability"
    @event
    true)
  
  (defun mint:string 
    ( collection:string
      account:string 
      guard:guard )
    @doc "Mint new NFT in collection"
    (with-read collections collection 
      { "minted" := minted
      , "max-supply" := max-supply
      , "base-uri" := base-uri
      , "mint-price" := price }
      
      (enforce (< minted max-supply) "Collection fully minted")
      
      ;; Generate token ID
      (let ( (token-id (format "{}:{}" [collection (+ minted 1)]))
             (uri (format "{}/{}.json" [base-uri (+ minted 1)])) )
        
        ;; Create token
        (insert tokens token-id {
          "id": token-id,
          "uri": uri,
          "owner": account,
          "guard": guard,
          "collection": collection,
          "created-at": (at 'block-time (chain-data)),
          "mint-number": (+ minted 1)
        })
        
        ;; Update collection
        (update collections collection {
          "minted": (+ minted 1)
        })
        
        ;; Update balance
        (with-default-read balances account
          { "balance": 0, "token-ids": [] }
          { "balance" := current-balance, "token-ids" := tokens-list }
          
          (write balances account {
            "account": account,
            "balance": (+ current-balance 1),
            "token-ids": (+ tokens-list [token-id])
          }))
        
        ;; Emit event would go here
        ;; (emit-event (MINT_EVENT token-id account))
        
        token-id)))
  
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
      ;; Update token
      (update tokens token-id {
        "owner": receiver,
        "guard": (create-user-guard (create-capability-guard (TRANSFER token-id receiver receiver)))
      })
      
      ;; Update sender balance
      (with-read balances sender 
        { "balance" := sender-balance, "token-ids" := sender-tokens }
        (update balances sender {
          "balance": (- sender-balance 1),
          "token-ids": (filter (!= token-id) sender-tokens)
        }))
      
      ;; Update receiver balance
      (with-default-read balances receiver
        { "balance": 0, "token-ids": [] }
        { "balance" := receiver-balance, "token-ids" := receiver-tokens }
        
        (write balances receiver {
          "account": receiver,
          "balance": (+ receiver-balance 1),
          "token-ids": (+ receiver-tokens [token-id])
        }))
      
      ;; Emit event would go here
      ;; (emit-event (TRANSFER_EVENT token-id sender receiver))
      
      "Transfer complete"))
  
  ;; Query Functions
  (defun get-owner:string (token-id:string)
    @doc "Get token owner"
    (at 'owner (read tokens token-id)))
  
  (defun get-token-info:object (token-id:string)
    @doc "Get token metadata"
    (read tokens token-id))
  
  (defun get-balance:integer (account:string)
    @doc "Get account NFT count"
    (with-default-read balances account
      { "balance": 0 }
      { "balance" := balance }
      balance))
  
  (defun get-collection-info:object (collection:string)
    @doc "Get collection information"
    (read collections collection))
  
  (defun get-token-collection:string (token-id:string)
    @doc "Extract collection from token ID"
    (at 'collection (read tokens token-id)))
  
  (defun get-token-guard:guard (token-id:string)
    @doc "Get token guard"
    (at 'guard (read tokens token-id)))
)

(create-table tokens)
(create-table collections)
(create-table balances)