;; modules/nft-basic.pact
;; Simplified NFT implementation for demonstration
(module nft-basic GOVERNANCE
  @doc "Basic NFT implementation"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'nft-admin))
  
  ;; Schemas
  (defschema nft
    owner:string
    uri:string
    collection:string)
  
  (defschema collection-info
    creator:string
    max-supply:integer
    minted:integer
    base-uri:string)
  
  ;; Tables
  (deftable nfts:{nft})
  (deftable collections:{collection-info})
  
  ;; Capabilities
  (defcap OWNER (token-id:string owner:string)
    @doc "Owner capability"
    (with-read nfts token-id { "owner" := current-owner }
      (enforce (= current-owner owner) "Not the owner")))
  
  ;; Functions
  (defun create-collection:string (name:string max-supply:integer base-uri:string)
    @doc "Create a new collection"
    (insert collections name {
      "creator": (at 'sender (chain-data)),
      "max-supply": max-supply,
      "minted": 0,
      "base-uri": base-uri
    })
    (format "Collection {} created" [name]))
  
  (defun mint:string (collection:string recipient:string)
    @doc "Mint new NFT"
    (with-read collections collection 
      { "minted" := minted, "max-supply" := max-supply, "base-uri" := base-uri }
      (enforce (< minted max-supply) "Collection fully minted")
      
      (let ((token-id (format "{}:{}" [collection (+ minted 1)])))
        (insert nfts token-id {
          "owner": recipient,
          "uri": (format "{}/{}.json" [base-uri (+ minted 1)]),
          "collection": collection
        })
        
        (update collections collection { "minted": (+ minted 1) })
        token-id)))
  
  (defun transfer:string (token-id:string new-owner:string)
    @doc "Transfer NFT"
    (with-read nfts token-id { "owner" := current-owner }
      (with-capability (OWNER token-id current-owner)
        (update nfts token-id { "owner": new-owner })
        "Transfer complete")))
  
  (defun get-owner:string (token-id:string)
    @doc "Get NFT owner"
    (at 'owner (read nfts token-id)))
  
  (defun get-nft:object (token-id:string)
    @doc "Get NFT details"
    (read nfts token-id))
  
  (defun get-collection:object (name:string)
    @doc "Get collection info"
    (read collections name))
)

(create-table nfts)
(create-table collections)