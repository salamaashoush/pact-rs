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