;; price-oracle.pact
;; Price oracle for DeFi protocol
(module price-oracle GOVERNANCE
  @doc "Price feed oracle for asset valuations"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Schemas
  (defschema price-feed
    @doc "Asset price information"
    asset:string
    price:decimal            ;; Price in USD
    last-update:time
    source:string           ;; Price source identifier
    confidence:decimal)     ;; Price confidence (0-1)
  
  (defschema price-history
    @doc "Historical price data"
    asset:string
    price:decimal
    timestamp:time)
  
  ;; Tables
  (deftable price-feeds:{price-feed})
  (deftable price-history-table:{price-history})
  
  ;; Constants
  (defconst MAX_PRICE_AGE 300)  ;; 5 minutes in seconds
  (defconst MIN_CONFIDENCE 0.95)
  
  ;; Capabilities
  (defcap UPDATE_PRICE (source:string asset:string)
    @doc "Capability to update price"
    (enforce-guard (at 'guard (read authorized-sources source))))
  
  ;; Price Updates
  (defun update-price:string 
    ( source:string 
      asset:string 
      price:decimal 
      confidence:decimal )
    @doc "Update asset price"
    (with-capability (UPDATE_PRICE source asset)
      (enforce (> price 0.0) "Price must be positive")
      (enforce (and (>= confidence 0.0) (<= confidence 1.0)) 
              "Invalid confidence")
      
      ;; Store current price in history
      (with-default-read price-feeds asset
        { "price": 0.0 }
        { "price" := old-price }
        
        (if (> old-price 0.0)
          (let ((history-key (format "{}:{}" 
                                    [asset (at 'block-time (chain-data))])))
            (insert price-history-table history-key {
              "asset": asset,
              "price": old-price,
              "timestamp": (at 'block-time (chain-data))
            }))
          "No previous price"))
      
      ;; Update price feed
      (write price-feeds asset {
        "asset": asset,
        "price": price,
        "last-update": (at 'block-time (chain-data)),
        "source": source,
        "confidence": confidence
      })
      
      (format "Price updated: {} = ${}" [asset price])))
  
  ;; Price Queries
  (defun get-price:decimal (asset:string)
    @doc "Get current asset price"
    (with-read price-feeds asset
      { "price" := price
      , "last-update" := last-update
      , "confidence" := confidence }
      
      ;; Check price age
      (let* ((now (at 'block-time (chain-data)))
             (age (diff-time now last-update)))
        
        (enforce (< age MAX_PRICE_AGE) "Price too old")
        (enforce (>= confidence MIN_CONFIDENCE) "Price confidence too low")
        
        price)))
  
  (defun get-price-safe:object (asset:string)
    @doc "Get price with metadata"
    (with-default-read price-feeds asset
      { "price": 0.0
      , "last-update": (at 'block-time (chain-data))
      , "confidence": 0.0 }
      { "price" := price
      , "last-update" := last-update
      , "confidence" := confidence }
      
      (let* ((now (at 'block-time (chain-data)))
             (age (diff-time now last-update))
             (is-valid (and (< age MAX_PRICE_AGE) 
                           (>= confidence MIN_CONFIDENCE)
                           (> price 0.0))))
        
        { "price": price
        , "valid": is-valid
        , "age": age
        , "confidence": confidence })))
  
  ;; Aggregation Functions
  (defun get-twap:decimal (asset:string hours:integer)
    @doc "Get time-weighted average price"
    (let* ((cutoff (add-time (at 'block-time (chain-data)) 
                            (- (* hours 3600))))
           (history (select price-history-table 
                           (and? (where 'asset (= asset))
                                (where 'timestamp (> cutoff)))))
           (prices (map (at 'price) history)))
      
      (if (> (length prices) 0)
        (/ (fold (+) 0.0 prices) (length prices))
        (get-price asset))))
  
  ;; Multi-asset Functions
  (defun get-prices:[object] (assets:[string])
    @doc "Get multiple asset prices"
    (map (lambda (asset)
           { "asset": asset
           , "price": (get-price asset) })
         assets))
  
  (defun calculate-value:decimal (asset:string amount:decimal)
    @doc "Calculate USD value of asset amount"
    (* amount (get-price asset)))
  
  (defun calculate-values:object (positions:[object])
    @doc "Calculate values for multiple positions"
    (let ((values (map (lambda (pos)
                        (let ((asset (at 'asset pos))
                              (amount (at 'amount pos)))
                          (* amount (get-price asset))))
                      positions)))
      { "positions": positions
      , "values": values
      , "total": (fold (+) 0.0 values) }))
  
  ;; Exchange Rate Functions
  (defun get-exchange-rate:decimal (from-asset:string to-asset:string)
    @doc "Get exchange rate between assets"
    (/ (get-price from-asset) (get-price to-asset)))
  
  (defun convert-amount:decimal 
    ( from-asset:string 
      to-asset:string 
      amount:decimal )
    @doc "Convert amount between assets"
    (* amount (get-exchange-rate from-asset to-asset)))
  
  ;; Authorized Sources Schema and Table
  (defschema authorized-source
    @doc "Authorized price sources"
    source:string
    guard:guard
    active:bool)
  
  (deftable authorized-sources:{authorized-source})
  
  ;; Source Management
  (defun authorize-source:string (source:string guard:guard)
    @doc "Authorize a price source"
    (with-capability (GOVERNANCE)
      (insert authorized-sources source {
        "source": source,
        "guard": guard,
        "active": true
      })
      (format "Source {} authorized" [source])))
  
  (defun revoke-source:string (source:string)
    @doc "Revoke a price source"
    (with-capability (GOVERNANCE)
      (update authorized-sources source {
        "active": false
      })
      (format "Source {} revoked" [source])))
  
  ;; Admin Functions
  (defun set-emergency-price:string (asset:string price:decimal)
    @doc "Set emergency price (governance only)"
    (with-capability (GOVERNANCE)
      (update-price "EMERGENCY" asset price 1.0)))
  
  ;; Initialize with common assets
  (defun init:string ()
    @doc "Initialize oracle with default prices"
    (with-capability (GOVERNANCE)
      ;; Authorize governance as emergency source
      (insert authorized-sources "EMERGENCY" {
        "source": "EMERGENCY",
        "guard": (keyset-ref-guard 'defi-admin),
        "active": true
      })
      
      ;; Set initial prices
      (update-price "EMERGENCY" "ETH" 2000.0 1.0)
      (update-price "EMERGENCY" "BTC" 40000.0 1.0)
      (update-price "EMERGENCY" "USDC" 1.0 1.0)
      (update-price "EMERGENCY" "KDA" 0.5 1.0)
      
      "Oracle initialized"))
)

(create-table price-feeds)
(create-table price-history-table)
(create-table authorized-sources)