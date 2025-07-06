;; namespace-setup.pact
;; Comprehensive namespace setup and organization examples

;; Define governance keysets
(env-data {
  "protocol-admin": ["admin-key-1", "admin-key-2"],
  "protocol-users": ["user-key-1", "user-key-2", "user-key-3", "admin-key-1"],
  "defi-admin": ["defi-admin-key"],
  "defi-users": ["defi-user-1", "defi-user-2"],
  "dex-admin": ["dex-admin-key"],
  "dex-users": ["dex-user-1", "dex-user-2"]
})

;; Create keysets for namespace governance
(define-keyset 'protocol-admin (read-keyset "protocol-admin"))
(define-keyset 'protocol-users (read-keyset "protocol-users"))
(define-keyset 'defi-admin (read-keyset "defi-admin"))
(define-keyset 'defi-users (read-keyset "defi-users"))
(define-keyset 'dex-admin (read-keyset "dex-admin"))
(define-keyset 'dex-users (read-keyset "dex-users"))

;; Root protocol namespace
(define-namespace 'kadena-protocol
  (keyset-ref-guard 'protocol-users)  ;; Users can deploy module-names
  (keyset-ref-guard 'protocol-admin))  ;; Admins can modify namespace

;; DeFi sub-protocol namespace
(define-namespace 'kadena-defi
  (keyset-ref-guard 'defi-users)
  (keyset-ref-guard 'defi-admin))

;; DEX sub-protocol namespace  
(define-namespace 'kadena-dex
  (keyset-ref-guard 'dex-users)
  (keyset-ref-guard 'dex-admin))

;; Utility namespace for shared components
(define-namespace 'protocol-utils
  (keyset-ref-guard 'protocol-users)
  (keyset-ref-guard 'protocol-admin))

;; =============================================================================
;; INFRASTRUCTURE LAYER - Shared utilities and core components
;; =============================================================================

(namespace 'protocol-utils)

(module math GOVERNANCE
  @doc "Mathematical operations and utilities"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'protocol-admin))
  
  ;; Constants
  (defconst PRECISION:integer 18
    @doc "Standard precision for calculations")
  
  (defconst MAX_DECIMAL:decimal 999999999999.999999999999999999
    @doc "Maximum safe decimal value")
  
  ;; Mathematical functions
  (defun safe-add:decimal (a:decimal b:decimal)
    @doc "Safe addition with overflow protection"
    (let ((result (+ a b)))
      (enforce (<= result MAX_DECIMAL) "Addition overflow")
      result))
  
  (defun safe-subtract:decimal (a:decimal b:decimal)
    @doc "Safe subtraction with underflow protection"
    (enforce (>= a b) "Subtraction underflow")
    (- a b))
  
  (defun safe-multiply:decimal (a:decimal b:decimal)
    @doc "Safe multiplication with overflow protection"
    (let ((result (* a b)))
      (enforce (<= result MAX_DECIMAL) "Multiplication overflow")
      result))
  
  (defun safe-divide:decimal (a:decimal b:decimal)
    @doc "Safe division with zero check"
    (enforce (!= b 0.0) "Division by zero")
    (/ a b))
  
  (defun calculate-percentage:decimal (value:decimal percentage:decimal)
    @doc "Calculate percentage of value"
    (safe-multiply value (safe-divide percentage 100.0)))
  
  (defun compound-interest:decimal (principal:decimal rate:decimal periods:integer)
    @doc "Calculate compound interest"
    (let ((rate-plus-one (safe-add 1.0 rate)))
      (safe-multiply principal (^ rate-plus-one periods))))
  
  ;; Precision utilities
  (defun round-to-precision:decimal (value:decimal precision:integer)
    @doc "Round value to specified precision"
    (round value precision))
  
  (defun format-currency:string (value:decimal)
    @doc "Format decimal as currency string"
    (format "{}" [(round value 2)]))
)

(module validation GOVERNANCE
  @doc "Input validation and sanitization"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'protocol-admin))
  
  ;; Validation functions
  (defun validate-positive:bool (value:decimal description:string)
    @doc "Validate value is positive"
    (enforce (> value 0.0) (format "{} must be positive" [description]))
    true)
  
  (defun validate-non-zero:bool (value:decimal description:string)
    @doc "Validate value is not zero"
    (enforce (!= value 0.0) (format "{} cannot be zero" [description]))
    true)
  
  (defun validate-account:bool (account:string)
    @doc "Validate account string"
    (enforce (!= account "") "Account cannot be empty")
    (enforce (<= (length account) 256) "Account name too long")
    true)
  
  (defun validate-percentage:bool (percentage:decimal)
    @doc "Validate percentage is between 0 and 100"
    (enforce (and (>= percentage 0.0) (<= percentage 100.0)) 
             "Percentage must be between 0 and 100")
    true)
  
  (defun validate-amount-precision:bool (amount:decimal max-precision:integer)
    @doc "Validate amount doesn't exceed precision"
    (enforce (= amount (round amount max-precision))
             (format "Amount precision exceeds {} decimal places" [max-precision]))
    true)
)

(module events GOVERNANCE
  @doc "Event logging and monitoring system"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'protocol-admin))
  
  ;; Event schema
  (defschema event-log
    @doc "Standard event log entry"
    event-type:string
    module-name-name:string
    function:string
    data:object
    user:string
    timestamp:time
    block-height:integer
    tx-hash:string)
  
  (deftable event-logs:{event-log})
  
  ;; Event types
  (defconst EVENT_TYPE_TRANSACTION:string "TRANSACTION")
  (defconst EVENT_TYPE_ERROR:string "ERROR")
  (defconst EVENT_TYPE_ADMIN:string "ADMIN")
  (defconst EVENT_TYPE_AUDIT:string "AUDIT")
  
  ;; Logging functions
  (defun log-event:string (event-type:string module-name-name:string function:string data:object)
    @doc "Log protocol event"
    (let ((event-id (hash [event-type module-name function (at 'block-time (chain-data))])))
      (insert event-logs event-id {
        "event-type": event-type,
        "module-name-name": module-name,
        "function": function,
        "data": data,
        "user": (tx-sender),
        "timestamp": (at 'block-time (chain-data)),
        "block-height": (chain-data 'block-height),
        "tx-hash": (hash (chain-data))
      })
      event-id))
  
  (defun log-transaction:string (module-name:string function:string data:object)
    @doc "Log transaction event"
    (log-event EVENT_TYPE_TRANSACTION module-name function data))
  
  (defun log-error:string (module-name:string function:string error:string data:object)
    @doc "Log error event"
    (log-event EVENT_TYPE_ERROR module-name function 
               (+ data { "error": error })))
  
  (defun log-admin-action:string (action:string data:object)
    @doc "Log administrative action"
    (log-event EVENT_TYPE_ADMIN "admin" action data))
  
  ;; Query functions
  (defun get-events-by-type:[object] (event-type:string)
    @doc "Get events by type"
    (select event-logs (where 'event-type (= event-type))))
  
  (defun get-events-by-module-name:[object] (module-name:string)
    @doc "Get events by module-name"
    (select event-logs (where 'module-name (= module-name))))
  
  (defun get-events-by-user:[object] (user:string)
    @doc "Get events by user"
    (select event-logs (where 'user (= user))))
)

;; =============================================================================
;; DEFI PROTOCOL MODULES
;; =============================================================================

(namespace 'kadena-defi)

(module token-registry GOVERNANCE
  @doc "Registry for DeFi protocol tokens"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Import utilities
  (use protocol-utils.validation)
  (use protocol-utils.events)
  
  ;; Token information schema
  (defschema token-info
    @doc "Token registry information"
    symbol:string
    name:string
    decimals:integer
    contract:module{fungible-v2}
    verified:bool
    listed:time
    total-supply:decimal
    circulating-supply:decimal
    metadata:object)
  
  (deftable tokens:{token-info})
  
  ;; Registry functions
  (defun register-token:string (symbol:string name:string decimals:integer 
                               contract:module{fungible-v2} metadata:object)
    @doc "Register new token in DeFi protocol"
    (validate-account symbol)
    (validate-account name)
    (enforce (and (>= decimals 0) (<= decimals 18)) "Invalid decimals")
    
    (with-capability (GOVERNANCE)
      (insert tokens symbol {
        "symbol": symbol,
        "name": name,
        "decimals": decimals,
        "contract": contract,
        "verified": false,
        "listed": (at 'block-time (chain-data)),
        "total-supply": (contract::get-supply),
        "circulating-supply": (contract::get-supply),
        "metadata": metadata
      })
      
      (log-admin-action "TOKEN_REGISTERED" {
        "symbol": symbol,
        "name": name,
        "contract": (format "{}" [contract])
      })
      
      (format "Token {} registered successfully" [symbol])))
  
  (defun verify-token:string (symbol:string)
    @doc "Mark token as verified"
    (with-capability (GOVERNANCE)
      (update tokens symbol { "verified": true })
      (log-admin-action "TOKEN_VERIFIED" { "symbol": symbol })
      (format "Token {} verified" [symbol])))
  
  (defun get-token:object (symbol:string)
    @doc "Get token information"
    (read tokens symbol))
  
  (defun list-verified-tokens:[object] ()
    @doc "List all verified tokens"
    (select tokens (where 'verified (= true))))
  
  (defun update-token-supply:string (symbol:string)
    @doc "Update token supply information"
    (with-read tokens symbol { "contract" := contract }
      (let ((current-supply (contract::get-supply)))
        (update tokens symbol { 
          "total-supply": current-supply,
          "circulating-supply": current-supply
        })
        (format "Supply updated for {}" [symbol]))))
)

(module lending-core GOVERNANCE
  @doc "Core lending protocol functionality"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Import dependencies
  (use protocol-utils.math)
  (use protocol-utils.validation)
  (use protocol-utils.events)
  (use token-registry)
  
  ;; Lending pool schema
  (defschema lending-pool
    @doc "Lending pool information"
    asset:string
    total-supplied:decimal
    total-borrowed:decimal
    supply-rate:decimal
    borrow-rate:decimal
    utilization-rate:decimal
    reserve-factor:decimal
    last-update:time)
  
  (deftable lending-pools:{lending-pool})
  
  ;; User position schema
  (defschema user-position
    @doc "User lending/borrowing position"
    user:string
    asset:string
    supplied:decimal
    borrowed:decimal
    collateral:decimal
    last-update:time)
  
  (deftable user-positions:{user-position})
  
  ;; Interest rate model
  (defun calculate-utilization:decimal (total-supplied:decimal total-borrowed:decimal)
    @doc "Calculate pool utilization rate"
    (if (= total-supplied 0.0)
        0.0
        (safe-divide total-borrowed total-supplied)))
  
  (defun calculate-borrow-rate:decimal (utilization:decimal)
    @doc "Calculate borrowing interest rate based on utilization"
    (let ((base-rate 0.02)  ;; 2% base rate
          (slope1 0.10)     ;; 10% slope for low utilization
          (slope2 0.50)     ;; 50% slope for high utilization
          (optimal-util 0.80)) ;; 80% optimal utilization
      (if (<= utilization optimal-util)
          (safe-add base-rate (safe-multiply utilization slope1))
          (safe-add base-rate 
                   (safe-add (safe-multiply optimal-util slope1)
                            (safe-multiply (safe-subtract utilization optimal-util) slope2))))))
  
  (defun calculate-supply-rate:decimal (borrow-rate:decimal utilization:decimal reserve-factor:decimal)
    @doc "Calculate supply interest rate"
    (safe-multiply borrow-rate 
                  (safe-multiply utilization (safe-subtract 1.0 reserve-factor))))
  
  ;; Core lending functions
  (defun create-pool:string (asset:string reserve-factor:decimal)
    @doc "Create new lending pool"
    (validate-account asset)
    (validate-percentage (safe-multiply reserve-factor 100.0))
    
    (with-capability (GOVERNANCE)
      ;; Verify asset is registered
      (get-token asset)  ;; Will fail if not found
      
      (insert lending-pools asset {
        "asset": asset,
        "total-supplied": 0.0,
        "total-borrowed": 0.0,
        "supply-rate": 0.0,
        "borrow-rate": 0.0,
        "utilization-rate": 0.0,
        "reserve-factor": reserve-factor,
        "last-update": (at 'block-time (chain-data))
      })
      
      (log-admin-action "POOL_CREATED" { "asset": asset })
      (format "Lending pool created for {}" [asset])))
  
  (defun supply:string (user:string asset:string amount:decimal)
    @doc "Supply assets to lending pool"
    (validate-account user)
    (validate-account asset)
    (validate-positive amount "Supply amount")
    
    ;; Update pool state
    (with-read lending-pools asset {
      "total-supplied" := supplied,
      "total-borrowed" := borrowed
    }
      (let ((new-supplied (safe-add supplied amount))
            (utilization (calculate-utilization new-supplied borrowed))
            (borrow-rate (calculate-borrow-rate utilization))
            (supply-rate (calculate-supply-rate borrow-rate utilization 0.1)))
        
        (update lending-pools asset {
          "total-supplied": new-supplied,
          "utilization-rate": utilization,
          "borrow-rate": borrow-rate,
          "supply-rate": supply-rate,
          "last-update": (at 'block-time (chain-data))
        })))
    
    ;; Update user position
    (with-default-read user-positions (format "{}:{}" [user asset])
      { "supplied": 0.0, "borrowed": 0.0, "collateral": 0.0 }
      { "supplied" := user-supplied }
      (write user-positions (format "{}:{}" [user asset]) {
        "user": user,
        "asset": asset,
        "supplied": (safe-add user-supplied amount),
        "borrowed": user-supplied,  ;; Keep existing borrowed
        "collateral": user-supplied, ;; Keep existing collateral
        "last-update": (at 'block-time (chain-data))
      }))
    
    (log-transaction "lending-core" "supply" {
      "user": user,
      "asset": asset,
      "amount": amount
    })
    
    (format "Supplied {} {} to lending pool" [amount asset]))
  
  ;; Query functions
  (defun get-pool:object (asset:string)
    @doc "Get lending pool information"
    (read lending-pools asset))
  
  (defun get-user-position:object (user:string asset:string)
    @doc "Get user position in lending pool"
    (read user-positions (format "{}:{}" [user asset])))
  
  (defun list-pools:[object] ()
    @doc "List all lending pools"
    (select lending-pools (constantly true)))
)

;; =============================================================================
;; DEX PROTOCOL MODULES  
;; =============================================================================

(namespace 'kadena-dex)

(module order-book GOVERNANCE
  @doc "Decentralized exchange order book"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'dex-admin))
  
  ;; Import dependencies
  (use protocol-utils.math)
  (use protocol-utils.validation)
  (use protocol-utils.events)
  (use kadena-defi.token-registry)
  
  ;; Order schema
  (defschema order
    @doc "Trading order"
    id:string
    trader:string
    side:string  ;; "BUY" or "SELL"
    base-asset:string
    quote-asset:string
    amount:decimal
    price:decimal
    filled:decimal
    status:string
    created:time
    expires:time)
  
  (deftable orders:{order})
  
  ;; Trading pair schema
  (defschema trading-pair
    @doc "Trading pair configuration"
    base-asset:string
    quote-asset:string
    active:bool
    min-order-size:decimal
    tick-size:decimal
    created:time)
  
  (deftable trading-pairs:{trading-pair})
  
  ;; Order management
  (defun create-trading-pair:string (base:string quote:string min-size:decimal tick:decimal)
    @doc "Create new trading pair"
    (validate-account base)
    (validate-account quote)
    (validate-positive min-size "Minimum order size")
    (validate-positive tick "Tick size")
    
    (with-capability (GOVERNANCE)
      ;; Verify both assets are registered
      (get-token base)
      (get-token quote)
      
      (insert trading-pairs (format "{}:{}" [base quote]) {
        "base-asset": base,
        "quote-asset": quote,
        "active": true,
        "min-order-size": min-size,
        "tick-size": tick,
        "created": (at 'block-time (chain-data))
      })
      
      (log-admin-action "TRADING_PAIR_CREATED" {
        "base": base,
        "quote": quote
      })
      
      (format "Trading pair {}/{} created" [base quote])))
  
  (defun place-order:string (order-id:string trader:string side:string 
                            base:string quote:string amount:decimal price:decimal)
    @doc "Place trading order"
    (validate-account order-id)
    (validate-account trader)
    (validate-account base)
    (validate-account quote)
    (validate-positive amount "Order amount")
    (validate-positive price "Order price")
    
    ;; Validate trading pair exists and is active
    (with-read trading-pairs (format "{}:{}" [base quote]) {
      "active" := is-active,
      "min-order-size" := min-size
    }
      (enforce is-active "Trading pair not active")
      (enforce (>= amount min-size) "Order below minimum size"))
    
    ;; Validate side
    (enforce (contains side ["BUY" "SELL"]) "Invalid order side")
    
    (insert orders order-id {
      "id": order-id,
      "trader": trader,
      "side": side,
      "base-asset": base,
      "quote-asset": quote,
      "amount": amount,
      "price": price,
      "filled": 0.0,
      "status": "OPEN",
      "created": (at 'block-time (chain-data)),
      "expires": (add-time (at 'block-time (chain-data)) (days 30))
    })
    
    (log-transaction "order-book" "place-order" {
      "order-id": order-id,
      "trader": trader,
      "side": side,
      "amount": amount,
      "price": price
    })
    
    (format "Order {} placed successfully" [order-id]))
  
  ;; Query functions
  (defun get-order:object (order-id:string)
    @doc "Get order details"
    (read orders order-id))
  
  (defun get-orders-by-trader:[object] (trader:string)
    @doc "Get all orders for trader"
    (select orders (where 'trader (= trader))))
  
  (defun get-open-orders:[object] (base:string quote:string)
    @doc "Get open orders for trading pair"
    (select orders 
            (and? (where 'base-asset (= base))
                  (and? (where 'quote-asset (= quote))
                        (where 'status (= "OPEN"))))))
)

;; Initialize tables
(create-table event-logs)
(create-table tokens)
(create-table lending-pools)
(create-table user-positions)
(create-table orders)
(create-table trading-pairs)