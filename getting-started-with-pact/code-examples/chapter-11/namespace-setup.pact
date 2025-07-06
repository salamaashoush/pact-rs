;; namespace-setup.pact
;; Comprehensive namespace setup and organization examples

;; Note: This file contains module definitions that should be loaded
;; after proper namespace and keyset setup in the test environment

;; =============================================================================
;; INFRASTRUCTURE LAYER - Shared utilities and core components
;; =============================================================================

(namespace 'protocol-utils)

(module math GOVERNANCE
  @doc "Mathematical operations and utilities"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'protocol-admin))
  
  ;; Basic math utilities
  (defun add-decimals:decimal (a:decimal b:decimal)
    @doc "Safe decimal addition"
    (+ a b))
  
  (defun sub-decimals:decimal (a:decimal b:decimal)
    @doc "Safe decimal subtraction"
    (- a b))
  
  (defun mul-decimals:decimal (a:decimal b:decimal)
    @doc "Safe decimal multiplication"
    (* a b))
  
  (defun div-decimals:decimal (a:decimal b:decimal)
    @doc "Safe decimal division"
    (enforce (!= b 0.0) "Division by zero")
    (/ a b))
  
  ;; Percentage calculations
  (defun calculate-percentage:decimal (amount:decimal percentage:decimal)
    @doc "Calculate percentage of amount"
    (enforce (>= percentage 0.0) "Percentage must be non-negative")
    (enforce (<= percentage 100.0) "Percentage cannot exceed 100")
    (* amount (/ percentage 100.0)))
  
  ;; Min/max functions
  (defun min:decimal (a:decimal b:decimal)
    @doc "Return minimum of two decimals"
    (if (< a b) a b))
  
  (defun max:decimal (a:decimal b:decimal)
    @doc "Return maximum of two decimals"
    (if (> a b) a b))
  
  ;; Compound interest calculation
  (defun compound-interest:decimal (principal:decimal rate:decimal periods:integer)
    @doc "Calculate compound interest"
    (enforce (> principal 0.0) "Principal must be positive")
    (enforce (> rate 0.0) "Rate must be positive")
    (enforce (> periods 0) "Periods must be positive")
    (* principal (^ (+ 1.0 rate) periods)))
)

(module validation GOVERNANCE
  @doc "Input validation and sanitization utilities"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'protocol-admin))
  
  ;; String validation
  (defun validate-account-id:bool (account:string)
    @doc "Validate account ID format"
    (let ((account-length (length account)))
      (enforce (>= account-length 3) "Account ID too short")
      (enforce (<= account-length 64) "Account ID too long")
      (enforce (is-charset CHARSET_LATIN1 account) "Invalid characters in account ID"))
    true)
  
  (defun validate-email:bool (email:string)
    @doc "Basic email validation"
    (enforce (contains "@" email) "Invalid email format")
    (enforce (> (length email) 5) "Email too short")
    true)
  
  ;; Numeric validation
  (defun validate-positive-amount:bool (amount:decimal)
    @doc "Validate amount is positive"
    (enforce (> amount 0.0) "Amount must be positive")
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
    module-name:string
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
  
  ;; Event logging functions
  (defun log-event:string (event-type:string module-name:string function:string data:object)
    @doc "Log system event"
    (insert event-logs (format "{}_{}" [event-type (int-to-str 10 (at 'block-time (chain-data)))]) {
      "event-type": event-type,
      "module-name": module-name,
      "function": function,
      "data": data,
      "user": (tx-sender),
      "timestamp": (at 'block-time (chain-data)),
      "block-height": (chain-data 'height),
      "tx-hash": (format "{}" [(chain-data 'block-hash)])
    })
    (format "Event logged: {} in {}.{}" [event-type module-name function]))
  
  (defun get-events:[object] (event-type:string)
    @doc "Get events by type"
    (select event-logs (where 'event-type (= event-type))))
  
  (defun get-recent-events:[object] (count:integer)
    @doc "Get recent events"
    (take count (sort (keys event-logs) (lambda (a b) (> a b)))))
)

;; =============================================================================
;; APPLICATION LAYER - Business logic modules
;; =============================================================================

(namespace 'defi-apps)

(module token-registry GOVERNANCE
  @doc "Token registration and metadata management"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Token metadata schema
  (defschema token-info
    @doc "Token metadata and information"
    symbol:string
    name:string
    decimals:integer
    total-supply:decimal
    contract:module{fungible-v2}
    verified:bool
    created:time
    creator:string
    metadata:object)
  
  (deftable tokens:{token-info})
  
  ;; Token registration
  (defun register-token:string (symbol:string name:string decimals:integer 
                                total-supply:decimal contract:module{fungible-v2}
                                metadata:object)
    @doc "Register new token"
    (with-capability (GOVERNANCE)
      (insert tokens symbol {
        "symbol": symbol,
        "name": name,
        "decimals": decimals,
        "total-supply": total-supply,
        "contract": contract,
        "verified": false,
        "created": (at 'block-time (chain-data)),
        "creator": (tx-sender),
        "metadata": metadata
      })
      (format "Token {} registered successfully" [symbol])))
  
  (defun verify-token:string (symbol:string)
    @doc "Verify token (admin only)"
    (with-capability (GOVERNANCE)
      (update tokens symbol { "verified": true })
      (format "Token {} verified" [symbol])))
  
  (defun get-token:object (symbol:string)
    @doc "Get token information"
    (read tokens symbol))
  
  (defun list-verified-tokens:[object] ()
    @doc "List all verified tokens"
    (select tokens (where 'verified (= true))))
)

(module lending-core GOVERNANCE
  @doc "Core lending protocol functionality"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'defi-admin))
  
  ;; Import utilities from other namespaces
  (use protocol-utils.math)
  (use protocol-utils.validation)
  
  ;; Lending pool schema
  (defschema lending-pool
    @doc "Lending pool information"
    token:string
    total-supplied:decimal
    total-borrowed:decimal
    supply-rate:decimal
    borrow-rate:decimal
    utilization-rate:decimal
    reserve-factor:decimal
    last-update:time)
  
  (deftable pools:{lending-pool})
  
  ;; User position schema  
  (defschema user-position
    @doc "User lending position"
    user:string
    token:string
    supplied:decimal
    borrowed:decimal
    collateral:decimal
    last-action:time)
  
  (deftable positions:{user-position})
  
  ;; Interest rate model
  (defun calculate-utilization:decimal (total-borrowed:decimal total-supplied:decimal)
    @doc "Calculate pool utilization rate"
    (if (= total-supplied 0.0)
        0.0
        (div-decimals total-borrowed total-supplied)))
  
  (defun calculate-supply-rate:decimal (utilization:decimal borrow-rate:decimal reserve-factor:decimal)
    @doc "Calculate supply interest rate"
    (mul-decimals utilization (sub-decimals borrow-rate (mul-decimals borrow-rate reserve-factor))))
  
  ;; Pool management
  (defun create-pool:string (token:string initial-borrow-rate:decimal reserve-factor:decimal)
    @doc "Create new lending pool"
    (with-capability (GOVERNANCE)
      (validate-positive-amount initial-borrow-rate)
      (insert pools token {
        "token": token,
        "total-supplied": 0.0,
        "total-borrowed": 0.0,
        "supply-rate": 0.0,
        "borrow-rate": initial-borrow-rate,
        "utilization-rate": 0.0,
        "reserve-factor": reserve-factor,
        "last-update": (at 'block-time (chain-data))
      })
      (format "Lending pool created for token: {}" [token])))
  
  (defun update-rates:string (token:string)
    @doc "Update pool interest rates"
    (with-read pools token {
      "total-supplied" := supplied,
      "total-borrowed" := borrowed,
      "borrow-rate" := borrow-rate,
      "reserve-factor" := reserve-factor
    }
      (let* ((utilization (calculate-utilization borrowed supplied))
             (supply-rate (calculate-supply-rate utilization borrow-rate reserve-factor)))
        (update pools token {
          "utilization-rate": utilization,
          "supply-rate": supply-rate,
          "last-update": (at 'block-time (chain-data))
        })
        (format "Rates updated for pool: {}" [token]))))
)

;; =============================================================================
;; SERVICE LAYER - Trading and exchange services  
;; =============================================================================

(namespace 'dex-core)

(module order-book GOVERNANCE
  @doc "Decentralized order book and matching engine"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'dex-admin))
  
  ;; Import math utilities
  (use protocol-utils.math)
  (use protocol-utils.validation)
  
  ;; Order schema
  (defschema order
    @doc "Trading order"
    order-id:string
    user:string
    token-pair:string
    side:string  ; "buy" or "sell"
    order-type:string  ; "market", "limit", "stop"
    quantity:decimal
    price:decimal
    filled:decimal
    status:string  ; "open", "filled", "cancelled", "partial"
    created:time
    expires:time)
  
  (deftable orders:{order})
  
  ;; Trade execution schema
  (defschema trade
    @doc "Executed trade"
    trade-id:string
    buy-order:string
    sell-order:string
    token-pair:string
    quantity:decimal
    price:decimal
    executed:time
    buyer:string
    seller:string)
  
  (deftable trades:{trade})
  
  ;; Order management
  (defun place-order:string (order-id:string token-pair:string side:string 
                           order-type:string quantity:decimal price:decimal
                           expires:time)
    @doc "Place new trading order"
    (validate-positive-amount quantity)
    (validate-positive-amount price)
    (enforce (contains side ["buy", "sell"]) "Invalid order side")
    (enforce (contains order-type ["market", "limit", "stop"]) "Invalid order type")
    
    (insert orders order-id {
      "order-id": order-id,
      "user": (tx-sender),
      "token-pair": token-pair,
      "side": side,
      "order-type": order-type,
      "quantity": quantity,
      "price": price,
      "filled": 0.0,
      "status": "open",
      "created": (at 'block-time (chain-data)),
      "expires": expires
    })
    (format "Order {} placed: {} {} {} at {}" [order-id side quantity token-pair price]))
  
  (defun cancel-order:string (order-id:string)
    @doc "Cancel existing order"
    (with-read orders order-id { "user" := order-user, "status" := status }
      (enforce (= order-user (tx-sender)) "Only order owner can cancel")
      (enforce (= status "open") "Order not open")
      (update orders order-id { "status": "cancelled" })
      (format "Order {} cancelled" [order-id])))
  
  (defun get-order-book:[object] (token-pair:string side:string)
    @doc "Get order book for token pair and side"
    (let ((side-filter (where 'side (= side)))
          (pair-filter (where 'token-pair (= token-pair)))
          (status-filter (where 'status (= "open"))))
      (select orders (and side-filter (and pair-filter status-filter)))))
  
  (defun execute-trade:string (buy-order-id:string sell-order-id:string 
                              trade-quantity:decimal trade-price:decimal)
    @doc "Execute trade between buy and sell orders"
    (with-capability (GOVERNANCE)
      (let ((trade-id (format "{}_{}" [buy-order-id sell-order-id])))
        (with-read orders buy-order-id {
          "user" := buyer,
          "quantity" := buy-qty,
          "filled" := buy-filled
        }
          (with-read orders sell-order-id {
            "user" := seller,
            "quantity" := sell-qty,
            "filled" := sell-filled
          }
            (enforce (!= buyer seller) "Cannot trade with self")
            (enforce (<= trade-quantity (- buy-qty buy-filled)) "Insufficient buy quantity")
            (enforce (<= trade-quantity (- sell-qty sell-filled)) "Insufficient sell quantity")
            
            ;; Record trade
            (insert trades trade-id {
              "trade-id": trade-id,
              "buy-order": buy-order-id,
              "sell-order": sell-order-id,
              "token-pair": (at 'token-pair (read orders buy-order-id)),
              "quantity": trade-quantity,
              "price": trade-price,
              "executed": (at 'block-time (chain-data)),
              "buyer": buyer,
              "seller": seller
            })
            
            ;; Update order fill amounts
            (update orders buy-order-id { 
              "filled": (+ buy-filled trade-quantity),
              "status": (if (= (+ buy-filled trade-quantity) buy-qty) "filled" "partial")
            })
            (update orders sell-order-id { 
              "filled": (+ sell-filled trade-quantity),
              "status": (if (= (+ sell-filled trade-quantity) sell-qty) "filled" "partial")
            })
            
            (format "Trade executed: {} {} at {}" [trade-quantity (at 'token-pair (read orders buy-order-id)) trade-price]))))))
  
  (defun get-user-orders:[object] (user:string)
    @doc "Get all orders for user"
    (select orders (where 'user (= user))))
  
  (defun get-recent-trades:[object] (token-pair:string count:integer)
    @doc "Get recent trades for token pair"
    (take count 
          (sort (select trades (where 'token-pair (= token-pair)))
                (lambda (a b) (> (at 'executed a) (at 'executed b))))))
)

;; Table creation will be handled in the test environment
;; after namespace and keyset setup