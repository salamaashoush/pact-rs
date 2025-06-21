# Chapter 9: Interfaces and Polymorphism

## Introduction

Interfaces in Pact provide a contract-based approach to achieving polymorphism and code reusability. They define abstract specifications that modules can implement, enabling standardized APIs across different implementations while maintaining Pact's emphasis on safety and determinism.

## Understanding Interfaces

### What is an Interface?

An interface in Pact is a specification that defines:
- Function signatures (name, parameters, return types)
- Capability signatures
- Schema definitions
- Constant declarations

Interfaces contain **only declarations** - no implementations. They serve as contracts that implementing modules must fulfill.

```pact
(interface fungible-v2
  @doc "Standard fungible token interface"
  
  ;; Core token functions
  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer amount from sender to receiver")
    
  (defun get-balance:decimal (account:string)
    @doc "Get the current balance for account")
    
  (defun get-supply:decimal ()
    @doc "Get total token supply")
    
  ;; Transfer capability
  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
    @doc "Capability for transfer operations")
    
  ;; Account schema
  (defschema account
    balance:decimal
    guard:guard)
)
```

### Interface Architecture

Based on the Haskell implementation in `/pact/Pact/Core/Syntax/ParseTree.hs`, interfaces are structured as:

```haskell
data Interface i = Interface
  { _ifName :: Text              -- Interface name
  , _ifDefns :: [IfDef i]       -- Interface definitions
  , _ifImports :: [Import i]     -- Imported modules/interfaces
  , _ifAnns :: [PactAnn i]      -- Annotations
  , _ifInfo :: i                -- Source info
  }
```

Interface definitions can include:
- **Function signatures** (`IfDfun`)
- **Capability signatures** (`IfDCap`) 
- **Schema definitions** (`IfDSchema`)
- **Constant declarations** (`IfDConst`)
- **Pact signatures** (`IfDPact`)

## Creating Interfaces

### Basic Interface Definition

```pact
;; interfaces/storage.pact
(interface storage-v1
  @doc "Generic storage interface"
  
  ;; Storage operations
  (defun store:string (key:string value:object)
    @doc "Store value under key")
    
  (defun retrieve:object (key:string)
    @doc "Retrieve value by key")
    
  (defun exists:bool (key:string)
    @doc "Check if key exists")
    
  (defun delete:string (key:string)
    @doc "Delete key-value pair")
    
  ;; Storage schema
  (defschema storage-entry
    key:string
    value:object
    created:time
    updated:time)
    
  ;; Administrative capability
  (defcap STORAGE_ADMIN:bool ()
    @doc "Administrative capability for storage operations")
)
```

### Complex Interface with Dependencies

```pact
(interface marketplace-v1
  @doc "Marketplace for trading assets"
  
  ;; Import dependencies
  (use fungible-v2)
  (use nft-policy-v1)
  
  ;; Trading functions
  (defun list-asset:string (asset-id:string price:decimal seller:string)
    @doc "List asset for sale")
    
  (defun buy-asset:string (asset-id:string buyer:string)
    @doc "Purchase listed asset")
    
  (defun cancel-listing:string (asset-id:string seller:string)
    @doc "Cancel asset listing")
    
  ;; Marketplace schemas
  (defschema listing
    asset-id:string
    price:decimal
    seller:string
    listed-at:time
    active:bool)
    
  (defschema sale-record
    asset-id:string
    seller:string
    buyer:string
    price:decimal
    completed-at:time)
    
  ;; Marketplace capabilities
  (defcap LIST_ASSET:bool (asset-id:string seller:string)
    @doc "Capability to list asset")
    
  (defcap BUY_ASSET:bool (asset-id:string buyer:string price:decimal)
    @doc "Capability to buy asset")
    
  ;; Constants
  (defconst MARKETPLACE_FEE:decimal 0.025
    @doc "Marketplace fee as decimal (2.5%)")
)
```

## Implementing Interfaces

### Module Implementation

Modules implement interfaces using the `implements` declaration:

```pact
(module simple-coin GOVERNANCE
  @doc "Simple coin implementation"
  
  ;; Declare interface implementation
  (implements fungible-v2)
  
  ;; Required schemas (from interface)
  (defschema account
    balance:decimal
    guard:guard)
    
  (deftable accounts:{account})
  
  ;; Implement required functions
  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer tokens between accounts"
    
    ;; Grant transfer capability
    (with-capability (TRANSFER sender receiver amount)
      
      ;; Validate transfer
      (enforce (> amount 0.0) "Amount must be positive")
      (enforce (!= sender receiver) "Cannot transfer to self")
      
      ;; Debit sender
      (with-read accounts sender { "balance" := sender-balance }
        (enforce (>= sender-balance amount) "Insufficient balance")
        (update accounts sender { "balance": (- sender-balance amount) }))
      
      ;; Credit receiver
      (with-default-read accounts receiver 
        { "balance": 0.0, "guard": (read-keyset "receiver-guard") }
        { "balance" := receiver-balance }
        (write accounts receiver { 
          "balance": (+ receiver-balance amount),
          "guard": (at "guard" (read accounts receiver))
        }))
      
      (format "Transferred {} from {} to {}" [amount sender receiver])))
  
  (defun get-balance:decimal (account:string)
    @doc "Get account balance"
    (at "balance" (read accounts account)))
  
  (defun get-supply:decimal ()
    @doc "Get total supply"
    (fold (+) 0.0 (map (at "balance") (select accounts (constantly true)))))
  
  ;; Implement required capability
  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
    @doc "Transfer capability"
    @managed amount TRANSFER_mgr
    
    ;; Enforce sender authorization
    (enforce-guard (at "guard" (read accounts sender)))
    
    ;; Validate transfer parameters
    (enforce (> amount 0.0) "Amount must be positive")
    (enforce (!= sender receiver) "Cannot transfer to self"))
  
  (defun TRANSFER_mgr:decimal (managed:decimal requested:decimal)
    @doc "Managed capability for transfers"
    (enforce (<= requested managed) "Transfer exceeds managed amount")
    (- managed requested))
)
```

### Multiple Interface Implementation

```pact
(module advanced-token GOVERNANCE
  @doc "Advanced token with multiple interfaces"
  
  ;; Implement multiple interfaces
  (implements fungible-v2)
  (implements burnable-v1)
  (implements mintable-v1)
  
  ;; Enhanced account schema
  (defschema account
    balance:decimal
    guard:guard
    frozen:bool)
    
  (deftable accounts:{account})
  
  ;; Track total supply
  (defschema supply-info
    total:decimal
    max-supply:decimal)
    
  (deftable supply:{supply-info})
  
  ;; Implement fungible-v2 functions
  (defun transfer:string (sender:string receiver:string amount:decimal)
    ;; Check if accounts are frozen
    (with-read accounts sender { "frozen" := sender-frozen }
      (enforce (not sender-frozen) "Sender account is frozen"))
    (with-read accounts receiver { "frozen" := receiver-frozen }
      (enforce (not receiver-frozen) "Receiver account is frozen"))
    
    ;; Standard transfer logic
    ;; ... implementation
    )
  
  ;; Implement burnable-v1 functions
  (defun burn:string (account:string amount:decimal)
    @doc "Burn tokens from account"
    (with-capability (BURN account amount)
      (with-read accounts account { "balance" := balance }
        (enforce (>= balance amount) "Insufficient balance to burn")
        (update accounts account { "balance": (- balance amount) })
        (update supply "total" { "total": (- (get-supply) amount) }))))
  
  ;; Implement mintable-v1 functions
  (defun mint:string (account:string amount:decimal)
    @doc "Mint new tokens to account"
    (with-capability (MINT account amount)
      (with-read supply "total" { "total" := current, "max-supply" := max }
        (enforce (<= (+ current amount) max) "Would exceed max supply")
        (credit-account account amount)
        (update supply "total" { "total": (+ current amount) }))))
)
```

## Interface Resolution and Polymorphism

### Static Resolution

Pact uses **static resolution** for interface calls. The compiler determines which module provides the implementation at compile time, not runtime.

From the Haskell implementation in `/pact/Pact/Core/IR/Desugar.hs`, the `checkImplements` function validates that modules properly implement interfaces:

```haskell
checkImplements :: i -> [Def Name Type b i] -> ModuleName -> ModuleName -> RenamerM e b i ()
checkImplements i defs moduleName ifaceName = do
  resolveModuleData ifaceName i >>= \case
    InterfaceData iface _deps ->
      traverse_ checkImplementedMember (_ifDefns iface)
    _ -> throwDesugarError (NoSuchInterface ifaceName) i
```

### Qualified Interface Calls

```pact
;; Interface usage in application code
(module dex-app GOVERNANCE
  @doc "Decentralized exchange application"
  
  ;; Use different token implementations
  (use coin [transfer get-balance])              ;; Standard coin
  (use wrapped-btc [transfer get-balance])       ;; Bitcoin wrapper
  (use stable-coin [transfer get-balance])       ;; Stablecoin
  
  (defun swap-tokens (
    token-a:module{fungible-v2}    ;; Polymorphic module parameter
    token-b:module{fungible-v2}
    user:string
    amount-a:decimal
    min-amount-b:decimal)
    @doc "Swap between any fungible tokens"
    
    ;; Calculate swap rate
    (let ((rate (get-swap-rate token-a token-b amount-a))
          (amount-b (* amount-a rate)))
      
      ;; Validate minimum output
      (enforce (>= amount-b min-amount-b) "Insufficient output")
      
      ;; Perform swap using interface functions
      (token-a::transfer user DEX_POOL amount-a)
      (token-b::transfer DEX_POOL user amount-b)
      
      (format "Swapped {} {} for {} {}" 
        [amount-a (token-a::get-name) amount-b (token-b::get-name)])))
)
```

### Interface Implementation Validation

The system validates implementations by checking:

1. **Function Signatures**: Exact parameter and return type matching
2. **Capability Signatures**: Matching capability declarations
3. **Schema Compatibility**: Required schemas must be present
4. **Constant Values**: Interface constants must be implemented

```pact
;; This implementation would FAIL validation
(module bad-coin GOVERNANCE
  (implements fungible-v2)
  
  ;; WRONG: Missing required parameter
  (defun transfer:string (sender:string amount:decimal) ; Missing 'receiver'
    ;; Implementation
    )
  
  ;; WRONG: Wrong return type  
  (defun get-balance:string (account:string) ; Should return decimal
    ;; Implementation
    )
  
  ;; MISSING: Required TRANSFER capability not implemented
)
```

## Advanced Interface Patterns

### 1. Versioned Interfaces

```pact
;; Version 1 interface
(interface fungible-v1
  (defun transfer:string (from:string to:string amount:decimal))
  (defun balance:decimal (account:string))
)

;; Version 2 interface (extended)
(interface fungible-v2
  ;; Include v1 functions
  (defun transfer:string (sender:string receiver:string amount:decimal))
  (defun get-balance:decimal (account:string))
  
  ;; New v2 functions
  (defun get-supply:decimal ())
  (defun transfer-create:string (sender:string receiver:string receiver-guard:guard amount:decimal))
  
  ;; Enhanced capabilities
  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal))
)

;; Migration-aware implementation
(module evolved-coin GOVERNANCE
  ;; Implement latest version
  (implements fungible-v2)
  
  ;; But provide backward compatibility
  (defun balance:decimal (account:string)
    @doc "Deprecated: Use get-balance instead"
    (get-balance account))
)
```

### 2. Mixin Pattern with Interfaces

```pact
;; Base storage interface
(interface storage-base
  (defun store:string (key:string value:object))
  (defun retrieve:object (key:string)))

;; Caching extension interface  
(interface cacheable
  (defun cache-get:object (key:string))
  (defun cache-set:string (key:string value:object ttl:integer))
  (defun cache-clear:string ()))

;; Audit extension interface
(interface auditable
  (defun log-access:string (key:string operation:string user:string))
  (defun get-audit-log:[object] (key:string)))

;; Implementation combining multiple interfaces
(module enterprise-storage GOVERNANCE
  @doc "Enterprise storage with caching and auditing"
  
  (implements storage-base)
  (implements cacheable)
  (implements auditable)
  
  ;; Combined functionality
  (defun store:string (key:string value:object)
    ;; Log the operation
    (log-access key "STORE" (tx-sender))
    
    ;; Store in primary storage
    (insert primary-storage key { "value": value, "stored": (chain-data 'time) })
    
    ;; Update cache
    (cache-set key value 3600)  ;; 1 hour TTL
    
    "Stored successfully")
)
```

### 3. Policy Interface Pattern

```pact
;; Policy interface for NFTs
(interface nft-policy-v1
  (defcap MINT:bool (token-id:string account:string))
  (defcap BURN:bool (token-id:string account:string))
  (defcap TRANSFER:bool (token-id:string sender:string receiver:string))
  
  (defun enforce-mint:bool (token-id:string account:string))
  (defun enforce-burn:bool (token-id:string account:string))
  (defun enforce-transfer:bool (token-id:string sender:string receiver:string))
)

;; Royalty policy implementation
(module royalty-policy GOVERNANCE
  (implements nft-policy-v1)
  
  (defschema royalty-info
    creator:string
    rate:decimal)  ;; 0.0 to 1.0
    
  (deftable royalties:{royalty-info})
  
  (defun enforce-transfer:bool (token-id:string sender:string receiver:string)
    @doc "Enforce royalty payment on transfer"
    
    ;; Only enforce on sales (not gifts)
    (if (sale-detected? token-id sender receiver)
        (with-read royalties token-id { "creator" := creator, "rate" := rate }
          (let ((sale-price (get-sale-price token-id))
                (royalty-amount (* sale-price rate)))
            ;; Transfer royalty to creator
            (coin.transfer receiver creator royalty-amount)))
        true))
)

;; NFT collection using policy
(module art-collection GOVERNANCE
  (use nft-policy-v1)  ;; Use interface
  
  (defun mint-with-policy (token-id:string account:string policy:module{nft-policy-v1})
    @doc "Mint NFT using specified policy"
    
    ;; Policy enforcement
    (with-capability (policy::MINT token-id account)
      (policy::enforce-mint token-id account)
      
      ;; Mint the NFT
      (insert tokens token-id {
        "owner": account,
        "policy": (format "{}" [policy])
      })))
)
```

## Interface Testing

### Unit Testing Interfaces

```pact
;; test-interfaces.repl
(begin-tx)

;; Load interface and implementation
(load "interfaces/fungible-v2.pact")
(load "modules/simple-coin.pact")

;; Test interface compliance
(simple-coin.create-account "alice" (read-keyset "alice-keyset"))
(simple-coin.create-account "bob" (read-keyset "bob-keyset"))

;; Test required functions exist and work
(expect "Transfer function works"
  "Transferred 100.0 from alice to bob"
  (simple-coin.transfer "alice" "bob" 100.0))

(expect "Balance function works"
  100.0
  (simple-coin.get-balance "bob"))

(expect "Supply function works"
  1000000.0
  (simple-coin.get-supply))

;; Test capability enforcement
(expect-failure "Transfer requires capability"
  "capability not granted"
  (simple-coin.transfer "bob" "alice" 50.0))

(commit-tx)
```

### Property Testing

```pact
(defun test-fungible-properties (token:module{fungible-v2})
  @doc "Test properties that all fungible tokens should satisfy"
  
  ;; Property: Balance non-negative
  (expect "All balances non-negative"
    true
    (all-balances-non-negative? token))
  
  ;; Property: Supply equals sum of balances
  (expect "Supply conservation"
    true
    (= (token::get-supply) (sum-all-balances token)))
  
  ;; Property: Transfer preserves total supply
  (let ((initial-supply (token::get-supply)))
    (token::transfer "alice" "bob" 10.0)
    (expect "Transfer preserves supply"
      initial-supply
      (token::get-supply))))
```

## Interface Migration and Evolution

### Backward Compatibility

```pact
;; Migration strategy for interface evolution
(module adaptive-token GOVERNANCE
  @doc "Token that supports multiple interface versions"
  
  ;; Implement current version
  (implements fungible-v2)
  
  ;; Provide v1 compatibility layer
  (defun balance:decimal (account:string)
    @doc "v1 compatibility function"
    (get-balance account))
  
  (defun old-transfer:string (from:string to:string amount:decimal)
    @doc "v1 compatibility function"
    (transfer from to amount))
  
  ;; Version detection
  (defun get-interface-version:string ()
    "fungible-v2")
  
  (defun supports-interface:bool (interface-name:string)
    (contains interface-name ["fungible-v1" "fungible-v2"]))
)
```

### Interface Registry Pattern

```pact
(module interface-registry GOVERNANCE
  @doc "Registry for interface implementations"
  
  (defschema implementation
    interface:string
    module:string
    version:string
    active:bool)
    
  (deftable implementations:{implementation})
  
  (defun register-implementation (interface:string module:string version:string)
    @doc "Register interface implementation"
    (enforce-keyset 'registry-admin)
    (insert implementations (format "{}:{}" [interface module]) {
      "interface": interface,
      "module": module,
      "version": version,
      "active": true
    }))
  
  (defun get-implementations (interface:string):[object]
    @doc "Get all implementations for interface"
    (select implementations (where 'interface (= interface))))
  
  (defun get-best-implementation (interface:string):string
    @doc "Get recommended implementation for interface"
    (let ((impls (filter (where 'active (= true)) 
                         (get-implementations interface))))
      ;; Return latest version
      (at 'module (last (sort ['version] impls)))))
)
```

## Performance Considerations

### Interface Call Overhead

Interface calls have minimal overhead since they resolve statically:

```pact
;; Direct call (no interface)
(coin.transfer "alice" "bob" 100.0)       ;; Fastest

;; Interface call (static resolution)
(some-token::transfer "alice" "bob" 100.0) ;; Same performance after resolution

;; Dynamic module parameter (requires runtime lookup)
(defun generic-transfer (token:module{fungible-v2} from to amount)
  (token::transfer from to amount))        ;; Slight overhead for module parameter
```

### Gas Optimization

```pact
;; GOOD: Batch interface operations
(defun batch-transfer (token:module{fungible-v2} transfers:[object])
  (map (lambda (t)
         (token::transfer (at 'from t) (at 'to t) (at 'amount t)))
       transfers))

;; BAD: Individual calls with interface lookups
(defun slow-batch-transfer (token-name:string transfers:[object])
  (let ((token (get-token-module token-name)))  ;; Lookup each time
    (map (lambda (t)
           (token::transfer (at 'from t) (at 'to t) (at 'amount t)))
         transfers)))
```

## Summary

Interfaces in Pact provide:

- **Contract-based Development**: Clear specifications for module behavior
- **Polymorphism**: Use different implementations through common interfaces
- **Code Reusability**: Write generic functions that work with any implementation
- **API Standardization**: Common interfaces across ecosystem
- **Type Safety**: Compile-time validation of implementations
- **Static Resolution**: Predictable performance with no runtime dispatch

Key principles:
1. **Interfaces are contracts** - define what, not how
2. **Static resolution** - determined at compile time
3. **Exact signature matching** - no type variance allowed
4. **Module-level implementation** - entire modules implement interfaces
5. **Multiple interfaces** - modules can implement several interfaces

Interface system enables building composable, interoperable smart contracts while maintaining Pact's safety guarantees.

## Exercises

1. Create a `storage` interface and implement it with both in-memory and database backends
2. Design a `marketplace` interface that can work with any `fungible` and `nft` implementations
3. Build a `governance` interface with voting and proposal capabilities
4. Implement a `bridge` interface for cross-chain asset transfers
5. Create an `oracle` interface with multiple data provider implementations

## References

- Interface parsing: `/pact/Pact/Core/Syntax/ParseTree.hs:436-576`
- Interface resolution: `/pact/Pact/Core/IR/Desugar.hs:658-1637`
- Runtime resolution: `/pact/Pact/Core/IR/Eval/Runtime/Utils.hs:160-207`
- Module lookup: `/pact/Pact/Core/Persistence/Utils.hs:121-137`