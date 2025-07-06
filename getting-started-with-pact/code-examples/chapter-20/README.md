# Chapter 20: DeFi Protocol Implementation

This directory contains a complete DeFi lending and borrowing protocol implementation in Pact.

## Core Modules

### 1. **defi-token.pact**
Interest-bearing token implementation that tracks deposits and accrues interest over time.
- Follows standard Pact token patterns with DEBIT/CREDIT capabilities
- Implements compound interest calculations
- Manages user balances with principal tracking

### 2. **lending-pool.pact**
Main lending pool logic for deposits, borrows, and collateral management.
- Multi-asset support with configurable parameters
- Health factor calculations for position safety
- Collateralization ratio enforcement
- Integration with price oracle for valuations

### 3. **liquidation.pact**
Liquidation engine for undercollateralized positions.
- Implements close factor (max 50% liquidation)
- Liquidation bonus incentives
- Batch liquidation support
- Liquidation history tracking

### 4. **price-oracle.pact**
Price feed oracle with security features.
- Authorized source management
- Price staleness checks
- Confidence scoring
- TWAP (Time-Weighted Average Price) calculations
- Multi-asset price queries

### 5. **yield-farming.pact**
Rewards distribution for liquidity providers.
- Time-based staking pools
- Accumulated reward calculations
- Partial stake/unstake support
- Pending reward tracking

## Test Files

- **test-defi-simple.repl**: Basic functionality tests
- **test-defi.repl**: Comprehensive protocol tests including liquidation scenarios
- **test-integration.repl**: Full integration test with all modules

## Key Patterns Used

### 1. Managed Capabilities
```pact
(defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
  @managed amount TRANSFER-mgr
  ...)

(defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
  (enforce (>= managed requested) "Transfer exceeds allowance")
  (- managed requested))
```

### 2. Time Handling
```pact
;; Current time
(at 'block-time (chain-data))

;; Time differences
(diff-time time1 time2)

;; Future time
(add-time current-time seconds)
```

### 3. Guard Patterns
```pact
;; Get account guard from coin module
(defun account-guard:guard (account:string)
  (at 'guard (coin.details account)))
```

### 4. Table Access Patterns
```pact
;; Default read for initialization
(with-default-read table key
  { "field": default-value }
  { "field" := value }
  ...)
```

## Running the Tests

```bash
# Test basic token functionality
pact test-defi-simple.repl

# Test full protocol with liquidations
pact test-defi.repl

# Run integration tests
pact test-integration.repl
```

## Architecture Notes

1. **Separation of Concerns**: Each module handles a specific aspect of the protocol
2. **Capability-Based Security**: All sensitive operations require proper capabilities
3. **Oracle Integration**: Price feeds are abstracted through the oracle module
4. **Interest Accrual**: Uses lazy evaluation pattern - interest is calculated on interaction
5. **Health Factor**: Positions are monitored using weighted collateral vs debt ratios

## Security Considerations

1. **Reentrancy Protection**: Managed capabilities prevent double-spending
2. **Access Control**: Governance capabilities for admin functions
3. **Price Manipulation**: Oracle has staleness checks and confidence requirements
4. **Liquidation Limits**: Close factor prevents excessive liquidations
5. **Input Validation**: All amounts and parameters are validated

This implementation provides a foundation for building production DeFi protocols on Kadena.