# Chain Data Usage in Pact

## Overview

The `chain-data` builtin function in Pact provides access to blockchain metadata including time, block height, and other chain-specific information.

## Haskell Implementation

From `/pact/Pact/Core/IR/Eval/CEK/CoreBuiltin.hs`:

```haskell
coreChainData :: (IsBuiltin b) => NativeFunction e b i
coreChainData info b cont handler _env = \case
  [] -> do
    PublicData publicMeta blockHeight blockTime prevBh <- viewEvalEnv eePublicData
    let (PublicMeta cid sender (GasLimit (Gas gasLimit)) (GasPrice gasPrice) _ttl _creationTime) = publicMeta
    let fields = M.fromList [ (cdChainId, PString (_chainId cid))
                 , (cdBlockHeight, PInteger (fromIntegral blockHeight))
                 , (cdBlockTime, PTime (PactTime.fromPosixTimestampMicros blockTime))
                 , (cdPrevBlockHash, PString prevBh)
                 , (cdSender, PString sender)
                 , (cdGasLimit, PInteger (fromIntegral gasLimit))
                 , (cdGasPrice, PDecimal gasPrice)]
    returnCEKValue cont handler (VObject fields)
  args -> argsError info b args
```

## Chain Data Fields

The `chain-data` function returns an object with the following fields:

1. **`chain-id`** - Platform-specific chain identifier (e.g., "0")
2. **`block-height`** - Current block height as an integer
3. **`block-time`** - Block creation time as a time value (microseconds since UNIX epoch)
4. **`prev-block-hash`** - Hash of the preceding block
5. **`sender`** - Sender gas account key
6. **`gas-limit`** - Gas limit for the transaction
7. **`gas-price`** - Per-unit gas price

## Usage Examples

### Getting Current Block Time

```pact
;; Get current block time
(let ((curr-time:time (at 'block-time (chain-data))))
  ;; Use curr-time for time-based logic
  )
```

### Direct Field Access

```pact
;; From performance-monitoring.pact
(let ((start-time (at 'block-time (chain-data)))
      (start-block (at 'block-height (chain-data))))
  ;; Perform operations
  (let ((end-time (at 'block-time (chain-data)))
        (end-block (at 'block-height (chain-data))))
    ;; Calculate execution time
    { "execution-time": (diff-time end-time start-time)
    , "block-span": (- end-block start-block) }))
```

### Complete Chain Data Access

```pact
;; Get all chain data
(let ((cd (chain-data)))
  { "chain": (at 'chain-id cd)
  , "height": (at 'block-height cd)
  , "time": (at 'block-time cd)
  , "prev-hash": (at 'prev-block-hash cd)
  , "sender": (at 'sender cd)
  , "gas-limit": (at 'gas-limit cd)
  , "gas-price": (at 'gas-price cd)
  })
```

### Common Patterns

1. **Time-based conditions**:
```pact
(enforce (> (at 'block-time (chain-data)) deadline) "Deadline has not passed")
```

2. **Recording timestamps**:
```pact
(insert my-table key { 
  "data": some-data
  , "created-at": (at 'block-time (chain-data))
  })
```

3. **Performance monitoring**:
```pact
(defun measure-execution ()
  (let ((start-time (at 'block-time (chain-data))))
    ;; Execute operation
    (let ((end-time (at 'block-time (chain-data))))
      (diff-time end-time start-time))))
```

## Data Types

- `chain-id`: string
- `block-height`: integer
- `block-time`: time (Pact time type)
- `prev-block-hash`: string
- `sender`: string
- `gas-limit`: integer
- `gas-price`: decimal

## Notes

1. The `block-time` is returned as a Pact `time` type, which can be used with time manipulation functions like `diff-time`, `add-time`, etc.
2. The time is stored internally as microseconds since UNIX epoch but is converted to Pact's time type for use
3. All fields are read-only and reflect the current blockchain state at execution time
4. The `chain-data` function takes no arguments and returns an object with all available fields