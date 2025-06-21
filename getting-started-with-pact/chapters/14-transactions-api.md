# Chapter 14: Transaction Structure and API Integration

## Introduction

Understanding Pact's transaction structure and API integration is crucial for building production applications. This chapter explores how transactions are constructed, signed, and submitted to the Kadena blockchain, along with client-side integration patterns.

## Transaction Anatomy

### Basic Transaction Structure

A Pact transaction consists of several key components that work together to execute smart contract code on the blockchain:

```json
{
  "hash": "transaction-hash",
  "sigs": [
    {
      "sig": "signature-data"
    }
  ],
  "cmd": "{\"networkId\":\"mainnet01\",\"payload\":{\"exec\":{\"data\":{},\"code\":\"(coin.transfer \\\"alice\\\" \\\"bob\\\" 10.0)\"}},\"signers\":[{\"pubKey\":\"alice-public-key\",\"clist\":[{\"name\":\"coin.TRANSFER\",\"args\":[\"alice\",\"bob\",10.0]}]}],\"meta\":{\"creationTime\":1640995200,\"ttl\":28800,\"gasLimit\":1000,\"chainId\":\"0\",\"gasPrice\":1.0e-5,\"sender\":\"alice\"},\"nonce\":\"\\\"unique-nonce\\\"\"}"
}
```

### Command Structure (cmd field)

The `cmd` field contains the actual transaction data as a JSON string:

```json
{
  "networkId": "mainnet01",
  "payload": {
    "exec": {
      "data": {},
      "code": "(coin.transfer \"alice\" \"bob\" 10.0)"
    }
  },
  "signers": [
    {
      "pubKey": "alice-public-key", 
      "clist": [
        {
          "name": "coin.TRANSFER",
          "args": ["alice", "bob", 10.0]
        }
      ]
    }
  ],
  "meta": {
    "creationTime": 1640995200,
    "ttl": 28800,
    "gasLimit": 1000,
    "chainId": "0", 
    "gasPrice": 0.00001,
    "sender": "alice"
  },
  "nonce": "unique-nonce"
}
```

## Transaction Components Deep Dive

### Payload Types

#### Execution Payload (exec)

Used for direct code execution:

```json
{
  "exec": {
    "data": {
      "user-keyset": {
        "keys": ["user-public-key"],
        "pred": "keys-all"
      },
      "amount": 100.0
    },
    "code": "(transfer \"alice\" \"bob\" (read-decimal \"amount\"))"
  }
}
```

#### Continuation Payload (cont)

Used for multi-step pact continuation:

```json
{
  "cont": {
    "pactId": "pact-transaction-id",
    "rollback": false,
    "step": 1,
    "data": {
      "additional-data": "for-this-step"
    },
    "proof": null
  }
}
```

### Signers and Capability Lists

Signers specify who is authorizing the transaction and what capabilities they're granting:

```json
{
  "signers": [
    {
      "pubKey": "alice-public-key",
      "clist": [
        {
          "name": "coin.TRANSFER",
          "args": ["alice", "bob", 100.0]
        },
        {
          "name": "coin.GAS", 
          "args": []
        }
      ]
    },
    {
      "pubKey": "bob-public-key",
      "clist": [
        {
          "name": "my-contract.RECEIVE",
          "args": ["bob", 100.0]
        }
      ]
    }
  ]
}
```

### Metadata Configuration

Transaction metadata controls execution parameters:

```json
{
  "meta": {
    "creationTime": 1640995200,  // Unix timestamp
    "ttl": 28800,                // Time to live (8 hours)
    "gasLimit": 150000,          // Maximum gas allowed
    "chainId": "0",              // Target chain
    "gasPrice": 0.00000001,      // Price per gas unit
    "sender": "alice"            // Gas paying account
  }
}
```

## Client-Side Transaction Construction

### JavaScript/TypeScript Integration

Using the `@kadena/client` library:

```typescript
import { Pact, createSignWithKeypair } from '@kadena/client';
import { keyFromAccount } from '@kadena/cryptography-utils';

// Create transaction
const transaction = Pact.builder
  .execution(
    `(coin.transfer "${fromAccount}" "${toAccount}" ${amount.toFixed(12)})`
  )
  .addData({
    'from-keyset': {
      keys: [fromPublicKey],
      pred: 'keys-all'
    },
    'to-keyset': {
      keys: [toPublicKey], 
      pred: 'keys-all'
    }
  })
  .addSigner(fromPublicKey, (withCapability) => [
    withCapability('coin.TRANSFER', fromAccount, toAccount, amount),
    withCapability('coin.GAS')
  ])
  .setMeta({
    chainId: '0',
    gasLimit: 1000,
    gasPrice: 0.00000001,
    ttl: 28800,
    creationTime: Math.floor(Date.now() / 1000),
    sender: fromAccount
  })
  .setNetworkId('mainnet01')
  .createTransaction();

// Sign transaction
const signWithKeypair = createSignWithKeypair(keyFromAccount(fromAccount));
const signedTransaction = await signWithKeypair(transaction);

// Submit transaction
const response = await fetch('https://api.chainweb.com/chainweb/0.0/mainnet01/chain/0/pact/api/v1/send', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json'
  },
  body: JSON.stringify(signedTransaction)
});
```

### Python Integration

Using `pact-python` library:

```python
import json
import time
from pact_python import PactClient, KeyPair

# Initialize client
client = PactClient('https://api.chainweb.com/chainweb/0.0/mainnet01/chain/0/pact')

# Create keypair
keypair = KeyPair.from_mnemonic('your mnemonic phrase here')

# Build transaction
transaction_data = {
    'networkId': 'mainnet01',
    'payload': {
        'exec': {
            'data': {
                'from-keyset': {
                    'keys': [keypair.public_key],
                    'pred': 'keys-all'
                }
            },
            'code': f'(coin.transfer "{from_account}" "{to_account}" {amount})'
        }
    },
    'signers': [
        {
            'pubKey': keypair.public_key,
            'clist': [
                {
                    'name': 'coin.TRANSFER',
                    'args': [from_account, to_account, amount]
                },
                {
                    'name': 'coin.GAS',
                    'args': []
                }
            ]
        }
    ],
    'meta': {
        'creationTime': int(time.time()),
        'ttl': 28800,
        'gasLimit': 1000,
        'chainId': '0',
        'gasPrice': 0.00000001,
        'sender': from_account
    },
    'nonce': f'nonce-{int(time.time())}'
}

# Sign and send
signed_tx = client.sign_transaction(transaction_data, keypair)
result = client.send_transaction(signed_tx)
```

## Advanced Transaction Patterns

### Multi-Signature Transactions

Transactions requiring multiple signatures:

```typescript
// Multi-sig transaction requiring 2 of 3 signatures
const multiSigTransaction = Pact.builder
  .execution(`
    (with-capability (MULTI_SIG_TRANSFER "${account}" ${amount})
      (transfer-multi-sig "${account}" "${recipient}" ${amount}))
  `)
  .addSigner(signer1PublicKey, (withCapability) => [
    withCapability('MULTI_SIG_TRANSFER', account, amount)
  ])
  .addSigner(signer2PublicKey, (withCapability) => [
    withCapability('MULTI_SIG_TRANSFER', account, amount)
  ])
  .addSigner(signer3PublicKey, (withCapability) => [
    withCapability('MULTI_SIG_TRANSFER', account, amount)
  ])
  .setMeta({
    chainId: '0',
    gasLimit: 2000,
    gasPrice: 0.00000001,
    ttl: 28800,
    sender: account
  })
  .createTransaction();

// Sign with required signers (2 of 3 in this case)
const signedTx = await signWithKeypair1(
  await signWithKeypair2(multiSigTransaction)
);
```

### Cross-Chain Transactions

Initiating cross-chain pacts:

```typescript
// Step 0: Initiate cross-chain transfer
const initiateCrossChain = Pact.builder
  .execution(`
    (${contractName}.transfer-crosschain 
      "${transferId}" 
      "${fromAccount}" 
      "${toAccount}" 
      ${amount} 
      "${targetChain}")
  `)
  .addSigner(fromPublicKey, (withCapability) => [
    withCapability(`${contractName}.TRANSFER_CROSSCHAIN`, 
                  fromAccount, toAccount, amount, targetChain)
  ])
  .setMeta({
    chainId: sourceChain,
    gasLimit: 5000,
    gasPrice: 0.00000001,
    ttl: 28800,
    sender: fromAccount
  })
  .createTransaction();

// Later: Continue on target chain
const continueCrossChain = Pact.builder
  .continuation({
    pactId: pactId,
    rollback: false,
    step: 1,
    data: {
      'target-account-guard': {
        keys: [toPublicKey],
        pred: 'keys-all'
      }
    }
  })
  .addSigner(toPublicKey, (withCapability) => [
    withCapability(`${contractName}.CREDIT`, toAccount, amount)
  ])
  .setMeta({
    chainId: targetChain,
    gasLimit: 3000,
    gasPrice: 0.00000001,
    ttl: 28800,
    sender: toAccount
  })
  .createTransaction();
```

### Batched Transactions

Executing multiple operations efficiently:

```typescript
// Batch multiple transfers in single transaction
const batchTransfers = Pact.builder
  .execution(`
    (let ((transfers [
      { "from": "${account1}", "to": "${account2}", "amount": ${amount1} },
      { "from": "${account2}", "to": "${account3}", "amount": ${amount2} },
      { "from": "${account3}", "to": "${account1}", "amount": ${amount3} }
    ]))
      (map (lambda (t)
             (coin.transfer (at 'from t) (at 'to t) (at 'amount t)))
           transfers))
  `)
  .addSigner(account1PublicKey, (withCapability) => [
    withCapability('coin.TRANSFER', account1, account2, amount1),
    withCapability('coin.GAS')
  ])
  .addSigner(account2PublicKey, (withCapability) => [
    withCapability('coin.TRANSFER', account2, account3, amount2)
  ])
  .addSigner(account3PublicKey, (withCapability) => [
    withCapability('coin.TRANSFER', account3, account1, amount3)
  ])
  .setMeta({
    chainId: '0',
    gasLimit: 5000,
    gasPrice: 0.00000001,
    ttl: 28800,
    sender: account1
  })
  .createTransaction();
```

## API Endpoints and Integration

### Core Pact API Endpoints

#### Submit Transaction

```http
POST /chainweb/0.0/{network}/chain/{chainId}/pact/api/v1/send
Content-Type: application/json

{
  "cmds": [
    {
      "hash": "transaction-hash",
      "sigs": [{"sig": "signature"}],
      "cmd": "transaction-command-json"
    }
  ]
}
```

#### Query Transaction Status

```http
POST /chainweb/0.0/{network}/chain/{chainId}/pact/api/v1/listen
Content-Type: application/json

{
  "listen": "transaction-request-key"
}
```

#### Poll Multiple Transactions

```http
POST /chainweb/0.0/{network}/chain/{chainId}/pact/api/v1/poll
Content-Type: application/json

{
  "requestKeys": [
    "request-key-1",
    "request-key-2",
    "request-key-3"
  ]
}
```

#### Local Query (No Gas Cost)

```http
POST /chainweb/0.0/{network}/chain/{chainId}/pact/api/v1/local
Content-Type: application/json

{
  "hash": "query-hash",
  "sigs": [],
  "cmd": "{\"networkId\":\"mainnet01\",\"payload\":{\"exec\":{\"data\":{},\"code\":\"(coin.get-balance \\\"alice\\\")\"}},\"signers\":[],\"meta\":{\"creationTime\":1640995200,\"ttl\":28800,\"gasLimit\":1000,\"chainId\":\"0\",\"gasPrice\":1.0e-5,\"sender\":\"\"},\"nonce\":\"query-nonce\"}"
}
```

### TypeScript API Client

```typescript
class PactAPIClient {
  constructor(
    private baseUrl: string,
    private networkId: string,
    private chainId: string
  ) {}

  async sendTransaction(signedTransaction: any): Promise<any> {
    const response = await fetch(
      `${this.baseUrl}/chainweb/0.0/${this.networkId}/chain/${this.chainId}/pact/api/v1/send`,
      {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({ cmds: [signedTransaction] })
      }
    );
    
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    
    return response.json();
  }

  async pollTransaction(requestKey: string): Promise<any> {
    const response = await fetch(
      `${this.baseUrl}/chainweb/0.0/${this.networkId}/chain/${this.chainId}/pact/api/v1/poll`,
      {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify({ requestKeys: [requestKey] })
      }
    );
    
    return response.json();
  }

  async localQuery(queryTransaction: any): Promise<any> {
    const response = await fetch(
      `${this.baseUrl}/chainweb/0.0/${this.networkId}/chain/${this.chainId}/pact/api/v1/local`,
      {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(queryTransaction)
      }
    );
    
    return response.json();
  }

  async waitForTransaction(
    requestKey: string, 
    timeout: number = 60000
  ): Promise<any> {
    const startTime = Date.now();
    
    while (Date.now() - startTime < timeout) {
      const result = await this.pollTransaction(requestKey);
      
      if (result[requestKey]) {
        return result[requestKey];
      }
      
      await new Promise(resolve => setTimeout(resolve, 2000));
    }
    
    throw new Error(`Transaction ${requestKey} timed out after ${timeout}ms`);
  }
}
```

## Error Handling and Retry Logic

### Transaction Error Types

```typescript
interface TransactionError {
  type: 'network' | 'gas' | 'execution' | 'authorization';
  message: string;
  details?: any;
}

class TransactionManager {
  async submitWithRetry(
    signedTransaction: any,
    maxRetries: number = 3
  ): Promise<any> {
    for (let attempt = 1; attempt <= maxRetries; attempt++) {
      try {
        const result = await this.client.sendTransaction(signedTransaction);
        return result;
      } catch (error) {
        if (attempt === maxRetries) {
          throw error;
        }
        
        // Exponential backoff
        const delay = Math.pow(2, attempt) * 1000;
        await new Promise(resolve => setTimeout(resolve, delay));
        
        // Optionally adjust gas parameters for retry
        if (this.isGasError(error)) {
          signedTransaction = this.adjustGasParameters(signedTransaction);
        }
      }
    }
  }

  private isGasError(error: any): boolean {
    return error.message?.includes('gas limit') || 
           error.message?.includes('out of gas');
  }

  private adjustGasParameters(transaction: any): any {
    const cmd = JSON.parse(transaction.cmd);
    cmd.meta.gasLimit = Math.floor(cmd.meta.gasLimit * 1.5);
    cmd.meta.gasPrice = cmd.meta.gasPrice * 1.1;
    
    return {
      ...transaction,
      cmd: JSON.stringify(cmd)
    };
  }
}
```

### Response Validation

```typescript
interface TransactionResult {
  result: {
    status: 'success' | 'failure';
    data?: any;
    error?: {
      message: string;
      type: string;
    };
  };
  events: Array<{
    name: string;
    params: any[];
    module: string;
  }>;
  gas: number;
  logs?: string;
}

function validateTransactionResult(result: any): TransactionResult {
  if (!result.result) {
    throw new Error('Invalid transaction result: missing result field');
  }
  
  if (result.result.status === 'failure') {
    throw new Error(`Transaction failed: ${result.result.error?.message}`);
  }
  
  return result;
}
```

## Real-World Integration Patterns

### Wallet Integration

```typescript
class WalletConnector {
  private provider: any;

  async connectWallet(): Promise<string[]> {
    if (typeof window !== 'undefined' && window.kadena) {
      this.provider = window.kadena;
      return await this.provider.request({ method: 'kda_connect' });
    }
    throw new Error('Kadena wallet not found');
  }

  async signTransaction(transaction: any): Promise<any> {
    return await this.provider.request({
      method: 'kda_sendTransaction',
      transaction
    });
  }

  async getAccounts(): Promise<string[]> {
    return await this.provider.request({ method: 'kda_getAccounts' });
  }
}
```

### DApp Transaction Flow

```typescript
class DAppTransactionFlow {
  constructor(
    private client: PactAPIClient,
    private wallet: WalletConnector
  ) {}

  async executeTransfer(
    fromAccount: string,
    toAccount: string,
    amount: number
  ): Promise<string> {
    try {
      // 1. Build transaction
      const transaction = this.buildTransferTransaction(
        fromAccount, 
        toAccount, 
        amount
      );

      // 2. Sign with wallet
      const signedTx = await this.wallet.signTransaction(transaction);

      // 3. Submit to blockchain
      const submitResult = await this.client.sendTransaction(signedTx);
      const requestKey = submitResult.requestKeys[0];

      // 4. Wait for confirmation
      const result = await this.client.waitForTransaction(requestKey);

      // 5. Validate result
      validateTransactionResult(result);

      return requestKey;
    } catch (error) {
      console.error('Transfer failed:', error);
      throw error;
    }
  }

  private buildTransferTransaction(
    from: string,
    to: string,
    amount: number
  ): any {
    return Pact.builder
      .execution(`(coin.transfer "${from}" "${to}" ${amount.toFixed(12)})`)
      .addSigner(from, (withCapability) => [
        withCapability('coin.TRANSFER', from, to, amount),
        withCapability('coin.GAS')
      ])
      .setMeta({
        chainId: '0',
        gasLimit: 1000,
        gasPrice: 0.00000001,
        ttl: 28800,
        creationTime: Math.floor(Date.now() / 1000),
        sender: from
      })
      .setNetworkId('mainnet01')
      .createTransaction();
  }
}
```

### Event Monitoring

```typescript
class EventMonitor {
  private eventSubscriptions: Map<string, Function[]> = new Map();

  async monitorTransaction(requestKey: string): Promise<void> {
    const result = await this.client.waitForTransaction(requestKey);
    
    if (result.events) {
      for (const event of result.events) {
        this.emitEvent(event.name, event);
      }
    }
  }

  subscribe(eventName: string, callback: Function): void {
    if (!this.eventSubscriptions.has(eventName)) {
      this.eventSubscriptions.set(eventName, []);
    }
    this.eventSubscriptions.get(eventName)!.push(callback);
  }

  private emitEvent(eventName: string, eventData: any): void {
    const callbacks = this.eventSubscriptions.get(eventName) || [];
    callbacks.forEach(callback => callback(eventData));
  }
}

// Usage
const monitor = new EventMonitor();

monitor.subscribe('TRANSFER', (event) => {
  console.log('Transfer event:', event.params);
});

monitor.subscribe('MINT', (event) => {
  console.log('Mint event:', event.params);
});
```

## Gas Optimization for Transactions

### Dynamic Gas Estimation

```typescript
class GasEstimator {
  async estimateGas(transaction: any): Promise<number> {
    // Create unsigned transaction for local execution
    const unsignedTx = {
      ...transaction,
      sigs: []
    };

    try {
      const result = await this.client.localQuery(unsignedTx);
      
      if (result.result.status === 'success') {
        // Add 20% buffer to estimated gas
        return Math.ceil(result.gas * 1.2);
      } else {
        // Default gas limit for failed estimation
        return 1000;
      }
    } catch (error) {
      console.warn('Gas estimation failed, using default:', error);
      return 1000;
    }
  }

  async optimizeTransaction(transaction: any): Promise<any> {
    const estimatedGas = await this.estimateGas(transaction);
    
    const cmd = JSON.parse(transaction.cmd);
    cmd.meta.gasLimit = estimatedGas;
    
    return {
      ...transaction,
      cmd: JSON.stringify(cmd)
    };
  }
}
```

### Gas Price Management

```typescript
class GasPriceManager {
  private currentGasPrice: number = 0.00000001;

  async getCurrentGasPrice(): Promise<number> {
    // In a real implementation, this would fetch current network conditions
    // For now, return stored value
    return this.currentGasPrice;
  }

  async adjustGasPrice(urgency: 'low' | 'normal' | 'high'): Promise<number> {
    const basePrice = await this.getCurrentGasPrice();
    
    switch (urgency) {
      case 'low':
        return basePrice * 0.8;
      case 'normal':
        return basePrice;
      case 'high':
        return basePrice * 1.5;
      default:
        return basePrice;
    }
  }
}
```

## Summary

Pact transaction structure and API integration involve:

**Transaction Components:**
- **Command structure** with payload, signers, and metadata
- **Capability lists** for fine-grained authorization
- **Multi-signature support** for complex authorization scenarios
- **Cross-chain coordination** through pact continuations

**API Integration:**
- **RESTful endpoints** for transaction submission and querying
- **Client libraries** for JavaScript/TypeScript and Python
- **Error handling** with retry logic and gas optimization
- **Event monitoring** for real-time updates

**Best Practices:**
1. **Validate inputs** before transaction submission
2. **Estimate gas** dynamically based on transaction complexity
3. **Implement retry logic** for network resilience
4. **Monitor events** for confirmation and state updates
5. **Handle errors gracefully** with user-friendly messages

**Security Considerations:**
1. **Never expose private keys** in client-side code
2. **Validate transaction results** before proceeding
3. **Use capability lists** to minimize authorization scope
4. **Implement timeouts** to prevent hanging operations
5. **Verify signatures** and transaction integrity

This foundation enables building robust, production-ready applications that interact securely and efficiently with the Kadena blockchain.

## Exercises

1. Build a complete transaction submission flow with error handling
2. Implement a multi-signature wallet interface
3. Create a cross-chain transfer application
4. Design an event monitoring system for DApp state management
5. Build a gas optimization service for transaction efficiency

## References

- Pact API documentation: https://api.chainweb.com/openapi/pact.html
- Kadena client libraries: https://github.com/kadena-community/kadena.js
- Transaction structure: Chainweb API specification
- Signing algorithms: `/pact/Pact/Core/Crypto.hs`