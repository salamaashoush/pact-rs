# Chapter 16: Cryptography and Zero-Knowledge Features

## Introduction

Pact 5 includes advanced cryptographic capabilities for modern blockchain applications, including zero-knowledge proofs, alternative hash functions, and cryptographic operations for cross-chain communication. This chapter covers these advanced features that extend Pact's security and interoperability capabilities.

## New Hash Functions

### Keccak-256 Hashing

Keccak-256 is the hash function used by Ethereum and provides compatibility for cross-chain applications:

```pact
;; Basic Keccak-256 hashing
(hash-keccak256 "hello world")
;; Returns: "47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"

;; Hash binary data
(hash-keccak256 (base64-decode "SGVsbG8gV29ybGQ="))

;; Hash structured data
(defun hash-transaction (from:string to:string amount:decimal)
  @doc "Create deterministic hash of transaction data"
  (hash-keccak256 (format "{}->{}: {}" [from to amount])))

;; Example usage
(hash-transaction "alice" "bob" 100.0)
;; Returns deterministic hash for the transaction
```

### Poseidon Hashing

Poseidon is a hash function optimized for zero-knowledge circuits:

```pact
;; Hash list of field elements
(hash-poseidon [1, 2, 3, 4, 5])
;; Returns field element suitable for ZK circuits

;; Hash account information for privacy
(defun private-account-hash (balance:decimal nonce:integer)
  @doc "Create privacy-preserving account hash"
  (hash-poseidon [(floor balance) nonce]))

;; Merkle tree leaf generation
(defun create-merkle-leaf (data:object)
  @doc "Create Merkle tree leaf from structured data"
  (let ((serialized [
          (str-to-int (at "id" data))
          (floor (at "amount" data))
          (str-to-int (at "timestamp" data))
        ]))
    (hash-poseidon serialized)))
```

### Specialized Poseidon Variant

The implementation includes a specialized Poseidon hash for hackathon compatibility:

```pact
;; Specialized Poseidon hash (hackathon compatibility)
(poseidon-hash-hack-a-chain [1, 2, 3])
;; Returns specialized hash for specific use cases
```

## Zero-Knowledge Cryptography

### Elliptic Curve Operations

Pact 5 provides native elliptic curve operations for zero-knowledge applications:

#### Point Addition

```pact
;; Add two elliptic curve points
(defun verify-commitment (point1:object point2:object)
  @doc "Add commitment points for verification"
  (point-add point1 point2))

;; Example with BN254 curve points
(let ((point-a { "x": "123...", "y": "456..." })
      (point-b { "x": "789...", "y": "abc..." }))
  (point-add point-a point-b))
```

#### Scalar Multiplication

```pact
;; Multiply point by scalar
(defun create-commitment (value:integer randomness:object generator:object)
  @doc "Create Pedersen commitment"
  (scalar-mult value generator))

;; Zero-knowledge proof verification helper
(defun verify-scalar-proof (scalar:integer point:object)
  @doc "Verify scalar multiplication proof"
  (let ((result (scalar-mult scalar point)))
    ;; Verification logic here
    result))
```

#### Pairing Verification

```pact
;; Verify bilinear pairing for zk-SNARKs
(defun verify-snark-proof (proof:object verification-key:object public-inputs:[integer])
  @doc "Verify a zk-SNARK proof using pairing"
  (let ((pairing-result (pairing-check 
                          (at "pi_a" proof)
                          (at "pi_b" proof))))
    (enforce pairing-result "Invalid proof")
    "Proof verified"))

;; Multi-pairing verification
(defun batch-verify-proofs (proofs:[object])
  @doc "Efficiently verify multiple proofs"
  (let ((pairing-inputs (map (lambda (proof)
                              [(at "g1" proof) (at "g2" proof)])
                            proofs)))
    (fold (lambda (acc pair)
            (and acc (pairing-check (at 0 pair) (at 1 pair))))
          true
          pairing-inputs)))
```

## Privacy-Preserving Patterns

### Private Voting System

```pact
(module private-voting GOVERNANCE
  @doc "Privacy-preserving voting using ZK proofs"
  
  (defschema vote-commitment
    commitment:string
    nullifier:string
    proof:object)
  
  (defschema proposal
    title:string
    description:string
    yes-votes:integer
    no-votes:integer
    nullifiers:[string])
  
  (deftable vote-commitments:{vote-commitment})
  (deftable proposals:{proposal})
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'admin-keyset))
  
  (defun create-proposal (prop-id:string title:string description:string)
    @doc "Create a new proposal"
    (with-capability (GOVERNANCE)
      (insert proposals prop-id {
        "title": title,
        "description": description,
        "yes-votes": 0,
        "no-votes": 0,
        "nullifiers": []
      })))
  
  (defun submit-private-vote (prop-id:string 
                             commitment:string 
                             nullifier:string
                             proof:object
                             vote:bool)
    @doc "Submit encrypted vote with ZK proof"
    ;; Verify the zero-knowledge proof
    (enforce (verify-vote-proof proof commitment vote)
             "Invalid vote proof")
    
    ;; Check nullifier hasn't been used
    (with-read proposals prop-id { "nullifiers" := used-nullifiers }
      (enforce (not (contains nullifier used-nullifiers))
               "Vote already cast"))
    
    ;; Record the vote
    (update proposals prop-id {
      "yes-votes": (if vote 
                      (+ (at "yes-votes" (read proposals prop-id)) 1)
                      (at "yes-votes" (read proposals prop-id))),
      "no-votes": (if (not vote)
                     (+ (at "no-votes" (read proposals prop-id)) 1)
                     (at "no-votes" (read proposals prop-id))),
      "nullifiers": (+ [nullifier] used-nullifiers)
    }))
  
  (defun verify-vote-proof (proof:object commitment:string vote:bool)
    @doc "Verify zero-knowledge vote proof"
    ;; Implementation would verify that:
    ;; 1. Commitment opens to a valid vote (0 or 1)
    ;; 2. Prover knows the randomness
    ;; 3. Vote is in correct range
    (pairing-check (at "pi_a" proof) (at "pi_b" proof)))
)
```

### Anonymous Credentials

```pact
(module anonymous-credentials GOVERNANCE
  @doc "Anonymous credential system using ZK proofs"
  
  (defschema credential-schema
    attribute-commitments:[string]
    proof:object
    public-key:string)
  
  (defschema verification-request
    required-attributes:[string]
    min-values:[integer]
    max-values:[integer])
  
  (deftable credentials:{credential-schema})
  (deftable verifications:{verification-request})
  
  (defun issue-credential (user-id:string 
                          attributes:[integer]
                          randomness:[integer])
    @doc "Issue anonymous credential"
    (let ((commitments (map (lambda (pair)
                             (bind pair [attr rand]
                               (hash-poseidon [attr rand])))
                           (zip attributes randomness))))
      (insert credentials user-id {
        "attribute-commitments": commitments,
        "proof": (create-issuance-proof attributes randomness),
        "public-key": (at "public-key" (read-msg "user-data"))
      })))
  
  (defun verify-credential-presentation (user-id:string 
                                       proof:object
                                       disclosed-attributes:[integer])
    @doc "Verify credential presentation with selective disclosure"
    (with-read credentials user-id {
      "attribute-commitments" := commitments,
      "public-key" := pub-key
    }
      ;; Verify proof that user knows attributes satisfying requirements
      (enforce (verify-presentation-proof proof commitments disclosed-attributes)
               "Invalid credential presentation")
      "Credential verified"))
  
  (defun create-issuance-proof (attributes:[integer] randomness:[integer])
    @doc "Create proof of correct credential issuance"
    ;; Would create ZK proof that commitments are well-formed
    { "pi_a": "...", "pi_b": "...", "pi_c": "..." })
  
  (defun verify-presentation-proof (proof:object 
                                  commitments:[string] 
                                  disclosed:[integer])
    @doc "Verify selective disclosure proof"
    ;; Would verify ZK proof of attribute knowledge
    (pairing-check (at "pi_a" proof) (at "pi_b" proof)))
)
```

## Cross-Chain Cryptographic Compatibility

### Ethereum Compatibility

```pact
(module eth-bridge GOVERNANCE
  @doc "Ethereum bridge using Keccak-256 for compatibility"
  
  (defschema eth-transaction
    from:string
    to:string
    value:decimal
    nonce:integer
    eth-hash:string)
  
  (deftable eth-transactions:{eth-transaction})
  
  (defun verify-eth-signature (message:string signature:string address:string)
    @doc "Verify Ethereum-style signature"
    (let ((message-hash (hash-keccak256 
                         (+ "\x19Ethereum Signed Message:\n32" message))))
      ;; Would implement ECDSA verification
      (enforce (validate-eth-sig message-hash signature address)
               "Invalid Ethereum signature")))
  
  (defun bridge-from-ethereum (eth-tx-hash:string 
                              from:string 
                              to:string 
                              amount:decimal
                              proof:object)
    @doc "Bridge tokens from Ethereum using merkle proof"
    ;; Verify inclusion proof using Keccak-256
    (let ((leaf-hash (hash-keccak256 (format "{},{},{},{}" 
                                            [eth-tx-hash from to amount]))))
      (enforce (verify-merkle-proof leaf-hash proof)
               "Invalid Ethereum transaction proof")
      
      ;; Credit bridged tokens
      (credit-account to amount)
      
      (insert eth-transactions eth-tx-hash {
        "from": from,
        "to": to,
        "value": amount,
        "nonce": (get-bridge-nonce),
        "eth-hash": eth-tx-hash
      })))
  
  (defun verify-merkle-proof (leaf:string proof:object)
    @doc "Verify Merkle tree inclusion using Keccak-256"
    (let ((root (fold (lambda (hash sibling)
                       (hash-keccak256 (+ hash sibling)))
                     leaf
                     (at "siblings" proof))))
      (= root (at "expected-root" proof))))
)
```

## Advanced Cryptographic Patterns

### Threshold Signatures

```pact
(module threshold-signatures GOVERNANCE
  @doc "Multi-party threshold signature scheme"
  
  (defschema key-share
    participant:string
    public-share:object
    commitment:string)
  
  (defschema signature-session
    threshold:integer
    participants:[string]
    shares:[object]
    final-signature:object)
  
  (deftable key-shares:{key-share})
  (deftable signature-sessions:{signature-session})
  
  (defun setup-threshold-keys (session-id:string 
                              participants:[string] 
                              threshold:integer)
    @doc "Setup threshold signature scheme"
    (enforce (>= (length participants) threshold)
             "Not enough participants for threshold")
    
    (insert signature-sessions session-id {
      "threshold": threshold,
      "participants": participants,
      "shares": [],
      "final-signature": {}
    }))
  
  (defun contribute-signature-share (session-id:string 
                                   participant:string
                                   share:object
                                   proof:object)
    @doc "Contribute partial signature"
    (with-read signature-sessions session-id {
      "participants" := participants,
      "shares" := current-shares,
      "threshold" := threshold
    }
      ;; Verify participant is authorized
      (enforce (contains participant participants)
               "Unauthorized participant")
      
      ;; Verify signature share proof
      (enforce (verify-share-proof share proof participant)
               "Invalid signature share")
      
      ;; Add share
      (let ((new-shares (+ current-shares [share])))
        (update signature-sessions session-id {
          "shares": new-shares
        })
        
        ;; Check if we have enough shares
        (if (>= (length new-shares) threshold)
          (combine-shares session-id new-shares)
          "Waiting for more shares"))))
  
  (defun combine-shares (session-id:string shares:[object])
    @doc "Combine threshold shares into final signature"
    ;; Would implement Lagrange interpolation
    (let ((final-sig (aggregate-signature-shares shares)))
      (update signature-sessions session-id {
        "final-signature": final-sig
      })
      "Threshold signature complete"))
  
  (defun verify-share-proof (share:object proof:object participant:string)
    @doc "Verify that signature share is valid"
    ;; Would verify zero-knowledge proof of correct share
    (pairing-check (at "pi_a" proof) (at "pi_b" proof)))
)
```

### Ring Signatures

```pact
(module ring-signatures GOVERNANCE
  @doc "Ring signature implementation for anonymous authorization"
  
  (defschema ring-member
    public-key:object
    commitment:string)
  
  (defschema ring-signature
    ring:[object]
    signature:object
    linkability-tag:string)
  
  (deftable rings:{ring-signature})
  
  (defun create-ring-signature (ring-id:string 
                               message:string
                               ring-members:[object]
                               secret-index:integer
                               randomness:integer)
    @doc "Create anonymous ring signature"
    (let ((sig (generate-ring-signature message ring-members secret-index randomness))
          (tag (hash-poseidon [secret-index randomness])))
      
      (insert rings ring-id {
        "ring": ring-members,
        "signature": sig,
        "linkability-tag": tag
      })
      
      "Ring signature created"))
  
  (defun verify-ring-signature (ring-id:string message:string)
    @doc "Verify ring signature anonymously"
    (with-read rings ring-id {
      "ring" := ring-members,
      "signature" := sig,
      "linkability-tag" := tag
    }
      ;; Verify signature is valid for one of the ring members
      (enforce (validate-ring-signature message ring-members sig)
               "Invalid ring signature")
      
      ;; Check for double spending via linkability tag
      (check-linkability-tag tag)
      
      "Ring signature verified"))
  
  (defun validate-ring-signature (message:string 
                                 ring:[object] 
                                 signature:object)
    @doc "Cryptographically verify ring signature"
    ;; Would implement ring signature verification algorithm
    (pairing-check (at "commitment" signature) (at "challenge" signature)))
)
```

## Integration with Existing Pact Features

### Capability-Based ZK Access

```pact
(module zk-capabilities GOVERNANCE
  @doc "Combine capabilities with zero-knowledge proofs"
  
  (defcap ZK_VERIFY (proof:object requirements:object)
    @doc "Capability requiring ZK proof verification"
    (enforce (verify-general-proof proof requirements)
             "ZK proof verification failed"))
  
  (defun private-transfer (from:string 
                          to:string 
                          amount-commitment:string
                          proof:object)
    @doc "Private transfer with ZK proof of sufficient balance"
    (with-capability (ZK_VERIFY proof { "type": "balance", "min": 0 })
      ;; Perform private transfer
      (update-private-balance from amount-commitment)
      (emit-event (PRIVATE_TRANSFER from to amount-commitment))))
  
  (defun verify-general-proof (proof:object requirements:object)
    @doc "General ZK proof verification"
    (let ((proof-type (at "type" requirements)))
      (cond
        ((= proof-type "balance") 
         (verify-balance-proof proof requirements))
        ((= proof-type "age") 
         (verify-age-proof proof requirements))
        ((= proof-type "membership") 
         (verify-membership-proof proof requirements))
        (enforce false "Unknown proof type"))))
)
```

## Performance Considerations

### Optimizing Cryptographic Operations

```pact
;; Batch verify multiple proofs for efficiency
(defun batch-verify-zk-proofs (proofs:[object])
  @doc "Efficiently verify multiple ZK proofs"
  (let ((combined-proof (aggregate-proofs proofs)))
    (pairing-check (at "aggregated_g1" combined-proof)
                   (at "aggregated_g2" combined-proof))))

;; Precompute common values
(defconst GENERATOR_POINT 
  { "x": "1", "y": "2" }
  "Standard generator point for commitments")

(defun efficient-commitment (value:integer randomness:integer)
  @doc "Create commitment using precomputed generator"
  (scalar-mult value GENERATOR_POINT))

;; Cache verification keys
(defschema verification-key-cache
  circuit-id:string
  vk:object
  last-used:time)

(deftable vk-cache:{verification-key-cache})
```

## Security Best Practices

### ZK Proof Security

```pact
;; Always validate proof structure
(defun safe-proof-verification (proof:object)
  @doc "Safely verify proof with structure validation"
  (enforce (contains "pi_a" proof) "Missing pi_a component")
  (enforce (contains "pi_b" proof) "Missing pi_b component")
  (enforce (contains "pi_c" proof) "Missing pi_c component")
  
  ;; Verify proof is in correct group
  (enforce (valid-group-element (at "pi_a" proof)) "Invalid pi_a")
  (enforce (valid-group-element (at "pi_b" proof)) "Invalid pi_b")
  
  (pairing-check (at "pi_a" proof) (at "pi_b" proof)))

;; Prevent replay attacks
(defschema proof-record
  proof-hash:string
  used-at:time
  context:string)

(deftable used-proofs:{proof-record})

(defun prevent-proof-replay (proof:object context:string)
  @doc "Prevent proof replay attacks"
  (let ((proof-hash (hash-keccak256 (format "{}" [proof]))))
    (with-default-read used-proofs proof-hash
      { "used-at": (time "1970-01-01T00:00:00Z") }
      { "used-at" := last-used }
      
      (enforce (= last-used (time "1970-01-01T00:00:00Z"))
               "Proof already used")
      
      (write used-proofs proof-hash {
        "proof-hash": proof-hash,
        "used-at": (at 'block-time (chain-data)),
        "context": context
      }))))
```

## Summary

Pact 5's cryptographic capabilities enable:

**Hash Functions:**
- **Keccak-256** for Ethereum compatibility
- **Poseidon** for ZK-friendly operations
- **Specialized variants** for specific use cases

**Zero-Knowledge Features:**
- **Elliptic curve operations** (point addition, scalar multiplication)
- **Pairing verification** for zk-SNARKs
- **Privacy-preserving patterns** (voting, credentials)

**Security Patterns:**
- **Proof replay prevention**
- **Capability-based ZK access**
- **Batch verification** for efficiency

**Best Practices:**
1. **Validate proof structure** before verification
2. **Prevent replay attacks** with proof tracking
3. **Use appropriate hash functions** for the use case
4. **Batch operations** for performance
5. **Cache verification keys** when possible

These cryptographic primitives enable building sophisticated privacy-preserving and cross-chain applications while maintaining Pact's safety and verification guarantees.

## Exercises

1. Implement a private auction system using ZK proofs
2. Create an Ethereum bridge with Merkle proof verification  
3. Build a credential system with selective disclosure
4. Design a threshold wallet using signature aggregation
5. Implement a private voting DAO with anonymous participation

## References

- Zero-Knowledge Proofs: Academic literature on zk-SNARKs and zk-STARKs
- Elliptic Curve Cryptography: Standards for BN254 and BLS12-381 curves
- Poseidon Hash: Original Poseidon paper and implementation
- Ethereum Compatibility: Yellow Paper for hash function specifications
- Pact Implementation: `/pact/Pact/Core/Builtin.hs` for cryptographic builtins