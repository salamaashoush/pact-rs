;; Chapter 16: Cryptography and Zero-Knowledge Features
;; Examples demonstrating new cryptographic capabilities in Pact 5

(module crypto-examples GOVERNANCE
  @doc "Examples of Pact 5 cryptographic features"
  
  ;; ============================================================================
  ;; HASH FUNCTION EXAMPLES
  ;; ============================================================================
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'admin-keyset))
  
  ;; Keccak-256 Hash Examples (Ethereum compatibility)
  (defun ethereum-compatible-hash:string (data:string)
    @doc "Create Ethereum-compatible hash for cross-chain operations"
    (hash-keccak256 [(base64-encode data)]))
  
  (defun verify-ethereum-message:bool (message:string expected-hash:string)
    @doc "Verify a message matches expected Ethereum hash"
    (= (hash-keccak256 [(base64-encode message)]) expected-hash))
  
  (defun create-merkle-leaf:string (transaction:object)
    @doc "Create Merkle tree leaf for Ethereum bridge verification"
    (let ((serialized (format "{},{},{},{}" 
                             [(at "from" transaction)
                              (at "to" transaction) 
                              (at "amount" transaction)
                              (at "nonce" transaction)])))
      (hash-keccak256 [(base64-encode serialized)])))
  
  ;; Poseidon Hash Examples (ZK-friendly)
  (defun create-private-commitment:string (value:integer randomness:integer)
    @doc "Create Poseidon commitment for privacy applications"
    (hash-poseidon value randomness))
  
  (defun hash-account-state:string (balance:decimal nonce:integer)
    @doc "Create privacy-preserving account state hash"
    (hash-poseidon (floor balance) nonce))
  
  (defun verify-merkle-path:bool (leaf:string path:[string] root:string)
    @doc "Verify Merkle inclusion proof using Poseidon hash"
    (let ((computed-root 
           (fold (lambda (current sibling)
                   (hash-poseidon current sibling))
                 leaf
                 path)))
      (= computed-root root)))
  
  ;; ============================================================================
  ;; ZERO-KNOWLEDGE PROOF EXAMPLES
  ;; ============================================================================
  
  (defschema zk-proof
    pi-a:object
    pi-b:object  
    pi-c:object
    public-inputs:[integer])
  
  (defun verify-balance-proof:bool (proof:object min-balance:integer)
    @doc "Verify zero-knowledge proof of minimum balance"
    (bind proof { 
      "pi-a" := pi-a,
      "pi-b" := pi-b,
      "public-inputs" := inputs
    }
      (and 
        ;; Verify the pairing check
        (pairing-check [pi-a] [pi-b])
        ;; Verify public input constraint
        (>= (at 0 inputs) min-balance))))
  
  (defun verify-age-proof:bool (proof:object min-age:integer)
    @doc "Verify zero-knowledge proof of minimum age"
    (bind proof {
      "pi-a" := pi-a,
      "pi-b" := pi-b,
      "public-inputs" := inputs
    }
      (and
        (pairing-check [pi-a] [pi-b])
        (>= (at 0 inputs) min-age))))
  
  ;; ============================================================================
  ;; ELLIPTIC CURVE OPERATIONS
  ;; ============================================================================
  
  (defconst GENERATOR-POINT 
    { "x": "1", "y": "2" }
    "Standard generator point for BN254 curve")
  
  (defun create-commitment:object (value:integer randomness:integer)
    @doc "Create Pedersen commitment using elliptic curve operations"
    (let ((value-point (scalar-mult 'g1 GENERATOR-POINT value))
          (random-point (scalar-mult 'g1 GENERATOR-POINT randomness)))
      (point-add 'g1 value-point random-point)))
  
  (defun verify-commitment:bool (commitment:object value:integer randomness:integer)
    @doc "Verify Pedersen commitment"
    (let ((expected (create-commitment value randomness)))
      (and 
        (= (at "x" commitment) (at "x" expected))
        (= (at "y" commitment) (at "y" expected)))))
  
  (defun aggregate-signatures:object (signatures:[object])
    @doc "Aggregate multiple BLS signatures"
    (fold (lambda (acc sig)
            (point-add 'g1 acc sig))
          (at 0 signatures)
          (drop 1 signatures)))
  
  ;; ============================================================================
  ;; PRIVACY-PRESERVING VOTING
  ;; ============================================================================
  
  (defschema vote-commitment
    commitment:string
    nullifier:string
    proof:object)
  
  (defschema proposal
    title:string
    yes-votes:integer
    no-votes:integer
    nullifiers:[string])
  
  (deftable vote-commitments:{vote-commitment})
  (deftable proposals:{proposal})
  
  (defun create-proposal:string (prop-id:string title:string)
    @doc "Create a new voting proposal"
    (with-capability (GOVERNANCE)
      (insert proposals prop-id {
        "title": title,
        "yes-votes": 0,
        "no-votes": 0,
        "nullifiers": []
      })))
  
  (defun submit-private-vote:string (prop-id:string 
                                   vote:bool
                                   nullifier:string 
                                   proof:object)
    @doc "Submit a private vote using zero-knowledge proof"
    ;; Verify the proof is valid
    (enforce (verify-vote-proof proof vote nullifier)
             "Invalid vote proof")
    
    ;; Check nullifier hasn't been used (prevent double voting)
    (with-read proposals prop-id { "nullifiers" := used-nullifiers }
      (enforce (not (contains nullifier used-nullifiers))
               "Vote already cast with this nullifier"))
    
    ;; Record the vote
    (with-read proposals prop-id { 
      "yes-votes" := yes-count,
      "no-votes" := no-count,
      "nullifiers" := nullifiers
    }
      (update proposals prop-id {
        "yes-votes": (if vote (+ yes-count 1) yes-count),
        "no-votes": (if (not vote) (+ no-count 1) no-count),
        "nullifiers": (+ nullifiers [nullifier])
      }))
    
    "Vote recorded successfully")
  
  (defun verify-vote-proof:bool (proof:object vote:bool nullifier:string)
    @doc "Verify zero-knowledge proof for voting"
    ;; In a real implementation, this would verify:
    ;; 1. The voter has a valid credential
    ;; 2. The vote is either 0 or 1
    ;; 3. The nullifier is correctly derived
    ;; 4. No double voting
    (bind proof {
      "pi-a" := pi-a,
      "pi-b" := pi-b,
      "public-inputs" := inputs
    }
      (and
        (pairing-check [pi-a] [pi-b])
        ;; Verify vote is binary (0 or 1)
        (or (= (at 0 inputs) 0) (= (at 0 inputs) 1))
        ;; Verify nullifier derivation (simplified)
        (!= nullifier ""))))
  
  ;; ============================================================================
  ;; ANONYMOUS CREDENTIALS
  ;; ============================================================================
  
  (defschema credential
    attribute-commitments:[string]
    issuer-signature:object
    holder-key:string)
  
  (defschema verification-context
    required-attributes:[string]
    min-values:[integer]
    verifier:string)
  
  (deftable credentials:{credential})
  (deftable verifications:{verification-context})
  
  (defun issue-credential:string (holder:string attributes:[integer] randomness:[integer])
    @doc "Issue anonymous credential to holder"
    (with-capability (GOVERNANCE)
      ;; Create commitments for each attribute
      (let ((commitments 
             (map (lambda (pair)
                    (let ((attr (at 0 pair))
                          (rand (at 1 pair)))
                      (hash-poseidon attr rand)))
                  (zip attributes randomness))))
        
        ;; Store credential
        (insert credentials holder {
          "attribute-commitments": commitments,
          "issuer-signature": (create-issuer-signature commitments),
          "holder-key": (at "public-key" (read-msg "holder-data"))
        })
        
        "Credential issued successfully")))
  
  (defun verify-credential-presentation:bool (holder:string 
                                            proof:object 
                                            disclosed-attributes:[integer])
    @doc "Verify selective disclosure of credential attributes"
    (with-read credentials holder {
      "attribute-commitments" := commitments,
      "issuer-signature" := signature
    }
      ;; Verify the credential is valid
      (enforce (verify-issuer-signature commitments signature)
               "Invalid credential signature")
      
      ;; Verify the proof of attribute knowledge
      (verify-disclosure-proof proof commitments disclosed-attributes)))
  
  (defun create-issuer-signature:object (commitments:[string])
    @doc "Create issuer signature for credential"
    ;; In practice, this would create a BLS signature
    { "signature": "mock-signature", "commitments": commitments })
  
  (defun verify-issuer-signature:bool (commitments:[string] signature:object)
    @doc "Verify issuer signature on credential"
    ;; Simplified verification
    (= (at "commitments" signature) commitments))
  
  (defun verify-disclosure-proof:bool (proof:object 
                                     commitments:[string] 
                                     disclosed:[integer])
    @doc "Verify proof of selective attribute disclosure"
    ;; Would verify zero-knowledge proof that:
    ;; 1. Holder knows attributes that open commitments
    ;; 2. Disclosed attributes satisfy requirements
    ;; 3. No additional information is revealed
    (bind proof {
      "pi-a" := pi-a,
      "pi-b" := pi-b
    }
      (pairing-check [pi-a] [pi-b])))
  
  ;; ============================================================================
  ;; UTILITY FUNCTIONS
  ;; ============================================================================
  
  (defun get-proposal-results:object (prop-id:string)
    @doc "Get voting results for a proposal"
    (read proposals prop-id ['title 'yes-votes 'no-votes]))
  
  (defun get-credential-info:object (holder:string)
    @doc "Get public information about a credential"
    (with-read credentials holder {
      "attribute-commitments" := commitments,
      "holder-key" := key
    }
      {
        "commitment-count": (length commitments),
        "holder-key": key,
        "issued": true
      }))
)

;; Create tables
(create-table vote-commitments)
(create-table proposals)
(create-table credentials)
(create-table verifications)