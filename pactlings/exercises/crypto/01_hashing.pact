;; Exercise: Cryptographic Hashing
;; Learn hash functions in Pact

(module crypto-exercise 'admin-keyset
  @doc "Cryptographic hashing examples"
  
  ;; TODO: Function to demonstrate basic hashing
  (defun basic-hash-demo (input:string)
    @doc "Demonstrate basic hash function"
    ;; TODO: Hash the input string using the default hash function
    YOUR_CODE_HERE)
  
  ;; TODO: Function to demonstrate Keccak256 hashing (Ethereum compatibility)
  (defun keccak256-demo (input:string)
    @doc "Demonstrate Keccak256 hashing"
    ;; TODO: Hash using Keccak256
    YOUR_CODE_HERE)
  
  ;; TODO: Function to demonstrate Poseidon hashing (for ZK applications)
  (defun poseidon-demo (input:string)
    @doc "Demonstrate Poseidon hashing"
    ;; TODO: Hash using Poseidon
    YOUR_CODE_HERE)
  
  ;; TODO: Function to create a hash-based commitment
  (defun create-commitment (value:string salt:string)
    @doc "Create a hash-based commitment using salt"
    ;; TODO: Combine value and salt, then hash
    YOUR_CODE_HERE)
  
  ;; TODO: Function to verify a commitment
  (defun verify-commitment (commitment:string value:string salt:string)
    @doc "Verify a hash-based commitment"
    ;; TODO: Recreate commitment and compare
    YOUR_CODE_HERE)
  
  ;; TODO: Function to hash structured data
  (defun hash-object (obj:object)
    @doc "Hash an object by converting to string first"
    ;; TODO: Convert object to string and hash
    YOUR_CODE_HERE)
  
  ;; TODO: Function to create a merkle tree leaf
  (defun create-merkle-leaf (data:string)
    @doc "Create a merkle tree leaf hash"
    ;; TODO: Hash data for merkle tree (usually double hash)
    YOUR_CODE_HERE)
  
  ;; TODO: Function to combine merkle tree nodes
  (defun combine-merkle-nodes (left:string right:string)
    @doc "Combine two merkle tree nodes"
    ;; TODO: Combine left and right hashes, then hash result
    YOUR_CODE_HERE)
  
  ;; TODO: Function to demonstrate hash collision resistance
  (defun test-collision-resistance ()
    @doc "Show that different inputs produce different hashes"
    (let ((input1 "hello")
          (input2 "world")
          (input3 "hello"))
      
      ;; TODO: Hash all inputs
      (let ((hash1 YOUR_CODE_HERE)
            (hash2 YOUR_CODE_HERE)
            (hash3 YOUR_CODE_HERE))
        
        ;; TODO: Return comparison results
        { "same-input-same-hash": YOUR_CODE_HERE
        , "different-input-different-hash": YOUR_CODE_HERE })))
  
  ;; TODO: Function to demonstrate deterministic hashing
  (defun test-deterministic-hashing (input:string)
    @doc "Show that hashing is deterministic"
    ;; TODO: Hash the same input multiple times and compare
    YOUR_CODE_HERE))