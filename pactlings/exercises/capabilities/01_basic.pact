;; Exercise: Basic Capabilities
;; Learn the fundamentals of Pact's capability system

(define-keyset 'admin-keyset
  (read-keyset "admin-keyset"))

(module capability-exercise 'admin-keyset
  @doc "Basic capability patterns"
  
  ;; TODO: Define a simple capability that checks the admin keyset
  (defcap YOUR_CAPABILITY_NAME ()
    @doc "Simple admin capability"
    YOUR_CODE_HERE)  ;; What should this capability check?
  
  ;; TODO: Define a parameterized capability for transfers
  (defcap TRANSFER (from:string to:string amount:decimal)
    @doc "Transfer capability with validation"
    YOUR_CODE_HERE   ;; Add validation logic here
    YOUR_CODE_HERE)  ;; What other checks are needed?
  
  ;; TODO: Use capability in function
  (defun admin-only-function ()
    @doc "Function that requires admin capability"
    (with-capability (YOUR_CAPABILITY_NAME)
      YOUR_CODE_HERE))  ;; What should admin function do?
  
  ;; TODO: Function using parameterized capability
  (defun transfer-money (from:string to:string amount:decimal)
    @doc "Transfer money with capability protection"
    (with-capability (TRANSFER from to amount)
      YOUR_CODE_HERE))) ;; Implement transfer logic