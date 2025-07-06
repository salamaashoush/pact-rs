;; Exercise: Basic Capabilities - SOLUTION
;; Learn the fundamentals of Pact's capability system

(define-keyset 'admin-keyset
  (read-keyset "admin-keyset"))

(module capability-exercise 'admin-keyset
  @doc "Basic capability patterns"
  
  ;; Simple capability that checks the admin keyset
  (defcap ADMIN ()
    @doc "Simple admin capability"
    (enforce-keyset 'admin-keyset))
  
  ;; Parameterized capability for transfers with validation
  (defcap TRANSFER (from:string to:string amount:decimal)
    @doc "Transfer capability with validation"
    (enforce (> amount 0.0) "Amount must be positive")
    (enforce (!= from to) "Cannot transfer to self"))
  
  ;; Function that requires admin capability
  (defun admin-only-function ()
    @doc "Function that requires admin capability"
    (with-capability (ADMIN)
      "Admin action performed successfully"))
  
  ;; Function using parameterized capability
  (defun transfer-money (from:string to:string amount:decimal)
    @doc "Transfer money with capability protection"
    (with-capability (TRANSFER from to amount)
      (format "Transferred {} from {} to {}" [amount from to]))))