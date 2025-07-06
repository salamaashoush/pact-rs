;; Exercise: Basic Guards
;; Learn different types of guards in Pact

;; Setup for testing
(begin-tx "setup")
(env-data { "admin-keyset": ["admin-key"] })
(env-keys ["admin-key"])
(define-keyset 'admin-keyset (read-keyset "admin-keyset"))

(module guards-exercise 'admin-keyset
  @doc "Explore different guard types"
  
  ;; TODO: Create a keyset-ref-guard
  ;; Replace "I AM NOT DONE" with: (keyset-ref-guard "admin-keyset")
  (defun create-admin-guard ()
    @doc "Create a keyset guard for admin"
    I AM NOT DONE)
  
  ;; Helper function for testing
  (defun test-guard (guard:guard)
    @doc "Test a guard by enforcing it"
    (enforce-guard guard)
    "Guard passed!"))

;; Create and test
(create-admin-guard)
(commit-tx)

;; Testing
(begin-tx "test-guards")
(use guards-exercise)

;; Test keyset guard
(env-keys ["admin-key"])
(expect "Keyset guard works with correct keys" 
  "Guard passed!" 
  (test-guard (create-admin-guard)))

;; Test guard failure
(env-keys [])
(expect-failure "Keyset guard fails without keys"
  "Keyset failure"
  (test-guard (create-admin-guard)))

(commit-tx)