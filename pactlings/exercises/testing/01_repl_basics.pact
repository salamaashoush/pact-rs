;; Exercise: REPL Testing Basics
;; Learn to test Pact code with expect and expect-failure

;; TODO: Set up test environment
(begin-tx "test-setup")

;; Define test keysets
(env-data 
  { "test-keyset": ["test-key"]
  , "admin-keyset": ["admin-key"] })

;; TODO: Set environment keys for testing
YOUR_CODE_HERE

;; Define keysets
(define-keyset 'test-keyset)
(define-keyset 'admin-keyset)

;; Test module
(module test-module 'admin-keyset
  @doc "Module for testing exercises"
  
  (defcap ADMIN ()
    @doc "Admin capability"
    (enforce-keyset 'admin-keyset))
  
  (defun add-numbers (a:integer b:integer)
    @doc "Add two integers"
    (+ a b))
  
  (defun divide-safe (a:decimal b:decimal)
    @doc "Safe division with error handling"
    (enforce (!= b 0.0) "Division by zero")
    (/ a b))
  
  (defun admin-only-function ()
    @doc "Function requiring admin capability"
    (with-capability (ADMIN)
      "Admin function executed"))
  
  (defun check-positive (n:integer)
    @doc "Check if number is positive"
    (enforce (> n 0) "Number must be positive")
    "Number is positive"))

(commit-tx)

;; TODO: Start testing transaction
YOUR_CODE_HERE

(use test-module)

;; TODO: Test successful function calls
(expect "add-numbers works correctly"
  5
  YOUR_CODE_HERE)

(expect "divide-safe works with valid inputs"
  2.5
  YOUR_CODE_HERE)

;; TODO: Test admin function with proper keys
YOUR_CODE_HERE  ;; Set admin keys

(expect "admin function works with admin keys"
  "Admin function executed"
  YOUR_CODE_HERE)

;; TODO: Test admin function failure without keys
YOUR_CODE_HERE  ;; Clear keys

(expect-failure "admin function fails without keys"
  "Keyset failure"
  YOUR_CODE_HERE)

;; TODO: Test error conditions
(expect-failure "divide by zero fails"
  "Division by zero"
  YOUR_CODE_HERE)

(expect-failure "negative number fails check"
  "Number must be positive"
  YOUR_CODE_HERE)

;; TODO: Test with different types
(expect "string concatenation"
  "Hello World"
  (+ "Hello " "World"))

(expect "decimal arithmetic"
  3.14
  (+ 1.14 2.0))

;; TODO: Test list operations
(expect "list length"
  3
  YOUR_CODE_HERE)

(expect "list contains element"
  true
  YOUR_CODE_HERE)

;; TODO: Test object operations
(let ((test-obj { "name": "Alice", "age": 30 }))
  
  (expect "object field access"
    "Alice"
    YOUR_CODE_HERE)
  
  (expect "object field access - age"
    30
    YOUR_CODE_HERE))

;; TODO: Test time operations
(let ((test-time (parse-time "%Y-%m-%d" "2023-01-01")))
  
  (expect "time parsing works"
    true
    YOUR_CODE_HERE)  ;; Check if test-time is a valid time
  
  (expect "time formatting works"
    "2023-01-01"
    YOUR_CODE_HERE)) ;; Format time back to string

;; TODO: Advanced testing - test with transactions
(begin-tx "isolation-test")

;; TODO: Test transaction isolation
YOUR_CODE_HERE

(commit-tx)

;; TODO: Test rollback behavior
(begin-tx "rollback-test")

;; TODO: Modify state
YOUR_CODE_HERE

(rollback-tx)

;; TODO: Verify rollback worked
YOUR_CODE_HERE

(commit-tx)