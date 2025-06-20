;; Simple test to verify the Pact interpreter is working

;; Test basic arithmetic
(+ 1 2)

;; Test let binding
(let ((x 5) (y 10))
  (+ x y))

;; Test function definition
(defun add (a b)
  (+ a b))

;; Use the function
(add 3 4)

;; Test list operations
(map (lambda (x) (* x 2)) [1 2 3 4 5])

;; Test conditionals
(if (> 5 3) "yes" "no")

;; Test module definition
(module test-module GOVERNANCE
  (defcap GOVERNANCE () true)
  
  (defun greet (name)
    (format "Hello, {}!" [name]))
)

;; Use the module function
(test-module.greet "World")

;; Test table operations
(create-table accounts)

(insert accounts "alice" {"balance": 100})
(read accounts "alice")

;; Test guards
(create-user-guard (read-keyset 'alice-keyset))