;; Exercise: Basic Expressions
;; Learn Pact's S-expression syntax
;;
;; Pact uses S-expressions (prefix notation) for all operations.
;; This means the operator comes first, followed by operands.
;; For example: (+ 1 2) adds 1 and 2

;; TODO: Complete the arithmetic expression below
;; Remove "I AM NOT DONE" and replace with the number 2 to make 1 + 2 = 3
(+ 1 I AM NOT DONE)

;; Testing
(begin-tx "test-expressions")

;; Verify your solution
(expect "Basic addition works" 3 (+ 1 2))
(expect "String concatenation" "Hello, World!" (+ "Hello, " "World!"))
(expect "Boolean logic" true (and true true))

(commit-tx)