;; Exercise: Types in Pact
;; Learn about Pact's type system
;;
;; Pact has strict typing with the following basic types:
;; - string: "hello"
;; - integer: 42
;; - decimal: 3.14
;; - bool: true or false
;; - time: (parse-time "%Y-%m-%d" "2023-01-01")

;; TODO: Replace "I AM NOT DONE" with a string value "Pact"
(let ((my-string I AM NOT DONE))
  my-string)

;; Testing
(begin-tx "test-types")

;; Test different Pact types
(expect "String type" "hello" "hello")
(expect "Integer type" 42 42)
(expect "Decimal type" 3.14 3.14)
(expect "Boolean type" true true)
(expect "Time parsing" 
  (parse-time "%Y-%m-%d" "2023-01-01")
  (parse-time "%Y-%m-%d" "2023-01-01"))

;; Test type operations
(expect "String concatenation" "hello world" (+ "hello " "world"))
(expect "Decimal arithmetic" 5.14 (+ 2.0 3.14))
(expect "Integer arithmetic" 10 (* 2 5))

(commit-tx)