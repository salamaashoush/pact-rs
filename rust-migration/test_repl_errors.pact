;; Test file for REPL error reporting

;; Test 1: Undefined variable
(+ x 2)

;; Test 2: Wrong argument count
(+ 1)

;; Test 3: Type mismatch
(+ "hello" 2)

;; Test 4: Parse error
(let ((x 1) (y 2)