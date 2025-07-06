;; Test examples for error reporting

;; 1. Simple arithmetic - should work
(+ 1 2)

;; 2. Undefined variable - should show helpful error
(+ x 5)

;; 3. Wrong argument count - should show argument mismatch error  
(+ 1)

;; 4. Type mismatch - should show type error
(+ "hello" 5)

;; 5. Parse error - missing parenthesis
(let ((x 1) (y 2)
  (+ x y))