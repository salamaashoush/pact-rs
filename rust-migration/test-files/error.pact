;; File with intentional errors for testing
(+ 1 2)  ; This should work

(/ 1 0)  ; Division by zero error

(unknown-function 42)  ; Unknown function error

(+ 1 "string")  ; Type mismatch error