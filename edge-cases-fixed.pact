(module edge-cases "keyset"
(defun nested-if (a:integer b:integer c:integer)
(if (> a b) (if (> b c) "a > b > c" "a > b but b <= c") (if (< a c) "a <= b and a < c" "a <= b and a >= c")))
(defun long-function-call (very-long-argument-name-1:string very-long-argument-name-2:string very-long-argument-name-3:string)
(format "Args: {}, {}, {}" [very-long-argument-name-1 very-long-argument-name-2 very-long-argument-name-3]))
(defun single-expr () 42)
)