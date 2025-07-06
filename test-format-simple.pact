;; Simple test file for formatter
(module test-module GOV
  (defun hello:string (name:string)
    (format "Hello, {}!" [name]))
  
  (defconst VERSION "1.0.0")
  
  (defcap GOV () true)
  
  (defschema person
    name:string
    age:integer)
  
  (deftable people:{person})
)

(let ((x 1) (y 2)) (+ x y))

[1, 2, 3, 4, 5]

{"name": "Alice", "age": 30}