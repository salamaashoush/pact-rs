(module my-module GOVERNANCE
  (defcap GOVERNANCE () true)
  
  (defun my-function (x:integer y:string)
    "This is my test function"
    (+ x 1))
  
  (defconst MY_CONSTANT 42)
  
  (defschema my-schema
    id:string
    value:integer)
  
  (deftable my-table:{my-schema})
  
  (defun use-function ()
    (my-function 10 "test"))
)
