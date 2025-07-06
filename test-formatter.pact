(module test-module "test-keyset"
@doc "Test module for formatter"
(defschema test-schema
name:string
age:integer)
(deftable test-table:{test-schema})
(defun add (x:integer y:integer)
@doc "Add two numbers"
(+ x y))
(defcap TRANSFER (from:string to:string amount:decimal)
@managed amount TRANSFER-mgr
(enforce (!= from to) "from and to must be different"))
(defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
(enforce (>= managed requested) "Insufficient funds")
requested)
(defpact multi-step-pact (name:string)
(step (let ((var1 10) (var2 20)) (+ var1 var2)))
(step-with-rollback (insert test-table name {"name": name, "age": 30}) (format "Rollback for {}" [name])))
(defun complex-function (items:[object] threshold:decimal)
(let* ((filtered (filter (lambda (item) (> (at 'value item) threshold)) items))
(sorted (sort (lambda (a b) (> (at 'value a) (at 'value b))) filtered)))
(if (> (length sorted) 0)
sorted
[]))
)
)