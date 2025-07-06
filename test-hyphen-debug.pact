(module test g
  (defcap g () true)
  
  (defun abc () 1)
  (defun a-b () 2)
  (defun get-todo-item () 3)
  
  (defun test ()
    (abc)         ;; works fine
    (a-b)         ;; issue with hyphen?
    (get-todo-item)) ;; multiple hyphens
)