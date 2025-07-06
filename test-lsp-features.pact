(module test-lsp G
  (defcap G () true)
  
  (defun get-todo (id:string)
    @doc "Get a todo item by ID"
    (format "Getting todo: {}" [id]))
  
  (defun test-nested ()
    ; Test hover on format inside another function
    (let ((msg (format "Nested {}" ["test"])))
      msg))
)