(interface my-interface
@doc "Test interface"
(defun interface-fun:string (x:integer))
(defcap INTERFACE-CAP (amount:decimal))
)

(module comprehensive "keyset"
@doc "Comprehensive test module"
(implements my-interface)
(use another-module)

(defschema account
@doc "Account schema"
balance:decimal
owner:string
created:time)

(deftable accounts:{account})

(defconst MAX_AMOUNT:decimal 1000000.0
"Maximum allowed amount")

(defcap TRANSFER (from:string to:string amount:decimal)
@event
@managed amount TRANSFER-mgr
(enforce-guard (at 'guard (read accounts from))))

(defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
(enforce (<= requested managed) "Transfer amount exceeds managed amount")
requested)

(defun complex-binding-example (obj:object)
{balance := bal, owner := own} obj
(format "Balance: {} Owner: {}" [bal own]))

(defpact two-phase-commit (from:string to:string amount:decimal)
@doc "Two phase commit example"
(step 
(with-capability (TRANSFER from to amount)
(debit from amount)))
(step-with-rollback
(credit to amount)
(credit from amount)))
)