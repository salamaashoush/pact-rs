(interface my-interface
(defun interface-fun:string (x:integer))
(defcap INTERFACE-CAP (amount:decimal))
)

(module working "keyset"
(implements my-interface)
(use another-module)

(defschema account
balance:decimal
owner:string)

(deftable accounts:{account})

(defconst MAX_AMOUNT:decimal 1000000.0)

(defcap TRANSFER (from:string to:string amount:decimal)
@managed amount TRANSFER-mgr
(enforce-guard (at 'guard (read accounts from))))

(defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
(enforce (<= requested managed) "Transfer amount exceeds managed amount")
requested)

(defpact two-phase-commit (from:string to:string amount:decimal)
(step 
(with-capability (TRANSFER from to amount)
(debit from amount)))
(step-with-rollback
(credit to amount)
(credit from amount)))
)