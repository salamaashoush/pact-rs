;; fungible-interface.pact
;; Standard fungible token interface definition

(interface fungible-v2
  @doc "Standard interface for fungible tokens"
  @model [(property (>= balance 0.0))]

  ;; Core transfer functions
  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer amount from sender to receiver"
    @model [(property (> amount 0.0))
            (property (!= sender receiver))])

  (defun transfer-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
    @doc "Transfer to receiver, creating account if necessary"
    @model [(property (> amount 0.0))
            (property (!= sender receiver))])

  ;; Balance and supply functions
  (defun get-balance:decimal (account:string)
    @doc "Get current balance for account"
    @model [(property (>= result 0.0))])

  (defun get-supply:decimal ()
    @doc "Get total token supply"
    @model [(property (>= result 0.0))])

  ;; Account management
  (defun create-account:string (account:string guard:guard)
    @doc "Create new account with guard")

  (defun rotate:string (account:string new-guard:guard)
    @doc "Rotate account guard")

  (defun precision:integer ()
    @doc "Return maximum precision for token amounts")

  (defun details:object{account} (account:string)
    @doc "Get account details")

  ;; Transfer capability
  (defcap TRANSFER:bool (sender:string receiver:string amount:decimal)
    @doc "Managed capability sealing AMOUNT for transfer from SENDER to RECEIVER. Permits any number of transfers up to AMOUNT."
    @managed amount TRANSFER-mgr)

  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    @doc "Manages TRANSFER AMOUNT linearly, such that a request for 1.0 amount on a 3.0 managed quantity emits updated amount 2.0.")

  ;; Account schema
  (defschema account
    @doc "Token account schema"
    balance:decimal
    guard:guard)

  ;; Constants
  (defconst MINIMUM_PRECISION:integer 12
    @doc "Minimum precision for token amounts")
)