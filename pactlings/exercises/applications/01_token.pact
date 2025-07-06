;; Exercise: Token Implementation
;; Build a complete fungible token following the fungible-v2 interface

;; TODO: Define the fungible interface first
(interface fungible-v2
  @doc "Fungible token interface"
  
  ;; TODO: Define interface functions
  (defun total-supply:decimal ()
    @doc "Get total token supply")
  
  (defun balance-of:decimal (account:string)
    @doc "Get account balance")
  
  (defun transfer:string (from:string to:string amount:decimal)
    @doc "Transfer tokens between accounts")
  
  (defun transfer-create:string (from:string to:string guard:guard amount:decimal)
    @doc "Transfer tokens and create target account if needed"))

;; TODO: Define keyset for token governance
(define-keyset 'YOUR_TOKEN_KEYSET
  (read-keyset "YOUR_TOKEN_KEYSET"))

;; TODO: Implement the token module
(module my-token 'YOUR_TOKEN_KEYSET
  @doc "Complete fungible token implementation"
  
  ;; TODO: Implement the fungible interface
  (implements YOUR_INTERFACE_NAME)
  
  ;; TODO: Define token schema and table
  (defschema account
    YOUR_SCHEMA_FIELDS)
  
  (deftable accounts:{account})
  
  ;; TODO: Define capabilities
  (defcap TRANSFER (from:string to:string amount:decimal)
    @doc "Transfer capability"
    YOUR_CODE_HERE)
  
  (defcap DEBIT (account:string amount:decimal)
    @doc "Debit capability"
    YOUR_CODE_HERE)
  
  (defcap CREDIT (account:string)
    @doc "Credit capability"
    YOUR_CODE_HERE)
  
  ;; TODO: Implement interface functions
  (defun total-supply:decimal ()
    YOUR_CODE_HERE)
  
  (defun balance-of:decimal (account:string)
    YOUR_CODE_HERE)
  
  (defun transfer:string (from:string to:string amount:decimal)
    YOUR_CODE_HERE)
  
  (defun transfer-create:string (from:string to:string guard:guard amount:decimal)
    YOUR_CODE_HERE)
  
  ;; TODO: Add utility functions
  (defun create-account (account:string guard:guard)
    @doc "Create a new account"
    YOUR_CODE_HERE)
  
  (defun mint (account:string guard:guard amount:decimal)
    @doc "Mint new tokens (admin only)"
    YOUR_CODE_HERE))

;; TODO: Create the table and initialize
YOUR_CODE_HERE