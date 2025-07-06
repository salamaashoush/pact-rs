;; Capability Examples - Fixed Version
;; Chapter 5: Capabilities - Complete Examples

(module capability-examples 'admin-keyset
  @doc "Comprehensive examples of capability patterns"
  
  ;; ========================================
  ;; Basic Capabilities
  ;; ========================================
  
  (defcap ADMIN ()
    @doc "Simple admin capability"
    (enforce-keyset 'admin-keyset))
  
  (defun admin-function ()
    @doc "Function requiring admin capability"
    (with-capability (ADMIN)
      "Admin action performed"))
  
  ;; ========================================
  ;; Account Management
  ;; ========================================
  
  (defschema account
    balance:decimal
    guard:guard)
  
  (deftable accounts:{account})
  
  (defun create-account:string (account:string guard:guard)
    @doc "Create a new account"
    (insert accounts account {
      "balance": 0.0,
      "guard": guard
    })
    (format "Account {} created" [account]))
  
  (defun get-balance:decimal (account:string)
    @doc "Get account balance"
    (at 'balance (read accounts account)))
  
  ;; ========================================
  ;; Managed Capabilities
  ;; ========================================
  
  (defcap TRANSFER (sender:string receiver:string amount:decimal)
    @doc "Managed transfer capability"
    @managed amount TRANSFER-mgr
    (enforce-guard (at 'guard (read accounts sender)))
    (enforce (> amount 0.0) "Amount must be positive"))
  
  (defun TRANSFER-mgr:decimal (managed:decimal requested:decimal)
    @doc "Transfer manager ensures linear spending"
    (enforce (>= managed requested) "Insufficient transfer allowance")
    (- managed requested))
  
  (defun transfer (sender:string receiver:string amount:decimal)
    @doc "Transfer with managed capability"
    (with-capability (TRANSFER sender receiver amount)
      (with-read accounts sender { "balance" := sender-balance }
        (enforce (>= sender-balance amount) "Insufficient balance")
        (update accounts sender { "balance": (- sender-balance amount) })
        (with-read accounts receiver
          { "balance" := receiver-balance }
          (update accounts receiver {
            "balance": (+ receiver-balance amount)
          })))))
  
  (defun transfer-create (sender:string receiver:string receiver-guard:guard amount:decimal)
    @doc "Transfer creating receiver if needed"
    (with-capability (TRANSFER sender receiver amount)
      (with-read accounts sender { "balance" := sender-balance }
        (enforce (>= sender-balance amount) "Insufficient balance")
        (update accounts sender { "balance": (- sender-balance amount) })
        (with-default-read accounts receiver
          { "balance": 0.0, "guard": receiver-guard }
          { "balance" := receiver-balance }
          (write accounts receiver {
            "balance": (+ receiver-balance amount),
            "guard": receiver-guard
          })))))
  
  ;; ========================================
  ;; Capability Composition
  ;; ========================================
  
  (defcap DEBIT (account:string amount:decimal)
    @doc "Debit capability"
    (enforce-guard (at 'guard (read accounts account)))
    (with-read accounts account { "balance" := balance }
      (enforce (>= balance amount) "Insufficient balance")))
  
  (defcap CREDIT (account:string)
    @doc "Credit capability"
    true)  ;; Anyone can receive
  
  (defcap ATOMIC_TRANSFER (from:string to:string amount:decimal)
    @doc "Composed transfer capability"
    (compose-capability (DEBIT from amount))
    (compose-capability (CREDIT to)))
  
  (defun atomic-transfer (from:string to:string amount:decimal)
    @doc "Transfer using composed capabilities"
    (with-capability (ATOMIC_TRANSFER from to amount)
      ;; DEBIT and CREDIT are automatically available
      (require-capability (DEBIT from amount))
      (require-capability (CREDIT to))
      ;; Perform transfer
      (update accounts from { 
        "balance": (- (at 'balance (read accounts from)) amount) 
      })
      (with-read accounts to
        { "balance" := to-balance }
        (update accounts to {
          "balance": (+ to-balance amount)
        }))))
  
  ;; ========================================
  ;; Event Capabilities
  ;; ========================================
  
  (defcap TRANSFER_EVENT (from:string to:string amount:decimal)
    @doc "Transfer event capability"
    @event
    true)
  
  (defun transfer-with-event (from:string to:string amount:decimal)
    @doc "Transfer that emits events"
    (transfer from to amount)  ;; Do the transfer
    (emit-event (TRANSFER_EVENT from to amount)))  ;; Emit event
  
  ;; ========================================
  ;; Autonomous Capabilities
  ;; ========================================
  
  (defcap ROTATE (account:string)
    @doc "One-time rotation capability"
    @managed  ;; No manager function = autonomous
    (enforce-guard (at 'guard (read accounts account))))
  
  (defun rotate-guard (account:string new-guard:guard)
    @doc "Rotate account guard (once per transaction)"
    (with-capability (ROTATE account)
      (update accounts account { "guard": new-guard })))
  
  ;; ========================================
  ;; Conditional Capabilities
  ;; ========================================
  
  (defcap TIME_LOCKED_ADMIN (unlock-time:time)
    @doc "Admin capability that's time-locked"
    (enforce (>= (at 'block-time (chain-data)) unlock-time) "Time lock active")
    (enforce-keyset 'admin-keyset))
  
  (defun emergency-function (unlock-time:time)
    @doc "Emergency function with time lock"
    (with-capability (TIME_LOCKED_ADMIN unlock-time)
      "Emergency action performed"))
  
  ;; ========================================
  ;; Multi-Signature Capabilities
  ;; ========================================
  
  (defcap MULTI_SIG (required-sigs:integer)
    @doc "Require multiple signatures"
    ;; In real usage, this would check actual signatures from chain-data
    (enforce (>= required-sigs 2) "Requires at least 2 signatures"))
  
  (defun multi-sig-function ()
    @doc "Function requiring multiple signatures"
    (with-capability (MULTI_SIG 2)
      "Multi-signature action performed"))
  
  ;; ========================================
  ;; Capability Guards
  ;; ========================================
  
  (defcap ESCROW_RELEASE (escrow-id:string)
    @doc "Capability to release escrow"
    (with-read escrows escrow-id 
      { "releaser" := releaser-guard }
      (enforce-guard releaser-guard)))
  
  (defschema escrow
    amount:decimal
    beneficiary:string
    releaser:guard
    release-time:time)
  
  (deftable escrows:{escrow})
  
  (defun create-escrow-guard (escrow-id:string)
    @doc "Create a capability guard for escrow"
    (create-capability-guard (ESCROW_RELEASE escrow-id)))
  
  ;; ========================================
  ;; Hierarchical Capabilities
  ;; ========================================
  
  (defcap SUPER_ADMIN ()
    @doc "Super admin capability"
    (enforce-keyset 'super-admin-keyset))
  
  (defcap MODULE_ADMIN ()
    @doc "Module admin capability"
    (enforce-one "Admin privileges required"
      [(enforce-keyset 'super-admin-keyset)
       (enforce-keyset 'module-admin-keyset)]))
  
  (defcap USER_ADMIN ()
    @doc "User admin capability"
    (enforce-one "User admin privileges required"
      [(enforce-keyset 'super-admin-keyset)
       (enforce-keyset 'module-admin-keyset)
       (enforce-keyset 'user-admin-keyset)]))
  
  ;; ========================================
  ;; Rate Limiting with Capabilities
  ;; ========================================
  
  (defschema rate-limit
    count:integer
    reset-time:time)
  
  (deftable rate-limits:{rate-limit})
  
  (defcap RATE_LIMITED (user:string count:integer)
    @doc "Rate limited capability"
    @managed count RATE_LIMIT_mgr
    (enforce-guard (at 'guard (read accounts user))))
  
  (defun RATE_LIMIT_mgr:integer (managed:integer requested:integer)
    @doc "Rate limit manager"
    (let ((new-count (+ managed requested)))
      (enforce (<= new-count 5) "Rate limit exceeded (5 per transaction)")
      new-count))
  
  (defun rate-limited-action (user:string)
    @doc "Action with rate limiting"
    (with-capability (RATE_LIMITED user 1)
      (format "Action performed by {}" [user])))
  
  ;; ========================================
  ;; Capability Scoping Examples
  ;; ========================================
  
  (defcap SCOPED_OPERATION (operation:string resource:string)
    @doc "Scoped capability for specific operations"
    (enforce (contains operation ["read" "write" "delete"]) "Invalid operation")
    (enforce-keyset 'resource-admin))
  
  (defun perform-operation (operation:string resource:string data:string)
    @doc "Perform scoped operation"
    (with-capability (SCOPED_OPERATION operation resource)
      (format "Performed {} on {} with data {}" [operation resource data])))
  
  ;; ========================================
  ;; Capability Installation Examples
  ;; ========================================
  
  (defun install-transfer-capability (from:string to:string amount:decimal)
    @doc "Install transfer capability for later use"
    (install-capability (TRANSFER from to amount)))
  
  (defun use-installed-capability (from:string to:string amount:decimal)
    @doc "Use previously installed capability"
    ;; Capability must be installed before this call
    (with-capability (TRANSFER from to amount)
      (transfer from to amount)))
)

;; Create tables
(create-table accounts)
(create-table escrows)
(create-table rate-limits)