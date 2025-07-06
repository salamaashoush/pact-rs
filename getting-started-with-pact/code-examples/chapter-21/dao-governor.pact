;; dao-governor.pact
(module dao-governor GOVERNANCE
  @doc "Main DAO governance system - proposal creation and voting"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dao-admin))
  
  ;; Schemas
  (defschema proposal
    @doc "Governance proposal"
    id:string
    proposer:string
    title:string
    description:string
    targets:[string]         ;; Contract addresses to call
    values:[decimal]         ;; Values to send
    functions:[string]       ;; Function signatures
    calldatas:[string]       ;; Encoded call data
    start-block:integer
    end-block:integer
    for-votes:decimal
    against-votes:decimal
    abstain-votes:decimal
    cancelled:bool
    executed:bool
    eta:time)               ;; Execution time (for timelock)
  
  (defschema receipt
    @doc "Voting receipt"
    proposal-id:string
    voter:string
    support:integer         ;; 0=against, 1=for, 2=abstain
    votes:decimal
    reason:string)
  
  ;; Tables
  (deftable proposals:{proposal})
  (deftable receipts:{receipt})
  
  ;; Events
  (defcap PROPOSAL_CREATED:bool (id:string proposer:string)
    @doc "Proposal created event"
    @event
    true)
  
  (defcap VOTE_CAST:bool (voter:string proposal-id:string support:integer votes:decimal)
    @doc "Vote cast event"
    @event
    true)
  
  (defcap PROPOSAL_CANCELLED:bool (id:string)
    @doc "Proposal cancelled event"
    @event
    true)
  
  (defcap PROPOSAL_EXECUTED:bool (id:string)
    @doc "Proposal executed event"
    @event
    true)
  
  ;; Constants
  (defconst PROPOSAL_THRESHOLD 100000.0) ;; Tokens needed to propose
  (defconst VOTING_DELAY 1)            ;; Blocks before voting starts
  (defconst VOTING_PERIOD 17280)       ;; ~3 days at 15s blocks
  (defconst QUORUM_PERCENTAGE 4)       ;; 4% of total supply
  
  ;; Capabilities
  (defcap PROPOSE:bool (proposer:string)
    @doc "Capability to create proposals"
    (let ((votes (governance-token.get-votes proposer)))
      (enforce (>= votes PROPOSAL_THRESHOLD) 
               (format "Insufficient voting power: {} < {}" [votes PROPOSAL_THRESHOLD]))))
  
  (defcap CANCEL:bool (proposal-id:string)
    @doc "Capability to cancel proposal"
    (with-read proposals proposal-id { "proposer" := proposer }
      (enforce-one "Unauthorized"
        [(enforce-guard (at 'guard (governance-token.details proposer)))
         (enforce-keyset 'dao-admin)])))
  
  ;; Main functions
  (defun propose:string 
    ( proposer:string
      title:string
      description:string
      targets:[string]
      values:[decimal]
      functions:[string]
      calldatas:[string] )
    @doc "Create a new proposal"
    
    (with-capability (PROPOSE proposer)
      (let* ((proposal-id (hash (format "{}:{}:{}" [proposer title (at 'block-time (chain-data))])))
             (start-block (+ (at 'block-height (chain-data)) VOTING_DELAY))
             (end-block (+ start-block VOTING_PERIOD)))
        
        ;; Validate inputs
        (enforce (= (length targets) (length values)) "Mismatched targets/values")
        (enforce (= (length targets) (length functions)) "Mismatched targets/functions")
        (enforce (= (length targets) (length calldatas)) "Mismatched targets/calldatas")
        (enforce (> (length targets) 0) "Must have at least one action")
        
        ;; Create proposal
        (insert proposals proposal-id {
          "id": proposal-id,
          "proposer": proposer,
          "title": title,
          "description": description,
          "targets": targets,
          "values": values,
          "functions": functions,
          "calldatas": calldatas,
          "start-block": start-block,
          "end-block": end-block,
          "for-votes": 0.0,
          "against-votes": 0.0,
          "abstain-votes": 0.0,
          "cancelled": false,
          "executed": false,
          "eta": (add-time (at 'block-time (chain-data)) (days 2))
        })
        
        ;; Emit event
        (emit-event (PROPOSAL_CREATED proposal-id proposer))
        
        (format "Proposal {} created. Voting starts at block {}" [proposal-id start-block]))))
  
  (defun cast-vote:string (proposal-id:string voter:string support:integer)
    @doc "Cast a vote on a proposal"
    (cast-vote-with-reason proposal-id voter support ""))
  
  (defun cast-vote-with-reason:string (proposal-id:string voter:string support:integer reason:string)
    @doc "Cast a vote with a reason"
    
    (with-read proposals proposal-id 
      { "start-block" := start-block
      , "end-block" := end-block
      , "cancelled" := cancelled
      , "executed" := executed }
      
      ;; Check proposal state
      (enforce (not cancelled) "Proposal cancelled")
      (enforce (not executed) "Proposal already executed")
      
      (let ((current-block (at 'block-height (chain-data))))
        (enforce (>= current-block start-block) "Voting not started")
        (enforce (<= current-block end-block) "Voting ended"))
      
      ;; Check if already voted
      (let ((receipt-id (format "{}:{}" [proposal-id voter])))
        (with-default-read receipts receipt-id
          { "votes": 0.0 }
          { "votes" := existing-votes }
          (enforce (= existing-votes 0.0) "Already voted")))
      
      ;; Get voting power at proposal start
      (let ((votes (governance-token.get-prior-votes voter start-block)))
        (enforce (> votes 0.0) "No voting power")
        
        ;; Record vote
        (insert receipts (format "{}:{}" [proposal-id voter]) {
          "proposal-id": proposal-id,
          "voter": voter,
          "support": support,
          "votes": votes,
          "reason": reason
        })
        
        ;; Update vote counts
        (cond
          ((= support 0) (update proposals proposal-id 
                          { "against-votes": (+ (at 'against-votes (read proposals proposal-id)) votes) }))
          ((= support 1) (update proposals proposal-id 
                          { "for-votes": (+ (at 'for-votes (read proposals proposal-id)) votes) }))
          ((= support 2) (update proposals proposal-id 
                          { "abstain-votes": (+ (at 'abstain-votes (read proposals proposal-id)) votes) }))
          (enforce false "Invalid vote type"))
        
        ;; Emit event
        (emit-event (VOTE_CAST voter proposal-id support votes))
        
        (format "Vote cast: {} votes {} on proposal {}" 
                [votes (if (= support 1) "for" (if (= support 0) "against" "abstain")) proposal-id]))))
  
  (defun cancel:string (proposal-id:string)
    @doc "Cancel a proposal"
    
    (with-capability (CANCEL proposal-id)
      (with-read proposals proposal-id 
        { "executed" := executed
        , "end-block" := end-block }
        
        (enforce (not executed) "Cannot cancel executed proposal")
        (enforce (< (at 'block-height (chain-data)) end-block) "Proposal already ended")
        
        (update proposals proposal-id { "cancelled": true })
        
        (emit-event (PROPOSAL_CANCELLED proposal-id))
        
        (format "Proposal {} cancelled" [proposal-id]))))
  
  ;; Query functions
  (defun get-proposal:object (proposal-id:string)
    @doc "Get proposal details"
    (read proposals proposal-id))
  
  (defun get-proposal-state:string (proposal-id:string)
    @doc "Get current state of proposal"
    (with-read proposals proposal-id 
      { "cancelled" := cancelled
      , "executed" := executed
      , "start-block" := start-block
      , "end-block" := end-block
      , "for-votes" := for-votes
      , "against-votes" := against-votes }
      
      (let ((current-block (at 'block-height (chain-data))))
        (cond
          (cancelled "Cancelled")
          (executed "Executed")
          ((< current-block start-block) "Pending")
          ((<= current-block end-block) "Active")
          ((not (proposal-succeeded proposal-id)) "Defeated")
          (true "Succeeded")))))
  
  (defun proposal-succeeded:bool (proposal-id:string)
    @doc "Check if proposal succeeded"
    (with-read proposals proposal-id 
      { "for-votes" := for-votes
      , "against-votes" := against-votes
      , "abstain-votes" := abstain-votes }
      
      (let ((total-votes (+ for-votes against-votes abstain-votes))
            (quorum (quorum-votes)))
        (and (> for-votes against-votes)
             (>= total-votes quorum)))))
  
  (defun quorum-votes:decimal ()
    @doc "Calculate votes needed for quorum"
    (let ((total-supply (governance-token.total-supply)))
      (* total-supply (/ QUORUM_PERCENTAGE 100.0))))
  
  (defun get-receipt:object (proposal-id:string voter:string)
    @doc "Get voting receipt"
    (read receipts (format "{}:{}" [proposal-id voter])))
  
  (defun has-voted:bool (proposal-id:string voter:string)
    @doc "Check if address has voted"
    (with-default-read receipts (format "{}:{}" [proposal-id voter])
      { "votes": 0.0 }
      { "votes" := votes }
      (> votes 0.0)))
  
  ;; Execution functions (would typically be handled by timelock)
  (defun execute:string (proposal-id:string)
    @doc "Execute a successful proposal"
    (with-read proposals proposal-id 
      { "executed" := executed
      , "cancelled" := cancelled
      , "end-block" := end-block
      , "eta" := eta }
      
      ;; Check state
      (enforce (not executed) "Already executed")
      (enforce (not cancelled) "Proposal cancelled")
      (enforce (>= (at 'block-height (chain-data)) end-block) "Voting still active")
      (enforce (proposal-succeeded proposal-id) "Proposal did not succeed")
      (enforce (>= (at 'block-time (chain-data)) eta) "Timelock not expired")
      
      ;; Mark as executed
      (update proposals proposal-id { "executed": true })
      
      ;; Emit event
      (emit-event (PROPOSAL_EXECUTED proposal-id))
      
      ;; Note: Actual execution of proposal actions would happen here
      ;; For now, we just mark it as executed
      (format "Proposal {} executed" [proposal-id])))
  
  ;; Batch operations
  (defun get-active-proposals:[object] ()
    @doc "Get all active proposals"
    (let ((current-block (at 'block-height (chain-data))))
      (select proposals 
        (and? (where 'cancelled (= false))
             (and? (where 'executed (= false))
                  (and? (where 'start-block (<= current-block))
                       (where 'end-block (>= current-block))))))))
  
  (defun get-proposals-by-state:[object] (state:string)
    @doc "Get proposals by state"
    (filter (lambda (proposal:object)
              (= (get-proposal-state (at 'id proposal)) state))
            (select proposals (constantly true))))
)

;; Create tables
(create-table proposals)
(create-table receipts)