# Chapter 21: Building a DAO Governance System

## Table of Contents
1. [Project Overview](#project-overview)
2. [DAO Architecture](#dao-architecture)
3. [Step 1: Governance Token](#step-1-governance-token)
4. [Step 2: Proposal System](#step-2-proposal-system)
5. [Step 3: Voting Mechanism](#step-3-voting-mechanism)
6. [Step 4: Delegation System](#step-4-delegation-system)
7. [Step 5: Timelock Controller](#step-5-timelock-controller)
8. [Step 6: Treasury Management](#step-6-treasury-management)
9. [Testing & Security](#testing-security)
10. [Frontend Integration](#frontend-integration)

## Project Overview

In this chapter, we'll build a complete DAO (Decentralized Autonomous Organization) governance system featuring:
- ERC20-style governance token with voting power
- Proposal creation and management
- Delegated voting capabilities
- Quorum and threshold mechanisms
- Timelock execution for safety
- Treasury management
- Multi-signature capabilities

### What You'll Learn
- Implementing on-chain governance
- Building secure voting mechanisms
- Managing delegated voting power
- Creating timelock controls
- Handling treasury operations
- Ensuring governance security

## DAO Architecture

### Core Components
```
dao-governance/
├── core/
│   ├── governance-token.pact    # Voting power token
│   ├── governor.pact           # Main governance logic
│   └── timelock.pact          # Execution delay
├── voting/
│   ├── proposals.pact         # Proposal management
│   ├── voting.pact           # Voting logic
│   └── delegation.pact       # Vote delegation
├── treasury/
│   ├── treasury.pact         # DAO treasury
│   ├── payments.pact         # Payment execution
│   └── vesting.pact          # Token vesting
└── utils/
    ├── snapshot.pact         # Balance snapshots
    ├── quorum.pact          # Quorum calculations
    └── upgrades.pact        # Upgrade mechanism
```

## Step 1: Governance Token

Let's start with the governance token that tracks voting power:

```pact
;; governance-token.pact
(module governance-token GOVERNANCE
  @doc "DAO governance token with voting and delegation"
  
  ;; Implements fungible-v2
  (implements fungible-v2)
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dao-admin))
  
  ;; Schemas
  (defschema account
    @doc "Account with balance and voting power"
    balance:decimal
    guard:guard
    votes:decimal           ;; Current voting power
    delegates:string        ;; Address delegated to
    checkpoint-count:integer) ;; Number of checkpoints
  
  (defschema checkpoint
    @doc "Historical balance checkpoint"
    account:string
    checkpoint-num:integer
    block-height:integer
    balance:decimal
    votes:decimal)
  
  (defschema delegate-info
    @doc "Delegation information"
    delegator:string
    delegatee:string
    amount:decimal
    timestamp:time)
  
  ;; Tables
  (deftable accounts:{account})
  (deftable checkpoints:{checkpoint})
  (deftable delegations:{delegate-info})
  
  ;; Events
  (defcap DELEGATE:bool (delegator:string delegatee:string amount:decimal)
    @doc "Delegation event"
    @event
    true)
  
  (defcap DELEGATE_VOTES_CHANGED:bool (delegate:string previous:decimal new:decimal)
    @doc "Delegate votes changed event"
    @event
    true)
  
  ;; Constants
  (defconst DECIMALS 18)
  (defconst VOTE_PRECISION 1000000) ;; 6 decimal places
  
  ;; Core token functions
  (defun create-account:string (account:string guard:guard)
    @doc "Create new account"
    (insert accounts account {
      "balance": 0.0,
      "guard": guard,
      "votes": 0.0,
      "delegates": account,  ;; Self-delegate by default
      "checkpoint-count": 0
    }))
  
  (defun get-balance:decimal (account:string)
    @doc "Get account balance"
    (at 'balance (read accounts account)))
  
  (defun get-votes:decimal (account:string)
    @doc "Get current voting power"
    (at 'votes (read accounts account)))
  
  (defun transfer:string (sender:string receiver:string amount:decimal)
    @doc "Transfer tokens and update voting power"
    (enforce (!= sender receiver) "Cannot transfer to self")
    (enforce (> amount 0.0) "Amount must be positive")
    
    (with-capability (TRANSFER sender receiver amount)
      ;; Move tokens
      (debit sender amount)
      (credit receiver amount)
      
      ;; Update voting power
      (move-voting-power sender receiver amount)))
  
  ;; Delegation functions
  (defun delegate:string (delegator:string delegatee:string)
    @doc "Delegate voting power to another address"
    (with-read accounts delegator 
      { "balance" := balance
      , "delegates" := current-delegate }
      
      (enforce (!= delegatee current-delegate) "Already delegated to this address")
      
      ;; Update delegation
      (update accounts delegator { "delegates": delegatee })
      
      ;; Move voting power
      (if (!= current-delegate delegator)
        (move-voting-power current-delegate delegatee balance)
        (move-voting-power delegator delegatee balance))
      
      ;; Record delegation
      (insert delegations (format "{}:{}" [delegator delegatee]) {
        "delegator": delegator,
        "delegatee": delegatee,
        "amount": balance,
        "timestamp": (at 'block-time (chain-data))
      })
      
      ;; Emit event
      (emit-event (DELEGATE delegator delegatee balance))
      
      (format "Delegated {} votes from {} to {}" [balance delegator delegatee])))
  
  (defun move-voting-power:string (from:string to:string amount:decimal)
    @doc "Move voting power between addresses"
    (if (!= from to)
      (do
        (if (!= from "")
          (let ((previous-votes (get-votes from))
                (new-votes (- previous-votes amount)))
            (update accounts from { "votes": new-votes })
            (write-checkpoint from new-votes)
            (emit-event (DELEGATE_VOTES_CHANGED from previous-votes new-votes))))
        
        (if (!= to "")
          (let ((previous-votes (get-votes to))
                (new-votes (+ previous-votes amount)))
            (update accounts to { "votes": new-votes })
            (write-checkpoint to new-votes)
            (emit-event (DELEGATE_VOTES_CHANGED to previous-votes new-votes)))))
      "No delegation change"))
  
  ;; Checkpoint functions for historical voting power
  (defun write-checkpoint:string (account:string new-votes:decimal)
    @doc "Write voting power checkpoint"
    (with-read accounts account 
      { "checkpoint-count" := count
      , "balance" := balance }
      
      (let ((checkpoint-id (format "{}:{}" [account count]))
            (block-height (at 'block-height (chain-data))))
        
        (insert checkpoints checkpoint-id {
          "account": account,
          "checkpoint-num": count,
          "block-height": block-height,
          "balance": balance,
          "votes": new-votes
        })
        
        (update accounts account { "checkpoint-count": (+ count 1) }))))
  
  (defun get-prior-votes:decimal (account:string block-height:integer)
    @doc "Get voting power at specific block height"
    (with-read accounts account { "checkpoint-count" := count }
      (if (= count 0)
        0.0
        (let ((checkpoint-id (format "{}:{}" [account (- count 1)])))
          (with-read checkpoints checkpoint-id 
            { "block-height" := cp-height
            , "votes" := cp-votes }
            (if (<= cp-height block-height)
              cp-votes
              (get-checkpoint-votes account block-height 0 (- count 1))))))))
  
  (defun get-checkpoint-votes:decimal (account:string block-height:integer low:integer high:integer)
    @doc "Binary search for checkpoint votes"
    (if (> low high)
      0.0
      (let* ((mid (/ (+ low high) 2))
             (checkpoint-id (format "{}:{}" [account mid])))
        (with-read checkpoints checkpoint-id 
          { "block-height" := cp-height
          , "votes" := cp-votes }
          (if (= cp-height block-height)
            cp-votes
            (if (< cp-height block-height)
              (if (or (= mid high) 
                     (< (at 'block-height (read checkpoints (format "{}:{}" [account (+ mid 1)]))) 
                        block-height))
                cp-votes
                (get-checkpoint-votes account block-height (+ mid 1) high))
              (get-checkpoint-votes account block-height low (- mid 1))))))))
  
  ;; Additional helper functions
  (defun get-delegates:string (account:string)
    @doc "Get current delegate for account"
    (at 'delegates (read accounts account)))
  
  (defun get-delegation-info:object (delegator:string delegatee:string)
    @doc "Get delegation details"
    (read delegations (format "{}:{}" [delegator delegatee])))
  
  ;; Supply management functions (implement remaining fungible-v2 interface)
  (defschema supply
    supply:decimal)
  
  (deftable supply-table:{supply})
  
  (defun total-supply:decimal ()
    @doc "Get total supply"
    (with-default-read supply-table "total"
      { "supply": 0.0 }
      { "supply" := s }
      s))
  
  (defun mint:string (account:string amount:decimal)
    @doc "Mint new tokens (governance only)"
    (with-capability (GOVERNANCE)
      (credit account amount)
      (with-default-read supply-table "total"
        { "supply": 0.0 }
        { "supply" := s }
        (write supply-table "total" { "supply": (+ s amount) }))))
  
  ;; Standard fungible-v2 functions
  (defun precision:integer () DECIMALS)
  
  (defun enforce-unit:bool (amount:decimal)
    (enforce (>= (floor amount DECIMALS) amount) "Precision violation"))
  
  (defun details:object{fungible-v2.account} (account:string)
    (with-read accounts account 
      { "balance" := bal, "guard" := g }
      { "account": account, "balance": bal, "guard": g }))
  
  (defun rotate:string (account:string new-guard:guard)
    (with-read accounts account { "guard" := old-guard }
      (enforce-guard old-guard)
      (update accounts account { "guard": new-guard })))
  
  ;; Internal functions
  (defun debit:string (account:string amount:decimal)
    (with-read accounts account { "balance" := balance }
      (enforce (>= balance amount) "Insufficient balance")
      (update accounts account { "balance": (- balance amount) })))
  
  (defun credit:string (account:string amount:decimal)
    (with-read accounts account { "balance" := balance }
      (update accounts account { "balance": (+ balance amount) })))
  
  (defun transfer-create:string (sender:string receiver:string receiver-guard:guard amount:decimal)
    (transfer sender receiver amount))
)

;; Create tables
(create-table accounts)
(create-table checkpoints)
(create-table delegations)
(create-table supply-table)
```

## Step 2: Proposal System

Now let's implement the proposal management system:

```pact
;; proposals.pact
(module proposals GOVERNANCE
  @doc "DAO proposal creation and management"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dao-admin))
  
  ;; Schemas
  (defschema proposal
    @doc "Proposal information"
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
          "eta": (at 'block-time (chain-data))
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
)

;; Create tables
(create-table proposals)
(create-table receipts)
```

## Step 3: Voting Mechanism

Let's implement the core voting logic:

```pact
;; voting.pact
(module voting GOVERNANCE
  @doc "Advanced voting mechanisms for DAO"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dao-admin))
  
  ;; Schemas
  (defschema vote-config
    @doc "Voting configuration"
    vote-weight-type:string    ;; "linear", "quadratic", "capped"
    min-turnout:decimal        ;; Minimum participation
    super-majority:decimal     ;; Required approval percentage
    vote-cap:decimal)          ;; Max votes per address (for capped)
  
  (defschema delegation-record
    @doc "Vote delegation tracking"
    delegator:string
    delegatee:string
    proposal-id:string
    voting-power:decimal
    timestamp:time)
  
  ;; Tables
  (deftable vote-configs:{vote-config})
  (deftable delegation-records:{delegation-record})
  
  ;; Capabilities
  (defcap VOTE:bool (voter:string proposal-id:string)
    @doc "Capability to vote"
    (enforce-guard (at 'guard (governance-token.details voter))))
  
  ;; Vote weight calculations
  (defun calculate-vote-weight:decimal (voter:string amount:decimal vote-type:string)
    @doc "Calculate voting weight based on type"
    (cond
      ((= vote-type "linear") amount)
      ((= vote-type "quadratic") (sqrt amount))
      ((= vote-type "capped") 
       (let ((cap (at 'vote-cap (read vote-configs "default"))))
         (if (> amount cap) cap amount)))
      (enforce false "Unknown vote weight type")))
  
  ;; Batch voting
  (defun cast-votes-batch:[string] (voter:string votes:[object])
    @doc "Cast multiple votes in one transaction"
    (map (lambda (vote:object)
           (let ((proposal-id (at 'proposal-id vote))
                 (support (at 'support vote)))
             (proposals.cast-vote proposal-id voter support)))
         votes))
  
  ;; Delegated voting
  (defun cast-vote-by-sig:string 
    ( proposal-id:string
      support:integer
      voter:string
      nonce:integer
      signature:string )
    @doc "Cast vote using signature (gasless voting)"
    
    ;; Verify signature
    (let* ((message (format "{}:{}:{}:{}" [proposal-id support voter nonce]))
           (message-hash (hash message))
           (pubkey (at 'public-key (read voter-keys voter))))
      
      (enforce (verify-sig pubkey message-hash signature) "Invalid signature")
      
      ;; Check nonce
      (with-default-read nonces voter
        { "nonce": -1 }
        { "nonce" := current-nonce }
        (enforce (= nonce (+ current-nonce 1)) "Invalid nonce"))
      
      ;; Cast vote
      (proposals.cast-vote proposal-id voter support)
      
      ;; Update nonce
      (write nonces voter { "nonce": nonce })))
  
  ;; Vote delegation for specific proposals
  (defun delegate-vote:string (proposal-id:string delegator:string delegatee:string)
    @doc "Delegate vote for specific proposal"
    
    (with-capability (VOTE delegator proposal-id)
      ;; Check if already voted
      (enforce (not (proposals.has-voted proposal-id delegator)) 
               "Already voted on this proposal")
      
      ;; Get voting power
      (let ((voting-power (governance-token.get-votes delegator)))
        
        ;; Record delegation
        (insert delegation-records (format "{}:{}:{}" [proposal-id delegator delegatee]) {
          "delegator": delegator,
          "delegatee": delegatee,
          "proposal-id": proposal-id,
          "voting-power": voting-power,
          "timestamp": (at 'block-time (chain-data))
        })
        
        (format "Delegated {} votes to {} for proposal {}" 
                [voting-power delegatee proposal-id]))))
  
  ;; Split voting
  (defun cast-split-vote:string 
    ( proposal-id:string
      voter:string
      for-votes:decimal
      against-votes:decimal
      abstain-votes:decimal )
    @doc "Cast a split vote (for advanced voting systems)"
    
    (with-capability (VOTE voter proposal-id)
      (let ((total-votes (governance-token.get-votes voter))
            (vote-sum (+ for-votes (+ against-votes abstain-votes))))
        
        ;; Validate split
        (enforce (= vote-sum total-votes) 
                 (format "Vote split must equal total votes: {} != {}" [vote-sum total-votes]))
        (enforce (>= for-votes 0.0) "For votes must be non-negative")
        (enforce (>= against-votes 0.0) "Against votes must be non-negative")
        (enforce (>= abstain-votes 0.0) "Abstain votes must be non-negative")
        
        ;; Record split vote (would need to modify proposal system to support this)
        (format "Split vote cast: {} for, {} against, {} abstain" 
                [for-votes against-votes abstain-votes]))))
  
  ;; Conviction voting (votes get stronger over time)
  (defschema conviction-vote
    @doc "Conviction voting record"
    voter:string
    proposal-id:string
    support:integer
    amount:decimal
    start-time:time
    conviction:decimal)
  
  (deftable conviction-votes:{conviction-vote})
  
  (defun calculate-conviction:decimal (amount:decimal duration:decimal)
    @doc "Calculate conviction based on time locked"
    ;; Conviction = amount * (1 + duration_in_days / 100)
    (let ((days (/ duration 86400.0)))
      (* amount (+ 1.0 (/ days 100.0)))))
  
  (defun cast-conviction-vote:string (proposal-id:string voter:string support:integer)
    @doc "Cast a conviction vote that grows stronger over time"
    
    (with-capability (VOTE voter proposal-id)
      (let ((amount (governance-token.get-votes voter))
            (current-time (at 'block-time (chain-data))))
        
        (insert conviction-votes (format "{}:{}" [proposal-id voter]) {
          "voter": voter,
          "proposal-id": proposal-id,
          "support": support,
          "amount": amount,
          "start-time": current-time,
          "conviction": amount
        })
        
        (format "Conviction vote started with {} tokens" [amount]))))
  
  ;; Ranked choice voting
  (defschema ranked-vote
    @doc "Ranked choice vote"
    voter:string
    proposal-id:string
    rankings:[string]    ;; Ordered list of choices
    voting-power:decimal)
  
  (deftable ranked-votes:{ranked-vote})
  
  (defun cast-ranked-vote:string (proposal-id:string voter:string choices:[string])
    @doc "Cast a ranked choice vote"
    
    (with-capability (VOTE voter proposal-id)
      (let ((voting-power (governance-token.get-votes voter)))
        
        ;; Validate no duplicates
        (enforce (= (length choices) (length (distinct choices)))
                 "Duplicate choices not allowed")
        
        (insert ranked-votes (format "{}:{}" [proposal-id voter]) {
          "voter": voter,
          "proposal-id": proposal-id,
          "rankings": choices,
          "voting-power": voting-power
        })
        
        (format "Ranked vote cast with {} choices" [(length choices)]))))
  
  ;; Vote privacy (commit-reveal)
  (defschema vote-commitment
    @doc "Committed vote (hidden)"
    voter:string
    proposal-id:string
    commitment:string
    revealed:bool)
  
  (deftable vote-commitments:{vote-commitment})
  
  (defun commit-vote:string (proposal-id:string voter:string commitment:string)
    @doc "Commit to a vote (hidden)"
    
    (with-capability (VOTE voter proposal-id)
      (insert vote-commitments (format "{}:{}" [proposal-id voter]) {
        "voter": voter,
        "proposal-id": proposal-id,
        "commitment": commitment,
        "revealed": false
      })
      
      "Vote committed"))
  
  (defun reveal-vote:string (proposal-id:string voter:string support:integer nonce:string)
    @doc "Reveal a committed vote"
    
    (with-read vote-commitments (format "{}:{}" [proposal-id voter])
      { "commitment" := commitment
      , "revealed" := revealed }
      
      (enforce (not revealed) "Vote already revealed")
      
      ;; Verify commitment
      (let ((computed-commitment (hash (format "{}:{}:{}" [proposal-id support nonce]))))
        (enforce (= commitment computed-commitment) "Invalid commitment"))
      
      ;; Update commitment
      (update vote-commitments (format "{}:{}" [proposal-id voter])
        { "revealed": true })
      
      ;; Cast actual vote
      (proposals.cast-vote proposal-id voter support)))
  
  ;; Helper tables
  (defschema voter-key
    public-key:string)
  
  (defschema nonce-record
    nonce:integer)
  
  (deftable voter-keys:{voter-key})
  (deftable nonces:{nonce-record})
)

;; Create tables
(create-table vote-configs)
(create-table delegation-records)
(create-table conviction-votes)
(create-table ranked-votes)
(create-table vote-commitments)
(create-table voter-keys)
(create-table nonces)

;; Initialize default config
(insert vote-configs "default" {
  "vote-weight-type": "linear",
  "min-turnout": 0.1,
  "super-majority": 0.66,
  "vote-cap": 1000000.0
})
```

## Step 4: Delegation System

Now let's create an advanced delegation system:

```pact
;; delegation.pact
(module delegation GOVERNANCE
  @doc "Advanced delegation mechanisms for DAO voting"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dao-admin))
  
  ;; Schemas
  (defschema delegation-config
    @doc "Delegation configuration"
    delegator:string
    delegatee:string
    percentage:decimal      ;; Percentage of voting power delegated
    topics:[string]        ;; Specific topics/tags delegated
    expiry:time           ;; When delegation expires
    revocable:bool)       ;; Can be revoked before expiry
  
  (defschema delegation-history
    @doc "Historical delegation record"
    delegator:string
    delegatee:string
    amount:decimal
    action:string         ;; "delegate", "revoke", "expire"
    timestamp:time)
  
  (defschema delegate-profile
    @doc "Delegate profile information"
    delegate:string
    name:string
    description:string
    website:string
    total-delegated:decimal
    delegator-count:integer
    voting-record:object)  ;; Stats on voting behavior
  
  ;; Tables
  (deftable delegation-configs:{delegation-config})
  (deftable delegation-history:{delegation-history})
  (deftable delegate-profiles:{delegate-profile})
  
  ;; Events
  (defcap DELEGATION_CREATED:bool (delegator:string delegatee:string percentage:decimal)
    @doc "Delegation created event"
    @event
    true)
  
  (defcap DELEGATION_REVOKED:bool (delegator:string delegatee:string)
    @doc "Delegation revoked event"
    @event
    true)
  
  ;; Capabilities
  (defcap DELEGATE:bool (delegator:string)
    @doc "Capability to delegate"
    (enforce-guard (at 'guard (governance-token.details delegator))))
  
  ;; Partial delegation
  (defun delegate-partial:string 
    ( delegator:string
      delegatee:string
      percentage:decimal
      topics:[string]
      duration:integer )
    @doc "Delegate a percentage of voting power for specific topics"
    
    (with-capability (DELEGATE delegator)
      ;; Validate inputs
      (enforce (> percentage 0.0) "Percentage must be positive")
      (enforce (<= percentage 100.0) "Percentage cannot exceed 100")
      (enforce (!= delegator delegatee) "Cannot delegate to self")
      
      ;; Calculate expiry
      (let ((expiry-time (add-time (at 'block-time (chain-data)) (days duration))))
        
        ;; Check existing delegations don't exceed 100%
        (let ((existing-percentage (get-total-delegated-percentage delegator)))
          (enforce (<= (+ existing-percentage percentage) 100.0) 
                   "Total delegations would exceed 100%"))
        
        ;; Create delegation
        (insert delegation-configs (format "{}:{}" [delegator delegatee]) {
          "delegator": delegator,
          "delegatee": delegatee,
          "percentage": percentage,
          "topics": topics,
          "expiry": expiry-time,
          "revocable": true
        })
        
        ;; Update voting power
        (let ((delegator-votes (governance-token.get-votes delegator))
              (delegated-amount (* delegator-votes (/ percentage 100.0))))
          
          ;; Transfer partial voting power
          (governance-token.move-voting-power delegator delegatee delegated-amount)
          
          ;; Record history
          (record-delegation-history delegator delegatee delegated-amount "delegate")
          
          ;; Update delegate profile
          (update-delegate-profile delegatee delegated-amount 1)
          
          ;; Emit event
          (emit-event (DELEGATION_CREATED delegator delegatee percentage))
          
          (format "Delegated {}% of voting power to {} until {}" 
                  [percentage delegatee expiry-time])))))
  
  ;; Multi-delegation
  (defun delegate-multiple:string (delegator:string delegations:[object])
    @doc "Delegate to multiple addresses with different percentages"
    
    (with-capability (DELEGATE delegator)
      ;; Validate total percentage
      (let ((total-percentage (fold (+) 0.0 (map (at 'percentage) delegations))))
        (enforce (<= total-percentage 100.0) "Total delegation exceeds 100%"))
      
      ;; Process each delegation
      (map (lambda (delegation:object)
             (delegate-partial 
               delegator
               (at 'delegatee delegation)
               (at 'percentage delegation)
               (at 'topics delegation [])
               (at 'duration delegation 90)))
           delegations)
      
      (format "Created {} delegations" [(length delegations)])))
  
  ;; Liquid delegation (delegation chains)
  (defun get-delegation-chain:[string] (address:string)
    @doc "Get the full delegation chain for an address"
    (let ((delegatee (governance-token.get-delegates address)))
      (if (= delegatee address)
        []
        (+ [delegatee] (get-delegation-chain delegatee)))))
  
  ;; Revoke delegation
  (defun revoke-delegation:string (delegator:string delegatee:string)
    @doc "Revoke an existing delegation"
    
    (with-capability (DELEGATE delegator)
      (with-read delegation-configs (format "{}:{}" [delegator delegatee])
        { "percentage" := percentage
        , "revocable" := revocable }
        
        (enforce revocable "Delegation is not revocable")
        
        ;; Calculate amount to return
        (let ((delegator-original-votes (governance-token.get-balance delegator))
              (delegated-amount (* delegator-original-votes (/ percentage 100.0))))
          
          ;; Return voting power
          (governance-token.move-voting-power delegatee delegator delegated-amount)
          
          ;; Remove delegation
          (update delegation-configs (format "{}:{}" [delegator delegatee])
            { "percentage": 0.0, "expiry": (at 'block-time (chain-data)) })
          
          ;; Record history
          (record-delegation-history delegator delegatee delegated-amount "revoke")
          
          ;; Update delegate profile
          (update-delegate-profile delegatee (- delegated-amount) -1)
          
          ;; Emit event
          (emit-event (DELEGATION_REVOKED delegator delegatee))
          
          (format "Revoked delegation to {}, recovered {} voting power" 
                  [delegatee delegated-amount])))))
  
  ;; Delegate profiles
  (defun create-delegate-profile:string 
    ( delegate:string
      name:string
      description:string
      website:string )
    @doc "Create or update delegate profile"
    
    (enforce-guard (at 'guard (governance-token.details delegate)))
    
    (write delegate-profiles delegate {
      "delegate": delegate,
      "name": name,
      "description": description,
      "website": website,
      "total-delegated": 0.0,
      "delegator-count": 0,
      "voting-record": {}
    })
    
    (format "Profile created for delegate {}" [delegate]))
  
  ;; Helper functions
  (defun get-total-delegated-percentage:decimal (delegator:string)
    @doc "Get total percentage currently delegated"
    (let ((delegations (select delegation-configs 
                         (and? (where 'delegator (= delegator))
                              (where 'expiry (< (at 'block-time (chain-data))))))))
      (fold (+) 0.0 (map (at 'percentage) delegations))))
  
  (defun record-delegation-history:string 
    ( delegator:string 
      delegatee:string 
      amount:decimal 
      action:string )
    @doc "Record delegation action in history"
    (insert delegation-history (format "{}:{}:{}" [delegator delegatee (at 'block-time (chain-data))]) {
      "delegator": delegator,
      "delegatee": delegatee,
      "amount": amount,
      "action": action,
      "timestamp": (at 'block-time (chain-data))
    }))
  
  (defun update-delegate-profile:string (delegate:string amount:decimal count-change:integer)
    @doc "Update delegate profile stats"
    (with-default-read delegate-profiles delegate
      { "total-delegated": 0.0
      , "delegator-count": 0 }
      { "total-delegated" := current-total
      , "delegator-count" := current-count }
      
      (update delegate-profiles delegate {
        "total-delegated": (+ current-total amount),
        "delegator-count": (+ current-count count-change)
      })))
  
  ;; Query functions
  (defun get-delegations:[object] (delegator:string)
    @doc "Get all active delegations for a delegator"
    (select delegation-configs 
      (and? (where 'delegator (= delegator))
           (where 'expiry (> (at 'block-time (chain-data)))))))
  
  (defun get-delegators:[object] (delegatee:string)
    @doc "Get all delegators for a delegate"
    (select delegation-configs 
      (and? (where 'delegatee (= delegatee))
           (where 'expiry (> (at 'block-time (chain-data)))))))
  
  (defun get-delegate-profile:object (delegate:string)
    @doc "Get delegate profile information"
    (read delegate-profiles delegate))
  
  (defun get-top-delegates:[object] (limit:integer)
    @doc "Get top delegates by total delegated amount"
    (take limit 
      (sort ['total-delegated] 
        (select delegate-profiles (constantly true)))))
)

;; Create tables
(create-table delegation-configs)
(create-table delegation-history)
(create-table delegate-profiles)
```

## Step 5: Timelock Controller

Let's implement the timelock controller for secure execution:

```pact
;; timelock.pact
(module timelock GOVERNANCE
  @doc "Timelock controller for DAO proposal execution"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dao-admin))
  
  ;; Schemas
  (defschema operation
    @doc "Scheduled operation"
    id:string
    targets:[string]
    values:[decimal]
    functions:[string]
    calldatas:[string]
    predecessor:string     ;; Previous operation that must execute first
    salt:string
    delay:integer         ;; Delay in seconds
    timestamp:time        ;; When operation becomes ready
    status:string)        ;; "pending", "ready", "executed", "cancelled"
  
  (defschema role-member
    @doc "Role membership"
    role:string
    member:string
    granted-by:string
    granted-at:time)
  
  ;; Tables
  (deftable operations:{operation})
  (deftable role-members:{role-member})
  
  ;; Events
  (defcap CALL_SCHEDULED:bool (id:string index:integer target:string value:decimal data:string delay:integer)
    @doc "Operation scheduled event"
    @event
    true)
  
  (defcap CALL_EXECUTED:bool (id:string index:integer target:string value:decimal data:string)
    @doc "Operation executed event"
    @event
    true)
  
  (defcap CANCELLED:bool (id:string)
    @doc "Operation cancelled event"
    @event
    true)
  
  ;; Constants
  (defconst MIN_DELAY 172800)    ;; 2 days minimum delay
  (defconst PROPOSER_ROLE "proposer")
  (defconst EXECUTOR_ROLE "executor")
  (defconst ADMIN_ROLE "admin")
  
  ;; Role management
  (defcap HAS_ROLE:bool (role:string account:string)
    @doc "Check if account has role"
    (with-default-read role-members (format "{}:{}" [role account])
      { "member": "" }
      { "member" := member }
      (= member account)))
  
  (defun grant-role:string (role:string account:string)
    @doc "Grant role to account"
    (with-capability (HAS_ROLE ADMIN_ROLE (at 'sender (chain-data)))
      (insert role-members (format "{}:{}" [role account]) {
        "role": role,
        "member": account,
        "granted-by": (at 'sender (chain-data)),
        "granted-at": (at 'block-time (chain-data))
      })
      (format "Granted {} role to {}" [role account])))
  
  (defun revoke-role:string (role:string account:string)
    @doc "Revoke role from account"
    (with-capability (HAS_ROLE ADMIN_ROLE (at 'sender (chain-data)))
      (update role-members (format "{}:{}" [role account])
        { "member": "" })
      (format "Revoked {} role from {}" [role account])))
  
  ;; Scheduling operations
  (defun schedule:string 
    ( targets:[string]
      values:[decimal]  
      functions:[string]
      calldatas:[string]
      predecessor:string
      salt:string
      delay:integer )
    @doc "Schedule an operation"
    
    (with-capability (HAS_ROLE PROPOSER_ROLE (at 'sender (chain-data)))
      ;; Validate inputs
      (enforce (>= delay MIN_DELAY) 
               (format "Delay must be at least {} seconds" [MIN_DELAY]))
      (enforce (= (length targets) (length values)) "Mismatched targets/values")
      (enforce (= (length targets) (length functions)) "Mismatched targets/functions")  
      (enforce (= (length targets) (length calldatas)) "Mismatched targets/calldatas")
      
      ;; Generate operation ID
      (let* ((id (hash-operation targets values functions calldatas predecessor salt))
             (timestamp (add-time (at 'block-time (chain-data)) (seconds delay))))
        
        ;; Check not already scheduled
        (with-default-read operations id
          { "status": "" }
          { "status" := status }
          (enforce (= status "") "Operation already scheduled"))
        
        ;; Schedule operation
        (insert operations id {
          "id": id,
          "targets": targets,
          "values": values,
          "functions": functions,
          "calldatas": calldatas,
          "predecessor": predecessor,
          "salt": salt,
          "delay": delay,
          "timestamp": timestamp,
          "status": "pending"
        })
        
        ;; Emit events for each call
        (map (lambda (idx:integer)
               (emit-event (CALL_SCHEDULED 
                 id idx 
                 (at idx targets)
                 (at idx values)
                 (at idx calldatas)
                 delay)))
             (enumerate 0 (- (length targets) 1)))
        
        (format "Operation {} scheduled for execution at {}" [id timestamp]))))
  
  ;; Execute operations
  (defun execute:string
    ( targets:[string]
      values:[decimal]
      functions:[string]
      calldatas:[string]
      predecessor:string
      salt:string )
    @doc "Execute a scheduled operation"
    
    (with-capability (HAS_ROLE EXECUTOR_ROLE (at 'sender (chain-data)))
      (let ((id (hash-operation targets values functions calldatas predecessor salt)))
        
        ;; Check operation is ready
        (with-read operations id 
          { "timestamp" := timestamp
          , "status" := status
          , "predecessor" := pred }
          
          (enforce (= status "pending") "Operation not pending")
          (enforce (<= timestamp (at 'block-time (chain-data))) "Operation not ready")
          
          ;; Check predecessor executed if set
          (if (!= pred "")
            (with-read operations pred { "status" := pred-status }
              (enforce (= pred-status "executed") "Predecessor not executed"))
            "No predecessor")
          
          ;; Update status
          (update operations id { "status": "ready" })
          
          ;; Execute each call
          (map (lambda (idx:integer)
                 (let ((target (at idx targets))
                       (value (at idx values))
                       (function (at idx functions))
                       (calldata (at idx calldatas)))
                   
                   ;; Execute the call (simplified - would need proper module calls)
                   (emit-event (CALL_EXECUTED id idx target value calldata))
                   
                   (format "Executed call {} on {}" [idx target])))
               (enumerate 0 (- (length targets) 1)))
          
          ;; Mark as executed
          (update operations id { "status": "executed" })
          
          (format "Operation {} executed successfully" [id])))))
  
  ;; Cancel operations
  (defun cancel:string (id:string)
    @doc "Cancel a scheduled operation"
    
    (with-capability (HAS_ROLE ADMIN_ROLE (at 'sender (chain-data)))
      (with-read operations id { "status" := status }
        (enforce (= status "pending") "Can only cancel pending operations")
        
        (update operations id { "status": "cancelled" })
        
        (emit-event (CANCELLED id))
        
        (format "Operation {} cancelled" [id]))))
  
  ;; Helper functions
  (defun hash-operation:string 
    ( targets:[string]
      values:[decimal]
      functions:[string]
      calldatas:[string]
      predecessor:string
      salt:string )
    @doc "Generate operation ID"
    (hash (format "{}:{}:{}:{}:{}:{}" 
                  [targets values functions calldatas predecessor salt])))
  
  (defun is-operation-ready:bool (id:string)
    @doc "Check if operation is ready for execution"
    (with-read operations id 
      { "timestamp" := timestamp
      , "status" := status }
      (and (= status "pending")
           (<= timestamp (at 'block-time (chain-data))))))
  
  (defun get-operation:object (id:string)
    @doc "Get operation details"
    (read operations id))
  
  (defun get-min-delay:integer ()
    @doc "Get minimum delay"
    MIN_DELAY)
  
  ;; Batch operations
  (defun schedule-batch:[string] (operations:[object])
    @doc "Schedule multiple operations"
    (map (lambda (op:object)
           (schedule 
             (at 'targets op)
             (at 'values op)
             (at 'functions op)
             (at 'calldatas op)
             (at 'predecessor op "")
             (at 'salt op)
             (at 'delay op MIN_DELAY)))
         operations))
)

;; Create tables
(create-table operations)
(create-table role-members)

;; Initialize admin role
(grant-role ADMIN_ROLE 'dao-admin)
```

## Step 6: Treasury Management

Finally, let's implement the DAO treasury:

```pact
;; treasury.pact
(module treasury GOVERNANCE
  @doc "DAO treasury management system"
  
  ;; Governance
  (defcap GOVERNANCE ()
    (enforce-keyset 'dao-admin))
  
  ;; Schemas
  (defschema treasury-asset
    @doc "Asset held in treasury"
    token:module{fungible-v2}
    balance:decimal
    reserved:decimal      ;; Amount reserved for pending payments
    last-update:time)
  
  (defschema payment-request
    @doc "Payment request from treasury"
    id:string
    recipient:string
    token:module{fungible-v2}
    amount:decimal
    description:string
    category:string       ;; "grant", "expense", "investment", etc
    proposal-id:string
    status:string         ;; "pending", "approved", "executed", "cancelled"
    created:time
    executed:time)
  
  (defschema budget-allocation
    @doc "Budget allocation by category"
    category:string
    token:module{fungible-v2}
    allocated:decimal
    spent:decimal
    period-start:time
    period-end:time)
  
  (defschema investment
    @doc "Treasury investment record"
    id:string
    protocol:string
    token:module{fungible-v2}
    amount:decimal
    expected-return:decimal
    start-time:time
    end-time:time
    status:string)
  
  ;; Tables
  (deftable treasury-assets:{treasury-asset})
  (deftable payment-requests:{payment-request})
  (deftable budget-allocations:{budget-allocation})
  (deftable investments:{investment})
  
  ;; Events
  (defcap PAYMENT_REQUESTED:bool (id:string recipient:string amount:decimal)
    @doc "Payment request created"
    @event
    true)
  
  (defcap PAYMENT_EXECUTED:bool (id:string recipient:string amount:decimal)
    @doc "Payment executed"
    @event
    true)
  
  (defcap INVESTMENT_MADE:bool (id:string protocol:string amount:decimal)
    @doc "Investment made"
    @event
    true)
  
  ;; Capabilities
  (defcap TREASURY_OPERATION:bool ()
    @doc "Capability for treasury operations"
    (enforce-one "Authorized treasury operation"
      [(enforce-keyset 'dao-admin)
       (enforce-keyset 'treasury-multisig)]))
  
  (defcap SPEND:bool (token:module{fungible-v2} amount:decimal)
    @doc "Capability to spend from treasury"
    (with-read treasury-assets (format "{}" [token])
      { "balance" := balance
      , "reserved" := reserved }
      (enforce (<= amount (- balance reserved)) "Insufficient unreserved balance")))
  
  ;; Treasury account management
  (defun get-treasury-account:string ()
    @doc "Get treasury account name"
    (create-principal (create-capability-guard (TREASURY_OPERATION))))
  
  ;; Asset management
  (defun deposit:string (token:module{fungible-v2} from:string amount:decimal)
    @doc "Deposit tokens to treasury"
    (let ((treasury-account (get-treasury-account)))
      
      ;; Transfer tokens
      (token::transfer from treasury-account amount)
      
      ;; Update balance
      (with-default-read treasury-assets (format "{}" [token])
        { "balance": 0.0, "reserved": 0.0 }
        { "balance" := current-balance
        , "reserved" := reserved }
        
        (write treasury-assets (format "{}" [token]) {
          "token": token,
          "balance": (+ current-balance amount),
          "reserved": reserved,
          "last-update": (at 'block-time (chain-data))
        }))
      
      (format "Deposited {} tokens to treasury" [amount])))
  
  ;; Payment requests
  (defun create-payment-request:string 
    ( recipient:string
      token:module{fungible-v2}
      amount:decimal
      description:string
      category:string
      proposal-id:string )
    @doc "Create a payment request"
    
    (let ((request-id (hash (format "{}:{}:{}:{}" 
                                   [recipient amount description (at 'block-time (chain-data))]))))
      
      ;; Validate category budget
      (validate-budget-allocation category token amount)
      
      ;; Create request
      (insert payment-requests request-id {
        "id": request-id,
        "recipient": recipient,
        "token": token,
        "amount": amount,
        "description": description,
        "category": category,
        "proposal-id": proposal-id,
        "status": "pending",
        "created": (at 'block-time (chain-data)),
        "executed": (at 'block-time (chain-data))
      })
      
      ;; Reserve funds
      (reserve-funds token amount)
      
      ;; Emit event
      (emit-event (PAYMENT_REQUESTED request-id recipient amount))
      
      (format "Payment request {} created for {} tokens to {}" 
              [request-id amount recipient])))
  
  (defun execute-payment:string (request-id:string)
    @doc "Execute an approved payment request"
    
    (with-capability (TREASURY_OPERATION)
      (with-read payment-requests request-id
        { "recipient" := recipient
        , "token" := token
        , "amount" := amount
        , "status" := status
        , "category" := category }
        
        (enforce (= status "approved") "Payment not approved")
        
        (let ((treasury-account (get-treasury-account)))
          
          ;; Execute transfer
          (with-capability (SPEND token amount)
            (install-capability (token::TRANSFER treasury-account recipient amount))
            (token::transfer treasury-account recipient amount))
          
          ;; Update request status
          (update payment-requests request-id {
            "status": "executed",
            "executed": (at 'block-time (chain-data))
          })
          
          ;; Update budget spending
          (update-budget-spending category token amount)
          
          ;; Unreserve funds
          (unreserve-funds token amount)
          
          ;; Emit event
          (emit-event (PAYMENT_EXECUTED request-id recipient amount))
          
          (format "Payment {} executed: {} tokens to {}" 
                  [request-id amount recipient])))))
  
  ;; Budget management
  (defun set-budget:string 
    ( category:string
      token:module{fungible-v2}
      amount:decimal
      period-days:integer )
    @doc "Set budget allocation for category"
    
    (with-capability (GOVERNANCE)
      (let ((period-start (at 'block-time (chain-data)))
            (period-end (add-time (at 'block-time (chain-data)) (days period-days))))
        
        (write budget-allocations (format "{}:{}" [category token]) {
          "category": category,
          "token": token,
          "allocated": amount,
          "spent": 0.0,
          "period-start": period-start,
          "period-end": period-end
        })
        
        (format "Set {} token budget of {} for category {}" 
                [token amount category]))))
  
  (defun validate-budget-allocation:bool (category:string token:module{fungible-v2} amount:decimal)
    @doc "Validate payment against budget"
    (with-default-read budget-allocations (format "{}:{}" [category token])
      { "allocated": 0.0
      , "spent": 0.0
      , "period-end": (at 'block-time (chain-data)) }
      { "allocated" := allocated
      , "spent" := spent
      , "period-end" := period-end }
      
      ;; Check period still valid
      (enforce (>= period-end (at 'block-time (chain-data))) 
               "Budget period expired")
      
      ;; Check budget available
      (enforce (<= (+ spent amount) allocated) 
               (format "Payment would exceed budget: {} + {} > {}" 
                       [spent amount allocated]))))
  
  (defun update-budget-spending:string (category:string token:module{fungible-v2} amount:decimal)
    @doc "Update budget spending"
    (with-read budget-allocations (format "{}:{}" [category token])
      { "spent" := spent }
      (update budget-allocations (format "{}:{}" [category token])
        { "spent": (+ spent amount) })))
  
  ;; Investment management
  (defun invest:string 
    ( protocol:string
      token:module{fungible-v2}
      amount:decimal
      expected-return:decimal
      duration-days:integer )
    @doc "Invest treasury funds"
    
    (with-capability (TREASURY_OPERATION)
      (with-capability (SPEND token amount)
        (let ((investment-id (hash (format "{}:{}:{}:{}" 
                                         [protocol amount (at 'block-time (chain-data))])))
              (end-time (add-time (at 'block-time (chain-data)) (days duration-days))))
          
          ;; Create investment record
          (insert investments investment-id {
            "id": investment-id,
            "protocol": protocol,
            "token": token,
            "amount": amount,
            "expected-return": expected-return,
            "start-time": (at 'block-time (chain-data)),
            "end-time": end-time,
            "status": "active"
          })
          
          ;; Reserve funds
          (reserve-funds token amount)
          
          ;; Would execute actual investment here
          ;; (protocol::deposit treasury-account amount)
          
          ;; Emit event
          (emit-event (INVESTMENT_MADE investment-id protocol amount))
          
          (format "Invested {} tokens in {} (expected return: {})" 
                  [amount protocol expected-return])))))
  
  ;; Helper functions
  (defun reserve-funds:string (token:module{fungible-v2} amount:decimal)
    @doc "Reserve funds for pending payment"
    (with-read treasury-assets (format "{}" [token])
      { "reserved" := reserved }
      (update treasury-assets (format "{}" [token])
        { "reserved": (+ reserved amount) })))
  
  (defun unreserve-funds:string (token:module{fungible-v2} amount:decimal)
    @doc "Unreserve funds after payment"
    (with-read treasury-assets (format "{}" [token])
      { "reserved" := reserved }
      (update treasury-assets (format "{}" [token])
        { "reserved": (- reserved amount) })))
  
  ;; Query functions
  (defun get-treasury-balance:object (token:module{fungible-v2})
    @doc "Get treasury balance for token"
    (read treasury-assets (format "{}" [token])))
  
  (defun get-available-balance:decimal (token:module{fungible-v2})
    @doc "Get available (unreserved) balance"
    (with-read treasury-assets (format "{}" [token])
      { "balance" := balance
      , "reserved" := reserved }
      (- balance reserved)))
  
  (defun get-budget-status:object (category:string token:module{fungible-v2})
    @doc "Get budget status for category"
    (let ((key (format "{}:{}" [category token])))
      (with-read budget-allocations key
        { "allocated" := allocated
        , "spent" := spent }
        { "category": category
        , "allocated": allocated
        , "spent": spent
        , "remaining": (- allocated spent)
        , "utilization": (/ spent allocated) })))
  
  (defun get-pending-payments:[object] ()
    @doc "Get all pending payment requests"
    (select payment-requests (where 'status (= "pending"))))
  
  (defun get-active-investments:[object] ()
    @doc "Get all active investments"
    (select investments (where 'status (= "active"))))
  
  ;; Multi-signature support
  (defschema multisig-approval
    @doc "Multi-signature approval"
    operation-id:string
    approver:string
    approved:bool
    timestamp:time)
  
  (deftable multisig-approvals:{multisig-approval})
  
  (defun approve-operation:string (operation-id:string)
    @doc "Approve a treasury operation"
    (enforce-keyset 'treasury-signer)
    
    (insert multisig-approvals (format "{}:{}" [operation-id (at 'sender (chain-data))]) {
      "operation-id": operation-id,
      "approver": (at 'sender (chain-data)),
      "approved": true,
      "timestamp": (at 'block-time (chain-data))
    })
    
    ;; Check if threshold met
    (let ((approvals (length (select multisig-approvals 
                              (and? (where 'operation-id (= operation-id))
                                   (where 'approved (= true)))))))
      (if (>= approvals 3) ;; Assuming 3-of-5 multisig
        (format "Operation {} approved and ready for execution" [operation-id])
        (format "Operation {} has {} approvals, need {}" [operation-id approvals 3]))))
)

;; Create tables
(create-table treasury-assets)
(create-table payment-requests)
(create-table budget-allocations)
(create-table investments)
(create-table multisig-approvals)
```

## Testing & Security

Let's create comprehensive tests for our DAO:

```pact
;; test-dao.repl
;; Test file for DAO governance system

;; Load modules
(load "governance-token.pact")
(load "proposals.pact")
(load "voting.pact")
(load "delegation.pact")
(load "timelock.pact")
(load "treasury.pact")

;; Setup test environment
(begin-tx)
(env-data {
  "dao-admin": ["admin-key"],
  "treasury-multisig": ["treasury-key-1", "treasury-key-2", "treasury-key-3"],
  "treasury-signer": ["signer-key"]
})
(env-keys ["admin-key", "treasury-key-1", "signer-key"])

;; Define keysets
(define-keyset 'dao-admin (read-keyset 'dao-admin))
(define-keyset 'treasury-multisig (read-keyset 'treasury-multisig))
(define-keyset 'treasury-signer (read-keyset 'treasury-signer))

(commit-tx)

;; Test 1: Token creation and voting power
(begin-tx)
(use governance-token)

;; Create accounts
(env-keys ["alice-key"])
(create-account "alice" (read-keyset 'alice))

(env-keys ["bob-key"])
(create-account "bob" (read-keyset 'bob))

;; Mint tokens
(env-keys ["admin-key"])
(mint "alice" 1000000.0)
(mint "bob" 500000.0)

;; Check balances
(expect "Alice balance" 1000000.0 (get-balance "alice"))
(expect "Bob balance" 500000.0 (get-balance "bob"))

;; Test delegation
(env-keys ["alice-key"])
(delegate "alice" "bob")

(expect "Bob voting power" 1500000.0 (get-votes "bob"))
(expect "Alice voting power" 0.0 (get-votes "alice"))

(commit-tx)

;; Test 2: Proposal creation and voting
(begin-tx)
(use proposals)

;; Create proposal
(env-keys ["bob-key"])
(propose 
  "bob"
  "Increase Treasury Budget"
  "Proposal to increase development budget by 50%"
  ["treasury"]
  [0.0]
  ["set-budget"]
  ["development:coin:1000000.0:90"])

;; Advance blocks for voting
(env-chain-data { "block-height": 10 })

;; Cast votes
(cast-vote "proposal-001" "bob" 1) ;; For

;; Check vote recorded
(expect "Vote recorded" true (has-voted "proposal-001" "bob"))

(commit-tx)

;; Test 3: Treasury operations
(begin-tx)
(use treasury)

;; Deposit to treasury
(env-keys ["alice-key"])
(deposit coin "alice" 100000.0)

;; Create payment request
(create-payment-request
  "developer-1"
  coin
  10000.0
  "Development work payment"
  "development"
  "proposal-001")

;; Execute payment (would need multisig in production)
(env-keys ["admin-key"])
(execute-payment "payment-001")

(commit-tx)

;; Test 4: Timelock execution
(begin-tx)
(use timelock)

;; Grant roles
(env-keys ["admin-key"])
(grant-role PROPOSER_ROLE "bob")
(grant-role EXECUTOR_ROLE "alice")

;; Schedule operation
(env-keys ["bob-key"])
(schedule
  ["treasury"]
  [0.0]
  ["set-budget"]
  ["marketing:coin:500000.0:30"]
  ""
  "salt123"
  172800) ;; 2 days

;; Fast forward time
(env-chain-data { "block-time": (add-time (at 'block-time (chain-data)) (days 3)) })

;; Execute operation
(env-keys ["alice-key"])
(execute
  ["treasury"]
  [0.0]
  ["set-budget"]
  ["marketing:coin:500000.0:30"]
  ""
  "salt123")

(commit-tx)

;; Test 5: Complex delegation scenarios
(begin-tx)
(use delegation)

;; Create delegate profile
(env-keys ["bob-key"])
(create-delegate-profile
  "bob"
  "Bob's Delegation Service"
  "Professional governance participant"
  "https://bob-delegate.com")

;; Partial delegation
(env-keys ["alice-key"])
(delegate-partial
  "alice"
  "bob"
  50.0
  ["defi", "treasury"]
  30)

;; Check delegation
(expect "Delegation created" 1 (length (get-delegations "alice")))

(commit-tx)

;; Security Tests
(begin-tx)

;; Test: Cannot vote twice
(expect-failure "Cannot vote twice"
  (cast-vote "proposal-001" "bob" 0))

;; Test: Cannot create proposal without threshold
(env-keys ["charlie-key"])
(expect-failure "Insufficient voting power"
  (propose "charlie" "Bad Proposal" "Should fail" ["test"] [0.0] ["test"] ["test"]))

;; Test: Cannot execute payment without approval
(expect-failure "Payment not approved"
  (execute-payment "non-existent"))

(commit-tx)

(print "All DAO tests passed!")
```

## Frontend Integration

Here's an example of how to integrate with the DAO frontend:

```javascript
// dao-integration.js
// Frontend integration for DAO governance

const Pact = require('pact-lang-api');

class DAOGovernance {
  constructor(nodeUrl, chainId) {
    this.nodeUrl = nodeUrl;
    this.chainId = chainId;
  }

  // Create a proposal
  async createProposal(
    account,
    title,
    description,
    targets,
    values,
    functions,
    calldatas
  ) {
    const cmd = {
      pactCode: `(proposals.propose 
        "${account}" 
        "${title}" 
        "${description}" 
        ${JSON.stringify(targets)}
        ${JSON.stringify(values)}
        ${JSON.stringify(functions)}
        ${JSON.stringify(calldatas)})`,
      keyPairs: [{
        publicKey: account.publicKey,
        secretKey: account.secretKey,
        clist: [
          { name: "proposals.PROPOSE", args: [account.address] }
        ]
      }],
      meta: {
        chainId: this.chainId,
        sender: account.address,
        gasLimit: 10000,
        gasPrice: 0.00001,
        ttl: 600
      }
    };

    return await Pact.send(cmd, this.nodeUrl);
  }

  // Cast a vote
  async castVote(account, proposalId, support, reason = "") {
    const cmd = {
      pactCode: reason 
        ? `(proposals.cast-vote-with-reason "${proposalId}" "${account.address}" ${support} "${reason}")`
        : `(proposals.cast-vote "${proposalId}" "${account.address}" ${support})`,
      keyPairs: [{
        publicKey: account.publicKey,
        secretKey: account.secretKey,
        clist: []
      }],
      meta: {
        chainId: this.chainId,
        sender: account.address,
        gasLimit: 5000,
        gasPrice: 0.00001,
        ttl: 600
      }
    };

    return await Pact.send(cmd, this.nodeUrl);
  }

  // Delegate voting power
  async delegate(delegator, delegatee) {
    const cmd = {
      pactCode: `(governance-token.delegate "${delegator.address}" "${delegatee}")`,
      keyPairs: [{
        publicKey: delegator.publicKey,
        secretKey: delegator.secretKey,
        clist: []
      }],
      meta: {
        chainId: this.chainId,
        sender: delegator.address,
        gasLimit: 3000,
        gasPrice: 0.00001,
        ttl: 600
      }
    };

    return await Pact.send(cmd, this.nodeUrl);
  }

  // Get proposal details
  async getProposal(proposalId) {
    const cmd = {
      pactCode: `(proposals.get-proposal "${proposalId}")`
    };

    const result = await Pact.local(cmd, this.nodeUrl);
    return result.result.data;
  }

  // Get voting power
  async getVotingPower(account) {
    const cmd = {
      pactCode: `(governance-token.get-votes "${account}")`
    };

    const result = await Pact.local(cmd, this.nodeUrl);
    return result.result.data;
  }

  // Get treasury balance
  async getTreasuryBalance(token) {
    const cmd = {
      pactCode: `(treasury.get-treasury-balance ${token})`
    };

    const result = await Pact.local(cmd, this.nodeUrl);
    return result.result.data;
  }
}

// Example usage
async function main() {
  const dao = new DAOGovernance('http://localhost:9001', '0');
  
  const alice = {
    address: 'alice',
    publicKey: 'alice-public-key',
    secretKey: 'alice-secret-key'
  };

  // Create a proposal
  const proposal = await dao.createProposal(
    alice,
    "Increase Development Budget",
    "Proposal to allocate 100k tokens for Q1 development",
    ["treasury"],
    [0.0],
    ["set-budget"],
    ["development:coin:100000.0:90"]
  );

  console.log('Proposal created:', proposal);

  // Cast a vote
  const vote = await dao.castVote(alice, proposal.id, 1, "Support development!");
  console.log('Vote cast:', vote);

  // Check voting power
  const votingPower = await dao.getVotingPower(alice.address);
  console.log('Voting power:', votingPower);
}

main().catch(console.error);
```

## Summary

In this chapter, we've built a comprehensive DAO governance system with:

1. **Governance Token**: ERC20-style token with voting power and delegation
2. **Proposal System**: Create and manage governance proposals
3. **Voting Mechanisms**: Multiple voting strategies including delegation
4. **Delegation System**: Advanced delegation with partial and topic-based delegation
5. **Timelock Controller**: Secure execution with time delays
6. **Treasury Management**: Multi-asset treasury with budget controls

### Key Takeaways

- DAOs require careful balance between decentralization and efficiency
- On-chain governance needs robust security mechanisms
- Timelock delays protect against malicious proposals
- Delegation enables participation without constant involvement
- Treasury management requires strict controls and transparency

### Testing Notes

The code examples include both comprehensive and simplified test files:
- `test-dao.repl` - Full feature testing
- `test-dao-simple.repl` - Basic functionality testing

Key testing patterns used:
- Transaction blocks with `begin-tx`/`commit-tx`
- Capability installation with `install-capability`
- Assertions with `expect` and `expect-failure`
- Environment setup with `env-data` and `env-keys`

### Next Steps

- Implement additional voting mechanisms (quadratic, conviction)
- Add cross-chain governance capabilities
- Create emergency pause mechanisms
- Build reputation systems for delegates
- Integrate with DeFi protocols for treasury yield

The DAO governance system we've built provides a solid foundation for decentralized decision-making while maintaining security and flexibility.