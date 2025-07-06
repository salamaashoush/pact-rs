;; governance-patterns.pact
;; Advanced namespace governance patterns and examples

;; =============================================================================
;; HIERARCHICAL GOVERNANCE PATTERN
;; =============================================================================

(namespace 'governance-system)

(module hierarchical-governance GOVERNANCE
  @doc "Hierarchical namespace governance with role-based access control"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'governance-admin))
  
  ;; Role definitions
  (defschema role
    @doc "Role definition with permissions"
    name:string
    level:integer
    permissions:[string]
    description:string)
  
  (deftable roles:{role})
  
  ;; User role assignments
  (defschema user-role
    @doc "User role assignment"
    user:string
    namespace:string
    role:string
    assigned-by:string
    assigned-at:time
    expires:time)
  
  (deftable user-roles:{user-role})
  
  ;; Namespace hierarchy
  (defschema namespace-hierarchy
    @doc "Namespace parent-child relationships"
    namespace:string
    parent:string
    level:integer
    created-at:time)
  
  (deftable namespace-tree:{namespace-hierarchy})
  
  ;; Role management capabilities
  (defcap ROOT_ADMIN:bool ()
    @doc "Root administrator capability"
    (enforce-keyset 'root-admin))
  
  (defcap NAMESPACE_ADMIN:bool (namespace:string)
    @doc "Namespace administrator capability"
    (let ((user-role (get-user-role (tx-sender) namespace)))
      (enforce (>= (at 'level user-role) 5) "Insufficient admin privileges")))
  
  (defcap ROLE_MANAGER:bool (namespace:string)
    @doc "Role management capability"
    (let ((user-role (get-user-role (tx-sender) namespace)))
      (enforce (>= (at 'level user-role) 3) "Insufficient role management privileges")))
  
  ;; Initialize default roles
  (defun setup-default-roles:string ()
    @doc "Setup default role hierarchy"
    (with-capability (ROOT_ADMIN)
      ;; Level 1: Basic user
      (insert roles "USER" {
        "name": "USER",
        "level": 1,
        "permissions": ["READ", "CALL"],
        "description": "Basic user with read and call permissions"
      })
      
      ;; Level 3: Module developer
      (insert roles "DEVELOPER" {
        "name": "DEVELOPER", 
        "level": 3,
        "permissions": ["READ", "CALL", "DEPLOY", "UPGRADE"],
        "description": "Module developer with deployment capabilities"
      })
      
      ;; Level 5: Namespace administrator
      (insert roles "ADMIN" {
        "name": "ADMIN",
        "level": 5,
        "permissions": ["READ", "CALL", "DEPLOY", "UPGRADE", "MANAGE_ROLES", "CREATE_CHILD_NS"],
        "description": "Namespace administrator with full permissions"
      })
      
      ;; Level 10: Root administrator
      (insert roles "ROOT" {
        "name": "ROOT",
        "level": 10,
        "permissions": ["*"],
        "description": "Root administrator with all permissions"
      })
      
      "Default roles created"))
  
  ;; Role assignment functions
  (defun assign-role:string (user:string namespace:string role:string expires-days:integer)
    @doc "Assign role to user for namespace"
    (with-capability (ROLE_MANAGER namespace)
      (let ((role-info (read roles role))
            (assigner (tx-sender))
            (assigner-role (get-user-role assigner namespace))
            (expires-time (add-time (at 'block-time (chain-data)) (days expires-days))))
        
        ;; Ensure assigner has higher level than assigned role
        (enforce (> (at 'level assigner-role) (at 'level role-info))
                 "Cannot assign role with equal or higher level")
        
        (write user-roles (format "{}:{}" [user namespace]) {
          "user": user,
          "namespace": namespace,
          "role": role,
          "assigned-by": assigner,
          "assigned-at": (at 'block-time (chain-data)),
          "expires": expires-time
        })
        
        (format "Role {} assigned to {} in namespace {}" [role user namespace]))))
  
  (defun revoke-role:string (user:string namespace:string)
    @doc "Revoke user role in namespace"
    (with-capability (ROLE_MANAGER namespace)
      (let ((existing-role (read user-roles (format "{}:{}" [user namespace]))))
        (update user-roles (format "{}:{}" [user namespace]) {
          "expires": (at 'block-time (chain-data))  ;; Immediately expire
        })
        (format "Role revoked for {} in namespace {}" [user namespace]))))
  
  ;; Namespace hierarchy management
  (defun create-child-namespace:string (parent:string child:string)
    @doc "Create child namespace under parent"
    (with-capability (NAMESPACE_ADMIN parent)
      (let ((parent-level (at 'level (read namespace-tree parent))))
        ;; Create child namespace with inherited governance
        (define-namespace child
          (create-capability-guard (NAMESPACE_USER child))
          (create-capability-guard (NAMESPACE_ADMIN child)))
        
        ;; Record hierarchy
        (insert namespace-tree child {
          "namespace": child,
          "parent": parent,
          "level": (+ parent-level 1),
          "created-at": (at 'block-time (chain-data))
        })
        
        ;; Auto-assign parent admins as child admins
        (auto-inherit-admin-roles parent child)
        
        (format "Child namespace {} created under {}" [child parent]))))
  
  (defun auto-inherit-admin-roles:string (parent:string child:string)
    @doc "Automatically inherit admin roles from parent namespace"
    (let ((parent-admins (select user-roles 
                                 (and? (where 'namespace (= parent))
                                       (where 'role (= "ADMIN"))))))
      (map (lambda (admin-entry)
             (assign-role (at 'user admin-entry) child "ADMIN" 365))
           parent-admins)
      "Admin roles inherited"))
  
  ;; Access control functions
  (defcap NAMESPACE_USER:bool (namespace:string)
    @doc "Check if user can use namespace"
    (let ((user-role (get-user-role (tx-sender) namespace)))
      (enforce (>= (at 'level user-role) 1) "No access to namespace")))
  
  (defun get-user-role:object (user:string namespace:string)
    @doc "Get user's effective role in namespace"
    (with-default-read user-roles (format "{}:{}" [user namespace])
      { "role": "NONE", "expires": (time "1970-01-01T00:00:00Z") }
      { "role" := role-name, "expires" := expires }
      (if (and (!= role-name "NONE") (> expires (at 'block-time (chain-data))))
          (read roles role-name)
          { "name": "NONE", "level": 0, "permissions": [], "description": "No role" })))
  
  (defun check-permission:bool (user:string namespace:string permission:string)
    @doc "Check if user has specific permission in namespace"
    (let ((user-role (get-user-role user namespace))
          (permissions (at 'permissions user-role)))
      (or (contains "*" permissions)
          (contains permission permissions))))
  
  ;; Query functions
  (defun list-namespace-users:[object] (namespace:string)
    @doc "List all users with roles in namespace"
    (select user-roles (where 'namespace (= namespace))))
  
  (defun list-user-namespaces:[object] (user:string)
    @doc "List all namespaces where user has roles"
    (select user-roles (where 'user (= user))))
  
  (defun get-namespace-children:[string] (parent:string)
    @doc "Get all child namespaces"
    (map (at 'namespace) (select namespace-tree (where 'parent (= parent)))))
)

;; =============================================================================
;; DEMOCRATIC GOVERNANCE PATTERN
;; =============================================================================

(module democratic-governance GOVERNANCE
  @doc "Democratic governance for namespace management"
  
  (defcap GOVERNANCE ()
    (enforce-keyset 'governance-admin))
  
  ;; Proposal schemas
  (defschema proposal
    @doc "Governance proposal"
    id:string
    title:string
    description:string
    proposer:string
    proposal-type:string
    target-namespace:string
    parameters:object
    created-at:time
    voting-period:integer
    execution-delay:integer
    status:string
    votes-for:decimal
    votes-against:decimal
    votes-abstain:decimal
    executed:bool)
  
  (deftable proposals:{proposal})
  
  ;; Voting power schema
  (defschema voting-power
    @doc "User voting power in namespace"
    user:string
    namespace:string
    power:decimal
    source:string
    last-update:time)
  
  (deftable voting-powers:{voting-power})
  
  ;; Vote record schema
  (defschema vote-record
    @doc "Individual vote record"
    proposal-id:string
    voter:string
    choice:string
    power:decimal
    timestamp:time)
  
  (deftable vote-records:{vote-record})
  
  ;; Governance parameters
  (defconst VOTING_PERIOD_DAYS:integer 7)
  (defconst EXECUTION_DELAY_DAYS:integer 2)
  (defconst QUORUM_THRESHOLD:decimal 0.10)  ;; 10% of total voting power
  (defconst APPROVAL_THRESHOLD:decimal 0.60)  ;; 60% approval required
  
  ;; Proposal types
  (defconst PROPOSAL_TYPE_PARAMETER:string "PARAMETER_CHANGE")
  (defconst PROPOSAL_TYPE_UPGRADE:string "MODULE_UPGRADE")
  (defconst PROPOSAL_TYPE_ADMIN:string "ADMIN_CHANGE")
  (defconst PROPOSAL_TYPE_EMERGENCY:string "EMERGENCY_ACTION")
  
  ;; Capabilities
  (defcap PROPOSE:bool (namespace:string)
    @doc "Capability to create proposals"
    (let ((voting-power (get-voting-power (tx-sender) namespace)))
      (enforce (>= voting-power 100.0) "Insufficient voting power to propose")))
  
  (defcap VOTE:bool (proposal-id:string)
    @doc "Capability to vote on proposal"
    (with-read proposals proposal-id { "status" := status }
      (enforce (= status "ACTIVE") "Proposal not active for voting")))
  
  (defcap EXECUTE:bool (proposal-id:string)
    @doc "Capability to execute proposal"
    (with-read proposals proposal-id { "status" := status }
      (enforce (= status "APPROVED") "Proposal not approved for execution")))
  
  ;; Voting power management
  (defun set-voting-power:string (user:string namespace:string power:decimal source:string)
    @doc "Set voting power for user (admin only)"
    (with-capability (GOVERNANCE)
      (write voting-powers (format "{}:{}" [user namespace]) {
        "user": user,
        "namespace": namespace,
        "power": power,
        "source": source,
        "last-update": (at 'block-time (chain-data))
      })
      (format "Voting power {} set for {} in {}" [power user namespace])))
  
  (defun get-voting-power:decimal (user:string namespace:string)
    @doc "Get user's voting power in namespace"
    (with-default-read voting-powers (format "{}:{}" [user namespace])
      { "power": 0.0 }
      { "power" := power }
      power))
  
  ;; Proposal creation
  (defun create-proposal:string (proposal-id:string title:string description:string
                                proposal-type:string target-namespace:string parameters:object)
    @doc "Create governance proposal"
    (with-capability (PROPOSE target-namespace)
      (insert proposals proposal-id {
        "id": proposal-id,
        "title": title,
        "description": description,
        "proposer": (tx-sender),
        "proposal-type": proposal-type,
        "target-namespace": target-namespace,
        "parameters": parameters,
        "created-at": (at 'block-time (chain-data)),
        "voting-period": VOTING_PERIOD_DAYS,
        "execution-delay": EXECUTION_DELAY_DAYS,
        "status": "ACTIVE",
        "votes-for": 0.0,
        "votes-against": 0.0,
        "votes-abstain": 0.0,
        "executed": false
      })
      (format "Proposal {} created for namespace {}" [proposal-id target-namespace])))
  
  ;; Voting functions
  (defun vote:string (proposal-id:string choice:string)
    @doc "Vote on proposal (FOR, AGAINST, ABSTAIN)"
    (with-capability (VOTE proposal-id)
      (with-read proposals proposal-id { 
        "target-namespace" := namespace,
        "votes-for" := current-for,
        "votes-against" := current-against,
        "votes-abstain" := current-abstain
      }
        (let ((voter (tx-sender))
              (voting-power (get-voting-power voter namespace)))
          
          (enforce (> voting-power 0.0) "No voting power")
          (enforce (contains choice ["FOR" "AGAINST" "ABSTAIN"]) "Invalid vote choice")
          
          ;; Check if already voted
          (with-default-read vote-records (format "{}:{}" [proposal-id voter])
            { "choice": "NONE" }
            { "choice" := existing-choice }
            (enforce (= existing-choice "NONE") "Already voted"))
          
          ;; Record vote
          (insert vote-records (format "{}:{}" [proposal-id voter]) {
            "proposal-id": proposal-id,
            "voter": voter,
            "choice": choice,
            "power": voting-power,
            "timestamp": (at 'block-time (chain-data))
          })
          
          ;; Update proposal vote counts
          (cond
            ((= choice "FOR")
             (update proposals proposal-id { "votes-for": (+ current-for voting-power) }))
            ((= choice "AGAINST")
             (update proposals proposal-id { "votes-against": (+ current-against voting-power) }))
            ((= choice "ABSTAIN")
             (update proposals proposal-id { "votes-abstain": (+ current-abstain voting-power) })))
          
          (format "Vote {} recorded for proposal {}" [choice proposal-id]))))
  
  ;; Proposal finalization
  (defun finalize-proposal:string (proposal-id:string)
    @doc "Finalize proposal after voting period"
    (with-read proposals proposal-id {
      "created-at" := created,
      "voting-period" := period,
      "votes-for" := for-votes,
      "votes-against" := against-votes,
      "votes-abstain" := abstain-votes,
      "target-namespace" := namespace,
      "status" := status
    }
      (enforce (= status "ACTIVE") "Proposal not active")
      (enforce (>= (diff-time (at 'block-time (chain-data)) created) (days period)) 
               "Voting period not ended")
      
      (let ((total-votes (+ (+ for-votes against-votes) abstain-votes))
            (total-power (get-total-voting-power namespace))
            (quorum (>= total-votes (* total-power QUORUM_THRESHOLD)))
            (approved (and quorum (>= (/ for-votes (+ for-votes against-votes)) APPROVAL_THRESHOLD))))
        
        (update proposals proposal-id {
          "status": (if approved "APPROVED" "REJECTED")
        })
        
        (if approved
            (format "Proposal {} approved (quorum: {}, approval: {})" 
                    [proposal-id quorum (/ for-votes (+ for-votes against-votes))])
            (format "Proposal {} rejected (quorum: {}, approval: {})" 
                    [proposal-id quorum (/ for-votes (+ for-votes against-votes))])))))
  
  ;; Proposal execution
  (defun execute-proposal:string (proposal-id:string)
    @doc "Execute approved proposal after delay"
    (with-capability (EXECUTE proposal-id)
      (with-read proposals proposal-id {
        "created-at" := created,
        "voting-period" := voting-period,
        "execution-delay" := delay,
        "proposal-type" := prop-type,
        "parameters" := params,
        "executed" := is-executed
      }
        (enforce (not is-executed) "Proposal already executed")
        (let ((execution-time (add-time created (days (+ voting-period delay)))))
          (enforce (>= (at 'block-time (chain-data)) execution-time) "Execution delay not passed")
          
          ;; Execute based on proposal type
          (execute-proposal-action prop-type params)
          
          (update proposals proposal-id { "executed": true })
          (format "Proposal {} executed successfully" [proposal-id]))))
  
  (defun execute-proposal-action:string (proposal-type:string parameters:object)
    @doc "Execute specific proposal action"
    (cond
      ((= proposal-type PROPOSAL_TYPE_PARAMETER)
       (execute-parameter-change parameters))
      ((= proposal-type PROPOSAL_TYPE_UPGRADE) 
       (execute-module-upgrade parameters))
      ((= proposal-type PROPOSAL_TYPE_ADMIN)
       (execute-admin-change parameters))
      ((= proposal-type PROPOSAL_TYPE_EMERGENCY)
       (execute-emergency-action parameters))
      "Unknown proposal type"))
  
  ;; Helper functions
  (defun get-total-voting-power:decimal (namespace:string)
    @doc "Get total voting power in namespace"
    (fold (+) 0.0 
          (map (at 'power) 
               (select voting-powers (where 'namespace (= namespace))))))
  
  (defun execute-parameter-change:string (params:object)
    @doc "Execute parameter change proposal"
    "Parameter change executed")
  
  (defun execute-module-upgrade:string (params:object)
    @doc "Execute module upgrade proposal"
    "Module upgrade executed")
  
  (defun execute-admin-change:string (params:object)
    @doc "Execute admin change proposal"
    "Admin change executed")
  
  (defun execute-emergency-action:string (params:object)
    @doc "Execute emergency action proposal"
    "Emergency action executed")
  
  ;; Query functions
  (defun get-proposal:object (proposal-id:string)
    @doc "Get proposal details"
    (read proposals proposal-id))
  
  (defun list-active-proposals:[object] ()
    @doc "List all active proposals"
    (select proposals (where 'status (= "ACTIVE"))))
  
  (defun get-user-votes:[object] (user:string)
    @doc "Get all votes by user"
    (select vote-records (where 'voter (= user))))
)

;; Initialize tables
(create-table roles)
(create-table user-roles)
(create-table namespace-tree)
(create-table proposals)
(create-table voting-powers)
(create-table vote-records)