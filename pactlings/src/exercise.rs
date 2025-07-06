use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

use crate::config::{ExerciseConfig, Difficulty};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Exercise {
    pub name: String,
    pub path: PathBuf,
    pub file: String,
    pub topic: String,
    pub difficulty: Difficulty,
    pub description: String,
    pub hint: String,
    pub solution_path: Option<PathBuf>,
    pub info_path: Option<PathBuf>,
    pub prerequisites: Vec<String>,
    pub test_commands: Vec<String>,
}

impl Exercise {
    pub fn from_config(config: ExerciseConfig, exercises_dir: &PathBuf) -> Result<Self> {
        let path = exercises_dir.join(&config.file);
        let solution_path = config.solution_file
            .map(|f| exercises_dir.parent().unwrap().join("solutions").join(f));
        let info_path = config.info_file
            .map(|f| exercises_dir.parent().unwrap().join("info").join(f));

        Ok(Self {
            name: config.name,
            path,
            file: config.file,
            topic: config.topic,
            difficulty: config.difficulty,
            description: config.description,
            hint: config.hint,
            solution_path,
            info_path,
            prerequisites: config.prerequisites,
            test_commands: config.test_commands,
        })
    }

    pub fn exists(&self) -> bool {
        self.path.exists()
    }

    pub fn read_content(&self) -> Result<String> {
        Ok(std::fs::read_to_string(&self.path)?)
    }

    pub fn write_content(&self, content: &str) -> Result<()> {
        if let Some(parent) = self.path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&self.path, content)?;
        Ok(())
    }

    pub fn read_solution(&self) -> Result<Option<String>> {
        if let Some(ref solution_path) = self.solution_path {
            if solution_path.exists() {
                return Ok(Some(std::fs::read_to_string(solution_path)?));
            }
        }
        Ok(None)
    }

    pub fn read_info(&self) -> Result<Option<String>> {
        if let Some(ref info_path) = self.info_path {
            if info_path.exists() {
                return Ok(Some(std::fs::read_to_string(info_path)?));
            }
        }
        Ok(None)
    }

    pub fn is_complete(&self) -> bool {
        // Check if exercise file exists and has some basic content
        if !self.exists() {
            return false;
        }

        // Basic heuristic: file should have some content and not contain common "TODO" markers
        if let Ok(content) = self.read_content() {
            !content.trim().is_empty() && 
            !content.contains("// TODO") && 
            !content.contains(";; TODO") &&
            !content.contains("YOUR CODE HERE") &&
            !content.contains("FILL IN THE BLANK")
        } else {
            false
        }
    }

    pub fn difficulty_emoji(&self) -> &'static str {
        match self.difficulty {
            Difficulty::Beginner => "🟢",
            Difficulty::Intermediate => "🟡", 
            Difficulty::Advanced => "🔴",
        }
    }

    pub fn create_template(&self) -> Result<()> {
        if self.exists() {
            return Ok(()); // Don't overwrite existing files
        }

        let template = self.generate_template();
        self.write_content(&template)?;
        Ok(())
    }

    fn generate_template(&self) -> String {
        match self.topic.as_str() {
            "Basics" => self.generate_basics_template(),
            "Modules" => self.generate_modules_template(),
            "Capabilities" => self.generate_capabilities_template(),
            "Database" => self.generate_database_template(),
            "Interfaces" => self.generate_interfaces_template(),
            "Pacts" => self.generate_pacts_template(),
            "Applications" => self.generate_applications_template(),
            "Guards" => self.generate_guards_template(),
            "Namespaces" => self.generate_namespaces_template(),
            "Governance" => self.generate_governance_template(),
            "Defpacts" => self.generate_defpacts_template(),
            "Testing" => self.generate_testing_template(),
            "Marmalade" => self.generate_marmalade_template(),
            "Time" => self.generate_time_template(),
            "Cryptography" => self.generate_crypto_template(),
            "Cross-chain" => self.generate_crosschain_template(),
            "Builtins" => self.generate_builtins_template(),
            "Real-world" => self.generate_realworld_template(),
            _ => self.generate_generic_template(),
        }
    }

    fn generate_basics_template(&self) -> String {
        match self.name.as_str() {
            "basics_01_expressions" => {
                r#";; Exercise: Basic Expressions
;; Learn Pact's S-expression syntax

;; TODO: Complete the following expressions
;; Add two numbers
(+ YOUR_CODE_HERE YOUR_CODE_HERE)

;; Subtract 5 from 10
(- YOUR_CODE_HERE YOUR_CODE_HERE)

;; Multiply 3 by 4
(* YOUR_CODE_HERE YOUR_CODE_HERE)

;; Divide 20 by 4
(/ YOUR_CODE_HERE YOUR_CODE_HERE)

;; Concatenate two strings
(+ "Hello, " YOUR_CODE_HERE)

;; Boolean operations
(and true YOUR_CODE_HERE)
(or false YOUR_CODE_HERE)
(not YOUR_CODE_HERE)
"#.to_string()
            },
            "basics_02_types" => {
                r#";; Exercise: Types in Pact
;; Learn about Pact's type system

;; TODO: Define variables with correct types
;; String type
(let ((my-string YOUR_CODE_HERE))
  my-string)

;; Decimal type
(let ((my-decimal YOUR_CODE_HERE))
  my-decimal)

;; Integer type
(let ((my-integer YOUR_CODE_HERE))
  my-integer)

;; Boolean type
(let ((my-bool YOUR_CODE_HERE))
  my-bool)

;; Time type (use parse-time)
(let ((my-time (parse-time "%Y-%m-%d" YOUR_CODE_HERE)))
  my-time)
"#.to_string()
            },
            "basics_03_variables" => {
                r#";; Exercise: Variable Binding
;; Learn let expressions and variable scoping

;; TODO: Complete the let expression
(let ((x YOUR_CODE_HERE)
      (y YOUR_CODE_HERE))
  ;; Add x and y
  (+ x y))

;; TODO: Nested let expressions
(let ((outer-var 10))
  (let ((inner-var YOUR_CODE_HERE))
    ;; Multiply outer-var by inner-var
    (* outer-var inner-var)))

;; TODO: Variable shadowing
(let ((name "World"))
  (let ((name YOUR_CODE_HERE))
    (+ "Hello, " name)))
"#.to_string()
            },
            "basics_04_functions" => {
                r#";; Exercise: Function Definition
;; Learn to define and call functions

;; TODO: Define a function that adds two decimals
(defun add-decimals (a:decimal b:decimal)
  @doc "Add two decimal numbers"
  YOUR_CODE_HERE)

;; TODO: Define a function that greets a person
(defun greet (name:string)
  @doc "Greet a person by name"
  YOUR_CODE_HERE)

;; TODO: Define a function with default behavior
(defun divide-safe (a:decimal b:decimal)
  @doc "Safely divide two numbers"
  (if (= b 0.0)
    YOUR_CODE_HERE  ;; What to return if b is 0?
    YOUR_CODE_HERE)) ;; Normal division

;; Test your functions
(add-decimals 3.5 2.1)
(greet "Alice")
(divide-safe 10.0 2.0)
(divide-safe 10.0 0.0)
"#.to_string()
            },
            _ => self.generate_generic_template(),
        }
    }

    fn generate_modules_template(&self) -> String {
        match self.name.as_str() {
            "modules_01_basic" => {
                r#";; Exercise: Basic Module Structure
;; Learn to create a Pact module

;; TODO: Define a keyset for module governance
(define-keyset 'YOUR_KEYSET_NAME
  (read-keyset "YOUR_KEYSET_NAME"))

;; TODO: Create a basic module
(module YOUR_MODULE_NAME 'YOUR_KEYSET_NAME
  @doc "YOUR_MODULE_DESCRIPTION"
  
  ;; TODO: Add a simple function to the module
  (defun hello-world ()
    @doc "Return a greeting"
    YOUR_CODE_HERE)
    
  ;; TODO: Add a function that takes parameters
  (defun calculate-area (length:decimal width:decimal)
    @doc "Calculate rectangle area"
    YOUR_CODE_HERE))
"#.to_string()
            },
            "modules_02_keysets" => {
                r#";; Exercise: Keyset Governance
;; Learn about module security with keysets

;; TODO: Define multiple keysets
(define-keyset 'admin-keyset
  (read-keyset "admin-keyset"))

(define-keyset 'user-keyset
  (read-keyset "user-keyset"))

;; TODO: Create a module with keyset governance
(module secure-module 'admin-keyset
  @doc "Module demonstrating keyset security"
  
  ;; TODO: Create a capability that requires admin keyset
  (defcap ADMIN ()
    @doc "Admin capability"
    YOUR_CODE_HERE)
  
  ;; TODO: Create a function that requires admin capability
  (defun admin-only-function ()
    @doc "Function only admins can call"
    (with-capability (ADMIN)
      YOUR_CODE_HERE))
  
  ;; TODO: Create a function that checks user keyset
  (defun user-function ()
    @doc "Function that checks user keyset"
    YOUR_CODE_HERE))
"#.to_string()
            },
            _ => self.generate_generic_template(),
        }
    }

    fn generate_capabilities_template(&self) -> String {
        match self.name.as_str() {
            "capabilities_01_basic" => {
                r#";; Exercise: Basic Capabilities
;; Learn the fundamentals of Pact's capability system

(module capability-exercise 'admin-keyset
  @doc "Basic capability patterns"
  
  ;; TODO: Define a simple capability
  (defcap YOUR_CAPABILITY_NAME ()
    @doc "Simple capability description"
    YOUR_CODE_HERE)  ;; What should this capability check?
  
  ;; TODO: Define a parameterized capability
  (defcap TRANSFER (from:string to:string amount:decimal)
    @doc "Transfer capability"
    YOUR_CODE_HERE)  ;; Add validation logic
  
  ;; TODO: Use capability in function
  (defun protected-function ()
    @doc "Function protected by capability"
    (with-capability (YOUR_CAPABILITY_NAME)
      YOUR_CODE_HERE))
  
  ;; TODO: Function using parameterized capability
  (defun transfer-money (from:string to:string amount:decimal)
    @doc "Transfer money with capability"
    (with-capability (TRANSFER from to amount)
      YOUR_CODE_HERE)))
"#.to_string()
            },
            "capabilities_02_managed" => {
                r#";; Exercise: Managed Capabilities
;; Learn about managed capabilities for resource control

(module managed-capability-exercise 'admin-keyset
  @doc "Managed capability patterns"
  
  ;; TODO: Define a managed capability
  (defcap SPEND (account:string amount:decimal)
    @doc "Managed spending capability"
    @managed amount YOUR_MANAGER_FUNCTION
    YOUR_CODE_HERE)  ;; Add account validation
  
  ;; TODO: Define the manager function
  (defun YOUR_MANAGER_FUNCTION:decimal (managed:decimal requested:decimal)
    @doc "Manages spending allowance"
    YOUR_CODE_HERE)  ;; Implement spending logic
  
  ;; TODO: Function using managed capability
  (defun spend-money (account:string amount:decimal)
    @doc "Spend money with managed capability"
    (with-capability (SPEND account amount)
      YOUR_CODE_HERE)))
"#.to_string()
            },
            _ => self.generate_generic_template(),
        }
    }

    fn generate_database_template(&self) -> String {
        match self.name.as_str() {
            "database_01_tables" => {
                r#";; Exercise: Database Tables
;; Learn to create database tables with schemas

(module database-exercise 'admin-keyset
  @doc "Database table examples"
  
  ;; TODO: Define a schema for user data
  (defschema user
    YOUR_SCHEMA_FIELDS)  ;; What fields should a user have?
  
  ;; TODO: Create a table using the schema
  (deftable YOUR_TABLE_NAME:{user})
  
  ;; TODO: Define another schema for accounts
  (defschema account
    YOUR_ACCOUNT_FIELDS)  ;; What fields should an account have?
  
  ;; TODO: Create the accounts table
  (deftable YOUR_ACCOUNTS_TABLE:{account}))

;; TODO: Create the tables
YOUR_CODE_HERE
"#.to_string()
            },
            "database_02_operations" => {
                r#";; Exercise: Database Operations
;; Learn CRUD operations on database tables

(module db-operations-exercise 'admin-keyset
  @doc "Database operation examples"
  
  (defschema person
    name:string
    age:integer
    email:string)
  
  (deftable people:{person})
  
  ;; TODO: Function to create a new person
  (defun create-person (id:string name:string age:integer email:string)
    @doc "Create a new person record"
    YOUR_CODE_HERE)  ;; Use insert
  
  ;; TODO: Function to read a person
  (defun get-person (id:string)
    @doc "Get a person by ID"
    YOUR_CODE_HERE)  ;; Use read
  
  ;; TODO: Function to update a person
  (defun update-person-age (id:string new-age:integer)
    @doc "Update a person's age"
    YOUR_CODE_HERE)  ;; Use update
  
  ;; TODO: Function to read with default values
  (defun get-person-safe (id:string)
    @doc "Get person with default if not found"
    YOUR_CODE_HERE))  ;; Use with-default-read

;; Create the table
(create-table people)
"#.to_string()
            },
            _ => self.generate_generic_template(),
        }
    }

    fn generate_interfaces_template(&self) -> String {
        r#";; Exercise: Interfaces
;; Learn to define and implement interfaces

;; TODO: Define an interface
(interface YOUR_INTERFACE_NAME
  @doc "Interface description"
  
  ;; TODO: Define interface functions
  (defun interface-function (param:string)
    @doc "Interface function"))

;; TODO: Implement the interface
(module implementation-module 'admin-keyset
  @doc "Implementation of the interface"
  
  ;; TODO: Declare interface implementation
  (implements YOUR_INTERFACE_NAME)
  
  ;; TODO: Implement the interface function
  (defun interface-function (param:string)
    @doc "Implementation of interface function"
    YOUR_CODE_HERE))
"#.to_string()
    }

    fn generate_pacts_template(&self) -> String {
        r#";; Exercise: Pacts (Multi-step Transactions)
;; Learn to create multi-step transactions

(module pact-exercise 'admin-keyset
  @doc "Multi-step transaction examples"
  
  ;; TODO: Define a simple pact
  (defpact simple-pact (param:string)
    @doc "Simple two-step pact"
    
    ;; TODO: Step 1
    (step
      YOUR_CODE_HERE)  ;; What happens in step 1?
    
    ;; TODO: Step 2
    (step
      YOUR_CODE_HERE))  ;; What happens in step 2?
  
  ;; TODO: Define a more complex pact with yield/resume
  (defpact escrow-pact (buyer:string seller:string amount:decimal)
    @doc "Escrow pact with yield/resume"
    
    (step
      ;; Buyer side
      YOUR_CODE_HERE)
    
    (step
      ;; Seller side
      YOUR_CODE_HERE)))
"#.to_string()
    }

    fn generate_applications_template(&self) -> String {
        match self.name.as_str() {
            "apps_01_token" => {
                r#";; Exercise: Token Implementation
;; Build a complete fungible token

;; TODO: Define the fungible interface (or use existing one)
(interface fungible-token
  (defun total-supply:decimal ())
  (defun balance-of:decimal (account:string))
  (defun transfer:string (from:string to:string amount:decimal)))

;; TODO: Implement the token module
(module my-token 'token-admin
  @doc "Complete fungible token implementation"
  
  ;; TODO: Implement the fungible interface
  (implements fungible-token)
  
  ;; TODO: Define token schema and table
  YOUR_CODE_HERE
  
  ;; TODO: Define capabilities
  YOUR_CODE_HERE
  
  ;; TODO: Implement interface functions
  YOUR_CODE_HERE)
"#.to_string()
            },
            "apps_02_marketplace" => {
                r#";; Exercise: Marketplace with Escrow
;; Build a decentralized marketplace

(module marketplace 'marketplace-admin
  @doc "Decentralized marketplace with escrow"
  
  ;; TODO: Define listing schema
  YOUR_CODE_HERE
  
  ;; TODO: Define escrow schema
  YOUR_CODE_HERE
  
  ;; TODO: Define capabilities
  YOUR_CODE_HERE
  
  ;; TODO: Implement marketplace functions
  YOUR_CODE_HERE)
"#.to_string()
            },
            _ => self.generate_generic_template(),
        }
    }

    fn generate_guards_template(&self) -> String {
        r#";; Exercise: Guards
;; Learn Pact's unified guard system

(module guards-exercise 'admin-keyset
  @doc "Guards unify all authorization in Pact"
  
  ;; TODO: Create different types of guards
  (defun create-keyset-guard ()
    YOUR_CODE_HERE)
  
  (defun create-user-guard ()
    YOUR_CODE_HERE)
  
  (defun create-module-guard ()
    YOUR_CODE_HERE)
  
  (defun create-capability-guard ()
    YOUR_CODE_HERE))
"#.to_string()
    }

    fn generate_namespaces_template(&self) -> String {
        r#";; Exercise: Namespaces
;; Learn namespace organization

;; TODO: Define namespace
(define-namespace 'YOUR_NAMESPACE YOUR_ADMIN_GUARD YOUR_USER_GUARD)

;; TODO: Set namespace
(namespace 'YOUR_NAMESPACE)

;; TODO: Create module in namespace
(module YOUR_MODULE 'YOUR_GOVERNANCE
  YOUR_CODE_HERE)
"#.to_string()
    }

    fn generate_governance_template(&self) -> String {
        r#";; Exercise: Governance
;; Implement voting and module governance

(module governance-exercise 'admin-keyset
  @doc "Governance patterns"
  
  ;; TODO: Implement voting mechanism
  (defun vote (proposal:string voter:string)
    YOUR_CODE_HERE)
  
  ;; TODO: Implement governance capability
  (defcap GOVERNANCE ()
    YOUR_CODE_HERE))
"#.to_string()
    }

    fn generate_defpacts_template(&self) -> String {
        r#";; Exercise: Defpacts
;; Multi-step transactions

(module defpact-exercise 'admin-keyset
  @doc "Multi-step transaction patterns"
  
  ;; TODO: Create a defpact
  (defpact multi-step-process ()
    (step YOUR_CODE_HERE)
    (step YOUR_CODE_HERE)))
"#.to_string()
    }

    fn generate_testing_template(&self) -> String {
        r#";; Exercise: Testing
;; Learn REPL testing

;; TODO: Set up test environment
(begin-tx)
(env-data { "test-keyset": ["test"] })
(define-keyset 'test-keyset)

;; TODO: Test with expect
(expect "test description" expected-value YOUR_CODE_HERE)

;; TODO: Test failures with expect-failure
(expect-failure "error test" "error message" YOUR_CODE_HERE)

(commit-tx)
"#.to_string()
    }

    fn generate_marmalade_template(&self) -> String {
        r#";; Exercise: Marmalade NFT
;; NFT patterns with Marmalade

(module nft-exercise 'admin-keyset
  @doc "NFT creation and management"
  
  ;; TODO: Create token with t: protocol
  (defun create-nft-token (token-data:object guard:guard)
    YOUR_CODE_HERE)
  
  ;; TODO: Implement policy interface
  YOUR_CODE_HERE)
"#.to_string()
    }

    fn generate_time_template(&self) -> String {
        r#";; Exercise: Time Operations
;; Work with time in Pact

(module time-exercise 'admin-keyset
  @doc "Time operations and scheduling"
  
  ;; TODO: Parse time from string
  (defun parse-date (date-string:string)
    YOUR_CODE_HERE)
  
  ;; TODO: Add duration to time
  (defun add-duration (base-time:time duration:integer)
    YOUR_CODE_HERE))
"#.to_string()
    }

    fn generate_crypto_template(&self) -> String {
        r#";; Exercise: Cryptography
;; Cryptographic operations

(module crypto-exercise 'admin-keyset
  @doc "Cryptographic functions"
  
  ;; TODO: Hash data
  (defun hash-data (data:string)
    YOUR_CODE_HERE)
  
  ;; TODO: Create commitment
  (defun create-commitment (value:string salt:string)
    YOUR_CODE_HERE))
"#.to_string()
    }

    fn generate_crosschain_template(&self) -> String {
        r#";; Exercise: Cross-chain
;; Cross-chain communication

(module crosschain-exercise 'admin-keyset
  @doc "Cross-chain patterns"
  
  ;; TODO: Create cross-chain defpact
  (defpact cross-chain-transfer ()
    (step 
      ;; TODO: Yield to target chain
      YOUR_CODE_HERE)
    (step
      ;; TODO: Resume on target chain
      YOUR_CODE_HERE)))
"#.to_string()
    }

    fn generate_builtins_template(&self) -> String {
        r#";; Exercise: Built-in Functions
;; Master Pact's built-in functions

(module builtins-exercise 'admin-keyset
  @doc "Built-in function examples"
  
  ;; TODO: Use map function
  (defun process-list (items:[string])
    YOUR_CODE_HERE)
  
  ;; TODO: Use filter function
  (defun filter-items (items:[integer] threshold:integer)
    YOUR_CODE_HERE)
  
  ;; TODO: Use fold function
  (defun sum-list (numbers:[decimal])
    YOUR_CODE_HERE))
"#.to_string()
    }

    fn generate_realworld_template(&self) -> String {
        r#";; Exercise: Real-world Application
;; Build production-ready smart contracts

(module realworld-exercise 'admin-keyset
  @doc "Real-world application patterns"
  
  ;; TODO: Implement business logic
  (defun core-business-function ()
    YOUR_CODE_HERE)
  
  ;; TODO: Add governance
  (defcap GOVERNANCE ()
    YOUR_CODE_HERE)
  
  ;; TODO: Add emergency functions
  (defun emergency-pause ()
    YOUR_CODE_HERE))
"#.to_string()
    }

    fn generate_generic_template(&self) -> String {
        format!(
            r#";; Exercise: {}
;; {}

;; TODO: Complete this exercise
;; Hint: {}

YOUR_CODE_HERE
"#,
            self.name, self.description, self.hint
        )
    }
}