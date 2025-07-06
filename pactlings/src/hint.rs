use anyhow::Result;
use colored::*;
use console::Term;
use std::collections::HashMap;

use crate::exercise::Exercise;

pub struct HintSystem {
    term: Term,
    custom_hints: HashMap<String, Vec<String>>,
}

impl HintSystem {
    pub fn new() -> Self {
        let mut custom_hints = HashMap::new();
        
        // Add progressive hints for each exercise
        custom_hints.insert("basics_01_expressions".to_string(), vec![
            "Pact uses prefix notation: (operator operand1 operand2)".to_string(),
            "Try (+ 1 2) for addition, (- 10 5) for subtraction".to_string(),
            "String concatenation also uses +: (+ \"Hello\" \" World\")".to_string(),
            "Boolean operations: (and true false), (or true false), (not true)".to_string(),
        ]);

        custom_hints.insert("basics_02_types".to_string(), vec![
            "Strings must be in quotes: \"Hello World\"".to_string(),
            "Decimals have dots: 3.14, integers don't: 42".to_string(),
            "Booleans are: true or false".to_string(),
            "Dates can be parsed: (parse-time \"%Y-%m-%d\" \"2023-01-01\")".to_string(),
        ]);

        custom_hints.insert("basics_03_variables".to_string(), vec![
            "Let expressions bind variables: (let ((x 10) (y 20)) ...)".to_string(),
            "Variables are available in the body after binding".to_string(),
            "You can nest let expressions for complex scoping".to_string(),
            "Variable names can be shadowed in inner scopes".to_string(),
        ]);

        custom_hints.insert("basics_04_functions".to_string(), vec![
            "Functions need type annotations: (defun add (a:decimal b:decimal) ...)".to_string(),
            "Use @doc strings to document your functions".to_string(),
            "Function body should return a value".to_string(),
            "Test your functions by calling them at the end".to_string(),
        ]);

        custom_hints.insert("modules_01_basic".to_string(), vec![
            "Define a keyset first: (define-keyset 'my-keyset ...)".to_string(),
            "Module syntax: (module name governance doc-string body)".to_string(),
            "Add functions inside the module body".to_string(),
            "Module names should be descriptive".to_string(),
        ]);

        custom_hints.insert("modules_02_keysets".to_string(), vec![
            "Multiple keysets can be defined for different purposes".to_string(),
            "Use (enforce-keyset 'keyset-name) to check keysets".to_string(),
            "Capabilities can check keysets for authorization".to_string(),
            "Admin functions should require admin keysets".to_string(),
        ]);

        custom_hints.insert("capabilities_01_basic".to_string(), vec![
            "Capabilities are defined with (defcap NAME () body)".to_string(),
            "Use (with-capability (NAME) body) to acquire capabilities".to_string(),
            "Capabilities should validate preconditions".to_string(),
            "Parameterized capabilities take arguments for validation".to_string(),
        ]);

        custom_hints.insert("capabilities_02_managed".to_string(), vec![
            "Managed capabilities use @managed annotation".to_string(),
            "Manager functions control resource usage".to_string(),
            "Manager signature: (defun manager:type (managed requested) ...)".to_string(),
            "Return remaining allowance from manager function".to_string(),
        ]);

        custom_hints.insert("database_01_tables".to_string(), vec![
            "Schemas define data structure: (defschema name field:type ...)".to_string(),
            "Tables use schemas: (deftable name:{schema})".to_string(),
            "Don't forget to create tables after definition".to_string(),
            "Common field types: string, decimal, integer, bool, guard".to_string(),
        ]);

        custom_hints.insert("database_02_operations".to_string(), vec![
            "Use (insert table key object) to create records".to_string(),
            "Use (read table key) to read records".to_string(),
            "Use (update table key object) to modify records".to_string(),
            "Use (with-read table key binding body) for safe reading".to_string(),
        ]);

        Self {
            term: Term::stdout(),
            custom_hints,
        }
    }

    pub fn show_hint(&self, exercise: &Exercise, hint_level: usize) -> Result<()> {
        self.term.write_line(&format!(
            "{} Hint for: {} {}",
            "💡".yellow(),
            exercise.name.bold(),
            exercise.difficulty_emoji()
        ))?;
        self.term.write_line("")?;

        // Show the basic hint first
        if hint_level == 0 {
            self.term.write_line(&format!("📝 {}", exercise.hint))?;
        }

        // Show progressive hints if available
        if let Some(hints) = self.custom_hints.get(&exercise.name) {
            if hint_level < hints.len() {
                self.term.write_line("")?;
                self.term.write_line(&format!("{} Progressive Hint {}:", "🔍".blue(), hint_level + 1))?;
                self.term.write_line(&format!("   {}", hints[hint_level]))?;
                
                if hint_level + 1 < hints.len() {
                    self.term.write_line("")?;
                    self.term.write_line(&format!(
                        "{} Run `pactlings hint {} --level {}` for the next hint",
                        "ℹ️".blue(),
                        exercise.name,
                        hint_level + 1
                    ))?;
                }
            } else {
                self.term.write_line("")?;
                self.term.write_line(&format!("{} All hints shown!", "✅".green()))?;
            }
        }

        // Show related concepts
        self.show_related_concepts(exercise)?;

        // Show common patterns
        self.show_common_patterns(exercise)?;

        // Show example if needed
        if hint_level > 2 {
            self.show_example(exercise)?;
        }

        Ok(())
    }

    fn show_related_concepts(&self, exercise: &Exercise) -> Result<()> {
        let concepts = match exercise.topic.as_str() {
            "Basics" => vec![
                "S-expressions: (operator operand1 operand2 ...)",
                "Type annotations: name:type",
                "Function calls: (function-name arg1 arg2)",
                "Variable binding: (let ((var value)) body)",
            ],
            "Modules" => vec![
                "Keysets: (define-keyset 'name (read-keyset \"name\"))",
                "Modules: (module name governance doc body)",
                "Governance: Controls who can upgrade the module",
                "Documentation: Use @doc strings for clarity",
            ],
            "Capabilities" => vec![
                "Authorization: Capabilities control access",
                "Acquisition: (with-capability (CAP) body)",
                "Validation: Capabilities check preconditions",
                "Composition: Capabilities can compose together",
            ],
            "Database" => vec![
                "Schemas: Define data structure",
                "Tables: Store data with schemas",
                "CRUD: Create, Read, Update, Delete operations",
                "Guards: Control row-level access",
            ],
            _ => vec![],
        };

        if !concepts.is_empty() {
            self.term.write_line("")?;
            self.term.write_line(&format!("{} Key Concepts:", "🧠".blue()))?;
            for concept in concepts {
                self.term.write_line(&format!("   • {}", concept))?;
            }
        }

        Ok(())
    }

    fn show_common_patterns(&self, exercise: &Exercise) -> Result<()> {
        let patterns = match exercise.name.as_str() {
            name if name.contains("expressions") => vec![
                "(+ 1 2)           ; Addition",
                "(* 3.14 2.0)      ; Multiplication with decimals", 
                "(and true false)  ; Boolean AND",
                "(+ \"Hello\" \" World\") ; String concatenation",
            ],
            name if name.contains("variables") => vec![
                "(let ((x 10)) x)  ; Simple binding",
                "(let ((x 1) (y 2)) (+ x y)) ; Multiple bindings",
                "(let ((outer 5)) (let ((inner 3)) (* outer inner))) ; Nested",
            ],
            name if name.contains("functions") => vec![
                "(defun add (a:decimal b:decimal) (+ a b)) ; Simple function",
                "(defun greet (name:string) (+ \"Hello, \" name)) ; String function",
                "@doc \"Function description\" ; Documentation",
            ],
            name if name.contains("modules") => vec![
                "(define-keyset 'admin (read-keyset \"admin\"))",
                "(module my-module 'admin \"Description\" ...)",
                "(defun public-function () \"Available to all\")",
            ],
            name if name.contains("capabilities") => vec![
                "(defcap ADMIN () (enforce-keyset 'admin))",
                "(with-capability (ADMIN) \"admin action\")",
                "(defcap TRANSFER (from:string to:string amount:decimal) ...)",
            ],
            _ => vec![],
        };

        if !patterns.is_empty() {
            self.term.write_line("")?;
            self.term.write_line(&format!("{} Common Patterns:", "📋".green()))?;
            for pattern in patterns {
                self.term.write_line(&format!("   {}", pattern.dimmed()))?;
            }
        }

        Ok(())
    }

    fn show_example(&self, exercise: &Exercise) -> Result<()> {
        self.term.write_line("")?;
        self.term.write_line(&format!("{} Example Solution Structure:", "📖".cyan()))?;
        
        let example = match exercise.name.as_str() {
            "basics_01_expressions" => {
                r#"   ;; Basic arithmetic
   (+ 1 2)        ; Returns 3
   (- 10 3)       ; Returns 7
   (* 4 5)        ; Returns 20
   (/ 20 4)       ; Returns 5.0
   
   ;; String operations
   (+ "hello" " world")  ; Returns "hello world"
   
   ;; Boolean operations
   (and true false)      ; Returns false
   (or true false)       ; Returns true
   (not true)           ; Returns false"#
            },
            "basics_04_functions" => {
                r#"   (defun add-decimals (a:decimal b:decimal)
     @doc "Add two decimal numbers"
     (+ a b))
   
   (defun greet (name:string)
     @doc "Greet a person by name"
     (+ "Hello, " name "!"))
   
   ;; Test the functions
   (add-decimals 3.5 2.1)  ; Should return 5.6
   (greet "Alice")         ; Should return "Hello, Alice!""#
            },
            "modules_01_basic" => {
                r#"   (define-keyset 'my-keyset
     (read-keyset "my-keyset"))
   
   (module calculator 'my-keyset
     "A simple calculator module"
     
     (defun add (a:decimal b:decimal)
       @doc "Add two numbers"
       (+ a b))
       
     (defun multiply (a:decimal b:decimal)
       @doc "Multiply two numbers"
       (* a b)))"#
            },
            _ => "   ;; Complete the exercise based on the requirements above",
        };

        for line in example.lines() {
            self.term.write_line(line)?;
        }

        Ok(())
    }

    pub fn show_interactive_hint(&self, exercise: &Exercise) -> Result<()> {
        use dialoguer::{theme::ColorfulTheme, Select};

        let options = vec![
            "💡 Show basic hint",
            "🔍 Show progressive hints",
            "🧠 Show key concepts", 
            "📋 Show common patterns",
            "📖 Show example structure",
            "❌ Exit hints",
        ];

        loop {
            let selection = Select::with_theme(&ColorfulTheme::default())
                .with_prompt(&format!("Hint options for {}", exercise.name.bold()))
                .items(&options)
                .default(0)
                .interact()?;

            match selection {
                0 => {
                    self.term.write_line(&format!("💡 {}", exercise.hint))?;
                },
                1 => {
                    if let Some(hints) = self.custom_hints.get(&exercise.name) {
                        for (i, hint) in hints.iter().enumerate() {
                            self.term.write_line(&format!("{}. {}", i + 1, hint))?;
                        }
                    } else {
                        self.term.write_line("No progressive hints available for this exercise.")?;
                    }
                },
                2 => {
                    self.show_related_concepts(exercise)?;
                },
                3 => {
                    self.show_common_patterns(exercise)?;
                },
                4 => {
                    self.show_example(exercise)?;
                },
                5 => break,
                _ => unreachable!(),
            }
            
            self.term.write_line("")?;
        }

        Ok(())
    }
}