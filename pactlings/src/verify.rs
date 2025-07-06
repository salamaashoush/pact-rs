use anyhow::{anyhow, Result};
use colored::*;
use console::Term;
use std::process::Stdio;
use tokio::process::Command as AsyncCommand;

use crate::config::Config;
use crate::exercise::Exercise;
use crate::run::{ExecutionResult, Runner};

pub struct Verifier {
    config: Config,
    runner: Runner,
    term: Term,
}

#[derive(Debug)]
pub struct VerificationResult {
    pub success: bool,
    pub error_message: Option<String>,
    pub compilation_result: Option<ExecutionResult>,
    pub test_results: Vec<ExecutionResult>,
    pub checks_passed: Vec<String>,
    pub checks_failed: Vec<String>,
}

impl Verifier {
    pub fn new(config: &Config) -> Self {
        Self {
            config: config.clone(),
            runner: Runner::new(config),
            term: Term::stdout(),
        }
    }

    pub async fn verify_exercise(&self, exercise: &Exercise) -> Result<VerificationResult> {
        self.term.write_line(&format!(
            "{} Verifying: {} {}",
            "🔍".blue(),
            exercise.name.bold(),
            exercise.difficulty_emoji()
        ))?;

        let mut result = VerificationResult {
            success: false,
            error_message: None,
            compilation_result: None,
            test_results: Vec::new(),
            checks_passed: Vec::new(),
            checks_failed: Vec::new(),
        };

        // Step 1: Check if file exists and has content
        if !exercise.exists() {
            result.error_message = Some("Exercise file not found".to_string());
            return Ok(result);
        }

        let content = exercise.read_content()?;
        if content.trim().is_empty() {
            result.error_message = Some("Exercise file is empty".to_string());
            return Ok(result);
        }

        // Step 2: Check for common incomplete patterns
        if self.has_incomplete_patterns(&content) {
            let incomplete_items = self.find_incomplete_patterns(&content);
            result.error_message = Some(format!(
                "Exercise appears incomplete. Found: {}",
                incomplete_items.join(", ")
            ));
            result.checks_failed.extend(incomplete_items);
            return Ok(result);
        }
        result.checks_passed.push("No incomplete patterns found".to_string());

        // Step 3: Syntax check (compilation)
        self.term.write_line("  📝 Checking syntax...")?;
        let compilation_result = self.check_syntax(&exercise.path).await?;
        result.compilation_result = Some(compilation_result.clone());

        if !compilation_result.success {
            result.error_message = Some(format!(
                "Syntax error: {}",
                compilation_result.error
            ));
            result.checks_failed.push("Syntax check failed".to_string());
            return Ok(result);
        }
        result.checks_passed.push("Syntax check passed".to_string());

        // Step 4: Exercise-specific validation
        self.term.write_line("  🔍 Running exercise-specific checks...")?;
        let exercise_checks = self.run_exercise_specific_checks(exercise, &content).await?;
        result.checks_passed.extend(exercise_checks.passed);
        let has_failed_checks = !exercise_checks.failed.is_empty();
        let failed_checks_message = exercise_checks.failed.join(", ");
        result.checks_failed.extend(exercise_checks.failed);

        if has_failed_checks {
            result.error_message = Some(format!(
                "Exercise checks failed: {}",
                failed_checks_message
            ));
            return Ok(result);
        }

        // Step 5: Run any test commands
        if !exercise.test_commands.is_empty() {
            self.term.write_line("  🧪 Running tests...")?;
            let test_result = self.runner.run_tests(exercise).await?;
            result.test_results = test_result.test_results;
            
            if !test_result.success {
                result.error_message = Some("Tests failed".to_string());
                result.checks_failed.push("Tests failed".to_string());
                return Ok(result);
            }
            result.checks_passed.push("All tests passed".to_string());
        }

        // Step 6: Final validation
        result.success = true;
        self.term.write_line(&format!("  {} Exercise verification complete!", "✅".green()))?;

        Ok(result)
    }

    fn has_incomplete_patterns(&self, content: &str) -> bool {
        let patterns = [
            "TODO",
            "YOUR_CODE_HERE",
            "FILL_IN_THE_BLANK",
            "YOUR_KEYSET_NAME",
            "YOUR_MODULE_NAME",
            "YOUR_CAPABILITY_NAME",
            "YOUR_MANAGER_FUNCTION",
            "YOUR_SCHEMA_FIELDS",
            "YOUR_TABLE_NAME",
            "YOUR_ACCOUNTS_TABLE",
            "YOUR_INTERFACE_NAME",
        ];

        patterns.iter().any(|pattern| content.contains(pattern))
    }

    fn find_incomplete_patterns(&self, content: &str) -> Vec<String> {
        let patterns = [
            "TODO",
            "YOUR_CODE_HERE", 
            "FILL_IN_THE_BLANK",
            "YOUR_KEYSET_NAME",
            "YOUR_MODULE_NAME",
            "YOUR_CAPABILITY_NAME",
        ];

        patterns.iter()
            .filter(|pattern| content.contains(*pattern))
            .map(|s| s.to_string())
            .collect()
    }

    async fn check_syntax(&self, file_path: &std::path::Path) -> Result<ExecutionResult> {
        let mut cmd = AsyncCommand::new(&self.config.pact_executable);
        cmd.arg("--check")  // Syntax check only
            .arg(file_path)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        let output = cmd.output().await?;
        
        Ok(ExecutionResult {
            success: output.status.success(),
            output: String::from_utf8_lossy(&output.stdout).to_string(),
            error: String::from_utf8_lossy(&output.stderr).to_string(),
        })
    }

    async fn run_exercise_specific_checks(&self, exercise: &Exercise, content: &str) -> Result<ExerciseCheckResult> {
        let mut passed = Vec::new();
        let mut failed = Vec::new();

        match exercise.topic.as_str() {
            "Basics" => {
                let checks = self.check_basics_exercise(exercise, content).await?;
                passed.extend(checks.passed);
                failed.extend(checks.failed);
            },
            "Modules" => {
                let checks = self.check_modules_exercise(exercise, content).await?;
                passed.extend(checks.passed);
                failed.extend(checks.failed);
            },
            "Capabilities" => {
                let checks = self.check_capabilities_exercise(exercise, content).await?;
                passed.extend(checks.passed);
                failed.extend(checks.failed);
            },
            "Database" => {
                let checks = self.check_database_exercise(exercise, content).await?;
                passed.extend(checks.passed);
                failed.extend(checks.failed);
            },
            "Interfaces" => {
                let checks = self.check_interfaces_exercise(exercise, content).await?;
                passed.extend(checks.passed);
                failed.extend(checks.failed);
            },
            "Pacts" => {
                let checks = self.check_pacts_exercise(exercise, content).await?;
                passed.extend(checks.passed);
                failed.extend(checks.failed);
            },
            "Applications" => {
                let checks = self.check_applications_exercise(exercise, content).await?;
                passed.extend(checks.passed);
                failed.extend(checks.failed);
            },
            _ => {
                // Generic checks
                passed.push("Exercise content present".to_string());
            }
        }

        Ok(ExerciseCheckResult { passed, failed })
    }

    async fn check_basics_exercise(&self, exercise: &Exercise, content: &str) -> Result<ExerciseCheckResult> {
        let mut passed = Vec::new();
        let mut failed = Vec::new();

        match exercise.name.as_str() {
            "basics_01_expressions" => {
                if content.contains("(+") {
                    passed.push("Contains addition expressions".to_string());
                } else {
                    failed.push("Missing addition expressions".to_string());
                }
                
                if content.contains("(-") {
                    passed.push("Contains subtraction expressions".to_string());
                } else {
                    failed.push("Missing subtraction expressions".to_string());
                }
                
                if content.contains("(*") {
                    passed.push("Contains multiplication expressions".to_string());
                } else {
                    failed.push("Missing multiplication expressions".to_string());
                }
            },
            "basics_02_types" => {
                if content.contains("\"") {
                    passed.push("Contains string literals".to_string());
                } else {
                    failed.push("Missing string literals".to_string());
                }
                
                if content.matches(".").count() > 0 && content.contains(char::is_numeric) {
                    passed.push("Contains decimal numbers".to_string());
                } else {
                    failed.push("Missing decimal numbers".to_string());
                }
            },
            "basics_03_variables" => {
                if content.contains("(let") {
                    passed.push("Contains let expressions".to_string());
                } else {
                    failed.push("Missing let expressions".to_string());
                }
            },
            "basics_04_functions" => {
                if content.contains("(defun") {
                    passed.push("Contains function definitions".to_string());
                } else {
                    failed.push("Missing function definitions".to_string());
                }
                
                if content.contains(":decimal") || content.contains(":string") {
                    passed.push("Contains type annotations".to_string());
                } else {
                    failed.push("Missing type annotations".to_string());
                }
            },
            _ => {}
        }

        Ok(ExerciseCheckResult { passed, failed })
    }

    async fn check_modules_exercise(&self, exercise: &Exercise, content: &str) -> Result<ExerciseCheckResult> {
        let mut passed = Vec::new();
        let mut failed = Vec::new();

        if content.contains("(module") {
            passed.push("Contains module definition".to_string());
        } else {
            failed.push("Missing module definition".to_string());
        }

        if content.contains("define-keyset") {
            passed.push("Contains keyset definition".to_string());
        } else {
            failed.push("Missing keyset definition".to_string());
        }

        Ok(ExerciseCheckResult { passed, failed })
    }

    async fn check_capabilities_exercise(&self, exercise: &Exercise, content: &str) -> Result<ExerciseCheckResult> {
        let mut passed = Vec::new();
        let mut failed = Vec::new();

        if content.contains("(defcap") {
            passed.push("Contains capability definition".to_string());
        } else {
            failed.push("Missing capability definition".to_string());
        }

        if content.contains("with-capability") {
            passed.push("Contains capability usage".to_string());
        } else {
            failed.push("Missing capability usage".to_string());
        }

        if exercise.name.contains("managed") {
            if content.contains("@managed") {
                passed.push("Contains managed capability annotation".to_string());
            } else {
                failed.push("Missing managed capability annotation".to_string());
            }
        }

        Ok(ExerciseCheckResult { passed, failed })
    }

    async fn check_database_exercise(&self, exercise: &Exercise, content: &str) -> Result<ExerciseCheckResult> {
        let mut passed = Vec::new();
        let mut failed = Vec::new();

        if content.contains("(defschema") {
            passed.push("Contains schema definition".to_string());
        } else {
            failed.push("Missing schema definition".to_string());
        }

        if content.contains("(deftable") {
            passed.push("Contains table definition".to_string());
        } else {
            failed.push("Missing table definition".to_string());
        }

        if exercise.name.contains("operations") {
            if content.contains("insert") || content.contains("read") || content.contains("update") {
                passed.push("Contains database operations".to_string());
            } else {
                failed.push("Missing database operations".to_string());
            }
        }

        Ok(ExerciseCheckResult { passed, failed })
    }

    async fn check_interfaces_exercise(&self, exercise: &Exercise, content: &str) -> Result<ExerciseCheckResult> {
        let mut passed = Vec::new();
        let mut failed = Vec::new();

        if content.contains("(interface") {
            passed.push("Contains interface definition".to_string());
        } else {
            failed.push("Missing interface definition".to_string());
        }

        if content.contains("implements") {
            passed.push("Contains interface implementation".to_string());
        } else {
            failed.push("Missing interface implementation".to_string());
        }

        Ok(ExerciseCheckResult { passed, failed })
    }

    async fn check_pacts_exercise(&self, exercise: &Exercise, content: &str) -> Result<ExerciseCheckResult> {
        let mut passed = Vec::new();
        let mut failed = Vec::new();

        if content.contains("(defpact") {
            passed.push("Contains pact definition".to_string());
        } else {
            failed.push("Missing pact definition".to_string());
        }

        if content.contains("(step") {
            passed.push("Contains step definitions".to_string());
        } else {
            failed.push("Missing step definitions".to_string());
        }

        Ok(ExerciseCheckResult { passed, failed })
    }

    async fn check_applications_exercise(&self, exercise: &Exercise, content: &str) -> Result<ExerciseCheckResult> {
        let mut passed = Vec::new();
        let mut failed = Vec::new();

        // Applications should combine multiple concepts
        let concepts = [
            ("module", "Module definition"),
            ("defcap", "Capability definitions"),
            ("defschema", "Schema definitions"),
            ("deftable", "Table definitions"),
            ("defun", "Function definitions"),
        ];

        for (pattern, description) in concepts {
            if content.contains(pattern) {
                passed.push(format!("Contains {}", description.to_lowercase()));
            } else {
                failed.push(format!("Missing {}", description.to_lowercase()));
            }
        }

        Ok(ExerciseCheckResult { passed, failed })
    }
}

#[derive(Debug)]
struct ExerciseCheckResult {
    passed: Vec<String>,
    failed: Vec<String>,
}