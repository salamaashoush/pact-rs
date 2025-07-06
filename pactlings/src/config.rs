use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

use crate::exercise::Exercise;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Config {
    pub exercises_dir: PathBuf,
    pub solutions_dir: PathBuf,
    pub info_dir: PathBuf,
    pub progress_file: PathBuf,
    pub pact_executable: String,
    pub auto_verify: bool,
    pub show_hints: bool,
    pub exercises: Vec<ExerciseConfig>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ExerciseConfig {
    pub name: String,
    pub file: String,
    pub topic: String,
    pub difficulty: Difficulty,
    pub description: String,
    pub hint: String,
    pub solution_file: Option<String>,
    pub info_file: Option<String>,
    pub prerequisites: Vec<String>,
    pub test_commands: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum Difficulty {
    Beginner,
    Intermediate,
    Advanced,
}

impl Config {
    pub fn load_or_default() -> Result<Self> {
        // Always use default for now since we have embedded exercises
        Ok(Self::default())
    }

    pub fn load_from_file(path: PathBuf) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let config: Config = toml::from_str(&content)?;
        Ok(config)
    }

    pub fn save(&self, path: PathBuf) -> Result<()> {
        let content = toml::to_string_pretty(self)?;
        std::fs::write(path, content)?;
        Ok(())
    }

    pub fn default_for_path(base_path: &PathBuf) -> Self {
        let mut config = Self::default();
        config.exercises_dir = base_path.join("exercises");
        config.solutions_dir = base_path.join("solutions"); 
        config.info_dir = base_path.join("info");
        config.progress_file = base_path.join(".pactlings_progress.json");
        config
    }

    pub fn load_exercises(&self) -> Result<Vec<Exercise>> {
        self.exercises
            .iter()
            .map(|config| Exercise::from_config(config.clone(), &self.exercises_dir))
            .collect()
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            exercises_dir: PathBuf::from("exercises"),
            solutions_dir: PathBuf::from("solutions"),
            info_dir: PathBuf::from("info"),
            progress_file: PathBuf::from(".pactlings_progress.json"),
            pact_executable: "pact".to_string(),
            auto_verify: true,
            show_hints: true,
            exercises: Self::default_exercises(),
        }
    }
}

impl Config {
    fn default_exercises() -> Vec<ExerciseConfig> {
        // Clean, maintainable exercise list - just references to external files
        // Like Rustlings, exercises are in separate .pact files that users edit directly
        vec![
            // BASICS
            ExerciseConfig {
                name: "basics_01_expressions".to_string(),
                file: "basics/01_expressions.pact".to_string(),
                topic: "Basics".to_string(),
                difficulty: Difficulty::Beginner,
                description: "Learn basic Pact expressions and S-expression syntax".to_string(),
                hint: "Pact uses S-expressions: (operator operand1 operand2). Replace 'I AM NOT DONE' with correct values.".to_string(),
                solution_file: None,
                info_file: None,
                prerequisites: vec![],
                test_commands: vec!["pact".to_string()],
            },
            ExerciseConfig {
                name: "basics_02_types".to_string(),
                file: "basics/02_types.pact".to_string(),
                topic: "Basics".to_string(),
                difficulty: Difficulty::Beginner,
                description: "Understand Pact's type system: strings, decimals, integers, booleans".to_string(),
                hint: "Use quotes for strings, decimal points for decimals. Replace 'I AM NOT DONE' with typed values.".to_string(),
                solution_file: None,
                info_file: None,
                prerequisites: vec!["basics_01_expressions".to_string()],
                test_commands: vec!["pact".to_string()],
            },
            
            // GUARDS 
            ExerciseConfig {
                name: "guards_01_basic".to_string(),
                file: "guards/01_basic.pact".to_string(),
                topic: "Guards".to_string(),
                difficulty: Difficulty::Intermediate,
                description: "Learn different types of guards: keyset, user, module, capability".to_string(),
                hint: "Guards unify all authorization patterns. Try keyset-ref-guard first.".to_string(),
                solution_file: None,
                info_file: None,
                prerequisites: vec!["basics_02_types".to_string()],
                test_commands: vec!["pact".to_string()],
            },
            
            // DEFPACTS
            ExerciseConfig {
                name: "defpacts_01_basic".to_string(),
                file: "defpacts/01_basic.pact".to_string(),
                topic: "Defpacts".to_string(),
                difficulty: Difficulty::Advanced,
                description: "Create multi-step pacts with continue-pact".to_string(),
                hint: "Defpacts execute across multiple transactions. Use step and continue-pact.".to_string(),
                solution_file: None,
                info_file: None,
                prerequisites: vec!["guards_01_basic".to_string()],
                test_commands: vec!["pact".to_string()],
            },
            
            // CRYPTOGRAPHY
            ExerciseConfig {
                name: "crypto_01_hashing".to_string(),
                file: "crypto/01_hashing.pact".to_string(),
                topic: "Cryptography".to_string(),
                difficulty: Difficulty::Intermediate,
                description: "Use hash functions: hash, hash-keccak256, hash-poseidon".to_string(),
                hint: "Hash functions create unique fingerprints. Use hash for general purpose.".to_string(),
                solution_file: None,
                info_file: None,
                prerequisites: vec!["basics_02_types".to_string()],
                test_commands: vec!["pact".to_string()],
            },
            
            // TESTING
            ExerciseConfig {
                name: "testing_01_repl_basics".to_string(),
                file: "testing/01_repl_basics.pact".to_string(),
                topic: "Testing".to_string(),
                difficulty: Difficulty::Intermediate,
                description: "Learn REPL testing with expect and expect-failure".to_string(),
                hint: "Use expect for success cases, expect-failure for error cases.".to_string(),
                solution_file: None,
                info_file: None,
                prerequisites: vec!["basics_02_types".to_string()],
                test_commands: vec!["pact".to_string()],
            },
        ]
    }
}