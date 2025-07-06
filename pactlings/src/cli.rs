use anyhow::{anyhow, Result};
use colored::*;
use console::Term;
use dialoguer::{theme::ColorfulTheme, Select};
use std::path::PathBuf;

use crate::config::Config;
use crate::exercise::Exercise;
use crate::progress::Progress;
use crate::run::Runner;
use crate::verify::Verifier;
use crate::watch::WatchMode;

pub struct PactlingsApp {
    pub config: Config,
    pub progress: Progress,
    pub term: Term,
}

impl PactlingsApp {
    pub fn new() -> Result<Self> {
        let config = Config::load_or_default()?;
        let progress = Progress::load_or_default(&config.progress_file)?;
        let term = Term::stdout();
        
        Ok(Self {
            config,
            progress,
            term,
        })
    }

    pub async fn init(&mut self, path: PathBuf) -> Result<()> {
        self.term.write_line(&format!(
            "{} Initializing pactlings in {}...",
            "🎯".green(),
            path.display()
        ))?;

        // Create directory structure
        std::fs::create_dir_all(&path)?;
        std::fs::create_dir_all(path.join("exercises"))?;
        std::fs::create_dir_all(path.join("solutions"))?;
        std::fs::create_dir_all(path.join("info"))?;

        // Copy exercises and info files
        self.copy_exercises(&path)?;
        self.copy_info_files(&path)?;

        // Initialize config
        let config = Config::default_for_path(&path);
        config.save(path.join("pactlings.toml"))?;

        self.term.write_line(&format!(
            "{} Pactlings initialized! Run `pactlings list` to see available exercises.",
            "✅".green()
        ))?;

        Ok(())
    }

    pub async fn list(&mut self, filter: Option<String>, show_completed: bool) -> Result<()> {
        let exercises = self.config.load_exercises()?;
        
        self.term.write_line(&format!("{} Available Exercises", "📚".blue()))?;
        self.term.write_line("")?;

        for (i, exercise) in exercises.iter().enumerate() {
            let is_completed = self.progress.is_completed(&exercise.name);
            
            // Apply filters
            if let Some(ref filter_str) = filter {
                if !exercise.name.contains(filter_str) && !exercise.topic.contains(filter_str) {
                    continue;
                }
            }
            
            if !show_completed && is_completed {
                continue;
            }

            let status = if is_completed {
                "✅".green()
            } else {
                "⭕".yellow()
            };

            let number = format!("{:2}", i + 1).cyan();
            let name = exercise.name.bold();
            let topic = format!("[{}]", exercise.topic).dimmed();
            
            self.term.write_line(&format!(
                "  {} {} {} {}",
                status, number, name, topic
            ))?;
        }

        let completed_count = exercises.iter().filter(|e| self.progress.is_completed(&e.name)).count();
        let total_count = exercises.len();
        
        self.term.write_line("")?;
        self.term.write_line(&format!(
            "{} Progress: {}/{} exercises completed ({:.1}%)",
            "📊".blue(),
            completed_count,
            total_count,
            (completed_count as f64 / total_count as f64) * 100.0
        ))?;

        Ok(())
    }

    pub async fn run(&mut self, exercise_name: String) -> Result<()> {
        let exercise = self.find_exercise(&exercise_name)?;
        let runner = Runner::new(&self.config);
        
        self.term.write_line(&format!(
            "{} Running exercise: {}",
            "🚀".green(),
            exercise.name.bold()
        ))?;

        runner.run_exercise(&exercise).await
    }

    pub async fn hint(&mut self, exercise_name: String) -> Result<()> {
        let exercise = self.find_exercise(&exercise_name)?;
        
        self.term.write_line(&format!(
            "{} Hint for exercise: {}",
            "💡".yellow(),
            exercise.name.bold()
        ))?;
        self.term.write_line("")?;
        
        self.term.write_line(&exercise.hint)?;
        
        Ok(())
    }

    pub async fn verify(&mut self, exercise_name: String) -> Result<()> {
        let exercise = self.find_exercise(&exercise_name)?;
        let verifier = Verifier::new(&self.config);
        
        self.term.write_line(&format!(
            "{} Verifying exercise: {}",
            "🔍".blue(),
            exercise.name.bold()
        ))?;

        let result = verifier.verify_exercise(&exercise).await?;
        
        if result.success {
            self.progress.mark_completed(&exercise.name);
            self.progress.save(&self.config.progress_file)?;
            
            self.term.write_line(&format!(
                "{} Exercise completed successfully!",
                "✅".green()
            ))?;
            
            if let Some(next_exercise) = self.get_next_exercise(&exercise.name)? {
                self.term.write_line(&format!(
                    "{} Next exercise: {}",
                    "➡️".blue(),
                    next_exercise.name.bold()
                ))?;
            } else {
                self.term.write_line(&format!(
                    "{} Congratulations! You've completed all exercises!",
                    "🎉".green()
                ))?;
            }
        } else {
            self.term.write_line(&format!(
                "{} Exercise failed:",
                "❌".red()
            ))?;
            self.term.write_line(&result.error_message.unwrap_or_default())?;
        }

        Ok(())
    }

    pub async fn verify_all(&mut self) -> Result<()> {
        let exercises = self.config.load_exercises()?;
        let verifier = Verifier::new(&self.config);
        
        self.term.write_line(&format!(
            "{} Verifying all exercises...",
            "🔍".blue()
        ))?;

        let mut passed = 0;
        let mut failed = 0;

        for exercise in &exercises {
            let result = verifier.verify_exercise(exercise).await?;
            
            if result.success {
                passed += 1;
                self.progress.mark_completed(&exercise.name);
                self.term.write_line(&format!(
                    "  {} {}",
                    "✅".green(),
                    exercise.name
                ))?;
            } else {
                failed += 1;
                self.term.write_line(&format!(
                    "  {} {} - {}",
                    "❌".red(),
                    exercise.name,
                    result.error_message.unwrap_or_default()
                ))?;
            }
        }

        self.progress.save(&self.config.progress_file)?;
        
        self.term.write_line("")?;
        self.term.write_line(&format!(
            "{} Summary: {} passed, {} failed",
            "📊".blue(),
            passed.to_string().green(),
            failed.to_string().red()
        ))?;

        Ok(())
    }

    pub async fn watch(&mut self, exercise_name: Option<String>) -> Result<()> {
        let exercise = if let Some(name) = exercise_name {
            self.find_exercise(&name)?
        } else {
            self.get_current_exercise()?
        };

        self.term.write_line(&format!(
            "{} Watching exercise: {}",
            "👀".yellow(),
            exercise.name.bold()
        ))?;
        self.term.write_line("Press Ctrl+C to stop watching.")?;

        let watcher = WatchMode::new(&self.config);
        watcher.watch_exercise(&exercise, &mut self.progress).await
    }

    pub async fn reset(&mut self, exercise_name: String) -> Result<()> {
        let exercise = self.find_exercise(&exercise_name)?;
        
        self.term.write_line(&format!(
            "{} Resetting exercise: {}",
            "🔄".yellow(),
            exercise.name.bold()
        ))?;

        // Reset the exercise file to its original state
        let original_path = self.config.exercises_dir.join("templates").join(&exercise.file);
        let current_path = self.config.exercises_dir.join(&exercise.file);
        
        if original_path.exists() {
            std::fs::copy(&original_path, &current_path)?;
            self.progress.mark_incomplete(&exercise.name);
            self.progress.save(&self.config.progress_file)?;
            
            self.term.write_line(&format!(
                "{} Exercise reset successfully!",
                "✅".green()
            ))?;
        } else {
            return Err(anyhow!("Template file not found for exercise: {}", exercise.name));
        }

        Ok(())
    }

    pub async fn progress(&mut self) -> Result<()> {
        let exercises = self.config.load_exercises()?;
        let completed_count = exercises.iter().filter(|e| self.progress.is_completed(&e.name)).count();
        let total_count = exercises.len();
        
        self.term.write_line(&format!("{} Progress Report", "📊".blue()))?;
        self.term.write_line("")?;
        
        // Progress bar
        let progress_percentage = (completed_count as f64 / total_count as f64) * 100.0;
        let bar_width = 50;
        let filled = ((progress_percentage / 100.0) * bar_width as f64) as usize;
        let empty = bar_width - filled;
        
        let bar = format!(
            "[{}{}] {:.1}%",
            "=".repeat(filled).green(),
            " ".repeat(empty),
            progress_percentage
        );
        
        self.term.write_line(&format!("  {}", bar))?;
        self.term.write_line(&format!("  {}/{} exercises completed", completed_count, total_count))?;
        
        // Topic breakdown
        self.term.write_line("")?;
        self.term.write_line(&format!("{} Topic Breakdown", "📚".blue()))?;
        
        let mut topic_stats = std::collections::HashMap::new();
        for exercise in &exercises {
            let entry = topic_stats.entry(&exercise.topic).or_insert((0, 0));
            entry.1 += 1; // total
            if self.progress.is_completed(&exercise.name) {
                entry.0 += 1; // completed
            }
        }
        
        for (topic, (completed, total)) in topic_stats {
            let percentage = (completed as f64 / total as f64) * 100.0;
            self.term.write_line(&format!(
                "  {} {}: {}/{} ({:.0}%)",
                if completed == total { "✅" } else { "⭕" },
                topic,
                completed,
                total,
                percentage
            ))?;
        }

        Ok(())
    }

    pub async fn repl(&mut self, exercise: Option<String>) -> Result<()> {
        self.term.write_line(&format!(
            "{} Starting Pact REPL...",
            "🔧".blue()
        ))?;

        if let Some(exercise_name) = exercise {
            let exercise = self.find_exercise(&exercise_name)?;
            self.term.write_line(&format!(
                "{} Loading exercise context: {}",
                "📝".yellow(),
                exercise.name.bold()
            ))?;
        }

        // TODO: Launch Pact REPL with appropriate context
        self.term.write_line("REPL functionality not yet implemented. Use `cabal run exe:pact` for now.")?;
        
        Ok(())
    }

    pub async fn interactive_mode(&mut self) -> Result<()> {
        self.term.write_line(&format!(
            "{} Welcome to Pactlings - Interactive Pact Tutorial!",
            "🎯".green()
        ))?;
        self.term.write_line("")?;

        loop {
            let options = vec![
                "📚 List exercises",
                "🚀 Run current exercise", 
                "🔍 Verify current exercise",
                "💡 Get hint",
                "📊 Show progress",
                "👀 Watch mode",
                "❌ Exit",
            ];

            let selection = Select::with_theme(&ColorfulTheme::default())
                .with_prompt("What would you like to do?")
                .items(&options)
                .default(0)
                .interact()?;

            match selection {
                0 => self.list(None, false).await?,
                1 => {
                    let current = self.get_current_exercise()?;
                    self.run(current.name).await?;
                },
                2 => {
                    let current = self.get_current_exercise()?;
                    self.verify(current.name).await?;
                },
                3 => {
                    let current = self.get_current_exercise()?;
                    self.hint(current.name).await?;
                },
                4 => self.progress().await?,
                5 => {
                    let current = self.get_current_exercise()?;
                    self.watch(Some(current.name)).await?;
                },
                6 => break,
                _ => unreachable!(),
            }

            self.term.write_line("")?;
        }

        self.term.write_line(&format!("{} Happy coding! 🚀", "👋".yellow()))?;
        Ok(())
    }

    fn find_exercise(&self, name_or_number: &str) -> Result<Exercise> {
        let exercises = self.config.load_exercises()?;
        
        // Try to parse as number first
        if let Ok(number) = name_or_number.parse::<usize>() {
            if number > 0 && number <= exercises.len() {
                return Ok(exercises[number - 1].clone());
            }
        }
        
        // Try to find by name
        exercises
            .into_iter()
            .find(|e| e.name == name_or_number)
            .ok_or_else(|| anyhow!("Exercise not found: {}", name_or_number))
    }

    fn get_current_exercise(&self) -> Result<Exercise> {
        let exercises = self.config.load_exercises()?;
        
        // Find first incomplete exercise
        exercises
            .into_iter()
            .find(|e| !self.progress.is_completed(&e.name))
            .ok_or_else(|| anyhow!("All exercises completed!"))
    }

    fn get_next_exercise(&self, current_name: &str) -> Result<Option<Exercise>> {
        let exercises = self.config.load_exercises()?;
        
        let current_index = exercises
            .iter()
            .position(|e| e.name == current_name)
            .ok_or_else(|| anyhow!("Current exercise not found"))?;
        
        Ok(exercises.get(current_index + 1).cloned())
    }

    fn copy_exercises(&self, path: &PathBuf) -> Result<()> {
        // This would copy exercise templates from embedded resources
        // For now, we'll create them dynamically
        Ok(())
    }

    fn copy_info_files(&self, path: &PathBuf) -> Result<()> {
        // This would copy info files from embedded resources
        Ok(())
    }
}