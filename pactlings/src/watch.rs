use anyhow::Result;
use colored::*;
use console::Term;
use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::path::Path;
use std::sync::mpsc;
use std::time::Duration;
use tokio::time::sleep;

use crate::config::Config;
use crate::exercise::Exercise;
use crate::progress::Progress;
use crate::verify::Verifier;

pub struct WatchMode {
    config: Config,
    term: Term,
}

impl WatchMode {
    pub fn new(config: &Config) -> Self {
        Self {
            config: config.clone(),
            term: Term::stdout(),
        }
    }

    pub async fn watch_exercise(&self, exercise: &Exercise, progress: &mut Progress) -> Result<()> {
        let verifier = Verifier::new(&self.config);
        
        // Set up file watcher
        let (tx, rx) = mpsc::channel();
        let mut watcher: RecommendedWatcher = Watcher::new(
            move |res: notify::Result<Event>| {
                if let Ok(event) = res {
                    if matches!(event.kind, EventKind::Modify(_)) {
                        let _ = tx.send(event);
                    }
                }
            },
            notify::Config::default().with_poll_interval(Duration::from_millis(500)),
        )?;

        // Watch the exercise file
        watcher.watch(&exercise.path, RecursiveMode::NonRecursive)?;

        self.term.write_line(&format!(
            "{} Watching: {} - edit the file and save to auto-verify",
            "👀".yellow(),
            exercise.path.display().to_string().bold()
        ))?;
        self.term.write_line("Press Ctrl+C to stop watching...")?;
        self.term.write_line("")?;

        let mut last_modification = std::time::SystemTime::now();
        let debounce_duration = Duration::from_millis(1000);

        loop {
            // Check for file changes
            if let Ok(event) = rx.try_recv() {
                let now = std::time::SystemTime::now();
                
                // Debounce file changes to avoid running verification too frequently
                if now.duration_since(last_modification).unwrap_or_default() > debounce_duration {
                    last_modification = now;
                    
                    self.term.write_line(&format!(
                        "{} File changed, verifying...",
                        "🔄".blue()
                    ))?;
                    
                    match verifier.verify_exercise(exercise).await {
                        Ok(result) => {
                            if result.success {
                                progress.mark_completed(&exercise.name);
                                if let Err(e) = progress.save(&self.config.progress_file) {
                                    self.term.write_line(&format!(
                                        "{} Warning: Could not save progress: {}",
                                        "⚠️".yellow(),
                                        e
                                    ))?;
                                }
                                
                                self.term.write_line(&format!(
                                    "{} Exercise completed successfully! 🎉",
                                    "✅".green()
                                ))?;
                                
                                self.show_completion_celebration(&exercise)?;
                                return Ok(());
                            } else {
                                let error_msg = result.error_message.as_ref()
                                    .map(|s| s.as_str())
                                    .unwrap_or("Unknown error");
                                self.term.write_line(&format!(
                                    "{} Verification failed: {}",
                                    "❌".red(),
                                    error_msg
                                ))?;
                                
                                self.show_verification_help(&result)?;
                            }
                        },
                        Err(e) => {
                            self.term.write_line(&format!(
                                "{} Error during verification: {}",
                                "💥".red(),
                                e
                            ))?;
                        }
                    }
                    
                    self.term.write_line("")?;
                    self.term.write_line(&format!(
                        "{} Continuing to watch for changes...",
                        "👀".yellow()
                    ))?;
                }
            }

            // Small delay to prevent busy waiting
            sleep(Duration::from_millis(100)).await;
        }
    }

    pub async fn watch_all_exercises(&self, progress: &mut Progress) -> Result<()> {
        let exercises = self.config.load_exercises()?;
        let verifier = Verifier::new(&self.config);
        
        // Set up file watcher for exercises directory
        let (tx, rx) = mpsc::channel();
        let mut watcher: RecommendedWatcher = Watcher::new(
            move |res: notify::Result<Event>| {
                if let Ok(event) = res {
                    if matches!(event.kind, EventKind::Modify(_)) {
                        let _ = tx.send(event);
                    }
                }
            },
            notify::Config::default().with_poll_interval(Duration::from_millis(500)),
        )?;

        // Watch the exercises directory
        watcher.watch(&self.config.exercises_dir, RecursiveMode::Recursive)?;

        self.term.write_line(&format!(
            "{} Watching all exercises in: {}",
            "👀".yellow(),
            self.config.exercises_dir.display().to_string().bold()
        ))?;
        self.term.write_line("Edit any exercise file and save to auto-verify")?;
        self.term.write_line("Press Ctrl+C to stop watching...")?;
        self.term.write_line("")?;

        let mut last_modifications: std::collections::HashMap<std::path::PathBuf, std::time::SystemTime> = 
            std::collections::HashMap::new();
        let debounce_duration = Duration::from_millis(1000);

        loop {
            if let Ok(event) = rx.try_recv() {
                for path in event.paths {
                    // Find which exercise this file belongs to
                    if let Some(exercise) = exercises.iter().find(|e| e.path == path) {
                        let now = std::time::SystemTime::now();
                        let last_mod = last_modifications.get(&path);
                        
                        // Debounce file changes
                        if last_mod.is_none() || 
                           now.duration_since(*last_mod.unwrap()).unwrap_or_default() > debounce_duration {
                            last_modifications.insert(path.clone(), now);
                            
                            self.term.write_line(&format!(
                                "{} {} changed, verifying...",
                                "🔄".blue(),
                                exercise.name.bold()
                            ))?;
                            
                            match verifier.verify_exercise(exercise).await {
                                Ok(result) => {
                                    if result.success {
                                        progress.mark_completed(&exercise.name);
                                        if let Err(e) = progress.save(&self.config.progress_file) {
                                            self.term.write_line(&format!(
                                                "{} Warning: Could not save progress: {}",
                                                "⚠️".yellow(),
                                                e
                                            ))?;
                                        }
                                        
                                        self.term.write_line(&format!(
                                            "{} {} completed! 🎉",
                                            "✅".green(),
                                            exercise.name.bold()
                                        ))?;
                                    } else {
                                        self.term.write_line(&format!(
                                            "{} {} failed: {}",
                                            "❌".red(),
                                            exercise.name,
                                            result.error_message.as_ref().unwrap_or(&"Unknown error".to_string())
                                        ))?;
                                    }
                                },
                                Err(e) => {
                                    self.term.write_line(&format!(
                                        "{} Error verifying {}: {}",
                                        "💥".red(),
                                        exercise.name,
                                        e
                                    ))?;
                                }
                            }
                        }
                    }
                }
            }

            sleep(Duration::from_millis(100)).await;
        }
    }

    fn show_completion_celebration(&self, exercise: &Exercise) -> Result<()> {
        self.term.write_line("")?;
        self.term.write_line("🎉 ════════════════════════════════════════ 🎉")?;
        self.term.write_line(&format!(
            "    {} EXERCISE COMPLETED! {}",
            "🌟".yellow(),
            "🌟".yellow()
        ))?;
        self.term.write_line(&format!(
            "    {} - {}",
            exercise.name.bold().green(),
            exercise.description
        ))?;
        self.term.write_line("🎉 ════════════════════════════════════════ 🎉")?;
        self.term.write_line("")?;
        
        // Show next steps
        self.term.write_line(&format!(
            "{} Great job! Your solution is working correctly.",
            "💯".green()
        ))?;
        self.term.write_line(&format!(
            "{} Run `pactlings list` to see your progress",
            "📊".blue()
        ))?;
        self.term.write_line(&format!(
            "{} Run `pactlings run <next-exercise>` to continue",
            "🚀".blue()
        ))?;
        
        Ok(())
    }

    fn show_verification_help(&self, result: &crate::verify::VerificationResult) -> Result<()> {
        if !result.checks_failed.is_empty() {
            self.term.write_line(&format!("{} Failed checks:", "❌".red()))?;
            for check in &result.checks_failed {
                self.term.write_line(&format!("  • {}", check))?;
            }
        }

        if !result.checks_passed.is_empty() {
            self.term.write_line(&format!("{} Passed checks:", "✅".green()))?;
            for check in &result.checks_passed {
                self.term.write_line(&format!("  • {}", check))?;
            }
        }

        if let Some(ref compilation) = result.compilation_result {
            if !compilation.success && !compilation.error.trim().is_empty() {
                self.term.write_line(&format!("{} Compilation error:", "🚨".red()))?;
                self.term.write_line(&compilation.error)?;
            }
        }

        Ok(())
    }
}