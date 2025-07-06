use anyhow::Result;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Progress {
    pub completed_exercises: HashSet<String>,
    pub exercise_times: HashMap<String, ExerciseProgress>,
    pub started_at: DateTime<Utc>,
    pub last_updated: DateTime<Utc>,
    pub total_time_spent: u64, // seconds
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ExerciseProgress {
    pub started_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,
    pub attempts: u32,
    pub time_spent: u64, // seconds
    pub hints_used: u32,
}

impl Progress {
    pub fn new() -> Self {
        let now = Utc::now();
        Self {
            completed_exercises: HashSet::new(),
            exercise_times: HashMap::new(),
            started_at: now,
            last_updated: now,
            total_time_spent: 0,
        }
    }

    pub fn load_or_default(path: &PathBuf) -> Result<Self> {
        if path.exists() {
            let content = std::fs::read_to_string(path)?;
            let progress: Progress = serde_json::from_str(&content)?;
            Ok(progress)
        } else {
            Ok(Self::new())
        }
    }

    pub fn save(&self, path: &PathBuf) -> Result<()> {
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        let content = serde_json::to_string_pretty(self)?;
        std::fs::write(path, content)?;
        Ok(())
    }

    pub fn is_completed(&self, exercise_name: &str) -> bool {
        self.completed_exercises.contains(exercise_name)
    }

    pub fn mark_completed(&mut self, exercise_name: &str) {
        self.completed_exercises.insert(exercise_name.to_string());
        self.last_updated = Utc::now();
        
        // Update exercise progress
        let exercise_progress = self.exercise_times
            .entry(exercise_name.to_string())
            .or_insert_with(|| ExerciseProgress {
                started_at: Utc::now(),
                completed_at: None,
                attempts: 0,
                time_spent: 0,
                hints_used: 0,
            });
        
        if exercise_progress.completed_at.is_none() {
            exercise_progress.completed_at = Some(Utc::now());
            exercise_progress.time_spent = (Utc::now() - exercise_progress.started_at)
                .num_seconds() as u64;
        }
    }

    pub fn mark_incomplete(&mut self, exercise_name: &str) {
        self.completed_exercises.remove(exercise_name);
        self.exercise_times.remove(exercise_name);
        self.last_updated = Utc::now();
    }

    pub fn start_exercise(&mut self, exercise_name: &str) {
        let exercise_progress = self.exercise_times
            .entry(exercise_name.to_string())
            .or_insert_with(|| ExerciseProgress {
                started_at: Utc::now(),
                completed_at: None,
                attempts: 0,
                time_spent: 0,
                hints_used: 0,
            });
        
        exercise_progress.attempts += 1;
        self.last_updated = Utc::now();
    }

    pub fn use_hint(&mut self, exercise_name: &str) {
        let exercise_progress = self.exercise_times
            .entry(exercise_name.to_string())
            .or_insert_with(|| ExerciseProgress {
                started_at: Utc::now(),
                completed_at: None,
                attempts: 0,
                time_spent: 0,
                hints_used: 0,
            });
        
        exercise_progress.hints_used += 1;
        self.last_updated = Utc::now();
    }

    pub fn get_completion_percentage(&self, total_exercises: usize) -> f64 {
        if total_exercises == 0 {
            return 100.0;
        }
        (self.completed_exercises.len() as f64 / total_exercises as f64) * 100.0
    }

    pub fn get_exercise_progress(&self, exercise_name: &str) -> Option<&ExerciseProgress> {
        self.exercise_times.get(exercise_name)
    }

    pub fn get_total_time_spent(&self) -> u64 {
        self.exercise_times
            .values()
            .map(|ep| ep.time_spent)
            .sum()
    }

    pub fn get_completed_count(&self) -> usize {
        self.completed_exercises.len()
    }

    pub fn get_average_completion_time(&self) -> Option<u64> {
        let completed_times: Vec<u64> = self.exercise_times
            .values()
            .filter(|ep| ep.completed_at.is_some())
            .map(|ep| ep.time_spent)
            .collect();
        
        if completed_times.is_empty() {
            None
        } else {
            Some(completed_times.iter().sum::<u64>() / completed_times.len() as u64)
        }
    }

    pub fn get_streak(&self) -> u32 {
        // Calculate current streak of consecutive days with completed exercises
        let mut streak = 0;
        let now = Utc::now();
        
        // Get completion dates sorted by date
        let mut completion_dates: Vec<DateTime<Utc>> = self.exercise_times
            .values()
            .filter_map(|ep| ep.completed_at)
            .collect();
        
        completion_dates.sort_by(|a, b| b.cmp(a)); // Sort descending (newest first)
        
        let mut current_date = now.date_naive();
        
        for completion_time in completion_dates {
            let completion_date = completion_time.date_naive();
            
            if completion_date == current_date {
                streak += 1;
                current_date = current_date.pred_opt().unwrap_or(current_date);
            } else if completion_date == current_date.pred_opt().unwrap_or(current_date) {
                streak += 1;
                current_date = completion_date.pred_opt().unwrap_or(completion_date);
            } else {
                break;
            }
        }
        
        streak
    }

    pub fn get_stats_summary(&self) -> ProgressStats {
        let total_time = self.get_total_time_spent();
        let completed_count = self.get_completed_count();
        let average_time = self.get_average_completion_time();
        let streak = self.get_streak();
        
        let total_attempts: u32 = self.exercise_times
            .values()
            .map(|ep| ep.attempts)
            .sum();
        
        let total_hints: u32 = self.exercise_times
            .values()
            .map(|ep| ep.hints_used)
            .sum();

        ProgressStats {
            completed_exercises: completed_count,
            total_time_spent: total_time,
            average_completion_time: average_time,
            current_streak: streak,
            total_attempts,
            total_hints_used: total_hints,
            started_at: self.started_at,
            last_activity: self.last_updated,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ProgressStats {
    pub completed_exercises: usize,
    pub total_time_spent: u64,
    pub average_completion_time: Option<u64>,
    pub current_streak: u32,
    pub total_attempts: u32,
    pub total_hints_used: u32,
    pub started_at: DateTime<Utc>,
    pub last_activity: DateTime<Utc>,
}

impl Default for Progress {
    fn default() -> Self {
        Self::new()
    }
}