use anyhow::{anyhow, Result};
use colored::*;
use console::Term;
use std::process::{Command, Stdio};
use tempfile::NamedTempFile;
use tokio::process::Command as AsyncCommand;

use crate::config::Config;
use crate::exercise::Exercise;

pub struct Runner {
    config: Config,
    term: Term,
}

impl Runner {
    pub fn new(config: &Config) -> Self {
        Self {
            config: config.clone(),
            term: Term::stdout(),
        }
    }

    pub async fn run_exercise(&self, exercise: &Exercise) -> Result<()> {
        if !exercise.exists() {
            return Err(anyhow!("Exercise file not found: {}", exercise.path.display()));
        }

        self.term.write_line(&format!(
            "{} Exercise: {} {}",
            exercise.difficulty_emoji(),
            exercise.name.bold(),
            format!("[{}]", exercise.topic).dimmed()
        ))?;
        
        self.term.write_line(&format!("📝 {}", exercise.description))?;
        self.term.write_line("")?;

        // Show the exercise file content
        let content = exercise.read_content()?;
        self.show_exercise_content(&content)?;

        // Show info if available
        if let Ok(Some(info)) = exercise.read_info() {
            self.term.write_line(&format!("{} Additional Information:", "ℹ️".blue()))?;
            self.term.write_line("")?;
            self.show_info_content(&info)?;
        }

        // Try to run the exercise
        self.term.write_line(&format!("{} Running exercise...", "🚀".green()))?;
        let result = self.execute_pact_file(&exercise.path).await?;
        
        if result.success {
            self.term.write_line(&format!("{} Exercise ran successfully!", "✅".green()))?;
            if !result.output.trim().is_empty() {
                self.term.write_line(&format!("{} Output:", "📄".blue()))?;
                self.term.write_line(&result.output)?;
            }
        } else {
            self.term.write_line(&format!("{} Exercise failed to run:", "❌".red()))?;
            self.term.write_line(&result.error)?;
            
            self.term.write_line("")?;
            self.term.write_line(&format!("{} Hint: {}", "💡".yellow(), exercise.hint))?;
        }

        self.term.write_line("")?;
        self.term.write_line(&format!(
            "{} Edit the file: {}",
            "✏️".blue(),
            exercise.path.display().to_string().bold()
        ))?;
        self.term.write_line(&format!(
            "{} Run `pactlings verify {}` when ready",
            "🔍".blue(),
            exercise.name
        ))?;

        Ok(())
    }

    fn show_exercise_content(&self, content: &str) -> Result<()> {
        self.term.write_line(&format!("{} Exercise Code:", "💻".blue()))?;
        self.term.write_line("")?;
        
        // Show code with syntax highlighting (simple)
        for (i, line) in content.lines().enumerate() {
            let line_num = format!("{:3}", i + 1).dimmed();
            let colored_line = self.syntax_highlight(line);
            self.term.write_line(&format!("{} │ {}", line_num, colored_line))?;
        }
        
        self.term.write_line("")?;
        Ok(())
    }

    fn show_info_content(&self, info: &str) -> Result<()> {
        // Parse and display markdown-like content
        for line in info.lines() {
            if line.starts_with("# ") {
                self.term.write_line(&line[2..].bold().to_string())?;
            } else if line.starts_with("## ") {
                self.term.write_line(&line[3..].blue().bold().to_string())?;
            } else if line.starts_with("### ") {
                self.term.write_line(&line[4..].cyan().to_string())?;
            } else if line.starts_with("- ") {
                self.term.write_line(&format!("  • {}", &line[2..]))?;
            } else if line.starts_with("```") {
                // Skip code fence markers
                continue;
            } else {
                self.term.write_line(line)?;
            }
        }
        self.term.write_line("")?;
        Ok(())
    }

    fn syntax_highlight(&self, line: &str) -> String {
        let trimmed = line.trim_start();
        
        if trimmed.starts_with(";;") {
            line.dimmed().to_string()
        } else if trimmed.starts_with("(def") {
            line.green().to_string()
        } else if trimmed.starts_with("(module") {
            line.blue().bold().to_string()
        } else if trimmed.starts_with("(with-capability") {
            line.yellow().to_string()
        } else if trimmed.contains("TODO") || trimmed.contains("YOUR_CODE_HERE") {
            line.red().bold().to_string()
        } else {
            line.to_string()
        }
    }

    async fn execute_pact_file(&self, file_path: &std::path::Path) -> Result<ExecutionResult> {
        // Try to execute the Pact file
        let mut cmd = AsyncCommand::new(&self.config.pact_executable);
        cmd.arg(file_path)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        let output = cmd.output().await?;
        
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        
        Ok(ExecutionResult {
            success: output.status.success(),
            output: stdout.to_string(),
            error: stderr.to_string(),
        })
    }

    pub async fn run_tests(&self, exercise: &Exercise) -> Result<TestResult> {
        let mut all_passed = true;
        let mut test_outputs = Vec::new();

        for test_command in &exercise.test_commands {
            let result = self.run_test_command(test_command, &exercise.path).await?;
            all_passed &= result.success;
            test_outputs.push(result);
        }

        Ok(TestResult {
            success: all_passed,
            test_results: test_outputs,
        })
    }

    async fn run_test_command(&self, command: &str, file_path: &std::path::Path) -> Result<ExecutionResult> {
        // Parse the test command and execute it
        let parts: Vec<&str> = command.split_whitespace().collect();
        if parts.is_empty() {
            return Err(anyhow!("Empty test command"));
        }

        let mut cmd = AsyncCommand::new(parts[0]);
        for part in &parts[1..] {
            if *part == "%FILE%" {
                cmd.arg(file_path);
            } else {
                cmd.arg(part);
            }
        }

        cmd.stdout(Stdio::piped())
            .stderr(Stdio::piped());

        let output = cmd.output().await?;
        
        Ok(ExecutionResult {
            success: output.status.success(),
            output: String::from_utf8_lossy(&output.stdout).to_string(),
            error: String::from_utf8_lossy(&output.stderr).to_string(),
        })
    }

    pub async fn format_code(&self, exercise: &Exercise) -> Result<String> {
        // Basic code formatting for Pact files
        let content = exercise.read_content()?;
        Ok(self.format_pact_code(&content))
    }

    fn format_pact_code(&self, code: &str) -> String {
        // Simple Pact code formatter
        let mut formatted = String::new();
        let mut indent_level = 0;
        let mut in_string = false;
        let mut escape_next = false;

        for line in code.lines() {
            let trimmed = line.trim();
            
            if trimmed.is_empty() {
                formatted.push('\n');
                continue;
            }

            if trimmed.starts_with(";;") {
                // Comment line
                formatted.push_str(&format!("{}{}\n", "  ".repeat(indent_level), trimmed));
                continue;
            }

            // Count parentheses for indentation
            let mut temp_indent = indent_level;
            for ch in trimmed.chars() {
                match ch {
                    '"' if !escape_next => in_string = !in_string,
                    '\\' if in_string => escape_next = !escape_next,
                    '(' if !in_string => temp_indent += 1,
                    ')' if !in_string => {
                        if temp_indent > 0 {
                            temp_indent -= 1;
                        }
                    },
                    _ => escape_next = false,
                }
            }

            // Adjust indent for closing parentheses
            let line_indent = if trimmed.starts_with(')') {
                indent_level.saturating_sub(1)
            } else {
                indent_level
            };

            formatted.push_str(&format!("{}{}\n", "  ".repeat(line_indent), trimmed));
            indent_level = temp_indent;
        }

        formatted
    }
}

#[derive(Debug, Clone)]
pub struct ExecutionResult {
    pub success: bool,
    pub output: String,
    pub error: String,
}

#[derive(Debug)]
pub struct TestResult {
    pub success: bool,
    pub test_results: Vec<ExecutionResult>,
}