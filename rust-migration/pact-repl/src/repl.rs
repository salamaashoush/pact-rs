//! REPL implementation

use colored::*;
use pact_errors::PactErrorI;
use pact_values::PactValue;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result as RustylineResult};
use std::fs;
use pact_compiler::{compile_and_evaluate, CompilationContext};
use pact_ir::TopLevel;
use crate::error_display::display_error;

/// The Pact REPL
pub struct Repl {
    ctx: CompilationContext,
    editor: DefaultEditor,
    history_file: String,
}

impl Repl {
    /// Create a new REPL instance
    pub fn new() -> RustylineResult<Self> {
        let mut editor = DefaultEditor::new()?;
        let history_file = ".pact_history".to_string();

        // Load history if it exists
        let _ = editor.load_history(&history_file);

        let ctx = CompilationContext::new();

        Ok(Repl {
            ctx,
            editor,
            history_file,
        })
    }

    /// Run the REPL
    pub fn run(&mut self) -> RustylineResult<()> {
        self.print_welcome();

        let prompt = format!("{} ", "pact>".bright_blue());

        loop {
            match self.editor.readline(&prompt) {
                Ok(line) => {
                    if line.trim().is_empty() {
                        continue;
                    }

                    // Add to history
                    self.editor.add_history_entry(&line)?;

                    // Check for special commands
                    match line.trim() {
                        ".exit" | ".quit" => {
                            println!("{}", "Goodbye!".bright_yellow());
                            break;
                        }
                        ".help" => {
                            self.print_help();
                            continue;
                        }
                        ".reset" => {
                            self.ctx = CompilationContext::new();
                            println!("{}", "Environment reset.".bright_green());
                            continue;
                        }
                        cmd if cmd.starts_with(".load") => {
                            self.handle_load_command(cmd);
                            continue;
                        }
                        _ => {}
                    }

                    // Evaluate the expression
                    self.evaluate_line(&line);
                }
                Err(ReadlineError::Interrupted) => {
                    println!("\n{}", "Use .exit to quit.".bright_yellow());
                }
                Err(ReadlineError::Eof) => {
                    println!("{}", "Goodbye!".bright_yellow());
                    break;
                }
                Err(err) => {
                    eprintln!("{}: {:?}", "Error".bright_red(), err);
                    break;
                }
            }
        }

        // Save history
        self.editor.save_history(&self.history_file)?;
        Ok(())
    }

    /// Print welcome message
    fn print_welcome(&self) {
        println!(
            "{}",
            r#"
 ____   _    ____ _____   ____  _____ ____  _     
|  _ \ / \  / ___|_   _| |  _ \| ____|  _ \| |    
| |_) / _ \| |     | |   | |_) |  _| | |_) | |    
|  __/ ___ \ |___  | |   |  _ <| |___|  __/| |___ 
|_| /_/   \_\____| |_|   |_| \_\_____|_|   |_____|

Welcome to Pact REPL!
Type .help for help, .exit to quit.
        "#
            .bright_green()
        );
    }

    /// Print help message
    fn print_help(&self) {
        println!(
            "{}",
            "
Available commands:
  .help    Show this help message
  .exit    Exit the REPL
  .quit    Same as .exit  
  .reset   Reset the environment
  .load <file>  Load and evaluate a file

Examples:
  (+ 1 2)
  (defun add (x y) (+ x y))
  (add 5 3)
  [1 2 3]
  { \"name\": \"alice\", \"balance\": 100 }
        "
            .bright_cyan()
        );
    }

    /// Handle .load command
    fn handle_load_command(&mut self, cmd: &str) {
        let parts: Vec<&str> = cmd.split_whitespace().collect();
        if parts.len() != 2 {
            eprintln!("{}: .load <filename>", "Usage".bright_red());
            return;
        }

        let filename = parts[1];
        match fs::read_to_string(filename) {
            Ok(content) => {
                println!("{} {}", "Loading".bright_yellow(), filename);
                self.evaluate_line(&content);
            }
            Err(err) => {
                eprintln!("{}: {}", "Failed to read file".bright_red(), err);
            }
        }
    }

    /// Evaluate a line of input
    fn evaluate_line(&mut self, line: &str) {
        // Try to compile and evaluate
        match compile_and_evaluate(line) {
            Ok((_, value)) => {
                // Display the result
                self.print_result(&value);
            }
            Err(err) => {
                self.print_error(&err, line);
            }
        }
    }

    /// Print an error with enhanced formatting
    fn print_error(&self, err: &PactErrorI, source: &str) {
        eprintln!("{}", display_error(err, source));
    }
    
    /// Print a result value
    fn print_result(&self, value: &PactValue) {
        // Format the value nicely
        match value {
            PactValue::String(s) if s.starts_with("Module loaded:") || 
                                    s.starts_with("Interface loaded:") ||
                                    s.starts_with("Import processed") => {
                // These are status messages, print them as-is
                println!("{}", s.bright_green());
            }
            _ => {
                // Regular values
                println!("{} {}", "→".bright_cyan(), format_pact_value(value));
            }
        }
    }
}

impl Default for Repl {
    fn default() -> Self {
        Self::new().expect("Failed to create REPL")
    }
}

/// Format a PactValue for display
fn format_pact_value(value: &PactValue) -> String {
    match value {
        PactValue::String(s) => format!("{}", format!("\"{}\"", s).bright_green()),
        PactValue::Integer(n) => format!("{}", n.to_string().bright_yellow()),
        PactValue::Decimal(d) => format!("{}", d.to_string().bright_yellow()),
        PactValue::Bool(b) => format!("{}", b.to_string().bright_magenta()),
        PactValue::Unit => format!("{}", "()".bright_blue()),
        PactValue::List(items) => {
            let formatted_items: Vec<String> = items.iter()
                .map(|v| format_pact_value(v))
                .collect();
            format!("[{}]", formatted_items.join(", "))
        }
        PactValue::Object(obj) => {
            let formatted_fields: Vec<String> = obj.clone().into_iter()
                .map(|(k, v)| format!("{}: {}", 
                    format!("\"{}\"", k).bright_cyan(), 
                    format_pact_value(&v)))
                .collect();
            format!("{{ {} }}", formatted_fields.join(", "))
        }
        _ => format!("{:?}", value),
    }
}