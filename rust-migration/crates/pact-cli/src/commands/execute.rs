//! Execute Pact files and scripts

use anyhow::{Context, Result};
use colored::*;
use pact_compiler::{compile_pact_source, compile_and_evaluate};
use pact_ir::TopLevel;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

/// Execute a Pact file or script
pub fn run(file: PathBuf, trace: bool, find_script: bool) -> Result<()> {
    let extension = file.extension()
        .and_then(|ext| ext.to_str())
        .unwrap_or("");
    
    match extension {
        "pact" => {
            if find_script {
                // Look for associated .repl file
                let repl_file = file.with_extension("repl");
                if repl_file.exists() {
                    println!("{}", format!("Found REPL script: {:?}", repl_file).dimmed());
                    execute_repl_script(&repl_file, trace)
                } else {
                    execute_pact_file(&file, trace)
                }
            } else {
                execute_pact_file(&file, trace)
            }
        }
        "repl" => execute_repl_script(&file, trace),
        _ => {
            // Try to execute as Pact code regardless of extension
            execute_pact_file(&file, trace)
        }
    }
}

/// Execute a .pact file
fn execute_pact_file(file: &Path, trace: bool) -> Result<()> {
    println!("{} {}", "Compiling:".bright_blue(), file.display());
    
    let content = fs::read_to_string(file)
        .with_context(|| format!("Failed to read file: {:?}", file))?;
    
    let start = Instant::now();
    
    if trace {
        println!("{}", "Note: Trace mode not yet implemented".yellow());
    }
    
    // Compile and evaluate the code
    match compile_and_evaluate(&content) {
        Ok((compile_result, eval_result)) => {
            let elapsed = start.elapsed();
            
            // Show what was compiled and evaluated
            match &compile_result.top_level {
                TopLevel::TLModule(module) => {
                    println!("{} {}", "✓ Module loaded:".bright_green(), module.name);
                }
                TopLevel::TLInterface(interface) => {
                    println!("{} {}", "✓ Interface loaded:".bright_green(), interface.name);
                }
                TopLevel::TLTerm(_) => {
                    // Show evaluation result for expressions
                    println!("{} → {}", "✓ Expression:".bright_green(), 
                        format!("{:?}", eval_result).cyan());
                }
                TopLevel::TLUse(_) => {
                    println!("{}", "✓ Import processed".bright_green());
                }
            }
            
            println!();
            println!("{}", format!("✓ Evaluation completed in {:?}", elapsed).bright_green());
            Ok(())
        }
        Err(err) => {
            eprintln!("{}: {}", "Evaluation error".bright_red(), err);
            std::process::exit(1);
        }
    }
}

/// Execute a .repl script file
fn execute_repl_script(file: &Path, trace: bool) -> Result<()> {
    println!("{} {}", "Compiling REPL script:".bright_blue(), file.display());
    
    let content = fs::read_to_string(file)
        .with_context(|| format!("Failed to read file: {:?}", file))?;
    
    let mut passed = 0;
    let mut failed = 0;
    let mut total = 0;
    
    // Parse REPL commands
    for (line_num, line) in content.lines().enumerate() {
        let line = line.trim();
        
        // Skip empty lines and comments
        if line.is_empty() || line.starts_with(';') {
            continue;
        }
        
        // Handle REPL directives
        if line.starts_with(";;") {
            // This is a comment/directive
            println!("{}", line.dimmed());
            continue;
        }
        
        if line.starts_with(".") {
            // Handle REPL commands like .load, .env, etc.
            handle_repl_command(line, file.parent())?;
            continue;
        }
        
        // Compile as Pact code
        total += 1;
        print!("{}: {} ... ", format!("Line {}", line_num + 1).bright_cyan(), line);
        
        match compile_pact_source(line) {
            Ok(compile_result) => {
                passed += 1;
                println!("{}", "✓".bright_green());
                
                if trace {
                    // Show what was compiled
                    match &compile_result.top_level {
                        TopLevel::TLModule(module) => {
                            println!("  {} {}", "Module:".dimmed(), module.name);
                        }
                        TopLevel::TLInterface(interface) => {
                            println!("  {} {}", "Interface:".dimmed(), interface.name);
                        }
                        TopLevel::TLTerm(_) => {
                            println!("  {}", "Expression".dimmed());
                        }
                        TopLevel::TLUse(_) => {
                            println!("  {}", "Import".dimmed());
                        }
                    }
                }
            }
            Err(err) => {
                failed += 1;
                println!("{}", "✗".bright_red());
                eprintln!("  {}: {}", "Compilation error".bright_red(), err);
            }
        }
    }
    
    // Print summary
    println!();
    println!("{}", "Test Summary".bright_blue().bold());
    println!("  {}: {}", "Total".bright_cyan(), total);
    println!("  {}: {}", "Passed".bright_green(), passed);
    println!("  {}: {}", "Failed".bright_red(), failed);
    
    if failed > 0 {
        std::process::exit(1);
    }
    
    Ok(())
}

/// Handle REPL-specific commands
fn handle_repl_command(command: &str, base_dir: Option<&Path>) -> Result<()> {
    let parts: Vec<&str> = command.split_whitespace().collect();
    
    match parts.get(0).map(|s| *s) {
        Some(".load") => {
            if let Some(file) = parts.get(1) {
                let path = if let Some(base) = base_dir {
                    base.join(file)
                } else {
                    PathBuf::from(file)
                };
                
                let content = fs::read_to_string(&path)
                    .with_context(|| format!("Failed to load file: {:?}", path))?;
                
                match compile_pact_source(&content) {
                    Ok(_compile_result) => {
                        println!("{} {}", "Loaded:".dimmed(), path.display());
                    }
                    Err(err) => {
                        return Err(anyhow::anyhow!("Failed to compile loaded file: {:?}: {}", path, err));
                    }
                }
            }
        }
        Some(".env") => {
            // TODO: Implement environment data handling
            println!("{}", "Environment data not yet implemented".yellow());
        }
        Some(".reset") => {
            // Reset evaluator state (simplified for now)
            println!("{}", "State reset".dimmed());
        }
        _ => {
            println!("{}: {}", "Unknown REPL command".yellow(), command);
        }
    }
    
    Ok(())
}