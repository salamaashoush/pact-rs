//! Check command for native shadowing

use anyhow::{Context, Result};
use colored::*;
// Note: Builtins are now in pact-cek
use std::collections::HashSet;
use std::fs;
use std::path::Path;

/// Check a Pact file for native shadowing
pub fn check_shadowing(file: &Path) -> Result<()> {
    println!("{} {}", "Checking:".bright_blue(), file.display());

    // Read file content
    let content = fs::read_to_string(file)
        .with_context(|| format!("Failed to read file: {:?}", file))?;

    // Get all built-in names - hardcoded for now since BuiltinRegistry doesn't expose them
    let builtin_names: HashSet<&str> = [
        // Math
        "+", "-", "*", "/", "mod", "abs", "exp", "sqrt", "ceiling", "floor", "round",
        // Comparison
        "=", "!=", "<", ">", "<=", ">=",
        // Logic
        "and", "or", "not", "if",
        // Lists
        "map", "filter", "fold", "length", "reverse", "sort", "take", "drop",
        "make-list", "enumerate", "distinct", "concat", "zip", "contains", "at",
        // Strings
        "str-to-int", "int-to-str", "str-length", "str-take", "str-drop",
        "str-concat", "str-contains", "str-split", "str-join", "format",
        // Database
        "create-table", "read", "write", "update", "insert", "with-read",
        "with-default-read", "select", "keys", "fold-db", "keylog", "txlog",
        "txids", "describe-table",
        // Capabilities
        "with-capability", "require-capability", "compose-capability",
        "install-capability", "emit-event",
        // Cryptography
        "hash", "keccak256", "validate-principal", "validate-keypair", "verify-sig",
        // Time
        "time", "add-time", "diff-time", "format-time", "parse-time",
        "hours", "minutes", "seconds", "days",
        // Type
        "typeof", "typeof-in-repl", "str-to-decimal", "decimal-to-str",
        // Module
        "module", "interface", "use", "implements", "defun", "defconst",
        "defschema", "defpact", "defcap",
        // Bindings
        "let", "let*", "bind", "lambda",
        // General
        "identity", "constantly", "compose", "try", "enforce", "enforce-one",
        "enforce-guard", "enforce-keyset", "yield", "resume", "pact-id", "chain-data",
    ].iter().cloned().collect();

    // Simple text-based checking for now
    let mut shadowed = Vec::new();
    check_text_for_shadowing(&content, &builtin_names, &mut shadowed);

    // Report results
    if shadowed.is_empty() {
        println!("{}", "✓ No native shadowing detected".bright_green());
        Ok(())
    } else {
        println!("{}", "✗ Native shadowing detected:".bright_red());
        for (name, line) in &shadowed {
            println!(
                "  {} {} shadows built-in function",
                format!("Line {}: '{}'", line, name).bright_yellow(),
                name.bright_cyan()
            );
        }
        println!();
        println!(
            "{}: {} shadowed built-ins found",
            "Total".bright_red(),
            shadowed.len()
        );
        std::process::exit(1)
    }
}

/// Text-based checking for shadowing
fn check_text_for_shadowing(
    content: &str,
    builtins: &HashSet<&str>,
    shadowed: &mut Vec<(String, usize)>,
) {
    // Simple regex-based patterns for common definitions
    let patterns = vec![
        (r"\(defun\s+(\S+)", "defun"),
        (r"\(defconst\s+(\S+)", "defconst"),
        (r"\(defcap\s+(\S+)", "defcap"),
        (r"\(defpact\s+(\S+)", "defpact"),
        (r"\(let\s+\(\s*\((\S+)", "let binding"),
        (r"\(let\*\s+\(\s*\((\S+)", "let* binding"),
        (r"\(lambda\s+\(([^)]+)\)", "lambda params"),
    ];

    for (line_num, line) in content.lines().enumerate() {
        for (pattern, def_type) in &patterns {
            let re = regex::Regex::new(pattern).unwrap();
            for cap in re.captures_iter(line) {
                if let Some(name_match) = cap.get(1) {
                    let name = name_match.as_str();

                    // For lambda params, split by whitespace
                    if def_type == &"lambda params" {
                        for param in name.split_whitespace() {
                            if builtins.contains(param) {
                                shadowed.push((param.to_string(), line_num + 1));
                            }
                        }
                    } else if builtins.contains(name) {
                        shadowed.push((name.to_string(), line_num + 1));
                    }
                }
            }
        }
    }
}
