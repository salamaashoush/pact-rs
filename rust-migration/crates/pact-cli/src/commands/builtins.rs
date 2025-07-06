//! Built-in functions listing command

use anyhow::Result;
use colored::*;
// Note: Builtins are now in pact-cek
use serde_json;
use serde_yaml;

/// List all built-in functions
pub fn execute(output_format: String) -> Result<()> {
    // For now, use the hardcoded list since builtins are in pact-cek
    let builtins = get_builtin_list();

    match output_format.as_str() {
        "json" => output_json_list(&builtins),
        "yaml" => output_yaml_list(&builtins),
        _ => output_text_list(&builtins),
    }
}

// Helper function to get builtin list
fn get_builtin_list() -> Vec<(&'static str, &'static str, &'static str)> {
    vec![
        // Math
        ("+", "Addition", "Math"),
        ("-", "Subtraction", "Math"),
        ("*", "Multiplication", "Math"),
        ("/", "Division", "Math"),
        ("mod", "Modulo", "Math"),
        ("abs", "Absolute value", "Math"),
        ("exp", "Exponentiation", "Math"),
        ("sqrt", "Square root", "Math"),
        ("ceiling", "Round up", "Math"),
        ("floor", "Round down", "Math"),
        ("round", "Round to nearest", "Math"),
        // Comparison
        ("=", "Equality", "Comparison"),
        ("!=", "Inequality", "Comparison"),
        ("<", "Less than", "Comparison"),
        (">", "Greater than", "Comparison"),
        ("<=", "Less than or equal", "Comparison"),
        (">=", "Greater than or equal", "Comparison"),
        // Logic
        ("and", "Logical AND", "Logic"),
        ("or", "Logical OR", "Logic"),
        ("not", "Logical NOT", "Logic"),
        ("if", "Conditional", "Logic"),
        // Lists
        ("map", "Map function over list", "Lists"),
        ("filter", "Filter list by predicate", "Lists"),
        ("fold", "Fold/reduce list", "Lists"),
        ("length", "Get list length", "Lists"),
        ("reverse", "Reverse a list", "Lists"),
        ("sort", "Sort a list", "Lists"),
        ("take", "Take first n elements", "Lists"),
        ("drop", "Drop first n elements", "Lists"),
        ("make-list", "Create a list", "Lists"),
        ("enumerate", "Add indices to list", "Lists"),
        ("distinct", "Remove duplicates", "Lists"),
        ("concat", "Concatenate lists", "Lists"),
        ("zip", "Zip two lists", "Lists"),
        ("contains", "Check if list contains element", "Lists"),
        ("at", "Get element at index", "Lists"),
        // Strings
        ("str-to-int", "Parse string to integer", "Strings"),
        ("int-to-str", "Convert integer to string", "Strings"),
        ("str-length", "Get string length", "Strings"),
        ("str-take", "Take substring", "Strings"),
        ("str-drop", "Drop characters from string", "Strings"),
        ("str-concat", "Concatenate strings", "Strings"),
        ("str-contains", "Check if string contains substring", "Strings"),
        ("str-split", "Split string", "Strings"),
        ("str-join", "Join strings with separator", "Strings"),
        ("format", "Format string", "Strings"),
        // Database
        ("create-table", "Create a new table", "Database"),
        ("read", "Read row from table", "Database"),
        ("write", "Write row to table", "Database"),
        ("update", "Update row in table", "Database"),
        ("insert", "Insert row into table", "Database"),
        ("with-read", "Read and bind row", "Database"),
        ("with-default-read", "Read with default", "Database"),
        ("select", "Select rows from table", "Database"),
        ("keys", "Get all keys from table", "Database"),
        ("fold-db", "Fold over database rows", "Database"),
        ("keylog", "Get key transaction log", "Database"),
        ("txlog", "Get transaction log", "Database"),
        ("txids", "Get transaction IDs", "Database"),
        ("describe-table", "Get table schema", "Database"),
        // Capabilities
        ("with-capability", "Acquire capability", "Capabilities"),
        ("require-capability", "Require capability", "Capabilities"),
        ("compose-capability", "Compose capabilities", "Capabilities"),
        ("install-capability", "Install capability", "Capabilities"),
        ("emit-event", "Emit an event", "Capabilities"),
        // Cryptography
        ("hash", "Blake2b hash", "Cryptography"),
        ("keccak256", "Keccak256 hash", "Cryptography"),
        ("validate-principal", "Validate principal", "Cryptography"),
        ("validate-keypair", "Validate keypair", "Cryptography"),
        ("verify-sig", "Verify signature", "Cryptography"),
        // Time
        ("time", "Get current time", "Time"),
        ("add-time", "Add to time", "Time"),
        ("diff-time", "Difference between times", "Time"),
        ("format-time", "Format time as string", "Time"),
        ("parse-time", "Parse time from string", "Time"),
        ("hours", "Create hours duration", "Time"),
        ("minutes", "Create minutes duration", "Time"),
        ("seconds", "Create seconds duration", "Time"),
        ("days", "Create days duration", "Time"),
        // Type
        ("typeof", "Get type of value", "Type"),
        ("typeof-in-repl", "Get type for REPL", "Type"),
        ("str-to-decimal", "Parse string to decimal", "Type"),
        ("decimal-to-str", "Convert decimal to string", "Type"),
        // Module
        ("module", "Define module", "Modules"),
        ("interface", "Define interface", "Modules"),
        ("use", "Use module", "Modules"),
        ("implements", "Implement interface", "Modules"),
        ("defun", "Define function", "Modules"),
        ("defconst", "Define constant", "Modules"),
        ("defschema", "Define schema", "Modules"),
        ("defpact", "Define pact", "Modules"),
        ("defcap", "Define capability", "Modules"),
        // Bindings
        ("let", "Local binding", "Bindings"),
        ("let*", "Sequential local bindings", "Bindings"),
        ("bind", "Bind object fields", "Bindings"),
        ("lambda", "Anonymous function", "Bindings"),
        // General
        ("identity", "Identity function", "General"),
        ("constantly", "Constant function", "General"),
        ("compose", "Function composition", "General"),
        ("try", "Try-catch", "General"),
        ("enforce", "Enforce condition", "General"),
        ("enforce-one", "Enforce one of conditions", "General"),
        ("enforce-guard", "Enforce guard", "General"),
        ("enforce-keyset", "Enforce keyset", "General"),
        ("yield", "Yield value", "General"),
        ("resume", "Resume with binding", "General"),
        ("pact-id", "Get pact ID", "General"),
        ("chain-data", "Get chain data", "General"),
    ]
}

fn output_text_list(builtins: &Vec<(&'static str, &'static str, &'static str)>) -> Result<()> {
    println!("{}", "Pact Built-in Functions".bright_blue().bold());
    println!("{}", "======================".bright_blue());
    println!();

    // Group builtins by category
    let mut categories: std::collections::HashMap<&str, Vec<(&str, &str)>> =
        std::collections::HashMap::new();

    for (name, desc, category) in builtins {
        categories.entry(category)
            .or_insert_with(Vec::new)
            .push((name, desc));
    }

    // Sort categories and print
    let mut sorted_categories: Vec<_> = categories.iter().collect();
    sorted_categories.sort_by_key(|(cat, _)| *cat);

    for (category, functions) in sorted_categories {
        println!("{}", format!("{}:", category).bright_green());

        // Sort functions within category
        let mut sorted_functions = functions.clone();
        sorted_functions.sort_by_key(|(name, _)| *name);

        for (name, description) in sorted_functions {
            println!("  {} - {}", name.bright_cyan(), description.dimmed());
        }
        println!();
    }

    // Count total builtins
    let total: usize = categories.values().map(|v| v.len()).sum();
    println!("{}: {}", "Total built-ins".bright_yellow(), total);

    Ok(())
}

fn output_json_list(builtins: &Vec<(&'static str, &'static str, &'static str)>) -> Result<()> {
    let mut json_map = serde_json::Map::new();

    for (name, desc, category) in builtins {
        let mut func_info = serde_json::Map::new();
        func_info.insert("description".to_string(), serde_json::Value::String(desc.to_string()));
        func_info.insert("category".to_string(), serde_json::Value::String(category.to_string()));

        json_map.insert(name.to_string(), serde_json::Value::Object(func_info));
    }

    let output = serde_json::to_string_pretty(&json_map)?;
    println!("{}", output);

    Ok(())
}

fn output_yaml_list(builtins: &Vec<(&'static str, &'static str, &'static str)>) -> Result<()> {
    let mut yaml_map = serde_yaml::Mapping::new();

    for (name, desc, category) in builtins {
        let mut func_info = serde_yaml::Mapping::new();
        func_info.insert(
            serde_yaml::Value::String("description".to_string()),
            serde_yaml::Value::String(desc.to_string())
        );
        func_info.insert(
            serde_yaml::Value::String("category".to_string()),
            serde_yaml::Value::String(category.to_string())
        );

        yaml_map.insert(
            serde_yaml::Value::String(name.to_string()),
            serde_yaml::Value::Mapping(func_info)
        );
    }

    let output = serde_yaml::to_string(&yaml_map)?;
    println!("{}", output);

    Ok(())
}

