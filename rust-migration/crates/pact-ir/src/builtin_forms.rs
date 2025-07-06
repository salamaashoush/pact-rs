//! Builtin form implementations and arity resolution
//!
//! This module handles the resolution of overloaded builtin functions
//! based on argument count, matching the Haskell implementation.

use crate::term::CoreBuiltin;

/// Arity-based builtin resolution
/// This matches the Haskell desugarCoreBuiltinArity function
pub fn resolve_builtin_arity(name: &str, arity: usize) -> Option<CoreBuiltin> {
    match (name, arity) {
        // Arithmetic operations
        ("+", 2) => Some(CoreBuiltin::CoreAdd),
        ("-", 1) => Some(CoreBuiltin::CoreNegate),
        ("-", 2) => Some(CoreBuiltin::CoreSub),
        ("*", 2) => Some(CoreBuiltin::CoreMultiply),
        ("/", 2) => Some(CoreBuiltin::CoreDivide),
        ("mod", 2) => Some(CoreBuiltin::CoreMod),
        ("^", 2) => Some(CoreBuiltin::CorePow),
        ("abs", 1) => Some(CoreBuiltin::CoreAbs),
        ("sqrt", 1) => Some(CoreBuiltin::CoreSqrt),
        ("ln", 1) => Some(CoreBuiltin::CoreLn),
        ("exp", 1) => Some(CoreBuiltin::CoreExp),
        ("round", 1) => Some(CoreBuiltin::CoreRound),
        ("ceiling", 1) => Some(CoreBuiltin::CoreCeiling),
        ("floor", 1) => Some(CoreBuiltin::CoreFloor),

        // Comparison operations
        ("=", 2) => Some(CoreBuiltin::CoreEq),
        ("!=", 2) => Some(CoreBuiltin::CoreNeq),
        ("<", 2) => Some(CoreBuiltin::CoreLT),
        (">", 2) => Some(CoreBuiltin::CoreGT),
        ("<=", 2) => Some(CoreBuiltin::CoreLEQ),
        (">=", 2) => Some(CoreBuiltin::CoreGEQ),

        // String operations
        ("length", 1) => Some(CoreBuiltin::CoreLength),
        ("take", 2) => Some(CoreBuiltin::CoreTake),
        ("drop", 2) => Some(CoreBuiltin::CoreDrop),
        ("concat", 2) => Some(CoreBuiltin::CoreConcat),
        ("reverse", 1) => Some(CoreBuiltin::CoreReverse),
        ("contains", 2) => Some(CoreBuiltin::CoreContains),

        // List operations
        ("map", 2) => Some(CoreBuiltin::CoreMap),
        ("filter", 2) => Some(CoreBuiltin::CoreFilter),
        ("fold", 3) => Some(CoreBuiltin::CoreFold),
        ("zip", 2) => Some(CoreBuiltin::CoreZip),
        ("at", 2) => Some(CoreBuiltin::CoreAt),
        ("sort", 1) => Some(CoreBuiltin::CoreSort),

        // Database operations
        ("insert", 3) => Some(CoreBuiltin::CoreInsert),
        ("update", 3) => Some(CoreBuiltin::CoreUpdate),
        ("with-read", 4) => Some(CoreBuiltin::CoreWithRead),
        ("with-default-read", 5) => Some(CoreBuiltin::CoreWithDefaultRead),
        ("read", 2) => Some(CoreBuiltin::CoreRead),
        ("select", 2) => Some(CoreBuiltin::CoreSelect),
        ("select", 3) => Some(CoreBuiltin::CoreSelectWithFields),
        ("keys", 1) => Some(CoreBuiltin::CoreKeys),
        ("txlog", 2) => Some(CoreBuiltin::CoreTxLog),

        // Capability operations
        ("enforce-keyset", 1) => Some(CoreBuiltin::CoreEnforceKeyset),
        ("enforce-keyset", 2) => Some(CoreBuiltin::CoreEnforceKeysetName),
        ("keylog", 3) => Some(CoreBuiltin::CoreKeyLog),

        // Cryptographic operations
        ("hash", 1) => Some(CoreBuiltin::CoreHash),
        ("verify-signature", 3) => Some(CoreBuiltin::CoreVerifySignature),

        // Time operations
        ("parse-time", 2) => Some(CoreBuiltin::CoreParseTime),
        ("format-time", 2) => Some(CoreBuiltin::CoreFormatTime),
        ("time", 1) => Some(CoreBuiltin::CoreTime),
        ("add-time", 2) => Some(CoreBuiltin::CoreAddTime),
        ("diff-time", 2) => Some(CoreBuiltin::CoreDiffTime),
        ("days", 1) => Some(CoreBuiltin::CoreDays),
        ("hours", 1) => Some(CoreBuiltin::CoreHours),
        ("minutes", 1) => Some(CoreBuiltin::CoreMinutes),

        // Type operations
        ("typeof", 1) => Some(CoreBuiltin::CoreTypeOf),
        ("enforce-guard", 1) => Some(CoreBuiltin::CoreEnforceGuard),

        // Module operations
        ("describe-module", 1) => Some(CoreBuiltin::CoreDescribeModule),
        ("describe-keyset", 1) => Some(CoreBuiltin::CoreDescribeKeyset),

        // REPL operations (for testing)
        ("begin-tx", 0) => Some(CoreBuiltin::CoreReplOnlyBeginTx),
        ("commit-tx", 0) => Some(CoreBuiltin::CoreReplOnlyCommitTx),
        ("rollback-tx", 0) => Some(CoreBuiltin::CoreReplOnlyRollbackTx),
        ("print", 1) => Some(CoreBuiltin::CoreReplOnlyPrintLn),

        // No match
        _ => None,
    }
}

/// Check if a name is a builtin function
pub fn is_builtin(name: &str) -> bool {
    // Common arities to check
    let common_arities = [0, 1, 2, 3, 4, 5];
    common_arities.iter().any(|&arity| resolve_builtin_arity(name, arity).is_some())
}

/// Get all possible arities for a builtin name
pub fn get_builtin_arities(name: &str) -> Vec<usize> {
    let mut arities = Vec::new();
    
    // Check arities 0-10 (should cover all Pact builtins)
    for arity in 0..=10 {
        if resolve_builtin_arity(name, arity).is_some() {
            arities.push(arity);
        }
    }
    
    arities
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arithmetic_builtins() {
        assert_eq!(resolve_builtin_arity("+", 2), Some(CoreBuiltin::CoreAdd));
        assert_eq!(resolve_builtin_arity("-", 1), Some(CoreBuiltin::CoreNegate));
        assert_eq!(resolve_builtin_arity("-", 2), Some(CoreBuiltin::CoreSub));
        assert_eq!(resolve_builtin_arity("*", 2), Some(CoreBuiltin::CoreMultiply));
        assert_eq!(resolve_builtin_arity("/", 2), Some(CoreBuiltin::CoreDivide));
    }

    #[test]
    fn test_comparison_builtins() {
        assert_eq!(resolve_builtin_arity("=", 2), Some(CoreBuiltin::CoreEq));
        assert_eq!(resolve_builtin_arity("<", 2), Some(CoreBuiltin::CoreLT));
        assert_eq!(resolve_builtin_arity(">", 2), Some(CoreBuiltin::CoreGT));
    }

    #[test]
    fn test_overloaded_builtins() {
        // select has different arities
        assert_eq!(resolve_builtin_arity("select", 2), Some(CoreBuiltin::CoreSelect));
        assert_eq!(resolve_builtin_arity("select", 3), Some(CoreBuiltin::CoreSelectWithFields));
        assert_eq!(resolve_builtin_arity("select", 1), None);
    }

    #[test]
    fn test_is_builtin() {
        assert!(is_builtin("+"));
        assert!(is_builtin("map"));
        assert!(is_builtin("select"));
        assert!(!is_builtin("defun"));
        assert!(!is_builtin("user-function"));
    }

    #[test]
    fn test_get_builtin_arities() {
        let plus_arities = get_builtin_arities("+");
        assert_eq!(plus_arities, vec![2]);
        
        let minus_arities = get_builtin_arities("-");
        assert_eq!(minus_arities, vec![1, 2]);
        
        let select_arities = get_builtin_arities("select");
        assert_eq!(select_arities, vec![2, 3]);
    }
}