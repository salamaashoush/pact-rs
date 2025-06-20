//! Arena-based AST allocation for optimal memory usage
//!
//! This module provides fast, memory-efficient allocation of AST nodes using
//! arena allocation patterns inspired by Oxc parser design.

use crate::ast::*;
use std::collections::HashMap;

/// High-performance arena allocator for AST nodes
/// Uses indices instead of pointers for 4-byte references
pub struct AstArena {
    // Core expression data
    exprs: Vec<ParsedExpr<u32>>,
    args: Vec<Vec<MArg<u32>>>,
    bindings: Vec<Vec<Binder<u32>>>,

    // Type information
    types: Vec<Type>,

    // String interning for identifiers
    strings: Vec<String>,
    string_map: HashMap<String, u32>,

    // Statistics
    peak_memory: usize,
    allocations: usize,
}

impl AstArena {
    pub fn new() -> Self {
        Self {
            exprs: Vec::new(),
            args: Vec::new(),
            bindings: Vec::new(),
            types: Vec::new(),
            strings: Vec::new(),
            string_map: HashMap::new(),
            peak_memory: 0,
            allocations: 0,
        }
    }

    /// Allocate an expression and return its index
    pub fn alloc_expr(&mut self, expr: ParsedExpr<u32>) -> u32 {
        let index = self.exprs.len() as u32;
        self.exprs.push(expr);
        self.allocations += 1;
        self.update_peak_memory();
        index
    }

    /// Allocate argument list and return its index
    pub fn alloc_args(&mut self, args: Vec<MArg<u32>>) -> u32 {
        let index = self.args.len() as u32;
        self.args.push(args);
        self.allocations += 1;
        self.update_peak_memory();
        index
    }

    /// Allocate binding list and return its index
    pub fn alloc_bindings(&mut self, bindings: Vec<Binder<u32>>) -> u32 {
        let index = self.bindings.len() as u32;
        self.bindings.push(bindings);
        self.allocations += 1;
        self.update_peak_memory();
        index
    }

    /// Allocate type and return its index
    pub fn alloc_type(&mut self, ty: Type) -> u32 {
        let index = self.types.len() as u32;
        self.types.push(ty);
        self.allocations += 1;
        self.update_peak_memory();
        index
    }

    /// Intern a string and return its index
    pub fn intern_string(&mut self, s: &str) -> u32 {
        if let Some(&index) = self.string_map.get(s) {
            return index;
        }

        let index = self.strings.len() as u32;
        self.string_map.insert(s.to_string(), index);
        self.strings.push(s.to_string());
        self.allocations += 1;
        self.update_peak_memory();
        index
    }

    /// Get expression by index
    pub fn get_expr(&self, index: u32) -> &ParsedExpr<u32> {
        &self.exprs[index as usize]
    }

    /// Get arguments by index
    pub fn get_args(&self, index: u32) -> &[MArg<u32>] {
        &self.args[index as usize]
    }

    /// Get bindings by index
    pub fn get_bindings(&self, index: u32) -> &[Binder<u32>] {
        &self.bindings[index as usize]
    }

    /// Get type by index
    pub fn get_type(&self, index: u32) -> &Type {
        &self.types[index as usize]
    }

    /// Get string by index
    pub fn get_string(&self, index: u32) -> &str {
        &self.strings[index as usize]
    }

    /// Update peak memory tracking
    fn update_peak_memory(&mut self) {
        let current = self.memory_usage();
        if current > self.peak_memory {
            self.peak_memory = current;
        }
    }

    /// Calculate current memory usage in bytes
    pub fn memory_usage(&self) -> usize {
        let expr_size = self.exprs.len() * std::mem::size_of::<ParsedExpr<u32>>();
        let args_size = self
            .args
            .iter()
            .map(|a| a.len() * std::mem::size_of::<MArg<u32>>())
            .sum::<usize>();
        let bindings_size = self
            .bindings
            .iter()
            .map(|b| b.len() * std::mem::size_of::<Binder<u32>>())
            .sum::<usize>();
        let types_size = self.types.len() * std::mem::size_of::<Type>();
        let strings_size = self.strings.iter().map(|s| s.capacity()).sum::<usize>();
        let map_size =
            self.string_map.len() * (std::mem::size_of::<String>() + std::mem::size_of::<u32>());

        expr_size + args_size + bindings_size + types_size + strings_size + map_size
    }

    /// Get allocation statistics
    pub fn stats(&self) -> ArenaStats {
        ArenaStats {
            expressions: self.exprs.len(),
            arg_lists: self.args.len(),
            binding_lists: self.bindings.len(),
            types: self.types.len(),
            interned_strings: self.strings.len(),
            current_memory: self.memory_usage(),
            peak_memory: self.peak_memory,
            total_allocations: self.allocations,
        }
    }

    /// Clear all allocations (useful for reusing arena)
    pub fn clear(&mut self) {
        self.exprs.clear();
        self.args.clear();
        self.bindings.clear();
        self.types.clear();
        self.strings.clear();
        self.string_map.clear();
        self.allocations = 0;
        // Keep peak_memory for statistics
    }
}

impl Default for AstArena {
    fn default() -> Self {
        Self::new()
    }
}

/// Arena allocation statistics
#[derive(Debug, Clone)]
pub struct ArenaStats {
    pub expressions: usize,
    pub arg_lists: usize,
    pub binding_lists: usize,
    pub types: usize,
    pub interned_strings: usize,
    pub current_memory: usize,
    pub peak_memory: usize,
    pub total_allocations: usize,
}

impl std::fmt::Display for ArenaStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Arena Statistics:\n\
             Expressions: {}\n\
             Argument lists: {}\n\
             Binding lists: {}\n\
             Types: {}\n\
             Interned strings: {}\n\
             Current memory: {} bytes\n\
             Peak memory: {} bytes\n\
             Total allocations: {}",
            self.expressions,
            self.arg_lists,
            self.binding_lists,
            self.types,
            self.interned_strings,
            self.current_memory,
            self.peak_memory,
            self.total_allocations
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arena_basic_allocation() {
        let mut arena = AstArena::new();

        // Test string interning
        let str1 = arena.intern_string("hello");
        let str2 = arena.intern_string("hello"); // Should reuse
        let str3 = arena.intern_string("world");

        assert_eq!(str1, str2); // Same string should have same index
        assert_ne!(str1, str3); // Different strings should have different indices

        assert_eq!(arena.get_string(str1), "hello");
        assert_eq!(arena.get_string(str3), "world");

        // Should only have 2 unique strings
        assert_eq!(arena.strings.len(), 2);
    }

    #[test]
    fn test_arena_memory_tracking() {
        let mut arena = AstArena::new();

        let initial_memory = arena.memory_usage();

        // Allocate some data
        arena.intern_string("test");
        arena.alloc_args(vec![]);

        let after_memory = arena.memory_usage();
        assert!(after_memory > initial_memory);

        let stats = arena.stats();
        assert!(stats.total_allocations > 0);
        assert!(stats.current_memory > 0);
    }

    #[test]
    fn test_arena_clear() {
        let mut arena = AstArena::new();

        arena.intern_string("test");
        arena.alloc_args(vec![]);

        assert!(arena.stats().total_allocations > 0);

        let peak_before_clear = arena.peak_memory;
        arena.clear();

        // Should be empty but preserve peak memory
        let stats = arena.stats();
        assert_eq!(stats.expressions, 0);
        assert_eq!(stats.interned_strings, 0);
        assert_eq!(stats.total_allocations, 0);
        assert_eq!(arena.peak_memory, peak_before_clear);
    }
}
