//! Comprehensive lexer benchmarks for performance measurement and memory tracking

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use pact_lexer::{lex, Lexer};

/// Benchmark basic token types
fn bench_token_types(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer/token_types");

    let test_cases = vec![
        (
            "keywords",
            "defun defcap defpact module interface let lambda",
        ),
        ("identifiers", "add multiply transfer balance account-key"),
        ("numbers", "42 -17 3.14159 -2.5 1000000 0.001"),
        (
            "strings",
            r#""hello" "world with spaces" "escaped\nstring""#,
        ),
        ("operators", "+ - * / = != < > <= >= and or not"),
        ("delimiters", "( ) [ ] { } , : . :: :="),
        ("annotations", "@doc @model @event @managed"),
        ("single_ticks", "'keyset-name 'admin 'user-guard"),
    ];

    for (name, input) in test_cases {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::new("basic_lex", name), input, |b, input| {
            b.iter(|| lex(black_box(input)))
        });

        group.bench_with_input(BenchmarkId::new("interned_lex", name), input, |b, input| {
            b.iter(|| {
                let mut lexer = Lexer::new();
                lexer.lex_interned(black_box(input))
            })
        });
    }

    group.finish();
}

/// Benchmark simple expressions from original lexer_bench.rs
fn bench_simple_expressions(c: &mut Criterion) {
    let inputs = vec![
        ("simple_add", "(+ 1 2)"),
        ("nested_expr", "(+ (* 3 4) (- 8 2))"),
        ("function_call", "(transfer \"alice\" \"bob\" 100.0)"),
        ("with_capability", "(with-capability (TRANSFER \"alice\" \"bob\" 100.0) (transfer \"alice\" \"bob\" 100.0))"),
    ];

    let mut group = c.benchmark_group("lexer/simple_expressions");

    for (name, input) in inputs {
        group.bench_with_input(BenchmarkId::new("lex", name), input, |b, input| {
            b.iter(|| lex(black_box(input)))
        });

        group.bench_with_input(BenchmarkId::new("interned_lex", name), input, |b, input| {
            b.iter(|| {
                let mut lexer = Lexer::new();
                lexer.lex_interned(black_box(input))
            })
        });
    }

    group.finish();
}

/// Benchmark different code sizes
fn bench_code_sizes(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer/code_sizes");

    // Small: simple function
    let small = r#"
        (defun add (x:integer y:integer)
          (+ x y))
    "#;

    // Medium: module with multiple functions
    let medium = r#"
        (module bank 'bank-admin
          (defschema account
            balance:decimal
            guard:guard)

          (deftable accounts:{account})

          (defcap TRANSFER (from:string to:string amount:decimal)
            (with-read accounts from { "guard" := guard }
              (enforce-guard guard))
            (enforce (> amount 0.0) "Amount must be positive"))

          (defun transfer:string (from:string to:string amount:decimal)
            (with-capability (TRANSFER from to amount)
              (with-read accounts from { "balance" := from-bal }
                (enforce (>= from-bal amount) "Insufficient balance")
                (update accounts from { "balance": (- from-bal amount) })
                (with-default-read accounts to { "balance": 0.0 } { "balance" := to-bal }
                  (write accounts to { "balance": (+ to-bal amount), "guard": (read-keyset "to-guard") })))))
        )
    "#;

    // Large: repeat medium module with variations
    let large = format!(
        "{}\n{}\n{}\n{}",
        medium
            .replace("bank", "bank1")
            .replace("'bank-admin", "'bank1-admin"),
        medium
            .replace("bank", "bank2")
            .replace("'bank-admin", "'bank2-admin"),
        medium
            .replace("bank", "bank3")
            .replace("'bank-admin", "'bank3-admin"),
        medium
            .replace("bank", "bank4")
            .replace("'bank-admin", "'bank4-admin")
    );

    // Very large: complex real-world-like contract
    let very_large = include_str!("../test-files/large-contract.pact");

    let test_cases = vec![
        ("small", small),
        ("medium", medium),
        ("large", &large),
        ("very_large", very_large),
    ];

    for (name, input) in test_cases {
        group.throughput(Throughput::Bytes(input.len() as u64));

        group.bench_with_input(BenchmarkId::new("basic_lex", name), input, |b, input| {
            b.iter(|| lex(black_box(input)))
        });

        group.bench_with_input(BenchmarkId::new("interned_lex", name), input, |b, input| {
            b.iter(|| {
                let mut lexer = Lexer::new();
                lexer.lex_interned(black_box(input))
            })
        });
    }

    group.finish();
}

/// Benchmark memory usage with string interning
fn bench_string_interning(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer/string_interning");

    // Code with many repeated identifiers (good for interning)
    let repeated_identifiers = (0..100)
        .map(|i| format!("(transfer sender{} receiver{} amount)", i % 10, i % 5))
        .collect::<Vec<_>>()
        .join("\n");

    // Code with unique identifiers (bad for interning)
    let unique_identifiers = (0..100)
        .map(|i| {
            format!(
                "(transfer sender{} receiver{} amount{})",
                i,
                i + 100,
                i + 200
            )
        })
        .collect::<Vec<_>>()
        .join("\n");

    let test_cases = vec![
        ("repeated_identifiers", &repeated_identifiers),
        ("unique_identifiers", &unique_identifiers),
    ];

    for (name, input) in test_cases {
        group.throughput(Throughput::Bytes(input.len() as u64));

        group.bench_with_input(
            BenchmarkId::new("without_interning", name),
            input,
            |b, input| b.iter(|| lex(black_box(input))),
        );

        group.bench_with_input(
            BenchmarkId::new("with_interning", name),
            input,
            |b, input| {
                b.iter(|| {
                    let mut lexer = Lexer::new();
                    lexer.lex_interned(black_box(input))
                })
            },
        );
    }

    group.finish();
}

/// Benchmark edge cases and complex patterns
fn bench_edge_cases(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer/edge_cases");

    let long_string = format!(r#""{}""#, "x".repeat(1000));
    let many_parens = format!("{}{}", "(".repeat(500), ")".repeat(500));
    let deep_nesting = (0..100).fold(String::new(), |acc, _| format!("(list {})", acc));
    let complex_identifiers = "test-func$with%special*chars&more!symbols";
    let numbers_and_decimals = "1 22 333 4.4 55.55 666.666 7777.7777";
    let comments_mixed = "; comment\n(+ 1 2) ; another\n; third\n(* 3 4)";

    let test_cases = vec![
        ("long_string", long_string.as_str()),
        ("many_parens", many_parens.as_str()),
        ("deep_nesting", deep_nesting.as_str()),
        ("complex_identifiers", complex_identifiers),
        ("numbers_and_decimals", numbers_and_decimals),
        ("comments_mixed", comments_mixed),
    ];

    for (name, input) in test_cases {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), input, |b, input| {
            b.iter(|| lex(black_box(input)))
        });
    }

    group.finish();
}

/// Benchmark error handling
fn bench_error_handling(c: &mut Criterion) {
    let error_cases = vec![
        ("invalid_chars", "@ # $ % ^"),
        ("unterminated_string", r#""hello world"#),
        ("mixed_valid_invalid", "(+ 1 @ 2)"),
    ];

    let mut group = c.benchmark_group("lexer/error_handling");

    for (name, input) in error_cases {
        group.bench_with_input(BenchmarkId::new("lex_errors", name), input, |b, input| {
            b.iter(|| {
                let _ = lex(black_box(input)); // Don't panic on errors
            })
        });
    }

    group.finish();
}

/// Memory usage analysis
fn bench_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("lexer/memory_analysis");

    let input = r#"
        (module test 'test-keyset
          (defun test-func (x:integer y:integer)
            (let ((result (+ x y))
                  (doubled (* result 2)))
              (if (> doubled 10)
                "large"
                "small"))))
    "#;

    group.bench_function("memory_tracking", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new();
            let _tokens = lexer.lex_interned(black_box(input)).unwrap();
            let usage = lexer.memory_usage();
            black_box(usage)
        })
    });

    group.finish();
}

/// Benchmark iterator vs batch performance
fn bench_iterator_vs_batch(c: &mut Criterion) {
    let input = r#"
        (module benchmark-test 'test-keyset
          (defun fibonacci (n:integer)
            (if (<= n 1)
              n
              (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
          (defun factorial (n:integer)
            (if (<= n 1)
              1
              (* n (factorial (- n 1)))))
        )
    "#;

    let mut group = c.benchmark_group("lexer/iteration_comparison");

    group.bench_function("batch_lex", |b| b.iter(|| lex(black_box(input))));

    group.bench_function("iterator_collect", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new();
            let _tokens = lexer.lex_interned(black_box(input));
        })
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_token_types,
    bench_simple_expressions,
    bench_code_sizes,
    bench_string_interning,
    bench_edge_cases,
    bench_error_handling,
    bench_memory_usage,
    bench_iterator_vs_batch,
);

criterion_main!(benches);
