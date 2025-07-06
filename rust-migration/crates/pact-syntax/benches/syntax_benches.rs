//! Combined benchmarks for lexer and parser performance
//!
//! This benchmark suite measures the performance of the complete syntax pipeline.

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use pact_syntax::{Lexer, Parser, parse_module, parse_expr};
use pact_core::names::ModuleName;

// Sample Pact code for benchmarking
const SIMPLE_EXPR: &str = "(+ 1 2)";

const COMPLEX_EXPR: &str = r#"
(let ((x 10)
      (y 20)
      (z (+ x y)))
  (if (> z 25)
      (* z 2)
      (- z 5)))
"#;

const SAMPLE_MODULE: &str = r#"
(module accounts GOVERNANCE
  (defcap GOVERNANCE () 
    (enforce-guard (keyset-ref-guard "accounts-admin")))
    
  (defschema account
    balance:decimal
    guard:guard)
    
  (deftable accounts:{account})
  
  (defun create-account:string (account:string guard:guard)
    (enforce (!= "" account) "Account name cannot be empty")
    (insert accounts account {
      "balance": 0.0,
      "guard": guard
    })
    (format "Account {} created" [account]))
    
  (defun get-balance:decimal (account:string)
    (at 'balance (read accounts account)))
    
  (defun transfer:string (from:string to:string amount:decimal)
    (enforce (> amount 0.0) "Amount must be positive")
    (with-read accounts from { "balance" := from-bal, "guard" := from-guard }
      (enforce-guard from-guard)
      (enforce (>= from-bal amount) "Insufficient funds")
      (update accounts from { "balance": (- from-bal amount) })
      (with-default-read accounts to { "balance": 0.0 } { "balance" := to-bal }
        (update accounts to { "balance": (+ to-bal amount) }))
      (format "Transferred {} from {} to {}" [amount from to]))))
"#;

fn bench_lexer_simple(c: &mut Criterion) {
    c.bench_function("lexer_simple_expr", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(SIMPLE_EXPR));
            black_box(lexer.tokenize().unwrap())
        })
    });
}

fn bench_lexer_complex(c: &mut Criterion) {
    c.bench_function("lexer_complex_expr", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(COMPLEX_EXPR));
            black_box(lexer.tokenize().unwrap())
        })
    });
}

fn bench_lexer_module(c: &mut Criterion) {
    c.bench_function("lexer_sample_module", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(SAMPLE_MODULE));
            black_box(lexer.tokenize().unwrap())
        })
    });
}

fn bench_parser_simple(c: &mut Criterion) {
    let mut lexer = Lexer::new(SIMPLE_EXPR);
    let tokens = lexer.tokenize().unwrap();
    
    c.bench_function("parser_simple_expr", |b| {
        b.iter(|| {
            let mut parser = Parser::new(black_box(tokens.clone()));
            black_box(parser.parse_expression().unwrap())
        })
    });
}

fn bench_parser_complex(c: &mut Criterion) {
    let mut lexer = Lexer::new(COMPLEX_EXPR);
    let tokens = lexer.tokenize().unwrap();
    
    c.bench_function("parser_complex_expr", |b| {
        b.iter(|| {
            let mut parser = Parser::new(black_box(tokens.clone()));
            black_box(parser.parse_expression().unwrap())
        })
    });
}

fn bench_parser_module(c: &mut Criterion) {
    let mut lexer = Lexer::new(SAMPLE_MODULE);
    let tokens = lexer.tokenize().unwrap();
    
    c.bench_function("parser_sample_module", |b| {
        b.iter(|| {
            let mut parser = Parser::new(black_box(tokens.clone()));
            black_box(parser.parse_module(Some(ModuleName::simple("accounts"))).unwrap())
        })
    });
}

fn bench_full_pipeline_expr(c: &mut Criterion) {
    c.bench_function("full_pipeline_simple_expr", |b| {
        b.iter(|| {
            black_box(parse_expr(black_box(SIMPLE_EXPR)).unwrap())
        })
    });
}

fn bench_full_pipeline_module(c: &mut Criterion) {
    c.bench_function("full_pipeline_module", |b| {
        b.iter(|| {
            black_box(parse_module(
                black_box(SAMPLE_MODULE), 
                Some(ModuleName::simple("accounts"))
            ).unwrap())
        })
    });
}

criterion_group!(
    lexer_benches,
    bench_lexer_simple,
    bench_lexer_complex,
    bench_lexer_module
);

criterion_group!(
    parser_benches,
    bench_parser_simple,
    bench_parser_complex,
    bench_parser_module
);

criterion_group!(
    pipeline_benches,
    bench_full_pipeline_expr,
    bench_full_pipeline_module
);

criterion_main!(lexer_benches, parser_benches, pipeline_benches);