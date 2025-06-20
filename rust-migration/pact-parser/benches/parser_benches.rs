//! Comprehensive parser benchmarks for performance measurement and memory tracking

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use pact_parser::Parser;

/// Benchmark simple expression parsing
fn bench_simple_expressions(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser/simple_expressions");

    let expressions = vec![
        ("integer", "42"),
        ("decimal", "3.14159"),
        ("string", r#""hello world""#),
        ("symbol", "my-symbol"),
        ("list", "[1 2 3 4 5]"),
        ("addition", "(+ 1 2)"),
        ("nested_arithmetic", "(* (+ 1 2) (- 4 3))"),
        ("comparison", "(> 10 5)"),
        ("logical", "(and true false)"),
    ];

    for (name, expr) in expressions {
        group.bench_with_input(BenchmarkId::from_parameter(name), expr, |b, expr| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(expr)).unwrap();
                parser.parse_program()
            });
        });
    }

    group.finish();
}

/// Benchmark different AST node types
fn bench_ast_node_types(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser/ast_node_types");

    let test_cases = vec![
        ("literal_integer", "42"),
        ("literal_decimal", "3.14159"),
        ("literal_string", r#""hello world""#),
        ("literal_bool", "true"),
        ("symbol", "my-symbol"),
        ("list_simple", "[1 2 3 4 5]"),
        ("list_nested", "[[1 2] [3 4] [5 6]]"),
        ("object_simple", r#"{ "name": "Alice", "age": 30 }"#),
        (
            "object_nested",
            r#"{ "user": { "name": "Bob" }, "active": true }"#,
        ),
        ("function_call", "(+ 1 2)"),
        ("nested_calls", "(* (+ 1 2) (- 4 3))"),
        ("let_binding", r#"(let ((x 1) (y 2)) (+ x y))"#),
        ("conditional", "(if (> x 0) x (- x))"),
        ("lambda", "(lambda (x y) (+ x y))"),
    ];

    for (name, input) in test_cases {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), input, |b, input| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(input)).unwrap();
                parser.parse_program()
            })
        });
    }

    group.finish();
}

/// Benchmark function definition parsing
fn bench_function_definitions(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser/function_definitions");

    let functions = vec![
        ("simple", "(defun add (x y) (+ x y))"),
        (
            "with_types",
            "(defun add:integer (x:integer y:integer) (+ x y))",
        ),
        (
            "with_doc",
            r#"(defun add (x y) "Adds two numbers" (+ x y))"#,
        ),
        (
            "complex_body",
            "(defun calc (x y z) (let ((a (+ x y))) (* a z)))",
        ),
        (
            "nested_lets",
            "(defun nested (x) (let ((a x)) (let ((b (* a 2))) (+ a b))))",
        ),
        (
            "defcap",
            r#"(defcap TRANSFER (from:string to:string amount:decimal)
                 @managed amount TRANSFER-mgr
                 (enforce-valid-account from)
                 (enforce-valid-account to)
                 (enforce (> amount 0.0) "Amount must be positive"))"#,
        ),
        (
            "defpact",
            r#"(defpact payment (sender:string receiver:string amount:decimal)
                 (step-with-rollback
                   (debit sender amount)
                   (credit sender amount))
                 (step (credit receiver amount)))"#,
        ),
        (
            "defschema",
            r#"(defschema account
                 balance:decimal
                 guard:guard
                 last-transaction:time
                 status:string)"#,
        ),
    ];

    for (name, code) in functions {
        group.throughput(Throughput::Bytes(code.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), code, |b, code| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(code)).unwrap();
                parser.parse_program()
            });
        });
    }

    group.finish();
}

/// Benchmark module parsing
fn bench_module_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser/modules");

    let small_module = r#"
(module token GOV
  (defcap GOV () true)

  (defschema token-schema
    balance:decimal
    owner:string)

  (deftable tokens:{token-schema})

  (defun transfer (from:string to:string amount:decimal)
    (with-read tokens from { "balance" := balance }
      (enforce (>= balance amount) "Insufficient balance")
      (update tokens from { "balance": (- balance amount) })
      (update tokens to { "balance": (+ balance amount) })))
)"#;

    let medium_module = r#"
(module marketplace GOV
  (defcap GOV () true)

  (defschema listing-schema
    seller:string
    price:decimal
    token-id:string
    active:bool)

  (defschema offer-schema
    buyer:string
    amount:decimal
    expiry:time)

  (deftable listings:{listing-schema})
  (deftable offers:{offer-schema})

  (defcap SELLER (account:string token-id:string)
    (with-read listings token-id { "seller" := seller }
      (enforce (= seller account) "Not the seller")))

  (defun list-token (token-id:string price:decimal)
    (enforce (> price 0.0) "Price must be positive")
    (insert listings token-id
      { "seller": (at "sender" (chain-data))
      , "price": price
      , "token-id": token-id
      , "active": true }))

  (defun buy-token (token-id:string)
    (with-read listings token-id
      { "price" := price, "seller" := seller, "active" := active }
      (enforce active "Listing not active")
      (transfer-coin (at "sender" (chain-data)) seller price)
      (update listings token-id { "active": false })))

  (defun make-offer (token-id:string amount:decimal expiry:time)
    (enforce (> amount 0.0) "Offer must be positive")
    (insert offers (compound-key token-id (at "sender" (chain-data)))
      { "buyer": (at "sender" (chain-data))
      , "amount": amount
      , "expiry": expiry }))
)"#;

    // Large module: banking system from test file
    let large_module = include_str!("../test-files/large-contract.pact");

    let test_cases = vec![
        ("small", small_module),
        ("medium", medium_module),
        ("large", large_module),
    ];

    for (name, input) in test_cases {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), input, |b, input| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(input)).unwrap();
                parser.parse_program()
            })
        });
    }

    group.finish();
}

/// Benchmark parsing complexity (nesting depth)
fn bench_parsing_complexity(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser/complexity");

    // Generate deeply nested expressions
    fn generate_nested_arithmetic(depth: usize) -> String {
        if depth == 0 {
            "1".to_string()
        } else {
            format!(
                "(+ {} {})",
                generate_nested_arithmetic(depth - 1),
                generate_nested_arithmetic(depth - 1)
            )
        }
    }

    // Generate deeply nested let expressions
    fn generate_nested_let(depth: usize) -> String {
        if depth == 0 {
            "x".to_string()
        } else {
            format!("(let ((x {})) {})", depth, generate_nested_let(depth - 1))
        }
    }

    // Generate large lists
    fn generate_large_list(size: usize) -> String {
        let elements: Vec<String> = (0..size).map(|i| i.to_string()).collect();
        format!("[{}]", elements.join(" "))
    }

    // Generate large objects
    fn generate_large_object(size: usize) -> String {
        let fields: Vec<String> = (0..size)
            .map(|i| format!(r#""field{}": {}"#, i, i))
            .collect();
        format!("{{ {} }}", fields.join(", "))
    }

    let test_cases = vec![
        ("nested_arithmetic_5", generate_nested_arithmetic(5)),
        ("nested_arithmetic_10", generate_nested_arithmetic(10)),
        ("nested_let_5", generate_nested_let(5)),
        ("nested_let_10", generate_nested_let(10)),
        ("large_list_100", generate_large_list(100)),
        ("large_list_1000", generate_large_list(1000)),
        ("large_object_50", generate_large_object(50)),
        ("large_object_200", generate_large_object(200)),
    ];

    for (name, input) in test_cases {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), &input, |b, input| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(input)).unwrap();
                parser.parse_program()
            })
        });
    }

    group.finish();
}

/// Benchmark deeply nested expressions
fn bench_nested_expressions(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser/nested_expressions");

    // Generate deeply nested expressions
    fn generate_nested(depth: usize) -> String {
        if depth == 0 {
            "x".to_string()
        } else {
            format!("(+ {} 1)", generate_nested(depth - 1))
        }
    }

    for depth in [5, 10, 20, 50] {
        let expr = generate_nested(depth);
        group.bench_with_input(BenchmarkId::from_parameter(depth), &expr, |b, expr| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(expr)).unwrap();
                parser.parse_program()
            });
        });
    }

    group.finish();
}

/// Benchmark list parsing with many elements
fn bench_large_lists(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser/large_lists");

    for size in [10, 100, 1000] {
        let elements: Vec<String> = (0..size).map(|i| i.to_string()).collect();
        let list = format!("[{}]", elements.join(" "));

        group.bench_with_input(BenchmarkId::from_parameter(size), &list, |b, list| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(list)).unwrap();
                parser.parse_program()
            });
        });
    }

    group.finish();
}

/// Benchmark object parsing
fn bench_object_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser/objects");

    let objects = vec![
        ("empty", "{}"),
        ("simple", r#"{ "name": "Alice", "age": 30 }"#),
        (
            "nested",
            r#"{ "user": { "name": "Bob", "email": "bob@example.com" }, "active": true }"#,
        ),
        (
            "array_values",
            r#"{ "items": [1, 2, 3], "tags": ["foo", "bar"] }"#,
        ),
    ];

    for (name, obj) in objects {
        group.bench_with_input(BenchmarkId::from_parameter(name), obj, |b, obj| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(obj)).unwrap();
                parser.parse_program()
            });
        });
    }

    group.finish();
}

/// Benchmark special Pact constructs
fn bench_special_constructs(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser/special_constructs");

    let test_cases = vec![
        (
            "with_capability",
            "(with-capability (TRANSFER sender receiver amount) (transfer sender receiver amount))",
        ),
        (
            "with_read",
            r#"(with-read accounts sender { "balance" := balance } (enforce (>= balance amount) "Insufficient balance"))"#,
        ),
        (
            "with_default_read",
            r#"(with-default-read accounts receiver { "balance": 0.0 } { "balance" := current } (+ current amount))"#,
        ),
        (
            "bind",
            r#"(bind { "name": "Alice", "age": 30 } { "name" := n, "age" := a } (format "{} is {} years old" [n a]))"#,
        ),
        ("try", "(try (/ 10 0) \"Division by zero error\")"),
        (
            "cond",
            r#"(cond
                 ((< x 0) "negative")
                 ((= x 0) "zero")
                 ((> x 0) "positive")
                 "unknown")"#,
        ),
        (
            "enforce_one",
            r#"(enforce-one "At least one condition must be true"
                 [(> x 0) (< x 10) (= x 5)])"#,
        ),
        (
            "step_with_rollback",
            "(step-with-rollback (debit sender amount) (credit sender amount))",
        ),
        (
            "create_user_guard",
            "(create-user-guard (validate-signature (at 'sig msg) (at 'pubkey msg)))",
        ),
    ];

    for (name, input) in test_cases {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), input, |b, input| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(input)).unwrap();
                parser.parse_program()
            })
        });
    }

    group.finish();
}

/// Benchmark error recovery performance
fn bench_error_recovery(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser/error_recovery");

    let error_cases = vec![
        ("missing_paren", "(defun foo (x y (+ x y))"),
        ("invalid_syntax", "(defun foo (x y) (+ x @#$))"),
        ("incomplete_expr", "(defun foo (x y) (+"),
        ("mismatched_brackets", "[1 2 3}"),
        ("incomplete_object", r#"{ "name": "Alice""#),
        ("missing_quotes", "{ name: Alice }"),
        ("incomplete_string", r#""hello world"#),
        (
            "nested_errors",
            "(defun foo (x) (let ((a (+ x @)) (b (* a #))) (if (> b) a)))",
        ),
    ];

    for (name, input) in error_cases {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), input, |b, input| {
            b.iter(|| {
                // Ignore parse errors for benchmarking
                let _ = Parser::new(black_box(input)).and_then(|mut p| p.parse_program());
            })
        });
    }

    group.finish();
}

/// Benchmark memory usage and arena allocation
fn bench_memory_usage(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser/memory_analysis");

    let sample_module = r#"
(module memory-test 'test-keyset
  (defschema user
    name:string
    age:integer
    email:string
    active:bool)

  (deftable users:{user})

  (defcap ADMIN ()
    (enforce-guard (keyset-ref-guard 'test-keyset)))

  (defun create-user (name:string age:integer email:string)
    (with-capability (ADMIN)
      (insert users name {
        "name": name,
        "age": age,
        "email": email,
        "active": true })))

  (defun get-user:object{user} (name:string)
    (read users name))

  (defun update-user:string (name:string updates:object)
    (with-capability (ADMIN)
      (update users name updates)
      "User updated"))

  (defun deactivate-user:string (name:string)
    (with-capability (ADMIN)
      (update users name { "active": false })
      "User deactivated"))
)
"#;

    group.bench_function("memory_tracking", |b| {
        b.iter(|| {
            let mut parser = Parser::new(black_box(sample_module)).unwrap();
            let _ast = parser.parse_program().unwrap();
            let stats = parser.memory_stats();
            black_box(stats)
        })
    });

    // Benchmark memory efficiency with repeated parsing
    group.bench_function("repeated_parsing", |b| {
        b.iter(|| {
            for _ in 0..10 {
                let mut parser = Parser::new(black_box(sample_module)).unwrap();
                let _ast = parser.parse_program().unwrap();
            }
        })
    });

    group.finish();
}

/// Benchmark different string patterns
fn bench_string_patterns(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser/string_patterns");

    let long_string = format!(r#""{}""#, "x".repeat(1000));
    let many_short_strings = (0..100)
        .map(|i| format!(r#""str{}""#, i))
        .collect::<Vec<_>>()
        .join(" ");

    let test_cases = vec![
        ("simple_strings", r#""hello" "world" "test""#),
        (
            "escaped_strings",
            r#""hello\nworld" "tab\there" "quote\"here""#,
        ),
        ("long_string", long_string.as_str()),
        ("many_short_strings", many_short_strings.as_str()),
        ("unicode_strings", r#""héllo" "wörld" "tëst" "🚀🎉""#),
        ("template_strings", r#""Hello {}" "Value: {}" "Result: {}""#),
    ];

    for (name, input) in test_cases {
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(name), input, |b, input| {
            b.iter(|| {
                let mut parser = Parser::new(black_box(input)).unwrap();
                parser.parse_program()
            })
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    bench_simple_expressions,
    bench_ast_node_types,
    bench_function_definitions,
    bench_module_parsing,
    bench_parsing_complexity,
    bench_nested_expressions,
    bench_large_lists,
    bench_object_parsing,
    bench_special_constructs,
    bench_error_recovery,
    bench_memory_usage,
    bench_string_patterns,
);

criterion_main!(benches);
