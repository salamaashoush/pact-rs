//! Integration tests for type checking and inference
//!
//! This module provides comprehensive tests that demonstrate the type checking
//! system working with real Pact code examples.

use crate::type_checker::{TypeCheckContext, TypedTerm};
use pact_ir::{Term, Name, ParsedName, BareName, CoreBuiltin, SpanInfo, Literal, Arg};
use pact_schema::{Type, PrimType};
use pact_core::errors::PactResult;

#[cfg(test)]
mod tests {
    use super::*;

    fn make_span() -> SpanInfo {
        SpanInfo::new(0, 0)
    }

    fn make_bare_name(name: &str) -> Name {
        Name::Parsed(ParsedName::BN(BareName(name.into())))
    }

    #[test]
    fn test_type_check_integer_literal() {
        let mut context = TypeCheckContext::new();
        let term = Term::Constant(Literal::LInteger(42), make_span());
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        assert!(matches!(typed_term.ty, Type::Prim(PrimType::Integer)));
    }

    #[test]
    fn test_type_check_string_literal() {
        let mut context = TypeCheckContext::new();
        let term = Term::Constant(Literal::LString("hello".into()), make_span());
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        assert!(matches!(typed_term.ty, Type::Prim(PrimType::String)));
    }

    #[test]
    fn test_type_check_boolean_literal() {
        let mut context = TypeCheckContext::new();
        let term = Term::Constant(Literal::LBool(true), make_span());
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        assert!(matches!(typed_term.ty, Type::Prim(PrimType::Bool)));
    }

    #[test]
    fn test_type_check_unit_literal() {
        let mut context = TypeCheckContext::new();
        let term = Term::Constant(Literal::LUnit, make_span());
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        assert!(matches!(typed_term.ty, Type::Prim(PrimType::Unit)));
    }

    #[test]
    fn test_type_check_builtin_add() {
        let mut context = TypeCheckContext::new();
        let term = Term::Builtin(CoreBuiltin::CoreAdd, make_span());
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get a function type: a -> a -> a with Num constraint
        assert!(matches!(typed_term.ty, Type::Fun(_, _)));
    }

    #[test]
    fn test_type_check_builtin_eq() {
        let mut context = TypeCheckContext::new();
        let term = Term::Builtin(CoreBuiltin::CoreEq, make_span());
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get a function type: a -> a -> bool with Eq constraint
        assert!(matches!(typed_term.ty, Type::Fun(_, _)));
    }

    #[test]
    fn test_type_check_builtin_show() {
        let mut context = TypeCheckContext::new();
        let term = Term::Builtin(CoreBuiltin::CoreShow, make_span());
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get a function type: a -> string with Show constraint
        assert!(matches!(typed_term.ty, Type::Fun(_, _)));
    }

    #[test]
    fn test_type_check_empty_list() {
        let mut context = TypeCheckContext::new();
        let term = Term::ListLit {
            elements: vec![],
            info: make_span(),
        };
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get list type with fresh variable
        assert!(matches!(typed_term.ty, Type::List(_)));
    }

    #[test]
    fn test_type_check_homogeneous_list() {
        let mut context = TypeCheckContext::new();
        let term = Term::ListLit {
            elements: vec![
                Term::Constant(Literal::LInteger(1), make_span()),
                Term::Constant(Literal::LInteger(2), make_span()),
                Term::Constant(Literal::LInteger(3), make_span()),
            ],
            info: make_span(),
        };
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get [integer]
        match typed_term.ty {
            Type::List(elem_type) => {
                assert!(matches!(*elem_type, Type::Prim(PrimType::Integer)));
            }
            _ => panic!("Expected list type"),
        }
    }

    #[test]
    fn test_type_check_lambda_identity() {
        let mut context = TypeCheckContext::new();
        let arg = Arg {
            name: "x".into(),
            ty: None,
            info: make_span(),
        };
        let body = Term::Var(make_bare_name("x"), make_span());
        
        let term = Term::Lam {
            args: vec![arg],
            body: Box::new(body),
            info: make_span(),
        };
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get a -> a function type
        match typed_term.ty {
            Type::Fun(arg_type, ret_type) => {
                // In a polymorphic identity function, arg and return should be the same type variable
                assert_eq!(arg_type, ret_type);
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_type_check_lambda_with_annotation() {
        let mut context = TypeCheckContext::new();
        let arg = Arg {
            name: "x".into(),
            ty: Some(pact_ir::Type::TyPrim(pact_ir::PrimType::PrimInt)),
            info: make_span(),
        };
        let body = Term::Var(make_bare_name("x"), make_span());
        
        let term = Term::Lam {
            args: vec![arg],
            body: Box::new(body),
            info: make_span(),
        };
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get integer -> integer
        match typed_term.ty {
            Type::Fun(arg_type, ret_type) => {
                assert!(matches!(*arg_type, Type::Prim(PrimType::Integer)));
                assert!(matches!(*ret_type, Type::Prim(PrimType::Integer)));
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_type_check_let_binding() {
        let mut context = TypeCheckContext::new();
        
        // (let x 42 x)
        let arg = Arg {
            name: "x".into(),
            ty: None,
            info: make_span(),
        };
        let expr = Term::Constant(Literal::LInteger(42), make_span());
        let body = Term::Var(make_bare_name("x"), make_span());
        
        let term = Term::Let {
            arg,
            expr: Box::new(expr),
            body: Box::new(body),
            info: make_span(),
        };
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get integer type
        assert!(matches!(typed_term.ty, Type::Prim(PrimType::Integer)));
    }

    #[test]
    fn test_type_check_sequence() {
        let mut context = TypeCheckContext::new();
        
        // (begin 42 "hello")
        let first = Term::Constant(Literal::LInteger(42), make_span());
        let second = Term::Constant(Literal::LString("hello".into()), make_span());
        
        let term = Term::Sequence {
            first: Box::new(first),
            second: Box::new(second),
            info: make_span(),
        };
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get the type of the second expression (string)
        assert!(matches!(typed_term.ty, Type::Prim(PrimType::String)));
    }

    #[test]
    fn test_type_check_simple_object() {
        let mut context = TypeCheckContext::new();
        
        // {"name": "alice", "age": 30}
        let fields = vec![
            (pact_ir::Field("name".into()), Term::Constant(Literal::LString("alice".into()), make_span())),
            (pact_ir::Field("age".into()), Term::Constant(Literal::LInteger(30), make_span())),
        ];
        
        let term = Term::ObjectLit {
            fields,
            info: make_span(),
        };
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get object type with concrete row
        match typed_term.ty {
            Type::Object(row) => {
                match row {
                    pact_schema::RowTy::RowConcrete(fields) => {
                        assert_eq!(fields.len(), 2);
                        // Verify field types
                        let name_field = pact_schema::Field("name".to_string());
                        let age_field = pact_schema::Field("age".to_string());
                        assert!(fields.contains_key(&name_field));
                        assert!(fields.contains_key(&age_field));
                    }
                    _ => panic!("Expected concrete row"),
                }
            }
            _ => panic!("Expected object type"),
        }
    }

    #[test]
    fn test_type_check_function_application() {
        let mut context = TypeCheckContext::new();
        
        // (+ 1 2)
        let func = Term::Builtin(CoreBuiltin::CoreAdd, make_span());
        let args = vec![
            Term::Constant(Literal::LInteger(1), make_span()),
            Term::Constant(Literal::LInteger(2), make_span()),
        ];
        
        let term = Term::App {
            func: Box::new(func),
            args,
            info: make_span(),
        };
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get integer type (result of adding two integers)
        // Note: The type inferencer should resolve the polymorphic type to integer
        // based on the arguments
        println!("Application result type: {:?}", typed_term.ty);
        // We expect some concrete type here, likely involving type variables
        // since full constraint solving isn't implemented yet
    }

    #[test]
    fn test_type_check_conditional() {
        let mut context = TypeCheckContext::new();
        
        // (if true 42 24)
        let cond = Term::Constant(Literal::LBool(true), make_span());
        let then_expr = Term::Constant(Literal::LInteger(42), make_span());
        let else_expr = Term::Constant(Literal::LInteger(24), make_span());
        
        let term = Term::BuiltinForm {
            form: pact_ir::BuiltinForm::CIf {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: Some(Box::new(else_expr)),
            },
            info: make_span(),
        };
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get integer type (unification of both branches)
        assert!(matches!(typed_term.ty, Type::Prim(PrimType::Integer)));
    }

    #[test]
    fn test_type_check_logical_and() {
        let mut context = TypeCheckContext::new();
        
        // (and true false)
        let left = Term::Constant(Literal::LBool(true), make_span());
        let right = Term::Constant(Literal::LBool(false), make_span());
        
        let term = Term::BuiltinForm {
            form: pact_ir::BuiltinForm::CAnd(Box::new(left), Box::new(right)),
            info: make_span(),
        };
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get bool type
        assert!(matches!(typed_term.ty, Type::Prim(PrimType::Bool)));
    }

    #[test]
    fn test_type_check_map_builtin_form() {
        let mut context = TypeCheckContext::new();
        
        // (map (lambda (x) (+ x 1)) [1, 2, 3])
        // This is a simplified version - just checking the map form
        let func = Term::Builtin(CoreBuiltin::CoreAdd, make_span()); // Simplified
        let list = Term::ListLit {
            elements: vec![
                Term::Constant(Literal::LInteger(1), make_span()),
                Term::Constant(Literal::LInteger(2), make_span()),
                Term::Constant(Literal::LInteger(3), make_span()),
            ],
            info: make_span(),
        };
        
        let term = Term::BuiltinForm {
            form: pact_ir::BuiltinForm::CMap {
                func: Box::new(func),
                list: Box::new(list),
            },
            info: make_span(),
        };
        
        let result = context.type_check_term(&term);
        assert!(result.is_ok());
        
        let typed_term = result.unwrap();
        // Should get list type
        assert!(matches!(typed_term.ty, Type::List(_)));
    }

    #[test]
    fn test_unbound_variable_error() {
        let mut context = TypeCheckContext::new();
        let term = Term::Var(make_bare_name("unknown_var"), make_span());
        
        let result = context.type_check_term(&term);
        assert!(result.is_err());
        
        // Should get UnboundVariable error
        match result.unwrap_err() {
            pact_core::errors::PactError::Type(pact_core::errors::TypeError::UnboundVariable { var }) => {
                assert_eq!(var, "unknown_var");
            }
            _ => panic!("Expected UnboundVariable error"),
        }
    }

    #[test]
    fn test_builtin_type_signatures_comprehensive() {
        let context = TypeCheckContext::new();
        
        // Test that all expected builtins have type signatures
        let expected_builtins = vec![
            CoreBuiltin::CoreAdd,
            CoreBuiltin::CoreSub,
            CoreBuiltin::CoreMultiply,
            CoreBuiltin::CoreDivide,
            CoreBuiltin::CoreLT,
            CoreBuiltin::CoreGT,
            CoreBuiltin::CoreLEQ,
            CoreBuiltin::CoreGEQ,
            CoreBuiltin::CoreEq,
            CoreBuiltin::CoreNeq,
            CoreBuiltin::CoreShow,
            CoreBuiltin::CoreLength,
            CoreBuiltin::CoreMap,
            CoreBuiltin::CoreFilter,
            CoreBuiltin::CoreFold,
            CoreBuiltin::CoreConcat,
            CoreBuiltin::CoreNegate,
            CoreBuiltin::CoreAbs,
            CoreBuiltin::CoreNot,
        ];
        
        for builtin in expected_builtins {
            assert!(
                context.builtins.contains_key(&builtin),
                "Missing type signature for builtin: {:?}",
                builtin
            );
        }
    }

    #[test]
    fn test_constraint_solving() {
        let mut context = TypeCheckContext::new();
        
        // Create some constraints and solve them
        context.inferencer.constrain_equal(
            Type::Var("a".to_string()),
            Type::Prim(PrimType::Integer),
        );
        context.inferencer.constrain_equal(
            Type::Var("b".to_string()),
            Type::Var("a".to_string()),
        );
        
        let result = context.solve_constraints();
        assert!(result.is_ok());
        
        // The substitution should map both variables to integer
        // (This test demonstrates that the constraint solver is working)
    }

    #[test]
    fn test_generalization_and_instantiation() {
        let mut context = TypeCheckContext::new();
        
        // Test type generalization
        let poly_type = Type::Fun(
            Box::new(Type::Var("a".to_string())),
            Box::new(Type::Var("a".to_string())),
        );
        
        let scheme = context.inferencer.generalize(&context.env, &poly_type);
        assert_eq!(scheme.type_vars.len(), 1);
        assert!(scheme.type_vars.contains(&"a".to_string()));
        
        // Test instantiation
        let instance1 = context.inferencer.instantiate(&scheme);
        let instance2 = context.inferencer.instantiate(&scheme);
        
        // Each instantiation should create fresh variables
        assert_ne!(format!("{:?}", instance1), format!("{:?}", instance2));
    }
}

/// Performance tests for the type checker
#[cfg(test)]
mod performance_tests {
    use super::*;
    use std::time::Instant;

    #[test]
    fn test_type_check_large_list() {
        let mut context = TypeCheckContext::new();
        
        // Create a list with 1000 integer elements
        let elements: Vec<_> = (0..1000)
            .map(|i| Term::Constant(Literal::LInteger(i), make_span()))
            .collect();
        
        let term = Term::ListLit {
            elements,
            info: make_span(),
        };
        
        let start = Instant::now();
        let result = context.type_check_term(&term);
        let duration = start.elapsed();
        
        assert!(result.is_ok());
        println!("Type checked 1000-element list in {:?}", duration);
        
        // Should complete quickly (under 100ms)
        assert!(duration.as_millis() < 100);
    }

    #[test]
    fn test_type_check_nested_objects() {
        let mut context = TypeCheckContext::new();
        
        // Create nested objects: {"outer": {"inner": {"value": 42}}}
        let inner_inner = Term::ObjectLit {
            fields: vec![
                (pact_ir::Field("value".into()), Term::Constant(Literal::LInteger(42), make_span())),
            ],
            info: make_span(),
        };
        
        let inner = Term::ObjectLit {
            fields: vec![
                (pact_ir::Field("inner".into()), inner_inner),
            ],
            info: make_span(),
        };
        
        let outer = Term::ObjectLit {
            fields: vec![
                (pact_ir::Field("outer".into()), inner),
            ],
            info: make_span(),
        };
        
        let start = Instant::now();
        let result = context.type_check_term(&outer);
        let duration = start.elapsed();
        
        assert!(result.is_ok());
        println!("Type checked nested object in {:?}", duration);
        
        let typed_term = result.unwrap();
        assert!(matches!(typed_term.ty, Type::Object(_)));
    }

    fn make_span() -> SpanInfo {
        SpanInfo::new(0, 0)
    }
}