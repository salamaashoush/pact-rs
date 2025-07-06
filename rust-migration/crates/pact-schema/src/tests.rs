//! Comprehensive tests for pact-schema
//!
//! Tests cover all type constructors, type operations, and schema functionality.

use crate::*;
use std::collections::{HashMap, HashSet};

#[cfg(test)]
mod type_tests {
    use super::*;

    #[test]
    fn test_primitive_types() {
        let types = vec![
            PrimType::Integer,
            PrimType::Decimal,
            PrimType::String,
            PrimType::Bool,
            PrimType::Time,
            PrimType::Guard,
            PrimType::Keyset,
            PrimType::Unit,
        ];

        for prim in types {
            let ty = Type::<String>::Prim(prim.clone());
            let display = format!("{}", ty);
            match prim {
                PrimType::Integer => assert_eq!(display, "integer"),
                PrimType::Decimal => assert_eq!(display, "decimal"),
                PrimType::String => assert_eq!(display, "string"),
                PrimType::Bool => assert_eq!(display, "bool"),
                PrimType::Time => assert_eq!(display, "time"),
                PrimType::Guard => assert_eq!(display, "guard"),
                PrimType::Keyset => assert_eq!(display, "keyset"),
                PrimType::Unit => assert_eq!(display, "unit"),
            }
        }
    }

    #[test]
    fn test_type_constructors() {
        // Function type
        let fun_ty = Type::<String>::Fun(
            Box::new(Type::Prim(PrimType::String)),
            Box::new(Type::Prim(PrimType::Integer)),
        );
        assert_eq!(format!("{}", fun_ty), "(string -> integer)");

        // Nullary function
        let nullary = Type::<String>::Nullary(Box::new(Type::Prim(PrimType::Bool)));
        assert_eq!(format!("{}", nullary), "(() -> bool)");

        // List type
        let list_ty = Type::<String>::List(Box::new(Type::Prim(PrimType::Integer)));
        assert_eq!(format!("{}", list_ty), "[integer]");

        // Nested list
        let nested_list =
            Type::<String>::List(Box::new(Type::List(Box::new(Type::Prim(PrimType::String)))));
        assert_eq!(format!("{}", nested_list), "[[string]]");
    }

    #[test]
    fn test_type_variables() {
        let var_a = Type::<String>::Var("a".to_string());
        let var_b = Type::<String>::Var("b".to_string());

        assert_eq!(format!("{}", var_a), "a");
        assert_eq!(format!("{}", var_b), "b");

        // Function with type variables
        let poly_fun = Type::<String>::Fun(Box::new(var_a.clone()), Box::new(var_b.clone()));
        assert_eq!(format!("{}", poly_fun), "(a -> b)");
    }

    #[test]
    fn test_special_types() {
        let cap = Type::<String>::Cap;
        let any = Type::<String>::Any;

        assert_eq!(format!("{}", cap), "capability");
        assert_eq!(format!("{}", any), "any");
    }

    #[test]
    fn test_complex_types() {
        // List of functions
        let list_of_funs = Type::<String>::List(Box::new(Type::Fun(
            Box::new(Type::Prim(PrimType::Integer)),
            Box::new(Type::Prim(PrimType::Bool)),
        )));
        assert_eq!(format!("{}", list_of_funs), "[(integer -> bool)]");

        // Function returning list
        let fun_returning_list = Type::<String>::Fun(
            Box::new(Type::Prim(PrimType::String)),
            Box::new(Type::List(Box::new(Type::Prim(PrimType::Integer)))),
        );
        assert_eq!(format!("{}", fun_returning_list), "(string -> [integer])");
    }
}

#[cfg(test)]
mod row_type_tests {
    use super::*;

    #[test]
    fn test_row_concrete() {
        let mut fields = HashMap::new();
        fields.insert(
            Field("name".to_string()),
            Type::<String>::Prim(PrimType::String),
        );
        fields.insert(
            Field("age".to_string()),
            Type::<String>::Prim(PrimType::Integer),
        );

        let row = RowTy::RowConcrete(fields.clone());

        match row {
            RowTy::RowConcrete(row_fields) => {
                assert_eq!(row_fields.len(), 2);
                assert_eq!(
                    row_fields.get(&Field("name".to_string())),
                    Some(&Type::Prim(PrimType::String))
                );
                assert_eq!(
                    row_fields.get(&Field("age".to_string())),
                    Some(&Type::Prim(PrimType::Integer))
                );
            }
            _ => panic!("Expected RowConcrete"),
        }
    }

    #[test]
    fn test_row_variable() {
        let row_var = RowTy::<String>::RowVar("r".to_string());

        match row_var {
            RowTy::RowVar(var) => assert_eq!(var, "r"),
            _ => panic!("Expected RowVar"),
        }
    }

    #[test]
    fn test_object_type() {
        let mut fields = HashMap::new();
        fields.insert(
            Field("x".to_string()),
            Type::<String>::Prim(PrimType::Decimal),
        );
        fields.insert(
            Field("y".to_string()),
            Type::<String>::Prim(PrimType::Decimal),
        );

        let obj_ty = Type::Object(RowTy::RowConcrete(fields));
        assert_eq!(format!("{}", obj_ty), "object");
    }

    #[test]
    fn test_table_type() {
        let mut fields = HashMap::new();
        fields.insert(
            Field("id".to_string()),
            Type::<String>::Prim(PrimType::Integer),
        );
        fields.insert(
            Field("data".to_string()),
            Type::<String>::Prim(PrimType::String),
        );

        let table_ty = Type::Table(RowTy::RowConcrete(fields));
        assert_eq!(format!("{}", table_ty), "table");
    }
}

#[cfg(test)]
mod module_ref_tests {
    use super::*;

    #[test]
    fn test_module_ref_var() {
        let mref_var = MRef::<String>::MRefVar("m".to_string());

        match mref_var {
            MRef::MRefVar(var) => assert_eq!(var, "m"),
            _ => panic!("Expected MRefVar"),
        }
    }

    #[test]
    fn test_module_ref_concrete() {
        let mut modules = HashSet::new();
        modules.insert(ModuleName::simple("my-module"));
        modules.insert(ModuleName::simple("other-module"));

        let mref: MRef<String> = MRef::MConcrete(modules.clone());

        match mref {
            MRef::MConcrete(mods) => {
                assert_eq!(mods.len(), 2);
                assert!(mods.contains(&ModuleName::simple("my-module")));
                assert!(mods.contains(&ModuleName::simple("other-module")));
            }
            _ => panic!("Expected MConcrete"),
        }
    }

    #[test]
    fn test_module_type() {
        let mut modules = HashSet::new();
        modules.insert(ModuleName::simple("coin"));

        let mod_ty = Type::<String>::Module(MRef::MConcrete(modules));
        assert_eq!(format!("{}", mod_ty), "module");
    }
}

#[cfg(test)]
mod schema_tests {
    use super::*;

    #[test]
    fn test_schema_creation() {
        let mut fields = HashMap::new();
        fields.insert(
            Field("account".to_string()),
            Type::<Void>::Prim(PrimType::String),
        );
        fields.insert(
            Field("balance".to_string()),
            Type::<Void>::Prim(PrimType::Decimal),
        );
        fields.insert(
            Field("guard".to_string()),
            Type::<Void>::Prim(PrimType::Guard),
        );

        let schema = Schema {
            name: QualifiedName::new(ModuleName::simple("coin"), "account-schema".to_string()),
            fields: fields.clone(),
        };

        assert_eq!(schema.fields.len(), 3);
        assert_eq!(
            schema.fields.get(&Field("account".to_string())),
            Some(&Type::<Void>::Prim(PrimType::String))
        );
        assert_eq!(
            schema.fields.get(&Field("balance".to_string())),
            Some(&Type::<Void>::Prim(PrimType::Decimal))
        );
        assert_eq!(
            schema.fields.get(&Field("guard".to_string())),
            Some(&Type::<Void>::Prim(PrimType::Guard))
        );
    }

    #[test]
    fn test_schema_with_complex_fields() {
        let mut fields = HashMap::new();

        // List field
        fields.insert(
            Field("items".to_string()),
            Type::<Void>::List(Box::new(Type::Prim(PrimType::String))),
        );

        // Nested object field (using Any since we can't have nested schemas)
        fields.insert(Field("metadata".to_string()), Type::<Void>::Any);

        let schema = Schema {
            name: QualifiedName::new(ModuleName::simple("schema"), "complex-schema".to_string()),
            fields,
        };

        assert_eq!(schema.fields.len(), 2);

        match schema.fields.get(&Field("items".to_string())) {
            Some(Type::List(elem)) => {
                assert_eq!(**elem, Type::<Void>::Prim(PrimType::String));
            }
            _ => panic!("Expected List type for items field"),
        }
    }
}

#[cfg(test)]
mod type_scheme_tests {
    use super::*;

    #[test]
    fn test_monomorphic_scheme() {
        let scheme = TypeScheme {
            type_vars: vec![],
            predicates: vec![],
            body: Type::<String>::Prim(PrimType::Integer),
        };

        assert!(scheme.type_vars.is_empty());
        assert!(scheme.predicates.is_empty());
        assert_eq!(scheme.body, Type::<String>::Prim(PrimType::Integer));
    }

    #[test]
    fn test_polymorphic_scheme() {
        // forall a. a -> a (identity function)
        let scheme = TypeScheme {
            type_vars: vec!["a".to_string()],
            predicates: vec![],
            body: Type::Fun(
                Box::new(Type::Var("a".to_string())),
                Box::new(Type::Var("a".to_string())),
            ),
        };

        assert_eq!(scheme.type_vars.len(), 1);
        assert_eq!(scheme.type_vars[0], "a");

        match &scheme.body {
            Type::Fun(arg, ret) => {
                assert_eq!(**arg, Type::Var("a".to_string()));
                assert_eq!(**ret, Type::Var("a".to_string()));
            }
            _ => panic!("Expected Fun type"),
        }
    }

    #[test]
    fn test_scheme_with_constraints() {
        // forall a. Num a => a -> a -> a (addition-like)
        let scheme = TypeScheme {
            type_vars: vec!["a".to_string()],
            predicates: vec![TypeClass::Num(Type::Var("a".to_string()))],
            body: Type::Fun(
                Box::new(Type::Var("a".to_string())),
                Box::new(Type::Fun(
                    Box::new(Type::Var("a".to_string())),
                    Box::new(Type::Var("a".to_string())),
                )),
            ),
        };

        assert_eq!(scheme.type_vars.len(), 1);
        assert_eq!(scheme.predicates.len(), 1);
        match &scheme.predicates[0] {
            TypeClass::Num(Type::Var(v)) => assert_eq!(v, "a"),
            _ => panic!("Expected TypeClass::Num with type variable 'a'"),
        }
    }

    #[test]
    fn test_multiple_type_vars() {
        // forall a b. a -> b -> a (const function)
        let scheme = TypeScheme {
            type_vars: vec!["a".to_string(), "b".to_string()],
            predicates: vec![],
            body: Type::Fun(
                Box::new(Type::Var("a".to_string())),
                Box::new(Type::Fun(
                    Box::new(Type::Var("b".to_string())),
                    Box::new(Type::Var("a".to_string())),
                )),
            ),
        };

        assert_eq!(scheme.type_vars.len(), 2);
        assert_eq!(scheme.type_vars[0], "a");
        assert_eq!(scheme.type_vars[1], "b");
    }
}

#[cfg(test)]
mod type_var_tests {
    use super::*;

    #[test]
    fn test_type_var_creation() {
        let ty_var = TypeVar {
            var: "a".to_string(),
            kind: PactKind::TyKind,
        };

        assert_eq!(ty_var.var, "a");
        assert_eq!(ty_var.kind, PactKind::TyKind);
    }

    #[test]
    fn test_different_kinds() {
        let type_var = TypeVar {
            var: "t".to_string(),
            kind: PactKind::TyKind,
        };

        let row_var = TypeVar {
            var: "r".to_string(),
            kind: PactKind::RowKind,
        };

        let modref_var = TypeVar {
            var: "m".to_string(),
            kind: PactKind::ModRefKind,
        };

        assert_eq!(type_var.kind, PactKind::TyKind);
        assert_eq!(row_var.kind, PactKind::RowKind);
        assert_eq!(modref_var.kind, PactKind::ModRefKind);
    }
}

#[cfg(test)]
mod field_tests {
    use super::*;

    #[test]
    fn test_field_display() {
        let field = Field("my-field".to_string());
        assert_eq!(format!("{}", field), "my-field");
    }

    #[test]
    fn test_field_equality() {
        let field1 = Field("name".to_string());
        let field2 = Field("name".to_string());
        let field3 = Field("age".to_string());

        assert_eq!(field1, field2);
        assert_ne!(field1, field3);
    }

    #[test]
    fn test_field_as_hashmap_key() {
        let mut map = HashMap::new();
        map.insert(Field("key1".to_string()), "value1");
        map.insert(Field("key2".to_string()), "value2");

        assert_eq!(map.get(&Field("key1".to_string())), Some(&"value1"));
        assert_eq!(map.get(&Field("key2".to_string())), Some(&"value2"));
        assert_eq!(map.get(&Field("key3".to_string())), None);
    }
}

#[cfg(test)]
mod type_equality_tests {
    use super::*;

    #[test]
    fn test_primitive_equality() {
        assert_eq!(
            Type::<String>::Prim(PrimType::Integer),
            Type::Prim(PrimType::Integer)
        );
        assert_ne!(
            Type::<String>::Prim(PrimType::Integer),
            Type::Prim(PrimType::String)
        );
    }

    #[test]
    fn test_complex_type_equality() {
        let fun1 = Type::<String>::Fun(
            Box::new(Type::Prim(PrimType::String)),
            Box::new(Type::Prim(PrimType::Integer)),
        );

        let fun2 = Type::<String>::Fun(
            Box::new(Type::Prim(PrimType::String)),
            Box::new(Type::Prim(PrimType::Integer)),
        );

        let fun3 = Type::<String>::Fun(
            Box::new(Type::Prim(PrimType::Integer)),
            Box::new(Type::Prim(PrimType::String)),
        );

        assert_eq!(fun1, fun2);
        assert_ne!(fun1, fun3);
    }

    #[test]
    fn test_nested_type_equality() {
        let nested1 = Type::<String>::List(Box::new(Type::List(Box::new(Type::Prim(
            PrimType::Integer,
        )))));
        let nested2 = Type::<String>::List(Box::new(Type::List(Box::new(Type::Prim(
            PrimType::Integer,
        )))));
        let nested3 =
            Type::<String>::List(Box::new(Type::List(Box::new(Type::Prim(PrimType::String)))));

        assert_eq!(nested1, nested2);
        assert_ne!(nested1, nested3);
    }
}

#[cfg(test)]
mod serialization_tests {
    use super::*;

    #[test]
    fn test_primitive_type_serialization() {
        let prim = PrimType::Integer;
        let serialized = serde_json::to_string(&prim).unwrap();
        let deserialized: PrimType = serde_json::from_str(&serialized).unwrap();
        assert_eq!(prim, deserialized);
    }

    #[test]
    fn test_type_serialization() {
        let ty = Type::<String>::Fun(
            Box::new(Type::Prim(PrimType::String)),
            Box::new(Type::List(Box::new(Type::Prim(PrimType::Integer)))),
        );

        let serialized = serde_json::to_string(&ty).unwrap();
        let deserialized: Type<String> = serde_json::from_str(&serialized).unwrap();
        assert_eq!(ty, deserialized);
    }

    #[test]
    fn test_schema_serialization() {
        let mut fields = HashMap::new();
        fields.insert(
            Field("name".to_string()),
            Type::<Void>::Prim(PrimType::String),
        );
        fields.insert(
            Field("age".to_string()),
            Type::<Void>::Prim(PrimType::Integer),
        );

        let schema = Schema {
            name: QualifiedName::new(ModuleName::simple("schema"), "person".to_string()),
            fields,
        };

        let serialized = serde_json::to_string(&schema).unwrap();
        let deserialized: Schema = serde_json::from_str(&serialized).unwrap();

        assert_eq!(schema.name, deserialized.name);
        assert_eq!(schema.fields.len(), deserialized.fields.len());
    }

    #[test]
    fn test_type_scheme_serialization() {
        let scheme = TypeScheme {
            type_vars: vec!["a".to_string(), "b".to_string()],
            predicates: vec![
                TypeClass::Num(Type::Var("a".to_string())),
                TypeClass::Ord(Type::Var("a".to_string())),
            ],
            body: Type::Fun(
                Box::new(Type::Var("a".to_string())),
                Box::new(Type::Var("b".to_string())),
            ),
        };

        let serialized = serde_json::to_string(&scheme).unwrap();
        let deserialized: TypeScheme<String> = serde_json::from_str(&serialized).unwrap();

        assert_eq!(scheme, deserialized);
    }
}
