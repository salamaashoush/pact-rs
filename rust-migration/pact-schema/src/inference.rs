//! Hindley-Milner type inference with constraints
//!
//! This module implements a complete type inference system based on the
//! Hindley-Milner algorithm with support for:
//! - Type variables and unification
//! - Constraint collection and solving
//! - Type schemes and generalization
//! - Row types for structural typing
//! - Type classes and predicates

use crate::{Field, Pred, PrimType, RowTy, Type, TypeConstraint, TypeEnv, TypeScheme, TypeError};
use pact_errors::PactResult;
use pact_shared_types::SpanInfo;
use std::collections::{HashMap, HashSet};

/// Type variable generator for creating fresh type variables
#[derive(Debug, Clone)]
pub struct TypeVarGen {
    counter: usize,
}

impl TypeVarGen {
    /// Create a new type variable generator
    pub fn new() -> Self {
        TypeVarGen { counter: 0 }
    }

    /// Generate a fresh type variable
    pub fn fresh(&mut self) -> String {
        let var = format!("t{}", self.counter);
        self.counter += 1;
        var
    }

    /// Generate a fresh row variable
    pub fn fresh_row(&mut self) -> String {
        let var = format!("r{}", self.counter);
        self.counter += 1;
        var
    }
}

impl Default for TypeVarGen {
    fn default() -> Self {
        Self::new()
    }
}

/// Type substitution map
pub type Substitution = HashMap<String, Type<String>>;

/// Constraint solver state
#[derive(Debug, Clone)]
pub struct ConstraintSolver {
    /// Current substitution
    pub subst: Substitution,
    /// Remaining constraints to solve
    pub constraints: Vec<TypeConstraint<String>>,
}

impl ConstraintSolver {
    /// Create a new constraint solver
    pub fn new() -> Self {
        ConstraintSolver {
            subst: HashMap::new(),
            constraints: Vec::new(),
        }
    }

    /// Add a constraint to solve
    pub fn add_constraint(&mut self, constraint: TypeConstraint<String>) {
        self.constraints.push(constraint);
    }

    /// Solve all constraints and return the final substitution
    pub fn solve(&mut self) -> PactResult<Substitution, SpanInfo> {
        while let Some(constraint) = self.constraints.pop() {
            self.solve_constraint(constraint)?;
        }
        Ok(self.subst.clone())
    }

    /// Solve a single constraint
    fn solve_constraint(&mut self, constraint: TypeConstraint<String>) -> PactResult<(), SpanInfo> {
        match constraint {
            TypeConstraint::Equal(t1, t2) => {
                let unified = self.unify(&t1, &t2)?;
                self.apply_substitution(unified);
                Ok(())
            }
            TypeConstraint::Subtype(t1, t2) => {
                // For now, treat subtyping as equality
                // TODO: Implement proper subtyping rules
                let unified = self.unify(&t1, &t2)?;
                self.apply_substitution(unified);
                Ok(())
            }
            TypeConstraint::HasField(obj_ty, field, field_ty) => {
                self.solve_has_field_constraint(obj_ty, field, field_ty)
            }
            TypeConstraint::Class(predicate) => self.solve_class_constraint(predicate),
        }
    }

    /// Unify two types and return the substitution
    fn unify(&self, t1: &Type<String>, t2: &Type<String>) -> PactResult<Substitution, SpanInfo> {
        use Type::*;

        match (t1, t2) {
            // Same types unify trivially
            (Prim(p1), Prim(p2)) if p1 == p2 => Ok(HashMap::new()),
            (Cap, Cap) => Ok(HashMap::new()),
            (Any, _) | (_, Any) => Ok(HashMap::new()),

            // Variable unification
            (Var(v), t) | (t, Var(v)) => {
                if let Var(v2) = t {
                    if v == v2 {
                        return Ok(HashMap::new());
                    }
                }

                // Occurs check
                if self.occurs_check(v, t) {
                    return Err(TypeError::InfiniteType {
                        var: v.clone(),
                        ty: format!("{:?}", t),
                    }
                    .into());
                }

                let mut subst = HashMap::new();
                subst.insert(v.clone(), t.clone());
                Ok(subst)
            }

            // Function types
            (Fun(a1, r1), Fun(a2, r2)) => {
                let s1 = self.unify(a1, a2)?;
                let r1_subst = self.apply_type_substitution(&s1, r1);
                let r2_subst = self.apply_type_substitution(&s1, r2);
                let s2 = self.unify(&r1_subst, &r2_subst)?;
                Ok(self.compose_substitutions(&s1, &s2))
            }

            // Nullary functions
            (Nullary(r1), Nullary(r2)) => self.unify(r1, r2),

            // List types
            (List(t1), List(t2)) => self.unify(t1, t2),

            // Object types
            (Object(row1), Object(row2)) => self.unify_rows(row1, row2),

            // Table types
            (Table(row1), Table(row2)) => self.unify_rows(row1, row2),

            // Type mismatch
            _ => Err(TypeError::TypeMismatch {
                expected: format!("{:?}", t1),
                actual: format!("{:?}", t2),
            }
            .into()),
        }
    }

    /// Unify two row types
    fn unify_rows(&self, row1: &RowTy<String>, row2: &RowTy<String>) -> PactResult<Substitution, SpanInfo> {
        use RowTy::*;

        match (row1, row2) {
            // Row variables
            (RowVar(v1), RowVar(v2)) if v1 == v2 => Ok(HashMap::new()),
            (RowVar(v), row) | (row, RowVar(v)) => {
                // For now, simple substitution
                // TODO: Implement proper row unification with constraints
                let mut subst = HashMap::new();
                subst.insert(v.clone(), Type::Object(row.clone()));
                Ok(subst)
            }

            // Concrete rows
            (RowConcrete(fields1), RowConcrete(fields2)) => {
                if fields1.len() != fields2.len() {
                    let mut all_fields = Vec::new();
                    all_fields.extend(fields1.keys().map(|f| f.0.clone()));
                    all_fields.extend(fields2.keys().map(|f| f.0.clone()));
                    return Err(TypeError::RowMismatchFields {
                        fields: all_fields,
                    }
                    .into());
                }

                let mut combined_subst = HashMap::new();

                for (field, ty1) in fields1 {
                    if let Some(ty2) = fields2.get(field) {
                        let field_subst = self.unify(ty1, ty2)?;
                        combined_subst = self.compose_substitutions(&combined_subst, &field_subst);
                    } else {
                        return Err(TypeError::MissingField {
                            field: field.0.clone(),
                        }
                        .into());
                    }
                }

                Ok(combined_subst)
            }
        }
    }

    /// Occurs check to prevent infinite types
    fn occurs_check(&self, var: &str, ty: &Type<String>) -> bool {
        use Type::*;

        match ty {
            Var(v) => v == var,
            Fun(arg, ret) => self.occurs_check(var, arg) || self.occurs_check(var, ret),
            Nullary(ret) => self.occurs_check(var, ret),
            List(elem) => self.occurs_check(var, elem),
            Object(row) => self.occurs_check_row(var, row),
            Table(row) => self.occurs_check_row(var, row),
            _ => false,
        }
    }

    /// Occurs check for row types
    fn occurs_check_row(&self, var: &str, row: &RowTy<String>) -> bool {
        use RowTy::*;

        match row {
            RowVar(v) => v == var,
            RowConcrete(fields) => fields.values().any(|ty| self.occurs_check(var, ty)),
        }
    }

    /// Apply a substitution to a type
    pub fn apply_type_substitution(&self, subst: &Substitution, ty: &Type<String>) -> Type<String> {
        use Type::*;

        match ty {
            Var(v) => {
                if let Some(replacement) = subst.get(v) {
                    // Recursively apply substitution to handle chains
                    self.apply_type_substitution(subst, replacement)
                } else {
                    ty.clone()
                }
            }
            Fun(arg, ret) => Fun(
                Box::new(self.apply_type_substitution(subst, arg)),
                Box::new(self.apply_type_substitution(subst, ret)),
            ),
            Nullary(ret) => Nullary(Box::new(self.apply_type_substitution(subst, ret))),
            List(elem) => List(Box::new(self.apply_type_substitution(subst, elem))),
            Object(row) => Object(self.apply_row_substitution(subst, row)),
            Table(row) => Table(self.apply_row_substitution(subst, row)),
            _ => ty.clone(),
        }
    }

    /// Apply substitution to row types
    fn apply_row_substitution(&self, subst: &Substitution, row: &RowTy<String>) -> RowTy<String> {
        use RowTy::*;

        match row {
            RowVar(_v) => {
                // TODO: Handle row variable substitutions properly
                row.clone()
            }
            RowConcrete(fields) => {
                let new_fields = fields
                    .iter()
                    .map(|(field, ty)| (field.clone(), self.apply_type_substitution(subst, ty)))
                    .collect();
                RowConcrete(new_fields)
            }
        }
    }

    /// Compose two substitutions
    fn compose_substitutions(&self, s1: &Substitution, s2: &Substitution) -> Substitution {
        let mut result = s1.clone();

        // Apply s2 to the range of s1
        for (_var, ty) in result.iter_mut() {
            *ty = self.apply_type_substitution(s2, ty);
        }

        // Add bindings from s2 that are not in s1
        for (var, ty) in s2 {
            if !result.contains_key(var) {
                result.insert(var.clone(), ty.clone());
            }
        }

        result
    }

    /// Apply the substitution to current state
    fn apply_substitution(&mut self, subst: Substitution) {
        // Update current substitution
        self.subst = self.compose_substitutions(&self.subst, &subst);

        // Apply to remaining constraints
        self.constraints = self
            .constraints
            .iter()
            .map(|constraint| self.apply_constraint_substitution(&subst, constraint))
            .collect();
    }

    /// Apply substitution to a constraint
    fn apply_constraint_substitution(
        &self,
        subst: &Substitution,
        constraint: &TypeConstraint<String>,
    ) -> TypeConstraint<String> {
        use TypeConstraint::*;

        match constraint {
            Equal(t1, t2) => Equal(
                self.apply_type_substitution(subst, t1),
                self.apply_type_substitution(subst, t2),
            ),
            Subtype(t1, t2) => Subtype(
                self.apply_type_substitution(subst, t1),
                self.apply_type_substitution(subst, t2),
            ),
            HasField(obj_ty, field, field_ty) => HasField(
                self.apply_type_substitution(subst, obj_ty),
                field.clone(),
                self.apply_type_substitution(subst, field_ty),
            ),
            Class(predicate) => Class(self.apply_predicate_substitution(subst, predicate)),
        }
    }

    /// Apply substitution to a predicate
    fn apply_predicate_substitution(&self, subst: &Substitution, predicate: &Pred) -> Pred {
        use crate::TypeClass::*;

        match predicate {
            Eq(ty) => Eq(self.apply_type_substitution(subst, ty)),
            Ord(ty) => Ord(self.apply_type_substitution(subst, ty)),
            Show(ty) => Show(self.apply_type_substitution(subst, ty)),
            Add(ty) => Add(self.apply_type_substitution(subst, ty)),
            Num(ty) => Num(self.apply_type_substitution(subst, ty)),
            ListLike(ty) => ListLike(self.apply_type_substitution(subst, ty)),
            Fractional(ty) => Fractional(self.apply_type_substitution(subst, ty)),
            EnforceRead(ty) => EnforceRead(self.apply_type_substitution(subst, ty)),
            IsValue(ty) => IsValue(self.apply_type_substitution(subst, ty)),
            // TODO: Handle row predicates
            RoseSubRow(r1, r2) => RoseSubRow(r1.clone(), r2.clone()),
            RoseRowEq(r1, r2) => RoseRowEq(r1.clone(), r2.clone()),
        }
    }

    /// Solve HasField constraint
    fn solve_has_field_constraint(
        &mut self,
        obj_ty: Type<String>,
        field: Field,
        field_ty: Type<String>,
    ) -> PactResult<(), SpanInfo> {
        match obj_ty {
            Type::Var(v) => {
                // Create a fresh row variable and constrain the object
                let _row_var = format!("{}_row", v);
                let mut fields = HashMap::new();
                fields.insert(field, field_ty);
                let concrete_row = RowTy::RowConcrete(fields);

                let mut subst = HashMap::new();
                subst.insert(v, Type::Object(concrete_row));
                self.apply_substitution(subst);
                Ok(())
            }
            Type::Object(row) => {
                match row {
                    RowTy::RowVar(row_var) => {
                        // Create concrete row with this field
                        let mut fields = HashMap::new();
                        fields.insert(field, field_ty);
                        let concrete_row = RowTy::RowConcrete(fields);

                        let mut subst = HashMap::new();
                        subst.insert(row_var, Type::Object(concrete_row));
                        self.apply_substitution(subst);
                        Ok(())
                    }
                    RowTy::RowConcrete(fields) => {
                        if let Some(existing_field_ty) = fields.get(&field) {
                            // Field exists, unify types
                            let unified = self.unify(existing_field_ty, &field_ty)?;
                            self.apply_substitution(unified);
                            Ok(())
                        } else {
                            Err(TypeError::MissingField { field: field.0 }.into())
                        }
                    }
                }
            }
            _ => Err(TypeError::TypeMismatch {
                expected: "object".to_string(),
                actual: format!("{:?}", obj_ty),
            }
            .into()),
        }
    }

    /// Solve type class constraint
    fn solve_class_constraint(&mut self, predicate: Pred) -> PactResult<(), SpanInfo> {
        use crate::TypeClass::*;

        match &predicate {
            Eq(ty) | Show(ty) => {
                // Most types support Eq and Show
                match ty {
                    Type::Var(_) => {
                        // Leave as constraint for later resolution
                        self.add_constraint(TypeConstraint::Class(predicate));
                        Ok(())
                    }
                    _ => Ok(()), // Assume all concrete types support Eq/Show
                }
            }
            Ord(ty) => {
                // Only certain types support Ord
                match ty {
                    Type::Prim(PrimType::Integer)
                    | Type::Prim(PrimType::Decimal)
                    | Type::Prim(PrimType::String) => Ok(()),
                    Type::Var(_) => {
                        self.add_constraint(TypeConstraint::Class(predicate));
                        Ok(())
                    }
                    _ => Err(TypeError::NoInstance {
                        class_name: "Ord".to_string(),
                        ty: format!("{:?}", ty),
                    }
                    .into()),
                }
            }
            Add(ty) | Num(ty) => {
                // Only numeric types support Add/Num
                match ty {
                    Type::Prim(PrimType::Integer) | Type::Prim(PrimType::Decimal) => Ok(()),
                    Type::Var(_) => {
                        self.add_constraint(TypeConstraint::Class(predicate));
                        Ok(())
                    }
                    _ => Err(TypeError::NoInstance {
                        class_name: "Num".to_string(),
                        ty: format!("{:?}", ty),
                    }
                    .into()),
                }
            }
            ListLike(ty) => match ty {
                Type::List(_) | Type::Prim(PrimType::String) => Ok(()),
                Type::Var(_) => {
                    self.add_constraint(TypeConstraint::Class(predicate));
                    Ok(())
                }
                _ => Err(TypeError::NoInstance {
                    class_name: "ListLike".to_string(),
                    ty: format!("{:?}", ty),
                }
                .into()),
            },
            Fractional(ty) => match ty {
                Type::Prim(PrimType::Decimal) => Ok(()),
                Type::Var(_) => {
                    self.add_constraint(TypeConstraint::Class(predicate));
                    Ok(())
                }
                _ => Err(TypeError::NoInstance {
                    class_name: "Fractional".to_string(),
                    ty: format!("{:?}", ty),
                }
                .into()),
            },
            _ => {
                // For other predicates, assume they're satisfiable for now
                Ok(())
            }
        }
    }
}

impl Default for ConstraintSolver {
    fn default() -> Self {
        Self::new()
    }
}

/// Type inferencer for expressions
#[derive(Debug)]
pub struct TypeInferencer {
    /// Type variable generator
    var_gen: TypeVarGen,
    /// Constraint solver
    solver: ConstraintSolver,
}

impl TypeInferencer {
    /// Create a new type inferencer
    pub fn new() -> Self {
        TypeInferencer {
            var_gen: TypeVarGen::new(),
            solver: ConstraintSolver::new(),
        }
    }

    /// Generate a fresh type variable
    pub fn fresh_var(&mut self) -> Type<String> {
        Type::Var(self.var_gen.fresh())
    }

    /// Generate a fresh row variable
    pub fn fresh_row_var(&mut self) -> RowTy<String> {
        RowTy::RowVar(self.var_gen.fresh_row())
    }

    /// Add an equality constraint
    pub fn constrain_equal(&mut self, t1: Type<String>, t2: Type<String>) {
        self.solver.add_constraint(TypeConstraint::Equal(t1, t2));
    }

    /// Add a type class constraint
    pub fn constrain_class(&mut self, predicate: Pred) {
        self.solver.add_constraint(TypeConstraint::Class(predicate));
    }

    /// Add a has-field constraint
    pub fn constrain_has_field(
        &mut self,
        obj_ty: Type<String>,
        field: Field,
        field_ty: Type<String>,
    ) {
        self.solver
            .add_constraint(TypeConstraint::HasField(obj_ty, field, field_ty));
    }

    /// Solve all constraints and return the substitution
    pub fn solve(&mut self) -> PactResult<Substitution, SpanInfo> {
        self.solver.solve()
    }

    /// Generalize a type by quantifying over free variables
    pub fn generalize(&self, env: &TypeEnv<String>, ty: &Type<String>) -> TypeScheme<String> {
        let env_vars = self.collect_env_type_vars(env);
        let ty_vars = self.collect_type_vars(ty);
        let free_vars: Vec<String> = ty_vars.difference(&env_vars).cloned().collect();

        TypeScheme {
            type_vars: free_vars,
            predicates: vec![], // TODO: Collect relevant predicates
            body: ty.clone(),
        }
    }

    /// Instantiate a type scheme with fresh variables
    pub fn instantiate(&mut self, scheme: &TypeScheme<String>) -> Type<String> {
        let mut subst = HashMap::new();

        // Create fresh variables for each quantified variable
        for var in &scheme.type_vars {
            subst.insert(var.clone(), self.fresh_var());
        }

        // Apply substitution to body
        self.solver.apply_type_substitution(&subst, &scheme.body)
    }

    /// Collect type variables from a type
    fn collect_type_vars(&self, ty: &Type<String>) -> HashSet<String> {
        use Type::*;

        match ty {
            Var(v) => {
                let mut vars = HashSet::new();
                vars.insert(v.clone());
                vars
            }
            Fun(arg, ret) => {
                let mut vars = self.collect_type_vars(arg);
                vars.extend(self.collect_type_vars(ret));
                vars
            }
            Nullary(ret) => self.collect_type_vars(ret),
            List(elem) => self.collect_type_vars(elem),
            Object(row) => self.collect_row_type_vars(row),
            Table(row) => self.collect_row_type_vars(row),
            _ => HashSet::new(),
        }
    }

    /// Collect type variables from a row type
    fn collect_row_type_vars(&self, row: &RowTy<String>) -> HashSet<String> {
        use RowTy::*;

        match row {
            RowVar(v) => {
                let mut vars = HashSet::new();
                vars.insert(v.clone());
                vars
            }
            RowConcrete(fields) => {
                let mut vars = HashSet::new();
                for ty in fields.values() {
                    vars.extend(self.collect_type_vars(ty));
                }
                vars
            }
        }
    }

    /// Collect type variables from environment
    fn collect_env_type_vars(&self, env: &TypeEnv<String>) -> HashSet<String> {
        let mut vars = HashSet::new();

        for scheme in env.vars.values() {
            vars.extend(self.collect_type_vars(&scheme.body));
        }

        vars
    }
}

impl Default for TypeInferencer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fresh_variables() {
        let mut gen = TypeVarGen::new();
        let var1 = gen.fresh();
        let var2 = gen.fresh();

        assert_ne!(var1, var2);
        assert!(var1.starts_with('t'));
        assert!(var2.starts_with('t'));
    }

    #[test]
    fn test_unify_primitives() {
        let solver = ConstraintSolver::new();
        let int_ty = Type::Prim(PrimType::Integer);
        let string_ty = Type::Prim(PrimType::String);

        // Same types should unify
        let result = solver.unify(&int_ty, &int_ty);
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());

        // Different types should fail
        let result = solver.unify(&int_ty, &string_ty);
        assert!(result.is_err());
    }

    #[test]
    fn test_unify_variables() {
        let solver = ConstraintSolver::new();
        let var_ty = Type::Var("a".to_string());
        let int_ty = Type::Prim(PrimType::Integer);

        let result = solver.unify(&var_ty, &int_ty).unwrap();
        assert_eq!(result.len(), 1);
        assert_eq!(result.get("a"), Some(&int_ty));
    }

    #[test]
    fn test_unify_functions() {
        let solver = ConstraintSolver::new();
        let fun1 = Type::Fun(
            Box::new(Type::Prim(PrimType::Integer)),
            Box::new(Type::Prim(PrimType::String)),
        );
        let fun2 = Type::Fun(
            Box::new(Type::Prim(PrimType::Integer)),
            Box::new(Type::Prim(PrimType::String)),
        );

        let result = solver.unify(&fun1, &fun2);
        assert!(result.is_ok());
    }

    #[test]
    fn test_occurs_check() {
        let solver = ConstraintSolver::new();
        let var_ty = Type::Var("a".to_string());
        let recursive_ty = Type::Fun(Box::new(var_ty.clone()), Box::new(var_ty.clone()));

        let result = solver.unify(&var_ty, &recursive_ty);
        assert!(result.is_err());
    }

    #[test]
    fn test_constraint_solving() {
        let mut solver = ConstraintSolver::new();
        let var_a = Type::Var("a".to_string());
        let var_b = Type::Var("b".to_string());
        let int_ty = Type::Prim(PrimType::Integer);

        solver.add_constraint(TypeConstraint::Equal(var_a.clone(), int_ty.clone()));
        solver.add_constraint(TypeConstraint::Equal(var_b.clone(), var_a.clone()));

        let subst = solver.solve().unwrap();
        assert_eq!(subst.get("a"), Some(&int_ty));
        assert_eq!(subst.get("b"), Some(&int_ty));
    }

    #[test]
    fn test_type_generalization() {
        let inferencer = TypeInferencer::new();
        let env = TypeEnv::new();
        let ty = Type::Fun(
            Box::new(Type::Var("a".to_string())),
            Box::new(Type::Var("a".to_string())),
        );

        let scheme = inferencer.generalize(&env, &ty);
        assert_eq!(scheme.type_vars.len(), 1);
        assert!(scheme.type_vars.contains(&"a".to_string()));
    }

    #[test]
    fn test_type_instantiation() {
        let mut inferencer = TypeInferencer::new();
        let scheme = TypeScheme {
            type_vars: vec!["a".to_string()],
            predicates: vec![],
            body: Type::Fun(
                Box::new(Type::Var("a".to_string())),
                Box::new(Type::Var("a".to_string())),
            ),
        };

        let instance1 = inferencer.instantiate(&scheme);
        let instance2 = inferencer.instantiate(&scheme);

        // Should create fresh variables each time
        assert_ne!(format!("{:?}", instance1), format!("{:?}", instance2));
    }
}
