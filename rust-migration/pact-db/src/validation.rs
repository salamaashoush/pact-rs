//! Schema validation for database operations
//!
//! This module provides schema validation to ensure that database rows
//! conform to their table schemas, matching Pact's type system requirements.

use crate::traits::SchemaValidator;
use crate::types::{DbError, DbResult, RowData, TableName};
use pact_schema::{Field, PrimType, Schema, Type, Void};
use pact_values::PactValue;

/// Default schema validator implementation
#[derive(Debug, Clone)]
pub struct DefaultValidator {
    /// Whether to enforce strict type checking
    strict: bool,
}

impl DefaultValidator {
    /// Create a new validator with strict type checking
    pub fn new() -> Self {
        DefaultValidator { strict: true }
    }

    /// Create a validator with custom strictness
    pub fn with_strict(strict: bool) -> Self {
        DefaultValidator { strict }
    }

    /// Check if a PactValue matches a type
    fn value_matches_type(&self, value: &PactValue, expected_type: &Type<Void>) -> bool {
        match (value, expected_type) {
            // Exact primitive matches
            (PactValue::Integer(_), Type::Prim(PrimType::Integer)) => true,
            (PactValue::Decimal(_), Type::Prim(PrimType::Decimal)) => true,
            (PactValue::String(_), Type::Prim(PrimType::String)) => true,
            (PactValue::Bool(_), Type::Prim(PrimType::Bool)) => true,
            (PactValue::Time(_), Type::Prim(PrimType::Time)) => true,
            (PactValue::Guard(_), Type::Prim(PrimType::Guard)) => true,
            (PactValue::Keyset(_), Type::Prim(PrimType::Keyset)) => true,
            (PactValue::Unit, Type::Prim(PrimType::Unit)) => true,

            // Numeric compatibility (integer can be decimal in non-strict mode)
            (PactValue::Integer(_), Type::Prim(PrimType::Decimal)) if !self.strict => true,

            // List types
            (PactValue::List(items), Type::List(elem_type)) => items
                .iter()
                .all(|item| self.value_matches_type(item, elem_type)),

            // Object types - simplified validation for now
            (PactValue::Object(_), Type::Object(_)) => true,

            // Any type matches anything
            (_, Type::Any) => true,

            // Type variables match anything (should be instantiated at this point)
            (_, Type::Var(_)) => true,

            _ => false,
        }
    }

    /// Get the display name for a type
    fn type_display(&self, ty: &Type<Void>) -> String {
        match ty {
            Type::Prim(prim) => match prim {
                PrimType::Integer => "integer".to_string(),
                PrimType::Decimal => "decimal".to_string(),
                PrimType::String => "string".to_string(),
                PrimType::Bool => "bool".to_string(),
                PrimType::Time => "time".to_string(),
                PrimType::Guard => "guard".to_string(),
                PrimType::Keyset => "keyset".to_string(),
                PrimType::Unit => "unit".to_string(),
            },
            Type::List(elem_type) => format!("[{}]", self.type_display(elem_type)),
            Type::Object(_) => "object".to_string(),
            Type::Table(_) => "table".to_string(),
            Type::Fun(_, _) => "function".to_string(),
            Type::Nullary(_) => "nullary".to_string(),
            Type::Module(_) => "module".to_string(),
            Type::Cap => "capability".to_string(),
            Type::Any => "*".to_string(),
            Type::Var(v) => format!("'{:?}", v),
        }
    }

    /// Get the display name for a PactValue's type
    fn value_type_display(&self, value: &PactValue) -> String {
        match value {
            PactValue::Integer(_) => "integer".to_string(),
            PactValue::Decimal(_) => "decimal".to_string(),
            PactValue::String(_) => "string".to_string(),
            PactValue::Bool(_) => "bool".to_string(),
            PactValue::Time(_) => "time".to_string(),
            PactValue::Guard(_) => "guard".to_string(),
            PactValue::Keyset(_) => "keyset".to_string(),
            PactValue::List(_) => "list".to_string(),
            PactValue::Object(_) => "object".to_string(),
            PactValue::Unit => "unit".to_string(),
            PactValue::ModRef(_) => "module-ref".to_string(),
            PactValue::Closure(_) => "closure".to_string(),
            PactValue::CapToken(_) => "capability-token".to_string(),
        }
    }
}

impl Default for DefaultValidator {
    fn default() -> Self {
        Self::new()
    }
}

impl SchemaValidator for DefaultValidator {
    fn validate_row(&self, schema: &Schema, row: &RowData) -> DbResult<()> {
        // Check required fields first
        self.check_required_fields(schema, row)?;

        // Then validate field types
        self.validate_field_types(schema, row)?;

        Ok(())
    }

    fn check_required_fields(&self, schema: &Schema, row: &RowData) -> DbResult<()> {
        for (field, _field_type) in &schema.fields {
            if !row.data.contains_key(&field.0) {
                return Err(DbError::MissingField {
                    table: schema.name.render(),
                    key: "unknown".to_string(), // Key not available at this level
                    field: field.0.clone(),
                });
            }
        }
        Ok(())
    }

    fn validate_field_types(&self, schema: &Schema, row: &RowData) -> DbResult<()> {
        for (field_name, value) in &row.data {
            // Find the field in the schema
            let schema_field = Field(field_name.clone());
            if let Some(expected_type) = schema.fields.get(&schema_field) {
                if !self.value_matches_type(value, expected_type) {
                    return Err(DbError::TypeMismatch {
                        table: schema.name.render(),
                        key: "unknown".to_string(), // Key not available at this level
                        field: field_name.clone(),
                        expected: self.type_display(expected_type),
                        actual: self.value_type_display(value),
                    });
                }
            }
            // Note: Extra fields not in schema are allowed (Pact is flexible)
        }
        Ok(())
    }
}

/// Validate a row against a table with more context
pub fn validate_row_with_context(
    validator: &impl SchemaValidator,
    table: &TableName,
    key: &str,
    schema: &Schema,
    row: &RowData,
) -> DbResult<()> {
    validator.validate_row(schema, row).map_err(|e| match e {
        DbError::MissingField { field, .. } => DbError::MissingField {
            table: table.render(),
            key: key.to_string(),
            field,
        },
        DbError::TypeMismatch {
            field,
            expected,
            actual,
            ..
        } => DbError::TypeMismatch {
            table: table.render(),
            key: key.to_string(),
            field,
            expected,
            actual,
        },
        DbError::SchemaValidation { reason, .. } => DbError::SchemaValidation {
            table: table.render(),
            key: key.to_string(),
            reason,
        },
        other => other,
    })
}

/// Permissive validator that allows type coercion
#[derive(Debug, Clone)]
pub struct PermissiveValidator;

impl SchemaValidator for PermissiveValidator {
    fn validate_row(&self, schema: &Schema, row: &RowData) -> DbResult<()> {
        // Only check that required fields exist, don't validate types strictly
        for (field, _) in &schema.fields {
            if !row.data.contains_key(&field.0) {
                return Err(DbError::MissingField {
                    table: schema.name.render(),
                    key: "unknown".to_string(),
                    field: field.0.clone(),
                });
            }
        }
        Ok(())
    }

    fn check_required_fields(&self, schema: &Schema, row: &RowData) -> DbResult<()> {
        self.validate_row(schema, row)
    }

    fn validate_field_types(&self, _schema: &Schema, _row: &RowData) -> DbResult<()> {
        // Permissive validator doesn't check types
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pact_names::{ModuleName, QualifiedName};
    use std::collections::HashMap;

    fn create_test_schema() -> Schema {
        let mut fields = HashMap::new();
        fields.insert(Field("name".to_string()), Type::Prim(PrimType::String));
        fields.insert(Field("age".to_string()), Type::Prim(PrimType::Integer));
        fields.insert(Field("active".to_string()), Type::Prim(PrimType::Bool));

        Schema {
            name: QualifiedName::new(ModuleName::simple("test".to_string()), "User".to_string()),
            fields,
        }
    }

    #[test]
    fn test_valid_row_validation() {
        let validator = DefaultValidator::new();
        let schema = create_test_schema();

        let mut row_data = HashMap::new();
        row_data.insert("name".to_string(), PactValue::string("Alice"));
        row_data.insert("age".to_string(), PactValue::integer(30));
        row_data.insert("active".to_string(), PactValue::bool(true));

        let row = RowData::from_map(row_data);

        assert!(validator.validate_row(&schema, &row).is_ok());
    }

    #[test]
    fn test_missing_field_validation() {
        let validator = DefaultValidator::new();
        let schema = create_test_schema();

        let mut row_data = HashMap::new();
        row_data.insert("name".to_string(), PactValue::string("Alice"));
        // Missing "age" and "active" fields

        let row = RowData::from_map(row_data);

        let result = validator.validate_row(&schema, &row);
        assert!(result.is_err());

        match result.unwrap_err() {
            DbError::MissingField { field, .. } => {
                assert!(field == "age" || field == "active");
            }
            _ => panic!("Expected MissingField error"),
        }
    }

    #[test]
    fn test_type_mismatch_validation() {
        let validator = DefaultValidator::new();
        let schema = create_test_schema();

        let mut row_data = HashMap::new();
        row_data.insert("name".to_string(), PactValue::string("Alice"));
        row_data.insert("age".to_string(), PactValue::string("thirty")); // Wrong type
        row_data.insert("active".to_string(), PactValue::bool(true));

        let row = RowData::from_map(row_data);

        let result = validator.validate_row(&schema, &row);
        assert!(result.is_err());

        match result.unwrap_err() {
            DbError::TypeMismatch {
                field,
                expected,
                actual,
                ..
            } => {
                assert_eq!(field, "age");
                assert_eq!(expected, "integer");
                assert_eq!(actual, "string");
            }
            _ => panic!("Expected TypeMismatch error"),
        }
    }

    #[test]
    fn test_extra_fields_allowed() {
        let validator = DefaultValidator::new();
        let schema = create_test_schema();

        let mut row_data = HashMap::new();
        row_data.insert("name".to_string(), PactValue::string("Alice"));
        row_data.insert("age".to_string(), PactValue::integer(30));
        row_data.insert("active".to_string(), PactValue::bool(true));
        row_data.insert("email".to_string(), PactValue::string("alice@example.com")); // Extra field

        let row = RowData::from_map(row_data);

        // Extra fields should be allowed
        assert!(validator.validate_row(&schema, &row).is_ok());
    }

    #[test]
    fn test_numeric_compatibility() {
        let strict_validator = DefaultValidator::with_strict(true);
        let lenient_validator = DefaultValidator::with_strict(false);

        let mut fields = HashMap::new();
        fields.insert(Field("value".to_string()), Type::Prim(PrimType::Decimal));

        let schema = Schema {
            name: QualifiedName::new(ModuleName::simple("test".to_string()), "Number".to_string()),
            fields,
        };

        let mut row_data = HashMap::new();
        row_data.insert("value".to_string(), PactValue::integer(42)); // Integer for decimal field

        let row = RowData::from_map(row_data);

        // Strict validator should reject
        assert!(strict_validator.validate_row(&schema, &row).is_err());

        // Lenient validator should accept
        assert!(lenient_validator.validate_row(&schema, &row).is_ok());
    }

    #[test]
    fn test_permissive_validator() {
        let validator = PermissiveValidator;
        let schema = create_test_schema();

        let mut row_data = HashMap::new();
        row_data.insert("name".to_string(), PactValue::string("Alice"));
        row_data.insert("age".to_string(), PactValue::string("thirty")); // Wrong type
        row_data.insert("active".to_string(), PactValue::integer(1)); // Wrong type

        let row = RowData::from_map(row_data);

        // Permissive validator should accept (only checks field presence)
        assert!(validator.validate_row(&schema, &row).is_ok());
    }
}
