/// Helper functions for database operations
/// Matches the Haskell database implementation patterns exactly

use crate::types::{TableSchema, ColumnType};
use pact_errors;

/// Convert TableSchema to Schema for error reporting
/// Matches the Haskell schema representation exactly
pub fn convert_table_schema_to_schema(table_schema: &TableSchema) -> pact_errors::Schema {
  let fields = table_schema.columns.iter().map(|col| {
    (
      pact_errors::Field(col.name.clone().into()),
      convert_column_type_to_type(&col.column_type)
    )
  }).collect();
  
  pact_errors::Schema { fields }
}

/// Convert ColumnType to Type for error reporting
fn convert_column_type_to_type(col_type: &ColumnType) -> pact_errors::Type {
  match col_type {
    ColumnType::String => pact_errors::Type::TyPrim(pact_errors::PrimType::PrimString),
    ColumnType::Integer => pact_errors::Type::TyPrim(pact_errors::PrimType::PrimInt),
    ColumnType::Decimal => pact_errors::Type::TyPrim(pact_errors::PrimType::PrimDecimal),
    ColumnType::Bool => pact_errors::Type::TyPrim(pact_errors::PrimType::PrimBool),
    ColumnType::Time => pact_errors::Type::TyPrim(pact_errors::PrimType::PrimTime),
    ColumnType::List => pact_errors::Type::TyAnyList,
    ColumnType::Object => pact_errors::Type::TyAnyObject,
    ColumnType::Guard => pact_errors::Type::TyPrim(pact_errors::PrimType::PrimGuard),
    ColumnType::Keyset => pact_errors::Type::TyKeyset,
    ColumnType::ModRef => pact_errors::Type::TyModRef(pact_errors::ModuleName("unknown".into())),
  }
}