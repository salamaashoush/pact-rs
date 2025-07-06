/// Helper functions for database operations
/// Matches the Haskell database implementation patterns exactly

use crate::types::{TableSchema, ColumnType};
use pact_errors;

/// Convert TableSchema to Schema for error reporting
/// Matches the Haskell schema representation exactly
pub fn convert_table_schema_to_schema(table_schema: &TableSchema) -> pact_core::errors::Schema {
  let fields = table_schema.columns.iter().map(|col| {
    (
      pact_core::errors::Field(col.name.clone().into()),
      convert_column_type_to_type(&col.column_type)
    )
  }).collect();
  
  pact_core::errors::Schema { fields }
}

/// Convert ColumnType to Type for error reporting
fn convert_column_type_to_type(col_type: &ColumnType) -> pact_core::errors::Type {
  match col_type {
    ColumnType::String => pact_core::errors::Type::TyPrim(pact_core::errors::PrimType::PrimString),
    ColumnType::Integer => pact_core::errors::Type::TyPrim(pact_core::errors::PrimType::PrimInt),
    ColumnType::Decimal => pact_core::errors::Type::TyPrim(pact_core::errors::PrimType::PrimDecimal),
    ColumnType::Bool => pact_core::errors::Type::TyPrim(pact_core::errors::PrimType::PrimBool),
    ColumnType::Time => pact_core::errors::Type::TyPrim(pact_core::errors::PrimType::PrimTime),
    ColumnType::List => pact_core::errors::Type::TyAnyList,
    ColumnType::Object => pact_core::errors::Type::TyAnyObject,
    ColumnType::Guard => pact_core::errors::Type::TyPrim(pact_core::errors::PrimType::PrimGuard),
    ColumnType::Keyset => pact_core::errors::Type::TyKeyset,
    ColumnType::ModRef => pact_core::errors::Type::TyModRef(pact_core::errors::ModuleName("unknown".into())),
  }
}