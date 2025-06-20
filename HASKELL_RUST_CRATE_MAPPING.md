# Haskell-to-Rust Crate Mapping Analysis

**Date**: December 2024  
**Current Rust Crates**: 23  
**Haskell Libraries**: 4  
**Over-segmentation**: 5.75x

## Executive Summary

The Rust migration has severe **over-segmentation** compared to the proven Haskell architecture. Haskell successfully implements the entire Pact system with just **4 libraries**, while Rust has fragmented it into **23 crates**. This creates massive code duplication, compilation issues, and maintenance complexity.

## Haskell Reference Architecture (4 Libraries)

Based on `pact-tng.cabal` analysis:

### 1. **Main Library (`pact-tng`)** - 73 modules
**Purpose**: Core language implementation

**Key Modules**:
- `Pact.Core.Compile` - Compilation orchestration
- `Pact.Core.Builtin` - All builtin functions  
- `Pact.Core.Names` - Name system
- `Pact.Core.PactValue` - Value types
- `Pact.Core.Errors` - Error system
- `Pact.Core.Gas.*` - Gas metering (3 modules)
- `Pact.Core.Persistence.*` - Database layer (4 modules)
- **Syntax**: `Pact.Core.Syntax.*` (4 modules) - Lexer, Parser, ParseTree
- **IR**: `Pact.Core.IR.*` (7 modules) - Term, Desugar, ModuleHashing, ConstEval
- **Evaluation**: `Pact.Core.IR.Eval.*` (8 modules) - CEK and Runtime evaluators
- **Supporting**: Capabilities, Guards, Hash, Type, etc.

### 2. **REPL Library (`pact-repl`)** - 12 modules  
**Purpose**: REPL and direct evaluator

**Key Modules**:
- `Pact.Core.Repl` - Main REPL implementation
- `Pact.Core.Repl.Compile` - REPL compilation
- `Pact.Core.IR.Eval.Direct.*` - Direct evaluator (4 modules)
- `Pact.Core.Typed.*` - Type system (3 modules)

### 3. **LSP Library (`pact-lsp`)** - 3 modules
**Purpose**: Language Server Protocol

**Key Modules**:
- `Pact.Core.LanguageServer` - Main LSP implementation
- `Pact.Core.LanguageServer.Utils` - LSP utilities  
- `Pact.Core.LanguageServer.Renaming` - Symbol renaming

### 4. **Request API Library (`pact-request-api`)** - 11 modules
**Purpose**: HTTP API and command processing

**Key Modules**:
- `Pact.Core.Command.*` - Command processing (7 modules)
- `Pact.Core.Crypto.WebAuthn.*` - WebAuthn support (4 modules)

### 5. **Crypto Library (`pact-crypto`)** - 5 modules
**Purpose**: Cryptographic primitives

**Key Modules**:
- Hash functions (Poseidon, Keccak256)
- Pairing operations
- Base64 encoding

## Current Rust Over-Segmentation (23 Crates)

### Critical Analysis by Category:

#### **Foundation Types** - 7 Rust crates vs 1 Haskell section
❌ **Over-segmented**: Should be in one core library

**Rust Crates**:
- `pact-values` ↔ `Pact.Core.PactValue` 
- `pact-names` ↔ `Pact.Core.Names`
- `pact-errors` ↔ `Pact.Core.Errors`
- `pact-math` ↔ *Embedded in PactValue*
- `pact-gas` ↔ `Pact.Core.Gas.*`
- `pact-shared-types` ↔ *No equivalent (artificial separation)*
- `pact-capability` ↔ `Pact.Core.Capabilities`

**Haskell**: All in main `pact-tng` library

#### **Syntax** - 2 Rust crates vs 1 Haskell section  
⚠️ **Slightly over-segmented**

**Rust Crates**:
- `pact-lexer` ↔ `Pact.Core.Syntax.Lexer`
- `pact-parser` ↔ `Pact.Core.Syntax.Parser`

**Haskell**: Both in `Pact.Core.Syntax.*` (4 modules total)

#### **IR and Compilation** - 4 Rust crates vs 1 Haskell section
❌ **Over-segmented**: Should be in main library

**Rust Crates**:
- `pact-ir` ↔ `Pact.Core.IR.*`
- `pact-eval` ↔ `Pact.Core.IR.ConstEval`
- `pact-compiler` ↔ `Pact.Core.Compile`
- `pact-schema` ↔ `Pact.Core.Type`

**Haskell**: All in main `pact-tng` library

#### **Evaluation** - 1 Rust crate ✅ **Correct**
- `pact-cek` ↔ `Pact.Core.IR.Eval.CEK.*`

#### **Database** - 1 Rust crate ✅ **Correct**  
- `pact-db` ↔ `Pact.Core.Persistence.*`

#### **Applications** - 3 Rust crates vs 2 Haskell libraries
⚠️ **Slightly over-segmented**

**Rust Crates**:
- `pact-repl` ↔ `pact-repl` library ✅
- `pact-lsp` ↔ `pact-lsp` library ✅  
- `pact-cli` ↔ *Main executable (no library)*

#### **API/Server** - 2 Rust crates vs 1 Haskell library
⚠️ **Slightly over-segmented**

**Rust Crates**:
- `pact-request-api` ↔ `pact-request-api` library ✅
- `pact-server` ↔ *Part of pact-request-api*

#### **Supporting** - 3 Rust crates
❌ **Unnecessary/Empty**

**Rust Crates**:
- `pact-tests` ↔ *No equivalent (empty)*
- `pact-modules` ↔ *Embedded in main library*
- `pact-crypto` ↔ `pact-crypto` library ✅

## Consolidation Recommendations

### Target Architecture: 9 Crates (Down from 23)

#### **Core Foundation** - 1 crate (merge 7)
**`pact-core`** - Merge:
- ✅ `pact-values` (canonical value types)
- ✅ `pact-names` (name system)  
- ✅ `pact-errors` (error hierarchy)
- ✅ `pact-gas` (gas metering)
- ✅ `pact-capability` (capability system)
- ❌ `pact-math` (duplicate of pact-values numeric)
- ❌ `pact-shared-types` (artificial separation)

#### **Language Processing** - 2 crates (merge 2)
**`pact-syntax`** - Merge:
- ✅ `pact-lexer` 
- ✅ `pact-parser`

**`pact-ir`** - Keep but absorb:
- ✅ `pact-eval` (constant evaluation)
- ✅ `pact-schema` (type system)
- ⚠️ `pact-compiler` (fix compilation issues first)

#### **Evaluation** - 1 crate ✅
**`pact-cek`** - Keep as-is

#### **Storage** - 1 crate ✅
**`pact-db`** - Keep as-is  

#### **Applications** - 2 crates
**`pact-repl`** - Keep as-is ✅
**`pact-cli`** - Keep as-is ✅

#### **Integration** - 2 crates (keep LSP separate)
**`pact-server`** - Merge:
- ✅ `pact-request-api`

**`pact-lsp`** - Keep separate ✅
- Distinct application layer
- Benefits from independent development

#### **Crypto** - 1 crate ✅
**`pact-crypto`** - Keep as-is

#### **Remove Completely**:
- ❌ `pact-tests` (2 lines, empty)
- ❌ `pact-modules` (functionality in core)

## Implementation Priority

### Phase 1: Critical Fixes (Week 1)
- **Fix `pact-compiler` compilation issues** 
- **Merge error types** to resolve CEK compilation

### Phase 2: Foundation Consolidation (Week 2)
- **Create `pact-core`** by merging 7 foundation crates
- **Remove duplicate math/shared-types**

### Phase 3: Language Layer (Week 3)  
- **Create `pact-syntax`** by merging lexer + parser
- **Consolidate IR layer**

### Phase 4: Final Cleanup (Week 4)
- **Remove empty crates**
- **Optimize dependencies**

## Success Metrics

**Before**: 23 crates, compilation errors, massive duplication  
**After**: 9 crates, matches Haskell architecture, clean compilation

**Benefits**:
- ✅ **2.5x reduction** in crate count (23 → 9)
- ✅ **Matches proven Haskell architecture** 
- ✅ **Eliminates code duplication**
- ✅ **Faster compilation** (fewer dependency edges)
- ✅ **Easier maintenance** (follows reference implementation)
- ✅ **Clear boundaries** (matches conceptual organization)

This consolidation directly follows the proven Haskell architecture while maintaining the performance benefits of the Rust implementation.