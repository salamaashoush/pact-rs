# Pact Parser Architecture

This document provides a comprehensive overview of the Pact parser's architecture, design decisions, and implementation details.

## Design Philosophy

The Pact parser is built with the following core principles:

1. **Haskell Compatibility**: 100% AST-level compatibility with the original implementation
2. **Performance**: Single-pass parsing with minimal allocations
3. **Error Recovery**: Robust error handling for better developer experience
4. **Maintainability**: Clear, readable code that matches the grammar structure
5. **Extensibility**: Architecture that supports future language evolution

## Overall Architecture

```
Token Stream (from Lexer)
        ↓
  Recursive Descent Parser
        ↓
    AST Construction
        ↓
   SpanInfo Preservation
        ↓
   Validated AST Output
```

### Parser Pipeline

1. **Token Consumption**: Read tokens from lexer with position tracking
2. **Grammar Recognition**: Apply recursive descent parsing rules
3. **AST Construction**: Build typed AST nodes with embedded information
4. **Error Handling**: Detect and recover from syntax errors
5. **Span Preservation**: Maintain source location for all nodes

## Core Components

### 1. Parser State (`Parser` struct)

```rust
pub struct Parser {
    tokens: VecDeque<(Token, SpanInfo)>,
}
```

**Design Decisions:**

- **VecDeque**: Efficient front removal for token consumption
- **SpanInfo embedding**: Source positions tracked for every token
- **Single state**: No separate lexer state, parser owns token stream
- **No backtracking**: Recursive descent without lookahead buffering

**Key Operations:**

```rust
impl Parser {
    fn advance(&mut self) -> Option<(Token, SpanInfo)>
    fn peek(&self) -> Option<&(Token, SpanInfo)>
    fn check(&self, expected: &Token) -> bool
    fn consume(&mut self, expected: Token) -> Result<SpanInfo>
    fn is_at_end(&self) -> bool
}
```

### 2. AST Types (`ast.rs`)

The AST is designed to mirror Haskell's ParseTree exactly:

```rust
// Parameterized by Info type for span information
pub enum ParsedExpr<Info> {
    Constant(Literal, Info),
    Var(ParsedName, Info),
    App(ParsedExpr<Info>, Vec<ParsedExpr<Info>>, Info),
    Conditional(ParsedExpr<Info>, ParsedExpr<Info>, ParsedExpr<Info>, Info),
    Lambda(Vec<(Arg<Info>, Info)>, ParsedExpr<Info>, Info),
    Let(Vec<(Arg<Info>, ParsedExpr<Info>, Info)>, ParsedExpr<Info>, Info),
    // ... additional variants
}
```

**Key Design Features:**

- **Generic Info parameter**: Supports different annotation types
- **Embedded spans**: Every AST node has source location
- **Recursive structure**: Mirrors the language's recursive nature
- **Type preservation**: All type annotations captured in AST

### 3. Error Handling (`error.rs`)

```rust
pub enum ParseError {
    UnexpectedToken { expected: String, found: String, position: usize },
    UnexpectedEOF { expected: String },
    InvalidNumber(String),
    Expected(String),
    LexError(String),
}
```

**Error Recovery Strategy:**

1. **Panic mode**: Skip tokens until synchronization point
2. **Multiple errors**: Continue parsing to find additional issues
3. **Context preservation**: Maintain partial AST for IDE support
4. **Detailed messages**: Specific error descriptions with suggestions

## Parsing Strategy

### Recursive Descent

The parser uses hand-written recursive descent following the grammar structure:

```rust
// Each grammar rule becomes a method
fn parse_program(&mut self) -> Result<Program<SpanInfo>>
fn parse_top_level(&mut self) -> Result<ParsedTopLevel<SpanInfo>>
fn parse_module(&mut self) -> Result<ParsedModule<SpanInfo>>
fn parse_expression(&mut self) -> Result<ParsedExpr<SpanInfo>>
```

**Advantages:**

- **Clear mapping**: Grammar rules → parser methods
- **Easy debugging**: Straightforward control flow
- **Good error messages**: Natural error reporting points
- **Performance**: No parser generator overhead

**Grammar Structure:**

```
Program     := TopLevel*
TopLevel    := Module | Interface | Use | Expression
Module      := '(' 'module' Name Governance Def* ')'
Expression  := Atom | Application | Conditional | Lambda | Let
```

### Expression Parsing

Expressions use precedence climbing for operators:

```rust
fn parse_expression(&mut self) -> Result<ParsedExpr<SpanInfo>> {
    self.parse_s_expr() // S-expression based, not operator precedence
}

fn parse_s_expr(&mut self) -> Result<ParsedExpr<SpanInfo>> {
    if self.check(&Token::TokenOpenParen) {
        self.parse_application()
    } else {
        self.parse_atom()
    }
}
```

**Pact-specific considerations:**

- **S-expression syntax**: Lisp-style prefix notation
- **No operator precedence**: All operators are function calls
- **Uniform application**: `(+ 1 2)` not `1 + 2`

### Top-Level Parsing

Top-level constructs are parsed based on leading keywords:

```rust
fn parse_top_level(&mut self) -> Result<ParsedTopLevel<SpanInfo>> {
    if self.check(&Token::TokenOpenParen) {
        self.advance();
        
        match self.peek() {
            Some((Token::TokenModule, _)) => self.parse_module_inner(),
            Some((Token::TokenInterface, _)) => self.parse_interface_inner(),
            Some((Token::TokenUse, _)) => self.parse_use_inner(),
            _ => self.parse_s_expr(), // Expression
        }
    } else {
        Err(ParseError::Expected("top-level construct".to_string()))
    }
}
```

## Module System Architecture

### Module Structure

```rust
pub struct ParsedModule<Info> {
    pub name: ParsedName,
    pub governance: Governance,
    pub docs: Option<String>,
    pub definitions: Vec<ParsedDef<Info>>,
    pub info: Info,
}
```

### Governance Types

```rust
pub enum Governance {
    KeyGov(ParsedName),    // "keyset-name"
    CapGov(ParsedName),    // CAPABILITY-NAME
}
```

### Definition Types

```rust
pub enum ParsedDef<Info> {
    Defun(ParsedDefun<Info>),
    Defcap(ParsedDefcap<Info>),
    Defschema(ParsedDefschema<Info>),
    Deftable(ParsedDeftable<Info>),
    Defconst(ParsedDefconst<Info>),
    Defpact(ParsedDefpact<Info>),
}
```

## Expression Architecture

### Application Parsing

Function applications are the core of Pact syntax:

```rust
fn parse_application(&mut self) -> Result<ParsedExpr<SpanInfo>> {
    let start = self.consume(Token::TokenOpenParen)?;
    
    if self.check(&Token::TokenCloseParen) {
        // Empty application is an error
        return Err(ParseError::Expected("function or expression".to_string()));
    }
    
    let func = self.parse_expression()?;
    let mut args = Vec::new();
    
    while !self.check(&Token::TokenCloseParen) && !self.is_at_end() {
        args.push(self.parse_expression()?);
    }
    
    let end = self.consume(Token::TokenCloseParen)?;
    let info = SpanInfo::new(start.start, end.end);
    
    Ok(ParsedExpr::App(Box::new(func), args, info))
}
```

**Special Forms:**

Special forms are recognized by function name and parsed specially:

```rust
match func {
    ParsedExpr::Var(name, _) if name.as_str() == "if" => {
        self.parse_conditional_args(args, info)
    }
    ParsedExpr::Var(name, _) if name.as_str() == "lambda" => {
        self.parse_lambda_args(args, info)
    }
    ParsedExpr::Var(name, _) if name.as_str() == "let" => {
        self.parse_let_args(args, info)
    }
    _ => Ok(ParsedExpr::App(Box::new(func), args, info))
}
```

### Literal Parsing

Literals are converted from tokens to AST nodes:

```rust
fn parse_atom(&mut self) -> Result<ParsedExpr<SpanInfo>> {
    if let Some((token, span)) = self.advance() {
        match token {
            Token::TokenInteger(n) => {
                let value = n.parse::<i128>()
                    .map_err(|_| ParseError::InvalidNumber("Invalid integer".to_string()))?;
                Ok(ParsedExpr::Constant(Literal::LInteger(value), span))
            }
            Token::TokenDecimal(n) => {
                self.parse_decimal(n, span)
            }
            Token::TokenString(s) => {
                Ok(ParsedExpr::Constant(Literal::LString(s), span))
            }
            Token::TokenTrue => {
                Ok(ParsedExpr::Constant(Literal::LBool(true), span))
            }
            Token::TokenFalse => {
                Ok(ParsedExpr::Constant(Literal::LBool(false), span))
            }
            Token::TokenIdent(name) => {
                Ok(ParsedExpr::Var(ParsedName::new(name), span))
            }
            _ => Err(ParseError::UnexpectedToken {
                expected: "expression".to_string(),
                found: format!("{:?}", token),
                position: span.start,
            })
        }
    } else {
        Err(ParseError::UnexpectedEOF {
            expected: "expression".to_string(),
        })
    }
}
```

## Type System Integration

### Type Annotations

Type annotations are parsed and embedded in the AST:

```rust
pub struct Arg<Info> {
    pub name: ParsedName,
    pub ty: Option<ParsedType>,
    pub info: Info,
}

fn parse_arg(&mut self) -> Result<(Arg<SpanInfo>, SpanInfo)> {
    let name_span = self.peek().unwrap().1.clone();
    let name = self.parse_name()?;
    
    if self.check(&Token::TokenColon) {
        self.advance(); // consume ':'
        let ty = self.parse_type()?;
        let info = SpanInfo::new(name_span.start, ty.span().end);
        Ok((Arg { name, ty: Some(ty), info: info.clone() }, info))
    } else {
        Ok((Arg { name, ty: None, info: name_span.clone() }, name_span))
    }
}
```

### Type Parsing

```rust
fn parse_type(&mut self) -> Result<ParsedType> {
    if let Some((token, span)) = self.advance() {
        match token {
            Token::TokenIdent(name) => {
                match name.as_str() {
                    "integer" => Ok(ParsedType::TyPrim(PrimType::TyInteger, span)),
                    "decimal" => Ok(ParsedType::TyPrim(PrimType::TyDecimal, span)),
                    "string" => Ok(ParsedType::TyPrim(PrimType::TyString, span)),
                    "bool" => Ok(ParsedType::TyPrim(PrimType::TyBool, span)),
                    "time" => Ok(ParsedType::TyPrim(PrimType::TyTime, span)),
                    "guard" => Ok(ParsedType::TyPrim(PrimType::TyGuard, span)),
                    _ => Ok(ParsedType::TyUser(ParsedName::new(name), span)),
                }
            }
            _ => Err(ParseError::Expected("type".to_string()))
        }
    } else {
        Err(ParseError::UnexpectedEOF { expected: "type".to_string() })
    }
}
```

## Error Recovery Architecture

### Synchronization Points

The parser defines synchronization points for error recovery:

```rust
fn synchronize(&mut self) {
    while !self.is_at_end() {
        match self.peek() {
            Some((Token::TokenOpenParen, _)) => return, // Start of new expression
            Some((Token::TokenDefun, _)) => return,     // Start of function
            Some((Token::TokenDefcap, _)) => return,    // Start of capability
            Some((Token::TokenModule, _)) => return,    // Start of module
            _ => { self.advance(); }                     // Skip token
        }
    }
}
```

### Error Collection

Multiple errors are collected during parsing:

```rust
struct ParseResult<T> {
    result: Option<T>,
    errors: Vec<ParseError>,
}

impl Parser {
    fn parse_with_recovery(&mut self) -> ParseResult<Program<SpanInfo>> {
        let mut errors = Vec::new();
        let mut items = Vec::new();
        
        while !self.is_at_end() {
            match self.parse_top_level() {
                Ok(item) => items.push(item),
                Err(err) => {
                    errors.push(err);
                    self.synchronize();
                }
            }
        }
        
        ParseResult {
            result: if items.is_empty() { None } else { Some(items) },
            errors,
        }
    }
}
```

## Performance Optimizations

### Memory Management

1. **Boxed recursive types**: Reduce enum size for better cache performance
2. **String interning**: Share common identifiers (future optimization)
3. **Arena allocation**: Bulk allocation for AST nodes (future optimization)
4. **Span compression**: Efficient position representation

### Parsing Optimizations

1. **Single-pass**: No separate AST transformation phases
2. **Minimal copying**: References where possible
3. **Early returns**: Fast paths for common cases
4. **Token buffering**: VecDeque for efficient consumption

### Example Optimizations

```rust
// Efficient token consumption
#[inline]
fn advance(&mut self) -> Option<(Token, SpanInfo)> {
    self.tokens.pop_front()
}

// Fast token checking
#[inline]
fn check(&self, expected: &Token) -> bool {
    self.tokens.front().map_or(false, |(token, _)| 
        std::mem::discriminant(token) == std::mem::discriminant(expected)
    )
}

// Minimal string allocation
fn parse_name(&mut self) -> Result<ParsedName> {
    if let Some((Token::TokenIdent(name), span)) = self.advance() {
        Ok(ParsedName::new(name)) // String moved, not copied
    } else {
        Err(ParseError::Expected("identifier".to_string()))
    }
}
```

## Testing Architecture

### Test Organization

```
tests/
├── comprehensive_parser_tests.rs    # Complete language coverage
├── debug_*.rs                      # Debugging specific issues
├── error_recovery_tests.rs         # Error handling verification
├── performance_tests.rs             # Performance regression tests
└── haskell_compatibility_tests.rs  # Cross-implementation verification
```

### Test Strategy

1. **Grammar coverage**: Every production rule tested
2. **Error coverage**: All error conditions tested
3. **Edge cases**: Boundary conditions and corner cases
4. **Performance**: Regression testing for speed
5. **Compatibility**: Parity with Haskell implementation

### Example Test Structure

```rust
#[test]
fn test_function_definition() {
    let test_cases = vec![
        // Simple function
        "(defun add (x y) (+ x y))",
        
        // With type annotations
        "(defun add (x:integer y:integer):integer (+ x y))",
        
        // With documentation
        "(defun add (x y) @doc \"Add numbers\" (+ x y))",
        
        // With properties
        "(defun add (x y) @model [(property (>= result x))] (+ x y))",
    ];
    
    for case in test_cases {
        let result = Parser::parse_expr_from_source(case);
        assert!(result.is_ok(), "Failed to parse: {} - {:?}", case, result.unwrap_err());
    }
}
```

## Integration Architecture

### Lexer Integration

```rust
impl Parser {
    pub fn parse_program_from_source(source: &str) -> Result<Program<SpanInfo>> {
        let tokens = lex_with_spans(source)
            .map_err(|e| ParseError::LexError(e.message))?;
        
        let mut parser = Parser::new(tokens);
        parser.parse_program()
    }
}
```

### Error Propagation

```rust
// Automatic conversion from lexer errors
impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        ParseError::LexError(err.message)
    }
}

// Result type alias for convenience
pub type Result<T> = std::result::Result<T, ParseError>;
```

### Future Integration Points

1. **Desugarer**: AST → IR transformation
2. **Type checker**: AST annotation with type information
3. **Pretty printer**: AST → source code generation
4. **IDE services**: Symbol resolution, completion, hover
5. **Macro expander**: Preprocessor integration

## Compatibility with Haskell

### AST Equivalence

Every Haskell AST node has a direct Rust equivalent:

| Haskell | Rust | Mapping |
|---------|------|---------|
| `Expr` | `ParsedExpr<Info>` | 1:1 |
| `TopLevel` | `ParsedTopLevel<Info>` | 1:1 |
| `Def` | `ParsedDef<Info>` | 1:1 |
| `Module` | `ParsedModule<Info>` | 1:1 |
| `Literal` | `Literal` | 1:1 |

### Grammar Compatibility

The parser follows identical grammar rules:

1. **Same precedence**: Operator precedence matches exactly
2. **Same associativity**: Left/right associativity preserved
3. **Same keywords**: All keywords recognized identically
4. **Same special forms**: if, let, lambda handled the same way
5. **Same module system**: Module, interface, use syntax identical

### Semantic Preservation

1. **Scoping**: Variable binding follows same rules
2. **Type annotations**: Identical type syntax support
3. **Documentation**: Same @doc and @model handling
4. **Capabilities**: Identical capability syntax

### Testing Compatibility

```rust
// Property: Same source should produce equivalent ASTs
#[test]
fn test_haskell_compatibility() {
    let test_cases = load_haskell_test_cases();
    
    for (source, expected_tokens) in test_cases {
        let rust_result = Parser::parse_program_from_source(&source);
        let haskell_result = call_haskell_parser(&source);
        
        assert_eq!(
            normalize_ast(rust_result), 
            normalize_ast(haskell_result),
            "AST mismatch for source: {}", source
        );
    }
}
```

## Future Enhancements

### Planned Improvements

1. **Incremental Parsing**:
   ```rust
   pub fn reparse_range(&mut self, range: Range<usize>, new_tokens: Vec<Token>) -> ParseResult
   ```

2. **Better Error Messages**:
   ```rust
   // Current: "Expected ')'"
   // Future: "Missing closing parenthesis for function call starting at line 3"
   ```

3. **Macro Support**:
   ```rust
   pub fn parse_with_macros(&mut self, macro_env: &MacroEnv) -> Result<Program<SpanInfo>>
   ```

4. **Stream Parsing**:
   ```rust
   pub fn parse_stream(tokens: impl Iterator<Item = Token>) -> impl Iterator<Item = ParseResult<TopLevel>>
   ```

### Performance Enhancements

1. **Arena allocation**: Bulk AST node allocation
2. **String interning**: Reduce identifier memory usage
3. **Parallel parsing**: Split large files across threads
4. **Memoization**: Cache parsing results for repeated patterns

### Language Extensions

1. **Custom operators**: User-defined operators
2. **Syntax macros**: Compile-time code generation
3. **Type-directed parsing**: Context-sensitive syntax
4. **Documentation extraction**: Rich documentation metadata

This architecture provides a robust foundation for the Pact parser that balances performance, maintainability, and compatibility with the existing Haskell implementation while providing room for future enhancements.