# Pact LSP Configurable Formatting

The Pact LSP now supports configurable document formatting with the following options:

## Configuration Options

```haskell
data PactFormattingConfig = PactFormattingConfig
  { -- | Number of spaces for indentation (default: 2)
    pactIndentSize :: Int
  , -- | Maximum line width before wrapping (default: 80)
    pactMaxLineWidth :: Int
  , -- | Add blank lines between top-level definitions (default: True)
    pactSpaceBetweenDefinitions :: Bool
  , -- | Place closing parentheses on separate lines (default: False)
    pactClosingParensOnNewLine :: Bool
  , -- | Add blank lines inside modules between different definition types (default: True)
    pactSpaceInsideModules :: Bool
  }
```

## Features Implemented

### 1. **Spacing Between Definitions**
- `pactSpaceBetweenDefinitions = True`: Adds blank lines between top-level definitions
- `pactSpaceInsideModules = True`: Adds spacing between different definition types in modules

### 2. **Closing Parentheses Placement**
- `pactClosingParensOnNewLine = True`: Places closing parentheses on separate lines
- Creates the style shown in your example:

```pact
(module todos GOVERNANCE
  "A simple todos module"

  (defcap GOVERNANCE ()
    "A capability to administer the todos module"
    true
  )

  (defschema todo
    "A todo item"
    id:string
    title:string
    completed:bool
    deleted:bool
  )

  (defun create-todo:string (id:string title:string)
    "Create new todo with ID and TITLE."
    (insert todo-table id {
      "id": id,
      "title": title,
      "completed": false,
      "deleted": false
    })
  )
)
```

### 3. **Configurable Indentation and Line Width**
- `pactIndentSize`: Controls indentation spacing (default: 2 spaces)
- `pactMaxLineWidth`: Controls when lines wrap (default: 80 characters)

## Current Default Configuration

The LSP currently uses this configuration by default:

```haskell
defaultPactFormattingConfig = PactFormattingConfig
  { pactIndentSize = 2
  , pactMaxLineWidth = 80
  , pactSpaceBetweenDefinitions = True
  , pactClosingParensOnNewLine = True  -- Enabled by default
  , pactSpaceInsideModules = True
  }
```

## LSP Protocol Integration

The formatting is accessible through standard LSP methods:
- `textDocument/formatting` - Format entire document
- `textDocument/rangeFormatting` - Format selected range

## Testing

All formatting features are tested with:
- ✅ **Document formatting works**
- ✅ **Configurable formatting with spacing and closing parens**
- ✅ **Formatting invalid syntax gracefully**

## Technical Implementation

The formatter works by:
1. Parsing the Pact document using existing lexer/parser
2. Applying configurable post-processing rules:
   - Adding spacing between definition types
   - Moving closing parentheses to separate lines
   - Controlling indentation and line width
3. Using existing Pretty instances as the foundation
4. Gracefully handling parse errors by returning `null` (standard LSP behavior)

This provides a solid foundation for customizable Pact code formatting that respects developer preferences while maintaining consistency with Pact's established syntax conventions.