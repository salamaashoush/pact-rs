# VS Code Pact Formatting Configuration Example

## Quick Setup

### 1. Create Workspace Settings

Create `.vscode/settings.json` in your Pact project:

```json
{
  "pact.languageServer.command": "cabal",
  "pact.languageServer.args": ["run", "exe:pact", "--", "--lsp"],
  
  // NEW: Configure Pact formatting behavior
  "pact": {
    "formatting": {
      "indentSize": 2,
      "maxLineWidth": 80,
      "spaceBetweenDefinitions": true,
      "closingParensOnNewLine": true,
      "spaceInsideModules": true
    }
  },
  
  "[pact]": {
    "editor.formatOnSave": true,
    "editor.formatOnPaste": true,
    "editor.tabSize": 2,
    "editor.insertSpaces": true,
    "editor.defaultFormatter": "pact-lsp"
  }
}
```

### 2. Available Formatting Options

The Pact LSP now reads configuration from workspace settings! Here are the available options:

- **`indentSize`**: Number of spaces for indentation (default: 2)
- **`maxLineWidth`**: Maximum line width before wrapping (default: 80)
- **`spaceBetweenDefinitions`**: Add blank lines between top-level definitions (default: true)
- **`closingParensOnNewLine`**: Place closing parentheses on separate lines (default: true)
- **`spaceInsideModules`**: Add spacing between different definition types in modules (default: true)

With the default settings, the formatter produces this style:

```pact
(namespace 'free)

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

  (deftable todo-table:{todo})

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

## Current Default Settings

The formatter currently uses these defaults:

- ✅ **Indent Size**: 2 spaces (follows `editor.tabSize`)
- ✅ **Max Line Width**: 80 characters
- ✅ **Space Between Definitions**: `true` (adds blank lines between top-level definitions)
- ✅ **Closing Parens on New Line**: `true` (places closing parens on separate lines)
- ✅ **Space Inside Modules**: `true` (adds spacing between different definition types)

## Usage

### Format Document
- **Keyboard**: `Shift+Alt+F` (Windows/Linux) or `Shift+Option+F` (Mac)
- **Command Palette**: `Format Document`
- **Right-click**: Context menu → `Format Document`

### Format Selection
- Select code
- **Keyboard**: `Ctrl+K Ctrl+F` (Windows/Linux) or `Cmd+K Cmd+F` (Mac)
- **Command Palette**: `Format Selection`

### Auto-Format
With `"editor.formatOnSave": true`, your code will be automatically formatted when you save.

## Testing Your Setup

1. Create a test file `test.pact`:
```pact
(namespace 'test)(module test-module GOVERNANCE "Test" (defcap GOVERNANCE () true)(defun test-function (x:integer) (+ x 1))(defconst TEST_CONST 42))
```

2. Save the file - it should automatically format to:
```pact
(namespace 'test)

(module test-module GOVERNANCE
  "Test"

  (defcap GOVERNANCE ()
    true
  )

  (defun test-function (x:integer)
    (+ x 1)
  )

  (defconst TEST_CONST 42)
)
```

## Example: Custom Configuration

Want different formatting? Customize it in your workspace settings:

```json
{
  "pact": {
    "formatting": {
      "indentSize": 4,
      "maxLineWidth": 120,
      "spaceBetweenDefinitions": false,
      "closingParensOnNewLine": false,
      "spaceInsideModules": true
    }
  }
}
```

This will format code more compactly with 4-space indentation:

```pact
(namespace 'free)
(module todos GOVERNANCE
    "A simple todos module"
    (defcap GOVERNANCE () "A capability to administer the todos module" true)
    (defschema todo "A todo item" id:string title:string completed:bool deleted:bool)
    (deftable todo-table:{todo})
    (defun create-todo:string (id:string title:string) "Create new todo with ID and TITLE." (insert todo-table id {"id": id, "title": title, "completed": false, "deleted": false})))
```

## Troubleshooting

### Formatting Not Working?

1. **Check LSP is running**: Look for "Pact Language Server" in the Output panel
2. **Verify file type**: Ensure file has `.pact` extension
3. **Check for syntax errors**: Fix any parse errors first
4. **Restart VS Code**: Sometimes needed after configuration changes

### LSP Server Issues?

Enable debug logging:
```json
{
  "pact.trace.server": "verbose"
}
```

Then check the "Pact Language Server" output for formatting requests.

## Example Project Structure

```
my-pact-project/
├── .vscode/
│   └── settings.json
├── contracts/
│   ├── token.pact
│   └── governance.pact
└── tests/
    └── token-tests.repl
```

With this setup, all your Pact files will be consistently formatted with the new style!