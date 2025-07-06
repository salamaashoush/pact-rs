# Pact LSP Formatting Configuration Guide

## VS Code Configuration

### Method 1: Workspace Settings (Recommended)

Create or edit `.vscode/settings.json` in your project root:

```json
{
  "pact.formatting.indentSize": 2,
  "pact.formatting.maxLineWidth": 80,
  "pact.formatting.spaceBetweenDefinitions": true,
  "pact.formatting.closingParensOnNewLine": true,
  "pact.formatting.spaceInsideModules": true,
  
  // VS Code editor settings for Pact files
  "[pact]": {
    "editor.formatOnSave": true,
    "editor.formatOnPaste": true,
    "editor.tabSize": 2,
    "editor.insertSpaces": true
  }
}
```

### Method 2: User Settings

Open VS Code Settings (Ctrl+,) and search for "pact formatting" or edit `settings.json`:

```json
{
  "pact.formatting": {
    "indentSize": 2,
    "maxLineWidth": 80,
    "spaceBetweenDefinitions": true,
    "closingParensOnNewLine": true,
    "spaceInsideModules": true
  }
}
```

### Method 3: Language Server Configuration

If using a generic LSP client, configure via `initializationOptions`:

```json
{
  "languageserver": {
    "pact": {
      "command": "pact",
      "args": ["--lsp"],
      "filetypes": ["pact"],
      "initializationOptions": {
        "formatting": {
          "indentSize": 2,
          "maxLineWidth": 80,
          "spaceBetweenDefinitions": true,
          "closingParensOnNewLine": true,
          "spaceInsideModules": true
        }
      }
    }
  }
}
```

## Neovim Configuration

### Using nvim-lspconfig

```lua
require'lspconfig'.pact.setup{
  cmd = {"pact", "--lsp"},
  filetypes = {"pact"},
  root_dir = require'lspconfig.util'.find_git_ancestor,
  init_options = {
    formatting = {
      indentSize = 2,
      maxLineWidth = 80,
      spaceBetweenDefinitions = true,
      closingParensOnNewLine = true,
      spaceInsideModules = true
    }
  }
}
```

### Using null-ls for formatting

```lua
local null_ls = require("null-ls")

null_ls.setup({
  sources = {
    null_ls.builtins.formatting.pact.with({
      extra_args = {
        "--format-config",
        "indentSize=2,maxLineWidth=80,spaceBetweenDefinitions=true,closingParensOnNewLine=true"
      }
    })
  }
})
```

## Emacs Configuration

### Using lsp-mode

```elisp
(use-package lsp-mode
  :config
  (add-to-list 'lsp-language-id-configuration '(pact-mode . "pact"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("pact" "--lsp"))
    :major-modes '(pact-mode)
    :server-id 'pact-lsp
    :initialization-options
    '((formatting . ((indentSize . 2)
                     (maxLineWidth . 80)
                     (spaceBetweenDefinitions . t)
                     (closingParensOnNewLine . t)
                     (spaceInsideModules . t)))))))
```

## Configuration Options Reference

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `indentSize` | number | 2 | Number of spaces for indentation |
| `maxLineWidth` | number | 80 | Maximum line width before wrapping |
| `spaceBetweenDefinitions` | boolean | true | Add blank lines between top-level definitions |
| `closingParensOnNewLine` | boolean | true | Place closing parentheses on separate lines |
| `spaceInsideModules` | boolean | true | Add spacing between different definition types in modules |

## Formatting Profiles

### Compact Style
```json
{
  "pact.formatting": {
    "spaceBetweenDefinitions": false,
    "closingParensOnNewLine": false,
    "spaceInsideModules": false,
    "maxLineWidth": 120
  }
}
```

### Standard Style (Default)
```json
{
  "pact.formatting": {
    "spaceBetweenDefinitions": true,
    "closingParensOnNewLine": true,
    "spaceInsideModules": true,
    "maxLineWidth": 80
  }
}
```

### Expanded Style
```json
{
  "pact.formatting": {
    "spaceBetweenDefinitions": true,
    "closingParensOnNewLine": true,
    "spaceInsideModules": true,
    "maxLineWidth": 120,
    "indentSize": 4
  }
}
```

## Command Line Usage

You can also format files directly using the Pact CLI:

```bash
# Format with default settings
pact format myfile.pact

# Format with custom settings
pact format --indent-size=4 --max-line-width=120 --closing-parens-newline myfile.pact

# Format and write back to file
pact format --in-place myfile.pact
```

## Project-Level Configuration

Create a `.pactrc` file in your project root:

```json
{
  "formatting": {
    "indentSize": 2,
    "maxLineWidth": 80,
    "spaceBetweenDefinitions": true,
    "closingParensOnNewLine": true,
    "spaceInsideModules": true
  }
}
```

## Integration with CI/CD

### GitHub Actions
```yaml
name: Format Check
on: [push, pull_request]
jobs:
  format-check:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - name: Setup Pact
      run: cabal install pact
    - name: Check formatting
      run: |
        find . -name "*.pact" -exec pact format --check {} \;
```

### Pre-commit Hook
```bash
#!/bin/sh
# .git/hooks/pre-commit
for file in $(git diff --cached --name-only --diff-filter=ACM | grep '\.pact$'); do
    pact format --check "$file" || {
        echo "File $file is not properly formatted"
        echo "Run: pact format --in-place $file"
        exit 1
    }
done
```

## Troubleshooting

### Common Issues

1. **Settings not taking effect**: Restart your editor after changing LSP settings
2. **Formatting not available**: Ensure the Pact LSP server is running (`pact --lsp`)
3. **Wrong indentation**: Check that `editor.tabSize` matches `pact.formatting.indentSize`

### Debug LSP Communication

Enable LSP logs in VS Code:
```json
{
  "pact.trace.server": "verbose"
}
```

Check the Output panel → "Pact Language Server" for formatting requests and responses.