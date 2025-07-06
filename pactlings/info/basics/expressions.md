# Pact Expressions and S-Expression Syntax

## Introduction

Pact uses S-expressions (symbolic expressions), a syntax style inherited from Lisp. In S-expressions, everything is written in prefix notation with parentheses.

## Basic Structure

```pact
(operator operand1 operand2 ...)
```

## Arithmetic Operations

### Addition
```pact
(+ 1 2)          ; Returns 3
(+ 1 2 3 4)      ; Multiple operands: Returns 10
(+ 3.14 2.86)    ; Decimals: Returns 6.0
```

### Subtraction
```pact
(- 10 3)         ; Returns 7
(- 5.5 2.5)      ; Decimals: Returns 3.0
```

### Multiplication
```pact
(* 4 5)          ; Returns 20
(* 2.5 4.0)      ; Decimals: Returns 10.0
```

### Division
```pact
(/ 20 4)         ; Returns 5.0 (always returns decimal)
(/ 7 2)          ; Returns 3.5
```

## String Operations

### Concatenation
```pact
(+ "Hello" " " "World")    ; Returns "Hello World"
(+ "Value: " (str 42))     ; Convert and concatenate
```

## Boolean Operations

### Logical AND
```pact
(and true true)     ; Returns true
(and true false)    ; Returns false
```

### Logical OR
```pact
(or true false)     ; Returns true
(or false false)    ; Returns false
```

### Logical NOT
```pact
(not true)          ; Returns false
(not false)         ; Returns true
```

## Key Points

1. **Prefix Notation**: Operator comes first, then operands
2. **Parentheses Required**: All function calls need parentheses
3. **Type Consistency**: Operations work on compatible types
4. **No Operator Precedence**: Explicit parentheses control evaluation order

## Common Mistakes

```pact
;; Wrong - infix notation
1 + 2

;; Correct - prefix notation
(+ 1 2)

;; Wrong - missing parentheses
+ 1 2

;; Correct - with parentheses
(+ 1 2)
```

## Next Steps

After mastering basic expressions, you'll learn about:
- Variable binding with `let`
- Function definitions with `defun`
- Conditional expressions with `if`
- Module organization