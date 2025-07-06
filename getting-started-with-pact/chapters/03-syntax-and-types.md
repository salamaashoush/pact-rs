# Chapter 3: Basic Syntax and Types

## Introduction

Pact is a Lisp-dialect designed specifically for blockchain smart contracts. Its syntax emphasizes clarity, safety, and formal verification capabilities. This chapter covers the fundamental syntax, data types, and language constructs that form the foundation of Pact programming.

## Pact Syntax Fundamentals

### S-Expression Structure

Pact uses S-expressions (symbolic expressions) where code and data share the same structure:

```pact
;; Basic S-expression: (operator operand1 operand2 ...)
(+ 1 2)                    ;; Function call: addition
(defun hello () "world")   ;; Function definition
(if true "yes" "no")       ;; Conditional expression
```

### Comments

```pact
;; Single-line comment

(defun example ()
  ;; Comments can appear anywhere
  "function body")

;; Multi-line comments using multiple ;; lines
;; This is a longer explanation
;; that spans multiple lines
```

### Whitespace and Formatting

Pact is whitespace-insensitive, allowing flexible formatting:

```pact
;; Compact format
(defun add (a b) (+ a b))

;; Expanded format for readability
(defun add (a:decimal b:decimal)
  @doc "Add two decimal numbers"
  (+ a b))

;; Aligned format
(defun complex-calculation (x:decimal y:decimal z:decimal)
  (let ((temp1 (* x y))
        (temp2 (+ y z)))
    (/ temp1 temp2)))
```

## Data Types

### Primitive Types

#### Integers

```pact
;; Integer literals
42                    ;; Positive integer
-17                   ;; Negative integer
0                     ;; Zero

;; Large integers (arbitrary precision)
123456789012345678901234567890

;; Integer operations
(+ 10 20)            ;; 30
(- 100 30)           ;; 70
(* 6 7)              ;; 42
(/ 20 4)             ;; 5
(mod 10 3)           ;; 1
```

#### Decimals

```pact
;; Decimal literals
3.14159              ;; Pi approximation
-2.5                 ;; Negative decimal
0.0                  ;; Zero as decimal

;; Decimal precision (up to 255 decimal places)
0.123456789012345678901234567890

;; Decimal operations
(+ 1.5 2.5)          ;; 4.0
(- 10.0 3.7)         ;; 6.3
(* 2.5 4.0)          ;; 10.0
(/ 22.0 7.0)         ;; 3.142857142857143
```

#### Strings

```pact
;; String literals
"Hello, World!"
"Pact smart contract"
""                   ;; Empty string

;; String escaping
"Quote: \"Hello\""   ;; Quote: "Hello"
"Newline:\nNext line"
"Tab:\tIndented"

;; String operations
(+ "Hello, " "World!")              ;; "Hello, World!" (concatenation)
(length "Pact")                     ;; 4
(take 5 "Hello, World!")            ;; "Hello"
(drop 7 "Hello, World!")            ;; "World!"
```

#### Booleans

```pact
;; Boolean literals
true
false

;; Boolean operations
(and true false)     ;; false
(or true false)      ;; true
(not true)           ;; false

;; Comparison operators return booleans
(= 1 1)              ;; true
(> 5 3)              ;; true
(< 2 10)             ;; true
(!= "a" "b")         ;; true
```

#### Time

```pact
;; Time literals (ISO 8601 format)
(time "2024-01-01T00:00:00Z")
(time "2024-12-31T23:59:59Z")

;; Time operations
(add-time (time "2024-01-01T00:00:00Z") (days 30))   ;; Add 30 days
(diff-time (time "2024-01-02T00:00:00Z") 
           (time "2024-01-01T00:00:00Z"))           ;; Difference in seconds

;; Time utilities
(days 1)             ;; 86400 seconds
(hours 2)            ;; 7200 seconds
(minutes 30)         ;; 1800 seconds
(seconds 45)         ;; 45 seconds
```

### Compound Types

#### Lists

```pact
;; List literals
[1, 2, 3, 4, 5]                    ;; Integer list
["apple", "banana", "cherry"]       ;; String list
[true, false, true]                 ;; Boolean list
[]                                  ;; Empty list

;; Mixed type lists (not recommended for type safety)
[1, "hello", true]

;; List operations
(length [1, 2, 3])                 ;; 3
(take 2 [1, 2, 3, 4])              ;; [1, 2]
(drop 2 [1, 2, 3, 4])              ;; [3, 4]
(+ [1, 2] [3, 4])                  ;; [1, 2, 3, 4] (concatenation)
(reverse [1, 2, 3])                ;; [3, 2, 1]
(sort [3, 1, 4, 2])                ;; [1, 2, 3, 4]

;; List access
(at 0 [1, 2, 3])                   ;; 1 (first element)
(at 2 [1, 2, 3])                   ;; 3 (third element)
```

#### Objects

```pact
;; Object literals
{ "name": "Alice", "age": 30, "active": true }
{ "x": 10, "y": 20 }
{}                                  ;; Empty object

;; Object access
(at "name" { "name": "Alice", "age": 30 })     ;; "Alice"
(at "age" { "name": "Alice", "age": 30 })      ;; 30

;; Object operations
(+ { "a": 1 } { "b": 2 })          ;; { "a": 1, "b": 2 } (merge)
(keys { "name": "Alice", "age": 30 })          ;; ["name", "age"]
(values { "name": "Alice", "age": 30 })        ;; ["Alice", 30]

;; Nested objects
{ "user": { "name": "Alice", "profile": { "email": "alice@example.com" } } }
```

## Type Annotations

### Function Parameter Types

```pact
;; Type annotations for clarity and safety
(defun add-numbers (a:integer b:integer)
  @doc "Add two integers"
  (+ a b))

(defun format-name (first:string last:string)
  @doc "Format full name"
  (format "{} {}" [first last]))

(defun calculate-area (length:decimal width:decimal)
  @doc "Calculate rectangle area"
  (* length width))
```

### Return Type Annotations

```pact
;; Explicit return type annotations
(defun get-greeting:string (name:string)
  @doc "Return a greeting message"
  (format "Hello, {}!" [name]))

(defun is-positive:bool (number:decimal)
  @doc "Check if number is positive"
  (> number 0.0))

(defun double:decimal (x:decimal)
  @doc "Double a decimal number"
  (* 2.0 x))
```

### Complex Type Annotations

```pact
;; List type annotations
(defun sum-numbers:[decimal] (numbers:[decimal])
  @doc "Return cumulative sum list"
  (let ((running-sum 0.0))
    (map (lambda (n) 
           (set running-sum (+ running-sum n))
           running-sum)
         numbers)))

;; Object type annotations
(defun create-user:object (name:string email:string)
  @doc "Create user object"
  { "name": name, "email": email, "created": (at 'block-time (chain-data)) })
```

## Variables and Binding

### Let Expressions

```pact
;; Simple let binding
(defun circle-area (radius:decimal)
  (let ((pi 3.14159))
    (* pi radius radius)))

;; Multiple bindings
(defun quadratic (a:decimal b:decimal c:decimal x:decimal)
  (let ((ax2 (* a x x))
        (bx (* b x)))
    (+ (+ ax2 bx) c)))

;; Nested let expressions
(defun complex-calculation (x:decimal y:decimal)
  (let ((temp1 (* x y)))
    (let ((temp2 (+ temp1 10.0)))
      (/ temp2 2.0))))
```

### Let* Sequential Binding

```pact
;; Sequential binding with let*
(defun compound-interest (principal:decimal rate:decimal years:integer)
  (let* ((annual-rate (/ rate 100.0))
         (factor (+ 1.0 annual-rate))
         (final-amount (* principal (^ factor years))))
    final-amount))
```

### Destructuring with Bind

```pact
;; Object destructuring
(defun process-user (user:object)
  (bind user { "name" := username, "age" := user-age }
    (format "User {} is {} years old" [username user-age])))

;; List destructuring (less common)
(defun process-coordinates (coords:[decimal])
  (bind coords [x, y]
    { "x": x, "y": y, "distance": (sqrt (+ (* x x) (* y y))) }))
```

## Control Flow

### Conditionals

#### If Expressions

```pact
;; Basic if expression
(defun absolute-value (x:decimal)
  (if (>= x 0.0)
      x
      (- x)))

;; Nested if expressions
(defun grade-letter (score:integer)
  (if (>= score 90)
      "A"
      (if (>= score 80)
          "B"
          (if (>= score 70)
              "C"
              (if (>= score 60)
                  "D"
                  "F")))))
```

#### Cond Expressions

```pact
;; Cond for multiple conditions (cleaner than nested if)
(defun grade-letter-improved (score:integer)
  (cond
    ((>= score 90) "A")
    ((>= score 80) "B")
    ((>= score 70) "C")
    ((>= score 60) "D")
    "F"))  ;; Default case

;; Complex conditions
(defun shipping-cost (weight:decimal distance:decimal premium:bool)
  (cond
    ((and premium (> distance 1000.0)) (* weight 2.5))
    ((and premium (<= distance 1000.0)) (* weight 2.0))
    ((> distance 1000.0) (* weight 1.5))
    (* weight 1.0)))
```

### Iteration and Mapping

#### Map Function

```pact
;; Apply function to each element
(map (+ 1) [1, 2, 3, 4])           ;; [2, 3, 4, 5]
(map (* 2) [1, 2, 3, 4])           ;; [2, 4, 6, 8]

;; Map with lambda
(map (lambda (x) (* x x)) [1, 2, 3, 4])  ;; [1, 4, 9, 16]

;; Map over strings
(map (+ "Hello, ") ["Alice", "Bob", "Charlie"])
;; ["Hello, Alice", "Hello, Bob", "Hello, Charlie"]
```

#### Filter Function

```pact
;; Filter elements based on predicate
(filter (> 5) [1, 6, 3, 8, 2])     ;; [6, 8]
(filter (lambda (x) (= (mod x 2) 0)) [1, 2, 3, 4, 5, 6])  ;; [2, 4, 6]

;; Filter strings
(filter (lambda (s) (> (length s) 3)) ["a", "hello", "hi", "world"])
;; ["hello", "world"]
```

#### Fold Function

```pact
;; Reduce list to single value
(fold (+) 0 [1, 2, 3, 4, 5])       ;; 15 (sum)
(fold (*) 1 [1, 2, 3, 4, 5])       ;; 120 (factorial of 5)

;; Fold with lambda
(fold (lambda (acc x) (if (> x acc) x acc)) 0 [3, 7, 2, 9, 1])  ;; 9 (max)

;; Complex fold: build object
(fold (lambda (acc item)
        (+ acc { item: (length item) }))
      {}
      ["apple", "banana", "cherry"])
;; { "apple": 5, "banana": 6, "cherry": 6 }
```

## Functions

### Function Definition

```pact
;; Basic function
(defun square (x:decimal)
  @doc "Calculate square of a number"
  (* x x))

;; Function with multiple parameters
(defun distance (x1:decimal y1:decimal x2:decimal y2:decimal)
  @doc "Calculate distance between two points"
  (let ((dx (- x2 x1))
        (dy (- y2 y1)))
    (sqrt (+ (* dx dx) (* dy dy)))))

;; Function with default behavior
(defun greet (name:string)
  @doc "Greet a person"
  (if (= name "")
      "Hello, Anonymous!"
      (format "Hello, {}!" [name])))
```

### Lambda Functions

```pact
;; Anonymous functions for use with higher-order functions
(lambda (x) (* x x))                ;; Square function
(lambda (x y) (+ x y))              ;; Addition function

;; Lambda with type annotations
(lambda (name:string) (format "Hello, {}!" [name]))

;; Using lambdas with map
(map (lambda (person)
       (format "Welcome, {}!" [(at "name" person)]))
     [{ "name": "Alice" }, { "name": "Bob" }])
```

### Higher-Order Functions

```pact
;; Function that takes another function as parameter
(defun apply-twice (f operation:decimal)
  @doc "Apply function twice to a value"
  (f (f operation)))

;; Usage
(apply-twice (lambda (x) (* x 2)) 5)  ;; 20 (5 * 2 * 2)

;; Function that returns a function
(defun make-multiplier (factor:decimal)
  @doc "Create a multiplication function"
  (lambda (x) (* x factor)))

;; Usage
(let ((double (make-multiplier 2.0))
      (triple (make-multiplier 3.0)))
  [(double 5.0), (triple 5.0)])      ;; [10.0, 15.0]
```

## Pattern Matching and Destructuring

### Object Pattern Matching

```pact
;; Extract values from objects
(defun process-order (order:object)
  (bind order { "customer" := cust, "amount" := amt, "items" := products }
    (format "Order for {} totaling {} with {} items" 
            [cust amt (length products)])))

;; Partial matching (only extract needed fields)
(defun get-customer-name (order:object)
  (bind order { "customer" := customer-info }
    (bind customer-info { "name" := name }
      name)))
```

### List Pattern Matching

```pact
;; Pattern match on list structure
(defun describe-list (items:[string])
  (cond
    ((= (length items) 0) "Empty list")
    ((= (length items) 1) (format "Single item: {}" [(at 0 items)]))
    ((= (length items) 2) (format "Two items: {} and {}" [(at 0 items) (at 1 items)]))
    (format "Multiple items, first is: {}" [(at 0 items)])))
```

## Error Handling

### Enforce Statements

```pact
;; Basic enforcement
(defun divide (a:decimal b:decimal)
  (enforce (!= b 0.0) "Division by zero")
  (/ a b))

;; Multiple enforcements
(defun withdraw (account:string amount:decimal)
  (enforce (> amount 0.0) "Amount must be positive")
  (enforce (<= amount (get-balance account)) "Insufficient funds")
  (enforce (account-exists? account) "Account does not exist")
  ;; Perform withdrawal
  )

;; Enforce with complex conditions
(defun transfer-with-limits (from:string to:string amount:decimal)
  (enforce (and (> amount 0.0) (< amount 10000.0)) 
           "Amount must be between 0 and 10000")
  (enforce (and (!= from to) (!= from "") (!= to ""))
           "Invalid account parameters")
  ;; Perform transfer
  )
```

### Enforce-One (Multiple Valid Conditions)

```pact
;; At least one condition must be true
(defun admin-or-owner-action (user:string resource-owner:string)
  (enforce-one "Must be admin or owner"
    [(enforce-keyset 'admin-keyset)              ;; Admin access
     (enforce (= user resource-owner) "Not owner")]))  ;; Owner access
```

## Type Conversion and Coercion

### String Conversions

```pact
;; Convert to string
(format "{}" [42])              ;; "42"
(format "{}" [3.14])            ;; "3.14"
(format "{}" [true])            ;; "true"

;; Parse from string
(str-to-int "42")               ;; 42
(str-to-decimal "3.14")         ;; 3.14

;; Safe parsing with validation
(defun safe-parse-int (s:string)
  (let ((result (str-to-int s)))
    (enforce (>= result 0) "Must be non-negative")
    result))
```

### Numeric Conversions

```pact
;; Integer to decimal
(defun int-to-decimal (i:integer)
  (+ 0.0 i))

;; Decimal to integer (truncation)
(defun decimal-to-int (d:decimal)
  (floor d))

;; Rounding operations
(ceiling 3.2)                   ;; 4
(floor 3.8)                     ;; 3
(round 3.5)                     ;; 4
(round 3.14159 2)               ;; 3.14 (2 decimal places)
```

## Advanced Type Features

### Type Guards and Validation

```pact
;; Type validation functions
(defun validate-email:bool (email:string)
  @doc "Basic email validation"
  (and (contains "@" email)
       (> (length email) 5)
       (not (contains " " email))))

(defun validate-positive-amount:bool (amount:decimal)
  @doc "Validate positive monetary amount"
  (and (> amount 0.0)
       (= amount (round amount 2))))  ;; Max 2 decimal places

;; Using validation in functions
(defun create-user (name:string email:string)
  (enforce (!= name "") "Name cannot be empty")
  (enforce (validate-email email) "Invalid email format")
  { "name": name, "email": email, "created": (at 'block-time (chain-data)) })
```

### Optional Values and Defaults

```pact
;; Default value pattern
(defun get-config-value (config:object key:string default-value:string)
  @doc "Get configuration value with default"
  (if (contains key config)
      (at key config)
      default-value))

;; Optional parameter simulation
(defun create-account-with-defaults (name:string)
  (let ((default-email (format "{}@example.com" [name]))
        (default-balance 0.0))
    { "name": name, 
      "email": default-email, 
      "balance": default-balance }))
```

## Best Practices

### Code Style

```pact
;; GOOD: Clear, descriptive names
(defun calculate-compound-interest (principal:decimal rate:decimal years:integer)
  @doc "Calculate compound interest"
  (let ((annual-rate (/ rate 100.0)))
    (* principal (^ (+ 1.0 annual-rate) years))))

;; BAD: Unclear names
(defun calc (p:decimal r:decimal y:integer)
  (let ((ar (/ r 100.0)))
    (* p (^ (+ 1.0 ar) y))))

;; GOOD: Consistent formatting
(defun transfer-funds (from-account:string 
                      to-account:string 
                      amount:decimal)
  @doc "Transfer funds between accounts"
  (enforce (> amount 0.0) "Amount must be positive")
  (let ((from-balance (get-balance from-account)))
    (enforce (>= from-balance amount) "Insufficient funds")
    (update-balance from-account (- from-balance amount))
    (update-balance to-account (+ (get-balance to-account) amount))))
```

### Type Safety

```pact
;; GOOD: Explicit type annotations
(defun safe-divide:decimal (numerator:decimal denominator:decimal)
  @doc "Safe division with type safety"
  (enforce (!= denominator 0.0) "Division by zero")
  (/ numerator denominator))

;; GOOD: Input validation
(defun create-user-profile (name:string age:integer email:string)
  @doc "Create user profile with validation"
  (enforce (!= name "") "Name cannot be empty")
  (enforce (and (>= age 0) (<= age 150)) "Invalid age")
  (enforce (validate-email email) "Invalid email")
  { "name": name, "age": age, "email": email })
```

## Summary

Pact's syntax and type system provide:

**Syntax Features:**
- **S-expressions** for consistent structure
- **Type annotations** for clarity and safety  
- **Pattern matching** for data extraction
- **Higher-order functions** for code reuse

**Type System:**
- **Primitive types**: integer, decimal, string, bool, time
- **Compound types**: lists, objects
- **Type safety** through annotations
- **Runtime enforcement** via validation

**Control Flow:**
- **Conditionals**: if, cond, enforce
- **Iteration**: map, filter, fold
- **Error handling**: enforce, enforce-one

**Best Practices:**
1. **Use type annotations** for all function parameters
2. **Validate inputs** with enforce statements
3. **Follow consistent naming** conventions
4. **Structure code clearly** with proper formatting
5. **Handle errors explicitly** rather than ignoring them

This foundation enables writing clear, safe, and maintainable Pact smart contracts.

## Exercises

1. Write functions demonstrating each primitive type with validation
2. Create a data processing pipeline using map, filter, and fold
3. Implement a calculator with proper error handling
4. Build a user management system with type-safe functions
5. Write property tests for type conversion functions

## References

- Pact syntax specification: Official Pact documentation
- Type system details: `/pact/Pact/Core/Type.hs`
- Built-in functions: `/pact/Pact/Core/IR/Eval/CEK/CoreBuiltin.hs`
- Parser implementation: `/pact/Pact/Core/Syntax/Parser.hs`