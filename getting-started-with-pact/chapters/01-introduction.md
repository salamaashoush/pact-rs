# Chapter 1: Introduction to Pact

## What is Pact?

Pact is a smart contract language specifically designed for blockchain applications. Unlike general-purpose languages adapted for blockchain use, Pact was built from the ground up with blockchain requirements in mind.

### Key Design Principles

1. **Human Readable**: Pact code is meant to be readable by non-programmers
2. **Turing Incomplete**: Deliberately limited to prevent infinite loops
3. **Database-Focused**: Built-in database with row-level security
4. **Formally Verifiable**: Supports property-based testing and formal verification
5. **Atomic Execution**: All-or-nothing transaction execution

## Why Pact?

### The Problem with Traditional Smart Contract Languages

Most smart contract languages (like Solidity) are adaptations of general-purpose languages. This leads to:
- Complex syntax that's hard to audit
- Vulnerability to infinite loops and gas attacks
- Difficult formal verification
- Poor database abstractions

### Pact's Solution

Pact addresses these issues through:

```pact
;; Simple, readable syntax
(defun transfer (from:string to:string amount:decimal)
  @doc "Transfer money between accounts"
  (with-capability (TRANSFER from to amount)
    (debit from amount)
    (credit to amount)))
```

## Core Concepts Overview

### 1. Modules
Modules are the primary code organization unit:
```pact
(module my-token 'admin-keyset
  "A simple token module"
  
  ;; Module contents here
)
```

### 2. Capabilities
Fine-grained permissions system:
```pact
(defcap TRANSFER (from:string to:string amount:decimal)
  @managed amount TRANSFER-mgr
  (enforce-guard (at 'guard (read accounts from))))
```

### 3. Tables
Built-in database with schemas:
```pact
(defschema account
  balance:decimal
  guard:guard)

(deftable accounts:{account})
```

### 4. Guards
Unified security model:
```pact
(enforce-guard (keyset-ref-guard 'admin-keyset))
```

## Your First Pact Program

Let's write a simple "Hello, World" program:

```pact
;; hello.pact
(module hello 'admin-keyset
  "My first Pact module"
  
  (defun greet:string (name:string)
    "Greet someone by name"
    (format "Hello, {}!" [name]))
)

;; Create the table
(create-table greetings)
```

To run this:
```bash
$ pact
pact> (load "hello.pact")
pact> (hello.greet "Alice")
"Hello, Alice!"
```

## How Pact Executes

Understanding Pact's execution model is crucial:

1. **Transaction Boundary**: All code executes within a transaction
2. **Atomic Execution**: Either all succeeds or all fails
3. **Deterministic**: Same inputs always produce same outputs
4. **No Side Effects**: Can't make external calls or generate randomness

### Execution Flow

```
Transaction Start
    ↓
Parse & Typecheck
    ↓
Capability Acquisition
    ↓
Code Execution
    ↓
State Updates
    ↓
Transaction Commit/Rollback
```

## Pact vs Other Languages

| Feature | Pact | Solidity | Rust/Solana |
|---------|------|----------|-------------|
| Syntax | Lisp-like | C-like | Rust |
| Turing Complete | No | Yes | Yes |
| Formal Verification | Built-in | External | Limited |
| Database | Built-in | Manual | Manual |
| Learning Curve | Gentle | Moderate | Steep |

## The Pact Philosophy

Pact embodies several philosophical principles:

1. **"Code is Law"**: The code should be readable as a legal document
2. **"Correct by Construction"**: Make it hard to write bugs
3. **"Fail Fast"**: Explicit errors are better than subtle bugs
4. **"Simple > Clever"**: Readability trumps cleverness

## What You'll Learn

By the end of this book, you'll understand:

- How to design and implement secure smart contracts
- Pact's unique capability-based security model
- Database design for blockchain applications
- Testing and formal verification techniques
- Real-world patterns and best practices

## Prerequisites

To get the most from this book, you should have:
- Basic programming experience (any language)
- Understanding of blockchain concepts
- Command line familiarity

No Lisp experience is required!

## Summary

Pact represents a fundamental rethink of smart contract languages. By embracing constraints (Turing incompleteness) and focusing on domain needs (database, capabilities, formal verification), Pact provides a safer, more productive environment for blockchain development.

In the next chapter, we'll set up your Pact development environment and write your first real smart contract.

## Exercises

1. **Conceptual**: List three ways Turing incompleteness makes Pact safer
2. **Research**: Find a real Pact contract on GitHub and identify its modules
3. **Practical**: Install Pact and run the hello world example

## Further Reading

- [Pact Whitepaper](https://kadena.io/pact-whitepaper)
- [Formal Verification in Pact](https://pact-language.readthedocs.io/en/stable/pact-properties.html)
- [Kadena Blockchain Architecture](https://kadena.io/techpaper)