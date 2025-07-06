# Lesson 1: Making Your First Zombie 🧟

Welcome to PactZombies! In this first lesson, you'll learn the basics of Pact while creating your very first zombie.

## What You'll Learn

- Basic Pact syntax
- Creating modules
- Defining schemas and tables
- Writing your first functions
- Creating zombie DNA

## Chapter 1: Getting Started

In Pact, smart contracts are organized into **modules**. A module is like a class in object-oriented programming - it contains functions and data definitions.

Let's start by creating our zombie factory!

### Your First Module

```pact
(module zombies GOVERNANCE
  ; Your zombie code will go here!
)
```

Every module needs:
1. A name (we're using `zombies`)
2. A governance capability (we'll use `GOVERNANCE` for now)

### Exercise 1.1

Create a new file called `zombie-factory.pact` and add the basic module structure above.

## Chapter 2: Zombie DNA

Our zombies will have DNA that determines their appearance. In CryptoZombies fashion, zombie DNA will be a 16-digit integer.

### Defining a Schema

In Pact, we store data in tables. First, we need to define what our data looks like using a **schema**:

```pact
(defschema zombie
  name:string
  dna:integer
  level:integer
  ready-time:time
  win-count:integer
  loss-count:integer
  owner:string)
```

### Creating a Table

Now let's create a table to store our zombies:

```pact
(deftable zombies-table:{zombie})
```

### Exercise 1.2

Add the zombie schema and table definition to your module.

## Chapter 3: Creating Zombies

Time to bring our zombies to life! We'll create a function that generates a new zombie.

### Generating Random DNA

Pact has built-in hash functions we can use to generate pseudo-random numbers:

```pact
(defun generate-dna:integer (name:string)
  @doc "Generate DNA from a name"
  (let ((hash-result (hash (+ name (format-time "%F %T" (at "block-time" (chain-data))))))
    (abs (str-to-int 64 (take 16 hash-result))))
)
```

### Creating a Zombie

Now let's create our main function:

```pact
(defun create-zombie:string (name:string)
  @doc "Create a new zombie"
  (let* ((owner (at "sender" (chain-data)))
         (dna (generate-dna name))
         (zombie-id (format "zombie_{}" [dna])))
    (insert zombies-table zombie-id
      { "name": name
      , "dna": dna
      , "level": 1
      , "ready-time": (at "block-time" (chain-data))
      , "win-count": 0
      , "loss-count": 0
      , "owner": owner
      })
    (format "Zombie {} created with DNA: {}" [name dna]))
)
```

### Exercise 1.3

Add these functions to your module and test creating your first zombie!

## Chapter 4: Testing Your Code

Let's create a REPL script to test our zombie factory:

```lisp
; Load the contract
(begin-tx)
(load "zombie-factory.pact")
(commit-tx)

; Create a zombie
(begin-tx)
(use zombies)
(env-sigs [{"key": "alice", "caps": []}])
(env-data {"sender": "alice"})
(create-zombie "Zombie Bob")
(commit-tx)

; Check our zombie
(begin-tx)
(use zombies)
(select zombies-table (constantly true))
(commit-tx)
```

### Exercise 1.4

1. Save the test script as `test-zombies.repl`
2. Run it using: `pact test-zombies.repl`
3. Create at least 3 different zombies

## Chapter 5: Challenge - Zombie Properties

Now it's your turn! Add these features to your zombie factory:

1. **Zombie Types**: Add a `type` field to the schema (e.g., "walker", "runner", "tank")
2. **Random Stats**: Generate random strength and speed attributes based on DNA
3. **Cooldown**: Prevent users from creating zombies too quickly (hint: check ready-time)

## Quiz

1. What keyword is used to define a module in Pact?
2. How do we define the structure of data in Pact?
3. What function did we use to generate pseudo-random numbers?
4. How do we get the current user's address in Pact?

## Summary

Congratulations! You've created your first Pact smart contract and brought zombies to life on the blockchain. 

In this lesson, you learned:
- ✅ Basic Pact syntax
- ✅ Creating modules and schemas
- ✅ Working with tables
- ✅ Generating pseudo-random numbers
- ✅ Getting blockchain data

Next lesson, we'll teach our zombies to attack and level up!

## Complete Code

Check your solution against the complete code in `zombie-factory-complete.pact`.