# 🎯 Pactlings - Interactive Pact Tutorial

[![Rust](https://img.shields.io/badge/rust-1.70+-blue.svg)](https://www.rust-lang.org)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**Pactlings** is an interactive tutorial system for learning the Pact smart contract language, inspired by [Rustlings](https://github.com/rust-lang/rustlings). It provides hands-on exercises that guide you through Pact concepts from basic syntax to advanced applications.

## 🌟 Features

- **Progressive Learning**: 15+ exercises covering basics to advanced topics
- **Interactive CLI**: Intuitive command-line interface with colored output
- **Auto-verification**: Real-time checking as you save files
- **Smart Hints**: Context-aware hints and examples
- **Progress Tracking**: Visual progress indicators and statistics
- **Watch Mode**: Automatic verification when files change
- **Comprehensive Coverage**: From syntax to real-world applications

## 📚 What You'll Learn

### 🟢 Basics (Beginner)
- S-expression syntax and evaluation
- Type system: strings, decimals, integers, booleans
- Variable binding with `let` expressions
- Function definition and type annotations

### 🟡 Core Concepts (Intermediate) 
- Module system and organization
- Keyset governance and security
- Database schemas and tables
- CRUD operations and data management

### 🔴 Advanced Features (Advanced)
- Capabilities-based authorization
- Managed capabilities and resource control
- Interfaces and polymorphism
- Multi-step pacts (transactions)
- Real-world application patterns

### 🏗️ Applications (Expert)
- Complete fungible token implementation
- Decentralized marketplace with escrow
- Cross-chain communication patterns
- Production deployment strategies

## 🚀 Quick Start

### Prerequisites

- Rust 1.70+ ([install](https://rustup.rs/))
- Pact executable ([install guide](https://docs.kadena.io/build/pact))

### Installation

```bash
# Clone the repository
git clone https://github.com/kadena-io/pact-5.git
cd pact-5/pactlings

# Build the tutorial
cargo build --release

# Install globally (optional)
cargo install --path .
```

### Initialize Tutorial

```bash
# Create a new tutorial workspace
pactlings init my-pact-tutorial
cd my-pact-tutorial

# List available exercises
pactlings list

# Start with the first exercise
pactlings run basics_01_expressions
```

## 🎮 Usage

### Basic Commands

```bash
# List all exercises
pactlings list

# Run a specific exercise
pactlings run basics_01_expressions

# Get hints for an exercise
pactlings hint basics_01_expressions

# Verify your solution
pactlings verify basics_01_expressions

# Verify all completed exercises
pactlings verify-all

# Watch for changes and auto-verify
pactlings watch basics_01_expressions

# Reset an exercise to its original state
pactlings reset basics_01_expressions

# Show progress statistics
pactlings progress

# Interactive mode
pactlings
```

### Watch Mode

Watch mode automatically verifies your solutions as you save files:

```bash
# Watch a specific exercise
pactlings watch basics_01_expressions

# Watch all exercises (auto-detects changes)
pactlings watch
```

When you save a file, Pactlings will:
1. 🔍 Check syntax and compilation
2. ✅ Run exercise-specific validations  
3. 🧪 Execute any test commands
4. 📊 Update your progress
5. 🎉 Celebrate completion!

## 📖 Exercise Structure

Each exercise includes:

- **📝 Exercise File**: Pact code with TODO markers to complete
- **💡 Hints**: Progressive hints to guide you
- **📚 Info**: Detailed explanations and examples
- **✅ Verification**: Automated checking of your solution
- **🎯 Solution**: Reference implementation (revealed after completion)

### Exercise Topics

| Topic | Count | Description |
|-------|-------|-------------|
| **Basics** | 4 | Syntax, types, variables, functions |
| **Modules** | 2 | Module system and governance |
| **Capabilities** | 2 | Authorization and security |
| **Database** | 2 | Schemas, tables, CRUD operations |
| **Interfaces** | 1 | Polymorphism and code reuse |
| **Pacts** | 1 | Multi-step transactions |
| **Applications** | 2 | Real-world implementations |

## 🎯 Learning Path

### For Beginners (New to Pact)
1. Start with `basics_*` exercises
2. Learn fundamental syntax and types
3. Understand functions and modules
4. Progress to capabilities and database

### For Intermediate Developers
1. Review basics quickly
2. Focus on capabilities system
3. Master database operations
4. Explore interfaces and pacts

### For Advanced Users
1. Jump to application exercises
2. Study production patterns
3. Analyze security implementations
4. Build complete systems

## 🔧 Configuration

Pactlings can be configured via `pactlings.toml`:

```toml
[config]
exercises_dir = "exercises"
solutions_dir = "solutions"  
info_dir = "info"
pact_executable = "pact"
auto_verify = true
show_hints = true

# Exercise definitions...
```

## 🎨 Features in Detail

### Smart Verification
- ✅ Syntax checking with Pact compiler
- 🔍 Exercise-specific validation rules
- 🧪 Automated test execution
- 📊 Detailed feedback on failures

### Progressive Hints
- 💡 Basic conceptual hints
- 🔍 Step-by-step guidance
- 📋 Common pattern examples
- 📖 Complete solution structures

### Progress Tracking
- 📈 Completion percentage
- ⏱️ Time spent per exercise
- 🔥 Learning streaks
- 📊 Topic-wise breakdown

### Interactive Experience
- 🎨 Colored terminal output
- 📱 Responsive command interface
- 🎮 Interactive mode for exploration
- 👀 Real-time file watching

## 🤝 Contributing

We welcome contributions! Here's how to help:

### Adding Exercises
1. Define exercise in `src/config.rs`
2. Create template in `src/exercise.rs`
3. Add verification logic in `src/verify.rs`
4. Include hints in `src/hint.rs`

### Improving Verification
- Add more sophisticated checking
- Include common error patterns
- Enhance feedback messages
- Support additional test frameworks

### Documentation
- Improve exercise descriptions
- Add more learning resources
- Create video tutorials
- Translate to other languages

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Inspired by [Rustlings](https://github.com/rust-lang/rustlings)
- Built for the [Pact](https://pact-language.readthedocs.io/) smart contract language
- Powered by [Kadena](https://kadena.io/) blockchain technology

## 📞 Support

- 🐛 [Report Issues](https://github.com/kadena-io/pact-5/issues)
- 💬 [Join Discord](https://discord.gg/kadena)
- 📖 [Read Docs](https://docs.kadena.io/build/pact)
- 🌐 [Visit Website](https://kadena.io)

---

**Happy Learning! 🚀**

Start your Pact journey today and master blockchain smart contract development through hands-on practice.