# PactZombies Instructor Guide 👩‍🏫

A comprehensive guide for teaching PactZombies in workshops, bootcamps, or classroom settings.

## Course Overview

**Duration**: 12-24 hours (can be adapted)
**Format**: Hands-on coding workshop
**Prerequisites**: Basic programming knowledge

## Suggested Schedule

### 2-Day Intensive Workshop

#### Day 1: Foundations (8 hours)
- **Hour 1-2**: Introduction & Setup
  - Kadena/Pact overview
  - Environment setup
  - First zombie (Lesson 1)
  
- **Hour 3-4**: Core Concepts
  - Complete Lesson 1
  - Start Lesson 2 (battles)
  
- **Hour 5-6**: Advanced Functions
  - Complete Lesson 2
  - Start Lesson 3 (capabilities)
  
- **Hour 7-8**: Security & Ownership
  - Complete Lesson 3
  - Q&A and review

#### Day 2: Advanced Topics (8 hours)
- **Hour 1-2**: Token Economics
  - Lesson 4 (fungible tokens)
  - Implement ZMB token
  
- **Hour 3-4**: DeFi Concepts
  - Staking and rewards
  - Marketplace mechanics
  
- **Hour 5-6**: Multi-Step Transactions
  - Lesson 5 (pacts)
  - Quest implementation
  
- **Hour 7-8**: Production & Deployment
  - Lesson 6 overview
  - Deployment demo
  - Final projects

### Weekly Course (6 weeks, 2 hours/week)

**Week 1**: Introduction and Setup
- Pact basics
- Lesson 1: First zombie

**Week 2**: Functions and Logic
- Lesson 2: Battle system
- Practice exercises

**Week 3**: Security and Capabilities
- Lesson 3: Ownership
- Capability patterns

**Week 4**: Token Economics
- Lesson 4: ZMB token
- DeFi primitives

**Week 5**: Advanced Features
- Lesson 5: Quests/Pacts
- Multi-party coordination

**Week 6**: Production Ready
- Lesson 6: Cross-chain
- Final project presentations

## Teaching Tips

### For Each Lesson

1. **Introduction (10 min)**
   - Review previous concepts
   - Preview new topics
   - Show final result

2. **Guided Coding (30 min)**
   - Live code the basic version
   - Explain each concept
   - Encourage questions

3. **Practice Time (30 min)**
   - Students implement features
   - Circulate and help
   - Share common issues

4. **Challenge Exercise (20 min)**
   - Present advanced challenge
   - Let students explore
   - Discuss solutions

### Key Concepts to Emphasize

#### Lesson 1: Foundations
- **Module structure**: Governance, schemas, tables
- **Type safety**: Pact's type system
- **Immutability**: Blockchain principles
- **Testing**: REPL-driven development

#### Lesson 2: Game Logic
- **Pure functions**: No side effects
- **State management**: Table updates
- **Randomness**: Challenges in blockchain
- **Gas considerations**: Efficient code

#### Lesson 3: Security
- **Capabilities**: Pact's security model
- **Ownership patterns**: Who can do what
- **Guard types**: Keys vs. capabilities
- **Best practices**: Security-first design

#### Lesson 4: Economics
- **Fungible tokens**: Standards and interfaces
- **Economic design**: Incentive alignment
- **DeFi patterns**: Staking, rewards, fees
- **Testing economics**: Simulation importance

#### Lesson 5: Pacts
- **Multi-step**: Different from functions
- **State management**: Yield and resume
- **Coordination**: Multi-party flows
- **Time considerations**: Timeouts and expiry

#### Lesson 6: Production
- **Cross-chain**: Kadena's architecture
- **Gas optimization**: Real costs
- **Upgradeability**: Planning ahead
- **Deployment**: Testnet to mainnet

## Common Student Questions

### Technical Questions

**Q: Why do we need (begin-tx) and (commit-tx)?**
A: These simulate blockchain transactions. In production, each transaction is atomic - either all changes succeed or none do.

**Q: How is randomness handled in Pact?**
A: True randomness is challenging in deterministic blockchains. We use block properties and hashing for pseudo-randomness.

**Q: What's the difference between defcap and defun?**
A: Functions (defun) compute values. Capabilities (defcap) grant permissions and must be acquired before protected operations.

**Q: Why use yield/resume in pacts?**
A: They allow transactions to span multiple blocks, enable multi-party coordination, and maintain state between steps.

### Conceptual Questions

**Q: How does this compare to Ethereum/Solidity?**
A: Pact is designed for safety with formal verification, human-readable code, and no reentrancy. Kadena offers multi-chain scaling vs. Ethereum's single chain.

**Q: When should I use a pact vs. multiple functions?**
A: Use pacts when you need multi-party coordination, time delays between steps, or cross-chain operations.

**Q: How do capabilities improve security?**
A: Capabilities make permissions explicit and composable. You must acquire specific capabilities before performing protected operations.

## Lab Exercises

### Lesson 1 Lab: Zombie Variations
- Create zombies with different rarity tiers
- Implement a name validation function
- Add timestamp-based features

### Lesson 2 Lab: Battle Mechanics
- Implement team battles (3v3)
- Add elemental types with advantages
- Create a tournament bracket system

### Lesson 3 Lab: Marketplace
- Build an offer/counter-offer system
- Implement zombie rentals
- Add reputation system for traders

### Lesson 4 Lab: Economic Simulation
- Design inflation/deflation mechanics
- Create liquidity pools
- Implement governance voting with tokens

### Lesson 5 Lab: Complex Quests
- Multi-stage boss battles
- Puzzle quests requiring specific zombies
- Time-limited event quests

### Lesson 6 Lab: Cross-Chain Game
- Zombie migration between chains
- Cross-chain tournaments
- Chain-specific zombie abilities

## Assessment Ideas

### Coding Challenges
1. **Zombie Abilities**: Add unique abilities based on DNA
2. **Alliance Wars**: Implement group battles
3. **Breeding Mechanics**: Genetic inheritance system
4. **Economic Balance**: Design sustainable token economy

### Project Ideas
1. **Zombie Racing**: Different game mode
2. **Zombie Farm**: Resource management
3. **Zombie TCG**: Trading card battles
4. **Zombie Metaverse**: Virtual world

### Grading Rubric
- **Functionality (40%)**: Code works as specified
- **Security (20%)**: Proper capability usage
- **Efficiency (20%)**: Gas optimization
- **Creativity (20%)**: Unique features

## Resources for Instructors

### Preparation Checklist
- [ ] Test all lessons locally
- [ ] Prepare slide deck
- [ ] Set up test environment
- [ ] Create starter repositories
- [ ] Prepare bonus challenges

### Useful Links
- [Pact Documentation](https://pact-language.readthedocs.io)
- [Kadena Docs](https://docs.kadena.io)
- [Example Contracts](https://github.com/kadena-io/pact-examples)

### Support Materials
- Solution walkthroughs
- Common error fixes
- Advanced topics for quick learners
- Simplified versions for struggling students

## Workshop Variations

### Hackathon Format (4-8 hours)
- 1 hour: Quick lessons 1-3
- 2 hours: Build basic game
- 1 hour: Add unique features
- Present and judge projects

### University Course (Semester)
- Weeks 1-3: Pact fundamentals
- Weeks 4-6: PactZombies lessons
- Weeks 7-9: Advanced topics
- Weeks 10-12: Team projects
- Weeks 13-15: Presentations

### Corporate Training (3 days)
- Day 1: Blockchain and Pact basics
- Day 2: Building with PactZombies
- Day 3: Production considerations

## Conclusion

PactZombies provides a fun, engaging way to learn blockchain development. Emphasize hands-on coding, encourage experimentation, and celebrate creative solutions!

Remember: The goal is not just to teach syntax, but to inspire developers to build on Kadena! 🚀