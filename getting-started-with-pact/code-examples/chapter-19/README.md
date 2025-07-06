# Chapter 19: Blockchain RPG Game Implementation

This directory contains a complete blockchain RPG game implementation in Pact.

## Core Modules

### 1. **game-characters.pact**
Character creation and management system.
- Character classes: Warrior, Mage, Rogue, Cleric
- Leveling system with stat progression
- Experience and status management

### 2. **game-combat-fixed.pact**
Turn-based combat system with improved randomness.
- Physical and magical attacks
- Spell casting with mana costs
- Battle logging and rewards
- Combat statistics tracking

### 3. **game-items-fixed.pact**
Complete item and equipment system.
- Item templates with rarity tiers
- Inventory management with stacking
- Equipment slots (weapon, armor, accessory)
- Consumable items with effects

### 4. **game-dungeons.pact**
Dungeon exploration with encounters and loot.
- Multi-floor dungeons with bosses
- Monster battles with loot drops
- Dungeon completion rewards
- Difficulty scaling

### 5. **game-quests.pact**
Quest and achievement system.
- Main, side, and daily quests
- Multi-objective quest tracking
- Achievement unlocking
- Quest rewards distribution

## Test Files

- **test-game-simple.repl**: Basic character creation test
- **test-game-complete.repl**: Comprehensive system test
- **test-game-full.repl**: Full integration test with all modules

## Running the Game

```bash
# Run the complete test suite
pact test-game-full.repl

# Test individual components
pact test-game-simple.repl
```

## Game Features

### Character System
- 4 unique classes with different stat distributions
- Automatic stat increases on level up
- Health and mana management
- Status tracking (active, in-combat, defeated)

### Combat System
- Turn-based battles between characters
- Damage calculation with defense and level modifiers
- Critical hit chances based on agility
- Spell casting with mana costs
- Experience rewards based on opponent level

### Item System
- 5 rarity tiers: Common, Uncommon, Rare, Epic, Legendary
- Stackable consumables and materials
- Equipment with stat bonuses
- Level requirements for items
- Inventory management with limited slots

### Dungeon System
- Multi-floor dungeons with increasing difficulty
- Random monster encounters
- Boss battles with special rewards
- Loot drop system with drop rates
- Flee and rest options

### Quest System
- Quest types: Main story, side quests, daily quests
- Multi-objective quest tracking
- Level and class requirements
- Achievement system
- Repeatable daily quests

## Key Design Patterns

### 1. Capability-Based Security
All sensitive operations require proper capabilities:
```pact
(defcap BATTLE (character-id:string)
  @doc "Battle participation capability"
  (with-read game-characters.characters character-id 
    { "owner" := owner, "status" := status }
    (enforce (= status "active") "Character not active")
    (enforce-guard (at 'guard (coin.details owner)))))
```

### 2. Pseudo-Random Generation
Using blockchain data for deterministic randomness:
```pact
(defun get-pseudo-random:decimal ()
  @doc "Generate pseudo-random number between 0 and 1"
  (let* ((height (at 'block-height (chain-data)))
         (seed-mod (mod height 1000)))
    (/ seed-mod 1000.0)))
```

### 3. Modular Architecture
Each game system is a separate module that can interact with others:
- Characters module is the foundation
- Combat uses character data
- Items modify character stats
- Dungeons create combat encounters
- Quests track various activities

### 4. State Management
Game state is stored in tables with proper schemas:
```pact
(defschema character
  character-id:string
  owner:string
  name:string
  class:string
  level:integer
  ;; ... more fields
)
```

## Example Game Flow

1. **Create Character**
```pact
(game-characters.create-character "alice" "Thorin" "warrior")
```

2. **Accept Quest**
```pact
(game-quests.accept-quest "CHAR-alice-1000" "tutorial-1")
```

3. **Enter Dungeon**
```pact
(game-dungeons.enter-dungeon "CHAR-alice-1000" "goblin-cave")
```

4. **Battle Monsters**
```pact
(game-dungeons.explore-floor "RUN-xxx" "fight")
```

5. **Equip Items**
```pact
(game-items.equip-item "CHAR-alice-1000" 0)
```

6. **Level Up**
```pact
(game-characters.gain-experience "CHAR-alice-1000" 100)
```

## Future Enhancements

- Multiplayer guild system
- Trading between players
- Crafting system
- PvP arena with rankings
- Seasonal events
- NFT integration for rare items

This implementation demonstrates how to build complex game logic on the blockchain while maintaining security and fairness through smart contracts.