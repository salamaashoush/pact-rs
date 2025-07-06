# Lesson 2: Zombies Attack Their Victims 🧟‍♂️⚔️

Welcome back! In the last lesson, you created your first zombie. Now it's time to make them dangerous! In this lesson, you'll learn how to:

- Create zombie feeding mechanics
- Implement battle systems
- Level up your zombies
- Work with more complex data structures

## Chapter 1: Feeding on Humans

Zombies need to eat! Let's create a feeding mechanism where zombies can attack humans and create new zombies.

### Understanding Function Parameters

In Pact, functions can take multiple parameters and return different types:

```pact
(defun feed-on-human (zombie-id:string victim-name:string)
  @doc "Zombie feeds on a human and creates a new zombie"
  ; Implementation goes here
)
```

### Exercise 2.1

Copy your `zombie-factory.pact` from lesson 1 to `zombie-feeding.pact` and we'll extend it.

## Chapter 2: Creating New Zombies from Victims

When a zombie feeds, it should create a new zombie from the victim. Let's implement this:

```pact
(defun feed-and-multiply (zombie-id:string victim-name:string species:string)
  @doc "Zombie feeds and creates a new zombie"
  (with-read zombies-table zombie-id
    { "owner" := owner
    , "dna" := zombie-dna
    , "name" := zombie-name
    , "level" := level
    }
    ; Only owner can make their zombie feed
    (enforce (= owner (at "sender" (chain-data))) "Not your zombie!")
    
    ; Generate new zombie DNA by combining
    (let* ((victim-dna (generate-dna victim-name))
           (new-dna (/ (+ zombie-dna victim-dna) 2))
           (new-zombie-name (format "Zombie {}" [victim-name]))
           (new-zombie-id (format "zombie_{}" [new-dna])))
      
      ; Create the new zombie
      (insert zombies-table new-zombie-id
        { "name": new-zombie-name
        , "dna": new-dna
        , "level": 1
        , "ready-time": (at "block-time" (chain-data))
        , "win-count": 0
        , "loss-count": 0
        , "owner": owner
        })
      
      ; Level up the feeding zombie
      (update zombies-table zombie-id
        { "level": (+ level 1)
        , "win-count": (+ (at "win-count" (read zombies-table zombie-id)) 1)
        })
      
      (format "{} fed on {} and created {}" 
        [zombie-name victim-name new-zombie-name]))))
```

### Exercise 2.2

Add the `feed-and-multiply` function to your module. Notice how we:
1. Read the zombie data
2. Check ownership
3. Create a new zombie
4. Level up the original zombie

## Chapter 3: Zombie Battles

Let's add a battle system where zombies can fight each other!

### Battle Mechanics

```pact
(defun calculate-battle-power:integer (zombie:object{zombie})
  @doc "Calculate zombie's battle power"
  (let ((dna (at "dna" zombie))
        (level (at "level" zombie))
        (wins (at "win-count" zombie)))
    (+ (* level 100) 
       (* wins 50) 
       (mod dna 100))))

(defun battle (attacker-id:string defender-id:string)
  @doc "Two zombies battle!"
  (let ((attacker (read zombies-table attacker-id))
        (defender (read zombies-table defender-id)))
    
    ; Check ownership of attacker
    (enforce (= (at "owner" attacker) (at "sender" (chain-data))) 
      "Not your zombie!")
    
    ; Calculate battle powers
    (let ((attacker-power (calculate-battle-power attacker))
          (defender-power (calculate-battle-power defender))
          (random-factor (mod (at "block-height" (chain-data)) 100)))
      
      ; Add some randomness
      (let ((final-attacker-power (+ attacker-power random-factor))
            (final-defender-power (+ defender-power (- 100 random-factor))))
        
        (if (> final-attacker-power final-defender-power)
          ; Attacker wins
          (let ()
            (update zombies-table attacker-id 
              { "win-count": (+ (at "win-count" attacker) 1)
              , "level": (+ (at "level" attacker) 1)
              })
            (update zombies-table defender-id
              { "loss-count": (+ (at "loss-count" defender) 1)
              })
            (format "{} defeated {}! Power: {} vs {}" 
              [(at "name" attacker) (at "name" defender) 
               final-attacker-power final-defender-power]))
          ; Defender wins
          (let ()
            (update zombies-table defender-id
              { "win-count": (+ (at "win-count" defender) 1)
              })
            (update zombies-table attacker-id
              { "loss-count": (+ (at "loss-count" attacker) 1)
              })
            (format "{} was defeated by {}! Power: {} vs {}"
              [(at "name" attacker) (at "name" defender)
               final-attacker-power final-defender-power])))))))
```

### Exercise 2.3

1. Add the battle functions to your module
2. Test battles between different zombies
3. Notice how level and wins affect battle power

## Chapter 4: Cooldowns and Ready Time

Zombies shouldn't be able to attack continuously. Let's add cooldowns:

```pact
(defconst BATTLE_COOLDOWN 300.0) ; 5 minutes in seconds

(defun check-ready (zombie-id:string)
  @doc "Check if zombie is ready to battle"
  (with-read zombies-table zombie-id { "ready-time" := ready-time }
    (let ((current-time (at "block-time" (chain-data))))
      (>= (diff-time current-time ready-time) BATTLE_COOLDOWN))))

(defun update-ready-time (zombie-id:string)
  @doc "Update zombie's ready time after action"
  (update zombies-table zombie-id 
    { "ready-time": (add-time (at "block-time" (chain-data)) BATTLE_COOLDOWN) }))
```

### Exercise 2.4

Update your battle function to:
1. Check if the attacking zombie is ready
2. Update the ready time after battle
3. Add appropriate error messages

## Chapter 5: Challenge - Special Abilities

Add these special features to your zombies:

1. **Critical Hits**: 10% chance to deal double damage
2. **Zombie Types**: Different types have advantages (rock-paper-scissors style)
3. **Experience Points**: Track XP separately from level
4. **Special Moves**: Unique abilities based on DNA

## Quiz

1. How do we check ownership of a zombie in Pact?
2. What's the purpose of the `with-read` construct?
3. How can we add randomness to our battle system?
4. Why do we need cooldowns in our game?

## Complete Code

Check `zombie-feeding-complete.pact` for the full implementation with all features!

## Summary

Great job! In this lesson you learned:
- ✅ Creating complex game mechanics
- ✅ Working with object data from tables
- ✅ Implementing ownership checks
- ✅ Creating battle systems with randomness
- ✅ Managing cooldowns and time-based mechanics

Next lesson: We'll dive deep into Pact's powerful capability system for advanced security!