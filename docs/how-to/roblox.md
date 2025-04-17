# Using Apollo with Roblox

This guide explains how to use Apollo-compiled code in Roblox projects.

## Basic Setup

### 1. Project Structure

Organize your project like this:
```
project/
├── src/
│   ├── game/
│   │   └── main.rkt    # Main game logic
│   └── ui/
│       └── components.rkt  # UI components
├── out/
│   └── game.luau       # Compiled output
└── apollo.config.json  # Compiler configuration
```

### 2. Compilation

Compile your Racket code for Roblox:
```bash
apollo compile src/game/main.rkt -o out/game.luau --target roblox
```

### 3. Integration

Add the compiled code to Roblox:
1.  Create a new ModuleScript in Roblox Studio
2.  Paste the compiled Luau code
3.  Require the module in your game scripts

## Best Practices

### 1. Type Safety

Use typed/racket for critical game logic:
```racket
#lang typed/racket

(: update-player-position (-> Player Number Number Void))
(define (update-player-position player x y)
  (set-Player-x! player x)
  (set-Player-y! player y))
```

### 2. Event Handling

Handle Roblox events in Racket:
```racket
(define (on-player-joined player)
  (printf "Player ~a joined the game\n" (Player-name player))
  (setup-player player))

; Compiles to:
local function onPlayerJoined(player)
    print(string.format("Player %s joined the game", player.Name))
    setupPlayer(player)
end

game.Players.PlayerAdded:Connect(onPlayerJoined)
```

### 3. Performance

Optimize for Roblox's environment:
```racket
#lang typed/racket

(: fast-update (-> (Vectorof Number) Void))
(define (fast-update positions)
  (for ([pos (in-vector positions)])
    (update-object pos)))
```

## Common Patterns

### 1. Game State Management

```racket
(struct GameState
  ([players : (HashTable String Player)]
   [objects : (Vectorof GameObject)]
   [score : Integer]))

(define (update-game-state state)
  (for ([(id player) (in-hash (GameState-players state))])
    (update-player player)))
```

### 2. UI Components

```racket
(struct UIElement
  ([name : String]
   [position : (Vector Number Number)]
   [visible : Boolean]))

(define (create-button text position)
  (UIElement text position #t))
```

## Troubleshooting

### Common Issues

1.  **Type Mismatches**
    - Use explicit type annotations
    - Check Roblox API types
    - Use contracts for runtime checking

2.  **Performance Issues**
    - Profile your code
    - Use appropriate data structures
    - Minimize table creation

3.  **Memory Usage**
    - Avoid creating unnecessary objects
    - Use object pooling where appropriate
    - Monitor garbage collection

## See Also

*   [Performance Optimization](../tutorials/advanced.md#performance-optimization)
*   [Type System](../explanation/type-system.md)
*   [Error Messages](../reference/errors.md) 