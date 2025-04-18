# Your First Project

This guide will help you create your first Apollo project.

## Creating a New Project

1. Create a new directory for your project:
   ```bash
   mkdir my-apollo-project
   cd my-apollo-project
   ```

2. Initialize a new Racket project:
   ```bash
   raco pkg init
   ```

3. Create a `main.rkt` file:
   ```racket
   #lang racket

   (require apollo)

   (define (hello-world)
     (displayln "Hello, Roblox!"))

   (hello-world)
   ```

4. Compile your project:
   ```bash
   apollo compile main.rkt
   ```

## Project Structure

A typical Apollo project has the following structure:

```
my-apollo-project/
├── main.rkt          # Main source file
├── info.rkt          # Package information
└── compiled/         # Compiled output
```

## Running Your Project

1. Open Roblox Studio
2. Create a new place
3. Insert a Script into the Workspace
4. Copy the compiled Luau code into the Script
5. Run the game

## Next Steps

- [Learn more about the language](../tutorials/)
- [Try the Playground](./playground)
- [Read the Reference](../reference/) 