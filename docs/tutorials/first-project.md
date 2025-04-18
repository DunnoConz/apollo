# Your First Apollo Project

This tutorial will guide you through creating and compiling your first Racket program with Apollo.

## Prerequisites

*   Apollo compiler installed (see [Installation](../tutorials/installation.md))
*   Basic understanding of Racket syntax
*   A text editor

## Step 1: Create a Simple Racket Program

Create a new file called `hello.rkt` with the following content:

```racket
#lang racket

(define (greet name)
  (printf "Hello, ~a!\n" name))

(greet "World")
```

## Step 2: Compile with Apollo

Run the Apollo compiler on your Racket file:

```bash
apollo compile hello.rkt -o hello.luau
```

This will generate a Luau file named `hello.luau`.

## Step 3: Run the Generated Luau Code

You can now use the generated Luau code in your Roblox project or test it in a Luau environment.

## What's Next?

*   Learn about [Apollo's type system](../explanation/type-system.md)
*   See more [examples](../tutorials/examples.md)
*   Check out the [CLI reference](../reference/cli.md) for more compiler options 