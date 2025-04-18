# Apollo Playground

Welcome to the Apollo Playground! Here you can experiment with Racket code and see how it compiles to Luau.

<Playground />

## Features

- Write and edit Racket code in a modern editor
- See the compiled Luau output
- Experiment with different Racket features
- Learn how Apollo translates Racket to Luau

## Tips

1. Start with simple expressions to see how they translate
2. Try different Racket language features
3. Check the output to understand the compilation process
4. Use the reset button to start fresh

## Examples

Here are some examples you can try:

```racket
#lang racket

;; Simple arithmetic
(+ 1 2 3)

;; Function definition
(define (square x)
  (* x x))

;; List operations
(map square '(1 2 3 4 5))

;; Conditionals
(if (> 5 3)
    "Five is greater than three"
    "This won't happen")
```

Happy coding! 