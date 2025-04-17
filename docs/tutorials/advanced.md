# Advanced Features

This tutorial covers advanced features of the Apollo compiler.

## Custom Type Definitions

Define custom types using Racket's type system:

```racket
#lang typed/racket

(struct Point ([x : Real] [y : Real]))

(: distance (-> Point Point Real))
(define (distance p1 p2)
  (sqrt (+ (sqr (- (Point-x p1) (Point-x p2)))
           (sqr (- (Point-y p1) (Point-y p2))))))
```

## Advanced Macros

Create complex macros with syntax-case:

```racket
(define-syntax (for stx)
  (syntax-case stx ()
    [(_ ([var init] ...) test step body ...)
     #'(let loop ([var init] ...)
         (when test
           body ...
           (loop step ...)))]))
```

## Module System

Organize code into modules:

```racket
#lang racket

(module utils racket
  (provide add multiply)
  
  (define (add x y) (+ x y))
  (define (multiply x y) (* x y)))

(require 'utils)
(add 1 2)  ; => 3
```

## Contracts

Add runtime checks with contracts:

```racket
(require racket/contract)

(define/contract (safe-divide x y)
  (-> number? (and/c number? (not/c zero?)) number?)
  (/ x y))
```

## Performance Optimization

Optimize your code:

1.  Use type annotations:
    ```racket
    (: fast-sum (-> (Listof Number) Number))
    (define (fast-sum lst)
      (foldl + 0 lst))
    ```

2.  Enable compiler optimizations:
    ```bash
    apollo compile program.rkt --optimize
    ```

3.  Use appropriate data structures:
    ```racket
    ; Use vectors for random access
    (define vec (vector 1 2 3))
    (vector-ref vec 0)
    ```

## Best Practices

1.  Use typed/racket for performance-critical code
2.  Keep macros simple and well-documented
3.  Use contracts for public APIs
4.  Profile and optimize hot paths
5.  Write unit tests for complex code

## What's Next?

*   [Type System](../explanation/type-system.md)
*   [Standard Library](../reference/stdlib.md)
*   [Debugging Guide](../how-to/debugging.md) 