# Using Racket Macros with Apollo

This guide explains how to use Racket macros in your Apollo projects.

## Basic Macro Usage

Apollo supports most Racket macros. Here's how to use them:

```racket
#lang racket

(define-syntax-rule (twice expr)
  (begin expr expr))

(twice (print "Hello"))  ; Prints "Hello" twice
```

## Common Patterns

### 1. Simple Syntax Rules

```racket
(define-syntax-rule (unless condition body ...)
  (if (not condition) (begin body ...)))
```

### 2. Pattern Matching Macros

```racket
(define-syntax (match-let stx)
  (syntax-case stx ()
    [(_ ([pat expr] ...) body ...)
     #'(let ([tmp expr] ...)
         (match-let* ([pat tmp] ...)
           body ...))]))
```

## Limitations

*   Some advanced macro features might not be supported
*   Macro expansion happens at compile time
*   Complex macro patterns might need special handling

## Troubleshooting

If you encounter issues with macros:

1.  Check the compiler output for macro-related errors
2.  Simplify complex macro patterns
3.  Use the `--verbose` flag for more detailed error messages

## See Also

*   [Macro Reference](../reference/macros.md)
*   [Advanced Macro Patterns](../tutorials/advanced-macros.md) 