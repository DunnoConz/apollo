# Macro Reference

This document provides a reference for Racket macros supported by Apollo.

## Syntax Rules

### define-syntax-rule

```racket
(define-syntax-rule (pattern) template)
```

**Example:**
```racket
(define-syntax-rule (twice expr)
  (begin expr expr))
```

## Pattern Matching

### syntax-case

```racket
(define-syntax (name stx)
  (syntax-case stx ()
    [pattern template]
    ...))
```

**Example:**
```racket
(define-syntax (match-let stx)
  (syntax-case stx ()
    [(_ ([pat expr] ...) body ...)
     #'(let ([tmp expr] ...)
         (match-let* ([pat tmp] ...)
           body ...))]))
```

## Special Forms

### begin

```racket
(begin expr ...)
```

### let

```racket
(let ([id expr] ...) body ...)
```

### if

```racket
(if test-expr then-expr else-expr)
```

## Limitations

1.  **Supported Features**
    - Basic syntax rules
    - Pattern matching
    - Special forms
    - Hygiene

2.  **Unsupported Features**
    - Complex macro transformers
    - Some advanced pattern matching
    - Certain special forms

## Best Practices

1.  **Keep it Simple**
    - Use simple patterns when possible
    - Avoid complex macro nesting
    - Document macro behavior

2.  **Error Handling**
    - Provide clear error messages
    - Use contracts when possible
    - Test thoroughly

## See Also

*   [Using Macros](../how-to/macros.md)
*   [Advanced Patterns](../tutorials/advanced-macros.md) 