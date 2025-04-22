# Intermediate Representation (IR) System

Apollo's Intermediate Representation (IR) system provides a robust foundation for language implementation. This reference documents the IR system's components, types, and usage.

## Core Concepts

The IR system translates source code through several stages:
1. Parsing → AST
2. AST → IR
3. IR → Optimized IR
4. IR → Target Code

## IR Types

### Program Structure

```racket
(struct ir-program (modules)
  #:transparent)
```
Represents a complete program containing multiple modules.

```racket
(struct ir-module (name path body)
  #:transparent)
```
Represents a single module with:
- `name`: Module identifier
- `path`: File system path
- `body`: List of expressions

### Expressions

```racket
;; Literals and Variables
(struct ir-literal (value))
(struct ir-var-ref (name))
(struct ir-var-set (name value))
(struct ir-define (name value))

;; Functions
(struct ir-lambda (formals kw-formals body))
(struct ir-app (func args kw-args))

;; Control Flow
(struct ir-if (test then else))
(struct ir-begin (exprs))
(struct ir-let (bindings body))
(struct ir-letrec (bindings body))
```

### Pattern Matching

```racket
;; Match Expression
(struct ir-match (target clauses))

;; Pattern Types
(struct ir-pat-literal (value))
(struct ir-pat-var (name))
(struct ir-pat-wildcard ())
(struct ir-pat-cons (head tail))
```

## IR Conversion

### Converting to IR

```racket
(convert-to-ir stx) → ir-program?
  stx : syntax?
```

Converts a syntax object to an IR program:

```racket
;; Example
(convert-to-ir #'(module test racket/base
                  (+ 1 2)))
;; Produces:
(ir-program
  (list
    (ir-module 'test "test.rkt"
      (list
        (ir-app (ir-var-ref '+)
                (list (ir-literal 1)
                      (ir-literal 2))
                '())))))
```

### Pattern Matching Conversion

```racket
(convert-pattern-to-ir pat) → ir-pattern?
  pat : syntax?
```

Converts pattern syntax to IR patterns:

```racket
;; Example
(convert-pattern-to-ir #'(cons x y))
;; Produces:
(ir-pat-cons
  (ir-pat-var 'x)
  (ir-pat-var 'y))
```

## Optimization

The IR system includes several optimization passes:

1. **Constant Folding**
   ```racket
   ;; Before
   (ir-app (ir-var-ref '+)
           (list (ir-literal 1) (ir-literal 2))
           '())
   ;; After
   (ir-literal 3)
   ```

2. **Dead Code Elimination**
   ```racket
   ;; Before
   (ir-begin
     (list (ir-literal 1)
           (ir-var-ref 'x)))
   ;; After
   (ir-var-ref 'x)
   ```

## Error Handling

The IR system provides comprehensive error checking:

```racket
;; Example error handling
(with-handlers ([exn:fail? (lambda (e)
                            (error 'ir "Invalid syntax: ~a" 
                                  (exn-message e)))])
  (convert-to-ir stx))
```

## Performance Considerations

> info: Caching
> The IR system uses caching to improve performance:
> - Pattern matching cache
> - Expression conversion cache
> - Module path resolution cache

## Best Practices

1. Always use transparent structs for IR nodes
2. Implement proper span tracking for error reporting
3. Use pattern matching for IR manipulation
4. Cache frequently used conversions

## See Also

- [Compiler Pipeline](compiler.md)
- [Pattern Matching](../how-to/pattern-matching.md)
- [IR Optimization](../how-to/optimization.md)
- [IR System Design](../explanation/ir-system.md) 