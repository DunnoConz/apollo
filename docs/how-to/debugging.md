# Debugging Apollo Projects

This guide explains how to debug issues in your Apollo projects.

## Common Issues and Solutions

### 1. Compilation Errors

If you get compilation errors:

```bash
# Enable verbose output
apollo compile program.rkt -v

# Check type errors specifically
apollo check program.rkt --strict
```

### 2. Runtime Errors in Generated Code

For runtime errors in the generated Luau code:

1.  Enable source maps:
    ```bash
    apollo compile program.rkt --source-maps
    ```

2.  Use the debug configuration:
    ```json
    {
      "debug": true,
      "sourceMaps": true
    }
    ```

### 3. Type Mismatches

For type-related issues:

1.  Add explicit type annotations:
    ```racket
    #lang typed/racket
    (: my-function (-> Number String))
    ```

2.  Use contracts for runtime checking:
    ```racket
    (require racket/contract)
    (define/contract (my-function x)
      (-> number? string?)
      ...)
    ```

## Debugging Tools

### 1. Logging

Add debug prints in Racket:
```racket
(define (debug-print msg)
  (printf "[DEBUG] ~a\n" msg))
```

### 2. Type Checking

Use the type checker in strict mode:
```bash
apollo check program.rkt --strict
```

### 3. Source Maps

Generate and use source maps to trace issues back to Racket code.

## Best Practices

1.  Start with type checking
2.  Use verbose compilation output
3.  Add strategic debug prints
4.  Keep Racket code simple when possible
5.  Test generated Luau code separately

## See Also

*   [Error Messages Reference](../reference/errors.md)
*   [Type System](../explanation/type-system.md) 