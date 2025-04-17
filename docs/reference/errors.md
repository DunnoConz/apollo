# Error Messages Reference

This document lists and explains common error messages in Apollo.

## Compilation Errors

### Type Errors

#### `Type mismatch: expected X, got Y`

**Example:**
```racket
(define (add x y) (+ x y))
(add "hello" 42)  ; Error: Type mismatch: expected number, got string
```

**Solution:**
- Check the types of your arguments
- Add type annotations if needed
- Use type conversion functions if appropriate

#### `Undefined identifier: X`

**Example:**
```racket
(print x)  ; Error: Undefined identifier: x
```

**Solution:**
- Define the variable before using it
- Check for typos in variable names
- Ensure the identifier is in scope

### Syntax Errors

#### `Invalid syntax at line X`

**Example:**
```racket
(define x 42  ; Missing closing parenthesis
```

**Solution:**
- Check for matching parentheses
- Verify proper indentation
- Ensure proper syntax for special forms

#### `Unexpected token: X`

**Example:**
```racket
(define x @ 42)  ; Error: Unexpected token: @
```

**Solution:**
- Check for invalid characters
- Verify proper syntax for the language construct
- Remove or replace invalid tokens

## Runtime Errors

### `Division by zero`

**Example:**
```racket
(/ 42 0)  ; Error: Division by zero
```

**Solution:**
- Add checks for zero denominators
- Use contracts to prevent zero values
- Handle the error case appropriately

### `Index out of bounds`

**Example:**
```racket
(vector-ref (vector 1 2 3) 5)  ; Error: Index out of bounds
```

**Solution:**
- Check array bounds before access
- Use safe access functions
- Add bounds checking

## Macro Errors

### `Macro expansion failed`

**Example:**
```racket
(define-syntax-rule (twice x) (begin x x))
(twice)  ; Error: Macro expansion failed: missing argument
```

**Solution:**
- Check macro argument count
- Verify macro pattern matching
- Ensure proper syntax in macro body

### `Invalid macro pattern`

**Example:**
```racket
(define-syntax (bad-macro stx)
  (syntax-case stx ()
    [(_ x y) #'(+ x)]))  ; Error: Invalid macro pattern
```

**Solution:**
- Check macro pattern syntax
- Ensure all pattern variables are used
- Verify macro expansion result

## Best Practices

1.  Enable verbose output for detailed error messages:
    ```bash
    apollo compile program.rkt -v
    ```

2.  Use type annotations to catch errors early:
    ```racket
    #lang typed/racket
    (: add (-> Number Number Number))
    ```

3.  Add contracts for runtime checking:
    ```racket
    (require racket/contract)
    (define/contract (safe-divide x y)
      (-> number? (not/c zero?) number?)
      (/ x y))
    ```

## See Also

*   [Debugging Guide](../how-to/debugging.md)
*   [Type System](../explanation/type-system.md) 