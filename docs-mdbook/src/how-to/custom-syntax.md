# Creating Custom Syntax

This guide shows you how to create custom syntax for your Apollo language. We'll walk through several common scenarios and patterns.

## Basic Syntax Definition

To define custom syntax, use the `define-syntax` form:

```racket
(define-syntax (my-if stx)
  (syntax-parse stx
    [(_ test then else)
     #'(if test then else)]))
```

## Common Patterns

### 1. Keyword Arguments

```racket
(define-syntax (define-command stx)
  (syntax-parse stx
    [(_ name:id
        (~optional (~seq #:description desc:str) #:defaults ([desc #'"No description"]))
        body:expr ...)
     #'(begin
         (hash-set! commands 'name
                    (command #:name 'name
                            #:description desc
                            #:handler (lambda () body ...))))]))

;; Usage:
(define-command greet
  #:description "Greets the user"
  (displayln "Hello!"))
```

### 2. Pattern Variables

```racket
(define-syntax (with-logging stx)
  (syntax-parse stx
    [(_ expr:expr ...)
     #'(begin
         (log-debug "Starting execution")
         (let ([result (begin expr ...)])
           (log-debug "Finished execution")
           result))]))

;; Usage:
(with-logging
  (perform-task)
  (cleanup))
```

### 3. Custom Pattern Classes

```racket
(define-syntax-class identifier-list
  #:description "comma-separated list of identifiers"
  (pattern (x:id ...)
           #:with (name ...) #'(x ...)))

(define-syntax (define-fields stx)
  (syntax-parse stx
    [(_ name:id (fields:identifier-list))
     #'(begin
         (define-struct name (fields.name ...))
         (provide name))]))

;; Usage:
(define-fields person (name, age, address))
```

## Error Handling

Add proper error messages and checks:

```racket
(define-syntax (safe-div stx)
  (syntax-parse stx
    [(_ n:expr d:expr)
     #:when (syntax-parse #'d
              [(~literal 0) #f]
              [_ #t])
     #'(let ([denom d])
         (if (zero? denom)
             (error 'safe-div "Division by zero")
             (/ n denom)))]))
```

## Hygiene

Maintain hygiene by using `syntax-local-introduce`:

```racket
(define-syntax (with-temp-file stx)
  (syntax-parse stx
    [(_ filename:id body:expr ...)
     (with-syntax ([temp (syntax-local-introduce #'filename)])
       #'(let ([temp (make-temporary-file)])
           (dynamic-wind
             void
             (lambda () body ...)
             (lambda () (delete-file temp)))))]))
```

## Debugging Tips

> tip: Debugging Macros
> Use these tools to debug your syntax:
> - `syntax->datum` to see the raw syntax
> - `(debug-print stx)` for detailed inspection
> - `expand-once` to see one level of expansion

## Best Practices

1. **Documentation**
   ```racket
   (define-syntax (my-for stx)
     (syntax-parse stx
       #:doc "Iterates over a list with index"
       [(_ ([i:id x:id] lst:expr) body:expr ...)
        #'(for/list ([x lst]
                     [i (range (length lst))])
            body ...)]))
   ```

2. **Phase Separation**
   ```racket
   (begin-for-syntax
     (define (check-identifier id)
       (unless (identifier? id)
         (raise-syntax-error 'check-identifier
                            "expected identifier"
                            id))))
   ```

3. **Testing**
   ```racket
   (module+ test
     (require rackunit)
     
     (check-equal? 
       (my-if #t 1 2)
       1)
     
     (check-exn
       exn:fail:syntax?
       (lambda () 
         (expand #'(my-if)))))
   ```

## Common Pitfalls

> warning: Common Issues
> - Forgetting to use `syntax-parse`
> - Not handling all cases
> - Breaking hygiene
> - Phase level mismatches

## See Also

- [Pattern Matching](pattern-matching.md)
- [Syntax Reference](../reference/syntax.md)
- [Macro System](../explanation/macros.md) 