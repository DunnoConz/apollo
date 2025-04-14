#lang racket

;; Basic Quasiquote Pattern Matching Examples
;; This example demonstrates how quasiquote patterns work in the Apollo compiler

;; Simple quasiquote pattern matching

;; Define a function to match and transform data using quasiquote patterns
(define (process-data data)
  (match data
    ;; Match a list with 'user as its first element
    [`(user ,name ,age)
     (format "User ~a is ~a years old" name age)]
    
    ;; Match a list with 'product as its first element
    [`(product ,name ,price)
     (format "Product ~a costs $~a" name price)]
    
    ;; Match a nested list structure
    [`(order (,customer-name ,customer-id) (,items ...))
     (format "Order from ~a (ID: ~a) with ~a items" 
             customer-name customer-id (length items))]
    
    ;; Match a list with any first element and exactly two more elements
    [`(,type ,name ,value)
     (format "~a: ~a = ~a" type name value)]
    
    ;; Default case
    [_ "Unknown data format"]))

;; Test the function with various data structures
(displayln "Basic pattern matching examples:")
(displayln (process-data '(user "Alice" 28)))
(displayln (process-data '(product "Laptop" 999.99)))
(displayln (process-data '(order ("Bob" 12345) ((item1 10) (item2 20) (item3 15)))))
(displayln (process-data '(config "timeout" 30)))
(displayln (process-data '(something-else)))

;; Pattern matching with guards
(define (analyze-number n)
  (match n
    [`(,x ,y) #:when (and (number? x) (number? y) (> x y))
     (format "~a is greater than ~a" x y)]
    
    [`(,x ,y) #:when (and (number? x) (number? y) (< x y))
     (format "~a is less than ~a" x y)]
    
    [`(,x ,y) #:when (and (number? x) (number? y) (= x y))
     (format "~a is equal to ~a" x y)]
    
    [_ "Not a valid comparison"]))

(displayln "\nPattern matching with guards:")
(displayln (analyze-number '(10 5)))
(displayln (analyze-number '(5 10)))
(displayln (analyze-number '(7 7)))
(displayln (analyze-number '("10" 5)))

;; Pattern matching with quasiquote and unquote
(define (parse-expression expr)
  (match expr
    ;; Binary operations
    [`(+ ,a ,b) (format "Addition: ~a + ~a" a b)]
    [`(- ,a ,b) (format "Subtraction: ~a - ~a" a b)]
    [`(* ,a ,b) (format "Multiplication: ~a * ~a" a b)]
    [`(/ ,a ,b) (format "Division: ~a / ~a" a b)]
    
    ;; Function calls
    [`(call ,func-name ,args ...)
     (format "Function call: ~a with arguments ~a" func-name args)]
    
    ;; Variable assignments
    [`(define ,var ,val)
     (format "Variable definition: ~a = ~a" var val)]
    
    ;; Conditionals
    [`(if ,cond ,then-expr ,else-expr)
     (format "Conditional: if ~a then ~a else ~a" cond then-expr else-expr)]
    
    ;; Default case
    [_ "Unknown expression"]))

(displayln "\nExpression parsing with quasiquote patterns:")
(displayln (parse-expression '(+ 5 3)))
(displayln (parse-expression '(- 10 4)))
(displayln (parse-expression '(call print "Hello" "World")))
(displayln (parse-expression '(define x 42)))
(displayln (parse-expression '(if (> x 10) "Greater" "Less or equal")))

;; Pattern matching with recursive structures
(define (count-elements tree)
  (match tree
    ;; Base case: empty list
    ['() 0]
    
    ;; Base case: atom (non-list)
    [(? (lambda (x) (not (list? x)))) 1]
    
    ;; Recursive case: a list with elements
    [`(,head . ,rest)
     (+ (count-elements head) (count-elements rest))]))

(displayln "\nRecursive pattern matching:")
(displayln (format "Count of elements in '(1 2 3): ~a" 
                  (count-elements '(1 2 3))))
(displayln (format "Count of elements in '((1 2) 3 (4 (5 6))): ~a" 
                  (count-elements '((1 2) 3 (4 (5 6))))))

;; Use quasiquote patterns for data transformation
(define (transform-data data)
  (match data
    ;; Transform a user record
    [`(user ,id ,name ,email)
     `(user-profile ,name ,email ,id)]
    
    ;; Transform a product record
    [`(product ,id ,name ,price ,category)
     `(item ,name ,category ,(* price 1.1) ,id)]
    
    ;; Transform a nested structure
    [`(order ,id (,items ...) ,customer)
     `(processed-order ,customer ,id ,(length items))]
    
    ;; Default: return as is
    [other other]))

(displayln "\nData transformation with quasiquote patterns:")
(displayln (transform-data '(user 123 "Alice" "alice@example.com")))
(displayln (transform-data '(product 456 "Headphones" 99.99 "Electronics")))
(displayln (transform-data '(order 789 ((item1) (item2) (item3)) "Bob")))

;; Example of generating Luau code using quasiquote patterns
(define (generate-luau-from-pattern pattern)
  (match pattern
    ;; Variable declaration
    [`(var ,name ,value)
     (format "local ~a = ~a" name value)]
    
    ;; Function declaration
    [`(function ,name (,params ...) ,body ...)
     (string-append 
      (format "local function ~a(" name)
      (string-join (map (lambda (p) (format "~a" p)) params) ", ")
      ")\n"
      "  "
      (string-join 
       (map (lambda (expr) 
              (generate-luau-from-pattern expr))
            body)
       "\n  ")
      "\nend")]
    
    ;; If statement
    [`(if ,condition ,then-expr ,else-expr)
     (string-append
      (format "if ~a then\n" (generate-luau-from-pattern condition))
      (format "  ~a\n" (generate-luau-from-pattern then-expr))
      (format "else\n")
      (format "  ~a\n" (generate-luau-from-pattern else-expr))
      (format "end"))]
    
    ;; Return statement
    [`(return ,value)
     (format "return ~a" value)]
    
    ;; String literal
    [(? string? s)
     (format "\"~a\"" s)]
    
    ;; Number literal
    [(? number? n)
     (format "~a" n)]
    
    ;; Simple identifier
    [(? symbol? sym)
     (symbol->string sym)]
    
    ;; Binary operation
    [`(,op ,a ,b) #:when (member op '(+ - * / == ~= > < >= <=))
     (format "(~a ~a ~a)" 
             (generate-luau-from-pattern a) 
             op 
             (generate-luau-from-pattern b))]
    
    ;; Function call
    [`(call ,func ,args ...)
     (format "~a(~a)" 
             func 
             (string-join 
              (map generate-luau-from-pattern args) 
              ", "))]
    
    ;; Default
    [_ "-- Unsupported pattern"]))

;; Example program to generate
(define example-program
  '(function calculateDiscount ((price quantity))
     (var baseDiscount (call math.min (* quantity 0.05) 0.2))
     (if (> price 100)
         (return (+ baseDiscount 0.05))
         (return baseDiscount))))

(displayln "\nGenerated Luau code:")
(displayln (generate-luau-from-pattern example-program)) 