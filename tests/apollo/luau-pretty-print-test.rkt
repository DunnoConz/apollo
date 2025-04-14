#lang racket/base

(require rackunit
         racket/string
         "../../src/apollo/compiler/luau-ast.rkt"
         "../../src/apollo/compiler/luau-pretty-print.rkt")

(provide luau-pretty-print-tests)

;; Define a simple Luau AST node for testing
(define simple-ast
  (LuauFunctionCall (LuauIdentifier "print")
                    (list (LuauStringLiteral "Hello, World!"))))

;; Define a slightly more complex Luau AST node
(define complex-ast
  (LuauAssignment (list (LuauIdentifier "x"))
                  (list (LuauBinaryOp '+ (LuauNumberLiteral 1) (LuauNumberLiteral 2)))))

;; Define if statement AST node
(define if-ast
  (LuauIfStatement (LuauBinaryOp '== (LuauIdentifier "a") (LuauNumberLiteral 1))
                   (list (LuauReturn (LuauBooleanLiteral #t)))
                   (list (LuauReturn (LuauBooleanLiteral #f)))))

;; Test suite for the Luau pretty-printer
(define luau-pretty-print-tests
  (test-suite
   "Luau Pretty Printer Tests"

   ;; Test case 1: Simple function call
   (test-case "Simple function call pretty-printing"
     (let ([printed-code (pretty-print-luau simple-ast)])
       (check-equal? printed-code "print(\"Hello, World!\")" "Pretty-printed simple function call is correct")))

   ;; Test case 2: Simple assignment with binary operation
   (test-case "Complex assignment pretty-printing"
     (let ([printed-code (pretty-print-luau complex-ast)])
       (check-equal? printed-code "x = 1 + 2" "Pretty-printed complex assignment is correct")))
   
   ;; Test case 3: Nested structures (Example: if statement)
   (test-case "Nested structure pretty-printing (if statement)"
     (let ([printed-code (pretty-print-luau if-ast)])
       ;; Note: Basic pretty-printer might not handle indentation perfectly
       (check-pred string? printed-code) ; Check if it's a string
       (check string-contains? printed-code "if a == 1 then" "Contains 'if' condition")
       (check string-contains? printed-code "return true" "Contains 'then' block content")
       (check string-contains? printed-code "else" "Contains 'else'")
       (check string-contains? printed-code "return false" "Contains 'else' block content")
       (check string-contains? printed-code "end" "Contains 'end'")))))

;; Removed run-tests call 