#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/string
         ;; Use relative paths
         "../../src/apollo/compiler/ir-types.rkt"
         "../../src/apollo/compiler/codegen.rkt")

(provide luau-pretty-print-tests)

;; Define a simple IR node for testing
(define simple-ir
  (ir-app (ir-var-ref 'print)
          (list (ir-literal "Hello, World!"))
          '()))

;; Define a slightly more complex IR node
(define complex-ir
  (ir-define 'x
             (ir-app (ir-var-ref '+)
                    (list (ir-literal 1) (ir-literal 2))
                    '())))

;; Define if statement IR node
(define if-ir
  (ir-if (ir-app (ir-var-ref '==)
                 (list (ir-var-ref 'a) (ir-literal 1))
                 '())
         (ir-literal #t)
         (ir-literal #f)))

;; Test suite for the Luau pretty-printer
(define luau-pretty-print-tests
  (test-suite
   "Luau Pretty Printer Tests"

   ;; Test case 1: Simple function call
   (test-case "Simple function call pretty-printing"
     (let ([printed-code (ir->luau simple-ir)])
       (check-equal? printed-code "print(\"Hello, World!\")" "Pretty-printed simple function call is correct")))

   ;; Test case 2: Simple assignment with binary operation
   (test-case "Complex assignment pretty-printing"
     (let ([printed-code (ir->luau complex-ir)])
       (check-equal? printed-code "local x = 1 + 2" "Pretty-printed complex assignment is correct")))
   
   ;; Test case 3: Nested structures (Example: if statement)
   (test-case "Nested structure pretty-printing (if statement)"
     (let ([printed-code (ir->luau if-ir)])
       ;; Note: Basic pretty-printer might not handle indentation perfectly
       (check-pred string? printed-code) ; Check if it's a string
       (check-true (string-contains? printed-code "if a == 1 then") "Contains 'if' condition")
       (check-true (string-contains? printed-code "true") "Contains 'then' block content")
       (check-true (string-contains? printed-code "else") "Contains 'else'")
       (check-true (string-contains? printed-code "false") "Contains 'else' block content")
       (check-true (string-contains? printed-code "end") "Contains 'end'")))))

;; Removed run-tests call 