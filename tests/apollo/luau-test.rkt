#lang racket/base

(require rackunit
         rackunit/text-ui
         ;; Use relative paths
         "../../src/apollo/compiler/types.rkt"
         "../../src/apollo/compiler/codegen.rkt"
         "../../src/apollo/compiler/ir-types.rkt") ; For ir-program and ir-literal

;; Test suite for Luau AST functionality
(define luau-tests
  (test-suite
   "Luau AST Tests"
   
   (test-case "Number literal"
     (check-equal? (ir->luau (ir-literal 42)) "42"))
   
   (test-case "String literal"
     (check-equal? (ir->luau (ir-literal "hello")) "\"hello\""))
   
   (test-case "Boolean literal"
     (check-equal? (ir->luau (ir-literal #t)) "true")
     (check-equal? (ir->luau (ir-literal #f)) "false"))
   
   (test-case "Nil literal"
     (check-equal? (ir->luau (ir-literal 'null)) "nil"))
   
   (test-case "Binary operation"
     (check-equal? (ir->luau 
                    (ir-app (ir-var-ref '+)
                           (list (ir-literal 1) (ir-literal 2))
                           '()))
                   "1 + 2"))
   
   (test-case "Function definition"
     (check-equal? (ir->luau 
                    (ir-define 'add
                              (ir-lambda '(a b)
                                        '()
                                        (list (ir-app (ir-var-ref '+)
                                                     (list (ir-var-ref 'a)
                                                           (ir-var-ref 'b))
                                                     '())))))
                   "local function add(a, b)\n  return a + b\nend"))
   
   (test-case "If statement"
     (check-equal? (ir->luau 
                    (ir-if (ir-app (ir-var-ref '>)
                                  (list (ir-var-ref 'x)
                                        (ir-literal 0))
                                  '())
                          (ir-literal 1)
                          (ir-literal 0)))
                   "if x > 0 then\n  1\nelse\n  0\nend"))
   
   (test-case "Local variable declaration"
     (check-equal? (ir->luau 
                    (ir-define 'x (ir-literal 10)))
                   "local x = 10"))))

(provide luau-tests) 