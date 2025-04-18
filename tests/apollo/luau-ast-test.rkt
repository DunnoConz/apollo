#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/list
         racket/string
         ;; Use relative paths
         "../../src/apollo/compiler/ir-types.rkt"
         "../../src/apollo/compiler/codegen.rkt")

(provide luau-ast-tests) ; Export the test suite

;; Test suite for Luau code generation
(define luau-ast-tests
  (test-suite
   "Luau AST Tests"
   
   ;; Example test: Create a local variable
   (test-case "Create local variable"
     (define ir (ir-define 'x (ir-literal 10)))
     (define luau-str (ir->luau ir))
     (check-equal? luau-str "local x = 10"))
   
   ;; Test expressions
   (test-case "Luau expressions"
     (check-equal? (ir->luau (ir-literal 42)) "42")
     (check-equal? (ir->luau (ir-var-ref 'x)) "x")
     (check-equal? (ir->luau (ir-app (ir-var-ref '+)
                                    (list (ir-literal 1) (ir-literal 2))
                                    '()))
                   "1 + 2")
     (check-equal? (ir->luau (ir-app (ir-var-ref 'not)
                                    (list (ir-literal #t))
                                    '()))
                   "not true")
     (check-equal? (ir->luau (ir-app (ir-var-ref 'f)
                                    '()
                                    '()))
                   "f()"))

   ;; Test statements
   (test-case "Luau statements"
     (check-equal? (ir->luau (ir-define 'x (ir-literal 5)))
                   "local x = 5")
     (check-equal? (ir->luau (ir-if (ir-literal #t)
                                   (ir-literal 1)
                                   (ir-literal 2)))
                   "if true then\n  1\nelse\n  2\nend")
     (check-equal? (ir->luau (ir-app (ir-var-ref 'print)
                                    (list (ir-literal "hello"))
                                    '()))
                   "print(\"hello\")"))

   ;; Test tables
   (test-case "Luau tables"
     (check-equal? (ir->luau (ir-literal '())) "{}")
     (check-equal? (ir->luau (ir-literal '(1 2 3))) "{1, 2, 3}")
     (check-equal? (ir->luau (ir-literal (hash 'x 10 'y 20)))
                   "{x = 10, y = 20}"))

   ;; Test module
   (test-case "Luau module"
     (define ir (ir-program
                 (list
                  (ir-module 'test #f
                            (list
                             (ir-define 'add
                                       (ir-lambda '(a b)
                                                 '()
                                                 (list
                                                  (ir-app (ir-var-ref '+)
                                                         (list (ir-var-ref 'a)
                                                               (ir-var-ref 'b))
                                                         '())))))))))
     (define luau-str (ir->luau ir))
     (check-true (string? luau-str))
     (check-true (> (string-length luau-str) 0))
     (check-true (string-contains? luau-str "local function add(a, b)"))
     (check-true (string-contains? luau-str "return a + b")))))

;; Run the tests (optional, raco test usually handles this)
;(run-tests luau-ast-tests) 