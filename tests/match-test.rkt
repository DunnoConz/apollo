#lang racket/base

(require rackunit
         (submod apollo/compiler/ir ir)
         apollo/compiler/codegen)

;; Test a simple match expression with variable and wildcard patterns
(define simple-match
  #'(module test racket
      (match 42
        [x x]
        [_ 0])))

;; Test a more complex match with list patterns
(define list-match
  #'(module test racket
      (match (list 1 2 3)
        [(list x y z) (+ x y z)]
        [_ 0])))

;; Convert to IR
(define simple-match-ir (racket-to-ir simple-match))
(define list-match-ir (racket-to-ir list-match))

;; Display the IR
(displayln "Simple match IR:")
(pretty-print (ir->datum simple-match-ir))

(displayln "\nList match IR:")
(pretty-print (ir->datum list-match-ir))

;; Convert to Luau
(define simple-match-luau (ir->luau simple-match-ir))
(define list-match-luau (ir->luau list-match-ir))

;; Display the generated Luau code
(displayln "\nSimple match Luau:")
(displayln (luau-ast->string simple-match-luau))

(displayln "\nList match Luau:")
(displayln (luau-ast->string list-match-luau))

;; Run tests
(test-case "Simple match test"
  (check-true (ir-program? simple-match-ir) 
              "Simple match should be an ir-program"))

(test-case "List match test"
  (check-true (ir-program? list-match-ir)
              "List match should be an ir-program")) 