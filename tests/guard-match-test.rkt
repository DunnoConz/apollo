#lang racket/base

(require rackunit
         (submod apollo/compiler/ir ir)
         apollo/compiler/codegen)

;; Test match with guard clauses
(define guard-match
  #'(module test-guard racket
      (match (list 1 2 3)
        ;; Match with a guard condition
        [(list x y z) (guard (> x 0)) (+ x y z)]
        
        ;; Match with a different guard
        [(list x y z) (guard (< x 0)) (- x y z)]
        
        ;; Fallback
        [_ 0])))

;; Convert to IR
(define guard-match-ir (racket-to-ir guard-match))

;; Display the IR
(displayln "Guard match IR:")
(pretty-print (ir->datum guard-match-ir))

;; Convert to Luau
(define guard-match-luau (ir->luau guard-match-ir))

;; Display the generated Luau code
(displayln "\nGuard match Luau:")
(displayln (luau-ast->string guard-match-luau))

;; Run test
(test-case "Guard match test"
  (check-true (ir-program? guard-match-ir) 
              "Guard match should be an ir-program")) 