#lang racket/base

(require rackunit
         (submod apollo/compiler/ir ir)
         apollo/compiler/codegen)

;; Test a struct match expression
(define struct-match
  #'(module test-struct racket
      (struct point (x y))
      (define my-point (point 10 20))
      (match my-point
        [(struct point a b) (+ a b)]
        [_ 0])))

;; Convert to IR
(define struct-match-ir (racket-to-ir struct-match))

;; Display the IR
(displayln "Struct match IR:")
(pretty-print (ir->datum struct-match-ir))

;; Convert to Luau
(define struct-match-luau (ir->luau struct-match-ir))

;; Display the generated Luau code
(displayln "\nStruct match Luau:")
(displayln (luau-ast->string struct-match-luau))

;; Run test
(test-case "Struct match test"
  (check-true (ir-program? struct-match-ir) 
              "Struct match should be an ir-program")) 