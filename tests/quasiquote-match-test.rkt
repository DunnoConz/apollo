#lang racket/base

(require rackunit
         (submod apollo/compiler/ir ir)
         apollo/compiler/codegen)

;; Test match with quasiquote patterns
(define quasiquote-match
  #'(module test-quasiquote racket
      (define data '(point 10 20))
      
      (match data
        ;; Quasiquote pattern with unquote
        [`(point ,x ,y) (+ x y)]
        
        ;; Nested quasiquote with multiple unquotes
        [`(,(and type 'point) ,x ,y) (list type x y)]
        
        ;; Simple quasiquote
        ['(1 2 3) 6]
        
        ;; Fallback
        [_ 0])))

;; Convert to IR
(define qq-match-ir (racket-to-ir quasiquote-match))

;; Display the IR
(displayln "Quasiquote match IR:")
(pretty-print (ir->datum qq-match-ir))

;; Convert to Luau
(define qq-match-luau (ir->luau qq-match-ir))

;; Display the generated Luau code
(displayln "\nQuasiquote match Luau:")
(displayln (luau-ast->string qq-match-luau))

;; Run test
(test-case "Quasiquote match test"
  (check-true (ir-program? qq-match-ir) 
              "Quasiquote match should be an ir-program")) 