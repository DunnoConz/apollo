#lang racket/base

(require rackunit
         (submod apollo/compiler/ir ir)
         (only-in (submod apollo/compiler/ir ir) ir->datum))

;; Function application test from ir-test.rkt
(let ([ast #'(module #f racket/base (+ 1 2))])
  (define result (racket-to-ir ast))
  (displayln "Test AST:")
  (pretty-print ast)
  (displayln "\nResult (raw):")
  (pretty-print result)
  (displayln "\nResult as datum:")
  (pretty-print (ir->datum result))
  
  (displayln "\nExpected from test:")
  (pretty-print '(ir-begin 
                 ((ir-literal "/* Inlined module: #f */")
                  (ir-app (ir-var-ref +) ((ir-literal 1) (ir-literal 2)))
                  (ir-begin ()))))
  
  (when (ir-begin? result)
    (displayln "\nExamining ir-begin expressions:")
    (for ([expr (ir-begin-exprs result)]
          [i (in-naturals)])
      (printf "Expression ~a: ~a\n" i expr)))) 