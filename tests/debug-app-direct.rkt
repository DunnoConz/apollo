#lang racket/base

(require rackunit
         (submod apollo/compiler/ir ir)
         (only-in (submod apollo/compiler/ir ir) ir->datum))

;; Direct test of function application expression
(let ([expr #'(+ 1 2)])
  (define result (racket-to-ir expr))
  (displayln "Function application AST:")
  (pretty-print expr)
  (displayln "\nResult (raw):")
  (pretty-print result)
  (displayln "\nResult as datum:")
  (pretty-print (ir->datum result))
  
  (displayln "\nSimulated manual construction:")
  (define func (ir-var-ref '+))
  (define args (list (ir-literal 1) (ir-literal 2)))
  (define manual-result (ir-app func args))
  (pretty-print manual-result)
  (displayln "\nManual result as datum:")
  (pretty-print (ir->datum manual-result))) 