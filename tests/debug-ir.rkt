#lang racket/base

(require rackunit
         (only-in (submod apollo/compiler/ir ir) racket-to-ir ir-begin? ir-begin-exprs ir->datum))

;; Test a simple module
(define ast #'(module #f racket/base 42))
(define result (racket-to-ir ast))
(define result-datum (ir->datum result))

;; Print debugging info
(displayln "Original AST:")
(pretty-print ast)
(displayln "\nIR Result (raw):")
(pretty-print result)
(displayln "\nIR Result (datum):")
(pretty-print result-datum)

;; Function application test
(displayln "\n\nFunction application test:")
(define app-ast #'(module #f racket/base (+ 1 2)))
(define app-result (racket-to-ir app-ast))
(define app-result-datum (ir->datum app-result))
(displayln "IR Result (raw):")
(pretty-print app-result)
(displayln "IR Result (datum):")
(pretty-print app-result-datum)

;; Print the expected structure for comparison
(displayln "\nExpected structure from tests:")
(pretty-print 
 '(ir-begin 
   ((ir-literal "/* Inlined module: #f */")
    (ir-app (ir-var-ref +) ((ir-literal 1) (ir-literal 2)))
    (ir-begin ()))))

;; Check if expressions in ir-begin
(when (ir-begin? result)
  (displayln "\nExpressions in ir-begin:")
  (for ([expr (ir-begin-exprs result)]
        [i (in-naturals)])
    (printf "Expression ~a: ~a\n" i expr)
    (when (ir-begin? expr)
      (printf "  Inner begin expressions: ~a\n" (length (ir-begin-exprs expr)))))) 