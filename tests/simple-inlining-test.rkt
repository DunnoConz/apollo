#lang racket/base

(require rackunit
         (submod apollo/compiler/ir ir))

;; Simple test module
(define test-module-expr
  #'(module test-module racket
      (define x 10)
      (+ x 5)))

;; Convert the module
(displayln "Converting simple module...")
(define module-ir (racket-to-ir test-module-expr))

;; Display debug info
(displayln (format "Result type: ~a" (if (ir-begin? module-ir) "ir-begin" "other")))
(displayln (format "Expressions: ~a" 
                 (if (ir-begin? module-ir)
                     (length (ir-begin-exprs module-ir))
                     "N/A")))

;; Print pretty representation 
(displayln "\nModule IR:")
(pretty-print module-ir) 