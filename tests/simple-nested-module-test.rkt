#lang racket/base

(require rackunit
         (submod apollo/compiler/ir ir))

;; Test a simple nested module
(define nested-module-example
  #'(module inner racket
      (define z 20)
      (+ z 5)))

;; Convert to IR
(define nested-module-ir (racket-to-ir nested-module-example))

;; Print the IR
(displayln "Nested module IR:")
(pretty-print nested-module-ir)

;; Run a simple test
(test-case "Basic nested module test"
  (check-true (ir-begin? nested-module-ir) "Nested module should be ir-begin")
  
  ;; If the module has contents
  (when (and (ir-begin? nested-module-ir) 
            (not (null? (ir-begin-exprs nested-module-ir))))
    (check-equal? (length (ir-begin-exprs nested-module-ir)) 4
                 "Nested module should have four expressions"))) 