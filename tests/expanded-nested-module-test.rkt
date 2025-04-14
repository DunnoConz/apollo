#lang racket/base

(require rackunit
         (submod apollo/compiler/ir ir))

;; Test an expanded nested module
(define nested-module-example
  #'(module inner racket (#%plain-module-begin
                         (define z 20)
                         (+ z 5))))

;; Convert to IR
(define nested-module-ir (racket-to-ir nested-module-example))

;; Print the IR
(displayln "Nested module IR:")
(pretty-print nested-module-ir)

;; Run a simple test
(test-case "Expanded nested module test"
  (check-true (ir-program? nested-module-ir) "Nested module with #%plain-module-begin should be ir-program")
  
  ;; Check contents
  (when (and (ir-program? nested-module-ir) 
            (not (null? (ir-program-body nested-module-ir))))
    (check-equal? (length (ir-program-body nested-module-ir)) 2
                 "Nested module should have two expressions"))) 