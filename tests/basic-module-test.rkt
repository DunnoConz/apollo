#lang racket/base
(require rackunit
         (submod apollo/compiler/ir ir))

;; Test module
(define test-mod
  #'(module small racket
      (define x 42)
      (+ x 1)))

;; Print syntax objects 
(displayln "Module syntax:")
(write test-mod)

;; Convert to IR
(displayln "\n\nConverting to IR...")
(define ir (racket-to-ir test-mod))

;; Display IR
(displayln "IR result:")
(pretty-print ir)

;; For debugging: print all expressions
(when (ir-begin? ir)
  (displayln "\nExpression count:")
  (displayln (length (ir-begin-exprs ir)))
  
  (displayln "\nExpressions:")
  (map (lambda (expr)
         (displayln (format "Expression type: ~a" 
                           (cond
                             [(ir-literal? expr) "ir-literal"]
                             [(ir-define? expr) "ir-define"]
                             [(ir-app? expr) "ir-app"]
                             [else "other"]))))
       (ir-begin-exprs ir))) 