#lang racket/base

(require rackunit
         (submod apollo/compiler/ir ir))

;; Test a simple nested module (without #%plain-module-begin)
(define simple-nested-module
  #'(module inner racket
      (define z 20)
      (+ z 5)))

;; Test an expanded nested module (with #%plain-module-begin)
(define expanded-nested-module
  #'(module inner racket (#%plain-module-begin
                         (define z 20)
                         (+ z 5))))

;; Test a nested module within a top-level module
(define module-with-nested-module
  #'(module outer racket (#%plain-module-begin
              (define y 10)
              (module inner racket
                (define z 20)
                (+ z 5))
              (+ y 2))))

;; Convert them all to IR
(define simple-ir (racket-to-ir simple-nested-module))
(define expanded-ir (racket-to-ir expanded-nested-module))
(define nested-in-top-ir (racket-to-ir module-with-nested-module))

;; Print the IR
(displayln "Simple nested module IR:")
(pretty-print simple-ir)
(displayln "\nExpanded nested module IR:")
(pretty-print expanded-ir)
(displayln "\nModule with nested module IR:")
(pretty-print nested-in-top-ir)

;; Run our tests
(test-case "Simple nested module test"
  (check-true (ir-begin? simple-ir) 
              "Simple nested module should be ir-begin"))

(test-case "Expanded nested module test"
  (check-true (ir-program? expanded-ir) 
              "Expanded nested module should be ir-program")
  (check-equal? (length (ir-program-body expanded-ir)) 2
               "Should have two expressions (define and app)"))

(test-case "Nested module within top-level module test"
  (check-true (ir-program? nested-in-top-ir) 
              "Top-level module should be ir-program")
  
  (let* ([body (ir-program-body nested-in-top-ir)]
         [nested-module-expr (findf ir-begin? body)])
    (check-true (ir-begin? nested-module-expr)
                "Nested module should be ir-begin")
    (check-equal? (length body) 3
                 "Should have three expressions (define, nested module, and app)"))) 