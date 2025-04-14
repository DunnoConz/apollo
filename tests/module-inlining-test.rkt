#lang racket/base

(require rackunit
         (submod apollo/compiler/ir ir))

;; Get debugging predicates
(define module-inlinable? 
  (dynamic-require '(submod apollo/compiler/ir ir) 'module-inlinable?))

;; Test small inlinable module (without #%plain-module-begin)
(define small-module-example
  #'(module small-helper racket
      (define x 10)
      (+ x 5)))

;; Test larger non-inlinable module (without #%plain-module-begin)
(define large-module-example
  #'(module larger-helper racket
      (define x 10)
      (define y 20)
      (define z 30)
      (+ x y z)))

;; Test nested modules - inner one should be inlined
(define nested-modules-example
  #'(module outer racket (#%plain-module-begin
              (define y 10)
              (module inner racket
                (define z 20)
                (+ z 5))
              (+ y 2))))

;; Print debug info
(displayln "Checking inlining status:")
(displayln (format "small-module-example inlinable? ~a" 
                  (module-inlinable? small-module-example)))
(displayln (format "large-module-example inlinable? ~a" 
                  (module-inlinable? large-module-example)))

;; Convert to IR
(define small-module-ir (racket-to-ir small-module-example))
(define large-module-ir (racket-to-ir large-module-example))
(define nested-modules-ir (racket-to-ir nested-modules-example))

;; Print IR for inspection
(displayln "Small module IR (should be inlined):")
(pretty-print small-module-ir)
(displayln "\nLarge module IR (should NOT be inlined):")
(pretty-print large-module-ir)
(displayln "\nNested modules IR (inner should be inlined):")
(pretty-print nested-modules-ir)

;; More detailed debugging of small module IR
(displayln "\nDebugging small module IR:")
(displayln (format "Is ir-begin? ~a" (ir-begin? small-module-ir)))
(when (ir-begin? small-module-ir)
  (displayln (format "Begin expressions count: ~a" 
                     (length (ir-begin-exprs small-module-ir))))
  (unless (null? (ir-begin-exprs small-module-ir))
    (displayln (format "First expression type: ~a" 
                      (cond
                        [(ir-literal? (car (ir-begin-exprs small-module-ir))) "ir-literal"]
                        [(ir-define? (car (ir-begin-exprs small-module-ir))) "ir-define"]
                        [else "other"])))))

;; Run tests
(test-case "Module inlining test"
  ;; Small module should be inlined with a comment marker
  (check-true (ir-begin? small-module-ir) 
              "Small module should be converted to ir-begin")
  
  ;; If we have an ir-begin, test if it has expressions
  (when (ir-begin? small-module-ir)
    (check-false (null? (ir-begin-exprs small-module-ir))
                "Inlined module should have expressions"))
  
  ;; If we have non-empty expressions, check for the comment marker
  (when (and (ir-begin? small-module-ir)
             (not (null? (ir-begin-exprs small-module-ir))))
    (check-true 
     (let ([first-expr (car (ir-begin-exprs small-module-ir))])
       (and (ir-literal? first-expr)
            (string? (ir-literal-value first-expr))
            (regexp-match? #rx"Inlined module" (ir-literal-value first-expr))))
     "Inlined module should contain a comment marker"))) 