#lang racket/base
(require rackunit
         ;; Use package paths
         (submod apollo/compiler/ir ir))

(require rackunit
         (submod "../../src/compiler/ir.rkt" ir)
         (only-in (submod "../../src/compiler/ir.rkt" ir) ir->datum))

(provide ir-tests)

;; Helper to simplify testing - convert IR to datum
; (define ir->datum ir:ir->datum) ; No longer needed

(define ir-tests
  (test-suite
   "IR Tests"

   (test-case "Convert simple literal to IR"
     (let ([ast #'(module #f racket/base 42)])
       (check-equal? (ir->datum (racket-to-ir ast))
                    '(ir-begin ((ir-literal "/* Inlined module: #f */") (ir-literal 42) (ir-begin ())))
                    "Should convert a number literal to IR datum")))

   (test-case "Convert variable reference to IR"
     (let ([ast #'(module #f racket/base x)])
       (check-equal? (ir->datum (racket-to-ir ast))
                    '(ir-begin ((ir-literal "/* Inlined module: #f */") (ir-var-ref x) (ir-begin ())))
                    "Should convert a variable reference to IR datum")))

   (test-case "Convert function application to IR"
     (let ([ast #'(module #f racket/base (+ 1 2))])
       (check-equal? (ir->datum (racket-to-ir ast))
                    '(ir-begin ((ir-literal "/* Inlined module: #f */") 
                               (ir-app (ir-var-ref +) ((ir-literal 1) (ir-literal 2)))
                               (ir-begin ())))
                    "Should convert a function application to IR datum")))
   
   (test-case "Convert if expression to IR"
     (let ([ast #'(module #f racket/base (if (> x 0) 1 0))])
       (check-equal? (ir->datum (racket-to-ir ast))
                    '(ir-begin ((ir-literal "/* Inlined module: #f */")
                               (ir-if (ir-app (ir-var-ref >) ((ir-var-ref x) (ir-literal 0)))
                                      (ir-literal 1)
                                      (ir-literal 0))
                               (ir-begin ())))
                    "Should convert an if expression to IR datum")))
   
   (test-case "Convert let expression to IR"
     (let ([ast #'(module #f racket/base (let ([x 1] [y 2]) (+ x y)))])
       (check-equal? (ir->datum (racket-to-ir ast))
                    '(ir-begin ((ir-literal "/* Inlined module: #f */")
                               (ir-let ((x (ir-literal 1)) (y (ir-literal 2)))
                                       ((ir-app (ir-var-ref +) ((ir-var-ref x) (ir-var-ref y)))))
                               (ir-begin ())))
                    "Should convert a let expression to IR datum")))
   
   (test-case "Convert lambda expression to IR"
     (let ([ast #'(module #f racket/base (lambda (x) x))])
       (check-equal? (ir->datum (racket-to-ir ast))
                    '(ir-begin ((ir-literal "/* Inlined module: #f */")
                               (ir-lambda (x) ((ir-var-ref x)))
                               (ir-begin ())))
                    "Should convert a lambda expression to IR datum")))
)) 