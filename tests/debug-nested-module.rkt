#lang racket/base

(require rackunit
         (submod apollo/compiler/ir ir)
         (only-in (submod apollo/compiler/ir ir) ir->datum racket-to-ir))

;; First, let's test a simple module
(let ([ast #'(module simple-module racket/base
               (#%plain-module-begin
                (+ 1 2)))])
  (displayln "\n==== Simple Module Test ====")
  (displayln "AST:")
  (pretty-print ast)
  
  (define result (racket-to-ir ast))
  (displayln "\nResult:")
  (pretty-print result)
  
  (displayln "\nResult as datum:")
  (pretty-print (ir->datum result)))

;; Now, let's test a nested module
(let ([ast #'(module outer-module racket/base
               (#%plain-module-begin
                (module nested-module racket/base
                  (#%plain-module-begin
                   (+ 1 2)))))])
  (displayln "\n==== Nested Module Test ====")
  (displayln "AST:")
  (pretty-print ast)
  
  (define result (racket-to-ir ast))
  (displayln "\nResult:")
  (pretty-print result)
  
  (displayln "\nResult as datum:")
  (pretty-print (ir->datum result)))

;; Let's also test direct application to ensure it works correctly
(let ([expr #'(+ 1 2)])
  (displayln "\n==== Direct Application Test ====")
  (displayln "Expression:")
  (pretty-print expr)
  
  (define result (racket-to-ir expr))
  (displayln "\nResult:")
  (pretty-print result)
  
  (displayln "\nResult as datum:")
  (pretty-print (ir->datum result))) 