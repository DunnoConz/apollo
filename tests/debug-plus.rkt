#lang racket/base

(require rackunit
         (submod apollo/compiler/ir ir)
         (only-in (submod apollo/compiler/ir ir) ir->datum))

;; Debug the plus function extraction and processing
(displayln "Testing how + is processed:")
(define plus-expr #'+)
(define plus-result (racket-to-ir plus-expr))
(displayln "Plus symbol AST:")
(pretty-print plus-expr)
(displayln "\nPlus symbol result (raw):")
(pretty-print plus-result)
(displayln "\nPlus symbol result as datum:")
(pretty-print (ir->datum plus-result))

;; Look at the whole expression with a print statement to see how it's handled
(displayln "\n\nChecking how racket-expr->ir is implemented:")
(define (print-transformer stx)
  (displayln (format "Processing: ~a" stx))
  (let ([result (racket-to-ir stx)])
    (displayln (format "Result: ~a" result))
    result))

;; Redefine racket-expr->ir to see what's happening
(define orig-racket-expr->ir racket-expr->ir)
(define (debug-racket-expr->ir stx)
  (displayln (format "racket-expr->ir called with: ~a" stx))
  (define result (orig-racket-expr->ir stx))
  (displayln (format "racket-expr->ir returned: ~a" result))
  result)

;; Set up a module with (+ 1 2)
(define module-ast #'(module #f racket/base (+ 1 2)))
(displayln "\nTest module AST:")
(pretty-print module-ast)

;; Extract the (+ 1 2) part
(define app-part (syntax-case module-ast (module)
                   [(module _ _ body) #'body]
                   [_ #f]))
(displayln "\nApplication part:")
(pretty-print app-part)

;; Test direct processing of (+ 1 2)
(define app-expr #'(+ 1 2))
(displayln "\nDirect app expression:")
(pretty-print app-expr)
(define app-result (racket-to-ir app-expr))
(displayln "Direct app result:")
(pretty-print app-result)
(displayln "Direct app result datum:")
(pretty-print (ir->datum app-result))

;; Extract and test the function part
(define func-part #'+)
(define args-part (list #'1 #'2))
(displayln "\nFunction part:")
(pretty-print func-part)
(displayln "Function part result:")
(pretty-print (racket-to-ir func-part))
(displayln "\nArgs parts:")
(for ([arg args-part])
  (printf "Arg: ~a -> ~a\n" arg (racket-to-ir arg))) 