#lang racket/base

(require (for-syntax racket/base
                    syntax/parse
                    racket/syntax))

(provide define-syntax->ir
         get-syntax->ir-clauses)

;; Storage for syntax->ir conversion rules
(define syntax->ir-clauses-param (make-parameter '()))

;; Macro for defining new syntax->ir conversion rules
(define-syntax (define-syntax->ir stx)
  (syntax-parse stx
    [(_ pattern
        #:where condition
        #:convert conversion)
     (with-syntax ([stx (datum->syntax stx 'stx)])
       #'(begin
           (syntax->ir-clauses-param 
            (cons (list (quote-syntax pattern) condition
                       (lambda (stx) conversion))
                  (syntax->ir-clauses-param)))))]
    [(_ pattern
        #:convert conversion)
     (with-syntax ([stx (datum->syntax stx 'stx)])
       #'(begin
           (syntax->ir-clauses-param
            (cons (list (quote-syntax pattern) #t
                       (lambda (stx) conversion))
                  (syntax->ir-clauses-param)))))]))

;; Function to get all registered syntax->ir clauses
(define (get-syntax->ir-clauses)
  (reverse (syntax->ir-clauses-param))) 