#lang racket/base

(require (for-syntax racket/base syntax/parse)
         racket/string
         syntax/parse ; For syntax-case/parse helpers
         "../compiler/dsl-helpers.rkt" ; Provides define-syntax->ir
         "../compiler/ir-types.rkt")  ; Provides ir-app, ir-literal, etc.

(provide shout-dsl-loaded) ; Provide something simple to confirm loading

(define shout-dsl-loaded #t)

(printf "[shout-dsl] Module loaded and registering syntax rules.\n")

;; Define the IR conversion rule for the (shout ...) form
(define-syntax->ir (shout message:str) ; Pattern matches (shout string-literal)
  #:convert
  ;; Convert to: (print (string-upcase message-datum))
  (let ([message-datum (syntax->datum message)])
    (ir-app (ir-var-ref 'print #:span (syntax-span ___stx___)) ; Function to call
            (list (ir-literal (string-upcase message-datum) #:span (syntax-span message))) ; Argument list
            '() ; No keyword arguments
            #:span (syntax-span ___stx___)))) ; Span for the whole ir-app

;; Example of another form (optional)
;(define-syntax->ir (whisper message:str)
;  #:convert
;  (let ([message-datum (syntax->datum message)])
;    (ir-app (ir-var-ref 'displayln #:span (syntax-span ___stx___))
;            (list (ir-literal (string-downcase message-datum) #:span (syntax-span message)))
;            '()
;            #:span (syntax-span ___stx___)))) 