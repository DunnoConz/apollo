#lang racket/base

;; Main entry point for the Apollo library (`require apollo`)

(require (for-syntax racket/base)
         racket/match ; Often useful for users of the library
         racket/list  ; Often useful for users of the library
         
         ;; Core compiler functionality
         apollo/compiler/parser
         (submod apollo/compiler/ir ir)
         apollo/compiler/codegen
         
         ;; Optional Rojo integration
         ;; Use `dynamic-require` to avoid hard dependency if not present
         )

;; Attempt to load Rojo module path for conditional export
(define-syntax (maybe-provide-rojo stx)
  (syntax-case stx ()
    [(_)
     (let ([mod
            (with-handlers ([exn:fail? (lambda (e) #f)]) ; Handle require failure
              (dynamic-require 'apollo/rojo/integration #f))])
       (if mod
           ; If require succeeded, generate the provide form
           #'(provide (all-from-out (quote-module-path apollo/rojo/integration)))
           ; If require failed, generate an empty begin (no-op)
           #'(begin)))]))

;; Re-export core compilation pipeline function
(provide compile-racket-string-to-luau)

;; Re-export individual stages for advanced use/debugging
(provide parse-racket-string)
(provide racket-to-ir)
(provide ir->luau)
(provide luau-ast->string)

;; Conditionally provide Rojo functions at the top level
(maybe-provide-rojo)

;; Helper function that combines pipeline steps (similar to old compiler-main)
(define (compile-racket-string-to-luau str #:source-file [source #f])
  ;; Add error handling later if needed (from error.rkt?)
  (let* ([racket-ast (parse-racket-string str #:source source)]
         [ir (racket-to-ir racket-ast)]
         [luau-ast (ir->luau ir)]
         [luau-str (luau-ast->string luau-ast)])
    luau-str))

;; Re-export IR submodule functions using prefixes (example)
(provide (prefix-out ir: (all-from-out (submod apollo/compiler/ir ir)))) 