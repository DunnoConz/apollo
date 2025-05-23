#lang racket/base

;; Main entry point for the Apollo library (`require apollo`)

(require (for-syntax racket/base)
         racket/match
         racket/list
         
         ;; Core compiler functionality
         "compiler/parser.rkt"
         "compiler/ir.rkt"
         "compiler/codegen.rkt"
         
         ;; Optional Rojo integration
         "rojo/integration.rkt")

;; Re-export core compilation pipeline function
(provide compile-racket-string-to-luau)

;; Re-export individual stages for advanced use/debugging
(provide parse-racket-string)
(provide racket-to-ir)
(provide ir->luau)
(provide luau-ast->string)

;; Re-export Rojo functions
(provide (all-from-out "rojo/integration.rkt"))

;; Helper function that combines pipeline steps
(define (compile-racket-string-to-luau str)
  (let* ([racket-ast (parse-racket-string str)]
         [ir (racket-to-ir racket-ast)]
         [luau-ast (ir->luau ir)]
         [luau-str (luau-ast->string luau-ast)])
    luau-str))

;; Re-export IR module functions using prefixes
(provide (prefix-out ir: (all-from-out "compiler/ir.rkt"))) 