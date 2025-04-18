#lang racket/base

;; Main entry point for the Apollo library (`require apollo`)

(require (for-syntax racket/base)
         racket/match ; Often useful for users of the library
         racket/list  ; Often useful for users of the library
         
         ;; Core compiler functionality
         apollo/compiler/parser
         apollo/compiler/ir
         apollo/compiler/codegen
         
         ;; Optional Rojo integration
         apollo/rojo/integration)

;; Re-export core compilation pipeline function
(provide compile-racket-string-to-luau)

;; Re-export individual stages for advanced use/debugging
(provide parse-racket-string)
(provide racket-to-ir)
(provide ir->luau)
(provide luau-ast->string)

;; Re-export Rojo functions
(provide (all-from-out apollo/rojo/integration))

;; Helper function that combines pipeline steps (similar to old compiler-main)
(define (compile-racket-string-to-luau str)
  ;; Add error handling later if needed (from error.rkt?)
  (let* ([racket-ast (parse-racket-string str)]
         [ir (racket-to-ir racket-ast)]
         [luau-ast (ir->luau ir)]
         [luau-str (luau-ast->string luau-ast)])
    luau-str))

;; Re-export IR module functions using prefixes
(provide (prefix-out ir: (all-from-out apollo/compiler/ir))) 