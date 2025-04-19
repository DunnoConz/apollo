#lang racket/base

(require "../../parser.rkt"
         "../../ir.rkt"
         "../../luau-pretty-print.rkt"
         "./ir-to-luau.rkt"
         "../optimizations/luau-opts.rkt"
         "../cache/cache.rkt"
         racket/runtime-path)

(provide compile-staged
         get-compilation-stats
         (all-from-out "../cache/cache.rkt"))

;; Main compilation pipeline:
;; 1. Parse Racket to IR (using existing compiler)
;; 2. Convert IR to staged Luau (with caching)
;; 3. Convert staged Luau to final Luau AST
;; 4. Apply Luau-specific optimizations
;; 5. Pretty print the result

(define (compile-staged racket-code)
  (let* ([ir (racket-to-ir racket-code)]
         [staged (cache-ir->luau ir)]
         [luau-ast (staged-luau->ast staged)]
         [optimized-ast (optimize-luau-ast luau-ast)])
    (pretty-print-luau optimized-ast)))

;; Get compilation statistics
(define (get-compilation-stats)
  (get-cache-stats)) 