#lang racket/base

(require "./parser-main.rkt"
         "./ir.rkt"
         "./codegen.rkt"
         "./ir-types.rkt")

(provide (all-from-out "./parser-main.rkt")
         (all-from-out "./codegen.rkt")
         (all-from-out "./ir-types.rkt")
         compile-to-luau)

;; Compile Racket code to Luau
(define (compile-to-luau code)
  (let* ([ir (racket-to-ir code)]
         [luau (ir->luau ir)])
    (luau-ast->string luau))) 