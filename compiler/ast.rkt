#lang racket/base

;; Minimal AST module to satisfy dependencies for ir.rkt
(provide (all-defined-out))

;; Any AST-related structures or functions that ir.rkt might use
(struct ast-module (name language body) #:transparent)
(struct ast-define (name value) #:transparent)
(struct ast-lambda (params body) #:transparent)
(struct ast-app (func args) #:transparent)
(struct ast-var-ref (name) #:transparent)
(struct ast-literal (value) #:transparent)

;; Placeholder for any functionality required by ir.rkt
(define (ast->ir ast)
  (error "ast->ir not implemented"))

;; Add any other structures or functions needed by ir.rkt 