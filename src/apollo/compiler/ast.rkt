#lang racket/base

;; AST module for the Apollo compiler
(provide (all-defined-out))

(require "ir-types.rkt"
         racket/match)

;; Module path handling
(define current-module-path (make-parameter #f))

;; AST node structures
(struct ast-module (name lang body) #:transparent)
(struct ast-define (name value) #:transparent)
(struct ast-lambda (params body) #:transparent)
(struct ast-app (func args) #:transparent)
(struct ast-var-ref (name) #:transparent)
(struct ast-literal (value) #:transparent)

;; Convert AST to IR
(define (ast->ir ast)
  (match ast
    [(ast-module name lang body)
     (ir-module name 
                (current-module-path)
                (map ast->ir body))]
    [(ast-define name value)
     (ir-define name (ast->ir value))]
    [(ast-lambda params body)
     (ir-lambda params 
                '() ; No keyword args in basic AST
                (map ast->ir body))]
    [(ast-app func args)
     (ir-app (ast->ir func)
             (map ast->ir args)
             '())] ; No keyword args in basic AST
    [(ast-var-ref name)
     (ir-var-ref name)]
    [(ast-literal value)
     (ir-literal value)]
    [_ (error 'ast->ir "Unsupported AST node: ~a" ast)])) 