#lang racket/base

(require racket/match
         racket/syntax
         racket/list
         racket/path)

(provide (all-defined-out))

;; IR Type Definitions
(struct ir-program (modules) #:transparent)
(struct ir-module (name path body) #:transparent)
(struct ir-literal (value) #:transparent)
(struct ir-var-ref (name) #:transparent)
(struct ir-var-set (name value) #:transparent)
(struct ir-define (name value) #:transparent)
(struct ir-lambda (formals kw-formals body) #:transparent)
(struct ir-app (func args kw-args) #:transparent)
(struct ir-if (test then else) #:transparent)
(struct ir-begin (exprs) #:transparent)
(struct ir-let (bindings body) #:transparent)
(struct ir-letrec (bindings body) #:transparent)
(struct ir-cond (clauses else-clause) #:transparent)
(struct ir-define-struct (name fields) #:transparent)
(struct ir-struct-ref (instance index name) #:transparent)
(struct ir-match (target clauses) #:transparent)
(struct ir-pat-literal (value) #:transparent)
(struct ir-pat-var (name) #:transparent)
(struct ir-pat-wildcard () #:transparent)
(struct ir-pat-list (elements rest) #:transparent)
(struct ir-pat-struct (name fields) #:transparent)
(struct ir-pat-quasiquote (pattern) #:transparent)
(struct ir-pat-unquote (pattern) #:transparent)
(struct ir-pat-cons (head tail) #:transparent)
(struct ir-ctfe (expr) #:transparent)
(struct ir-pred (test) #:transparent)
(struct ir-and-pat (pred pat) #:transparent)
(struct ir-var-pat (name) #:transparent)
(struct ir-wildcard-pat () #:transparent) 