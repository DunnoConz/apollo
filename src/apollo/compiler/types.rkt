#lang racket/base

(require racket/contract
         racket/struct)

(provide 
 ;; Intermediate Representation structures
 ir-module ir-module? ir-module-requires ir-module-provides ir-module-body
 ir-literal ir-literal? ir-literal-val
 ir-var ir-var? ir-var-name
 ir-lambda ir-lambda? ir-lambda-params ir-lambda-body
 ir-app ir-app? ir-app-func ir-app-args
 ir-let ir-let? ir-let-bindings ir-let-body
 ir-if ir-if? ir-if-test ir-if-then ir-if-else
 ir-begin ir-begin? ir-begin-exprs
 ir-define ir-define? ir-define-name ir-define-value
 ir-require ir-require? ir-require-path ir-require-imports

 ;; FFI-specific structures
 ir-ffi-get-service ir-ffi-get-service? ir-ffi-get-service-name
 
 ;; Luau AST structures
 luau-module luau-module? luau-module-requires luau-module-body luau-module-exports
 luau-var luau-var? luau-var-name
 luau-literal luau-literal? luau-literal-val
 luau-if luau-if? luau-if-test luau-if-then luau-if-else
 luau-block luau-block? luau-block-stmts
 luau-func luau-func? luau-func-params luau-func-body
 luau-call luau-call? luau-call-func luau-call-args
 luau-method-call luau-method-call? luau-method-call-obj luau-method-call-method luau-method-call-args
 luau-binop luau-binop? luau-binop-op luau-binop-left luau-binop-right
 luau-unop luau-unop? luau-unop-op luau-unop-arg
 luau-assign luau-assign? luau-assign-target luau-assign-val
 luau-assign-local luau-assign-local? luau-assign-local-var luau-assign-local-val
 luau-typed-assign-local luau-typed-assign-local? luau-typed-assign-local-name 
 luau-typed-assign-local-type luau-typed-assign-local-value
 luau-assign-field luau-assign-field? luau-assign-field-obj luau-assign-field-field luau-assign-field-value
 luau-field-access luau-field-access? luau-field-access-obj luau-field-access-field
 luau-expr-stmt luau-expr-stmt? luau-expr-stmt-expr
 luau-function-def luau-function-def? luau-function-def-name luau-function-def-params luau-function-def-body
 luau-require luau-require? luau-require-name luau-require-path
 luau-return luau-return? luau-return-val
 luau-table-literal luau-table-literal? luau-table-literal-items
 luau-table-kv-literal luau-table-kv-literal? luau-table-kv-literal-pairs
 luau-table-pair luau-table-pair? luau-table-pair-key luau-table-pair-value
 ;; For loops
 luau-for-numeric luau-for-numeric? luau-for-numeric-var luau-for-numeric-start luau-for-numeric-end luau-for-numeric-step luau-for-numeric-body)

;; ===== IR STRUCTURES =====

;; Top-level module structure
(struct ir-module (requires provides body)
  #:transparent)

;; Basic values
(struct ir-literal (val)
  #:transparent)

(struct ir-var (name)
  #:transparent)

;; Functions and applications
(struct ir-lambda (params body)
  #:transparent)

(struct ir-app (func args)
  #:transparent)

;; Control flow
(struct ir-if (test then else)
  #:transparent)

;; Variable bindings and sequences
(struct ir-let (bindings body)
  #:transparent)

(struct ir-begin (exprs)
  #:transparent)

;; Definitions
(struct ir-define (name value)
  #:transparent)

;; Module imports
(struct ir-require (path imports)
  #:transparent)

;; FFI-specific structures
(struct ir-ffi-get-service (name)
  #:transparent)

;; ===== LUAU AST STRUCTURES =====

;; Top-level module structure
(struct luau-module (requires body exports)
  #:transparent)

;; Basic expressions
(struct luau-literal (val)
  #:transparent)

(struct luau-var (name)
  #:transparent)

;; Control flow
(struct luau-if (test then else)
  #:transparent)

;; Blocks and functions
(struct luau-block (stmts)
  #:transparent)

(struct luau-func (params body)
  #:transparent)

;; Function calls and method calls
(struct luau-call (func args)
  #:transparent)

(struct luau-method-call (obj method args)
  #:transparent)

;; Operators
(struct luau-binop (op left right)
  #:transparent)

(struct luau-unop (op arg)
  #:transparent)

;; Assignments
(struct luau-assign (target val)
  #:transparent)

(struct luau-assign-local (var val)
  #:transparent)

(struct luau-typed-assign-local (name type value)
  #:transparent)

(struct luau-assign-field (obj field value)
  #:transparent)

;; Field access
(struct luau-field-access (obj field)
  #:transparent)

;; Statements
(struct luau-expr-stmt (expr)
  #:transparent)

;; Function definition (named)
(struct luau-function-def (name params body)
  #:transparent)

(struct luau-require (name path)
  #:transparent)

(struct luau-return (val)
  #:transparent)

;; Tables
(struct luau-table-literal (items)
  #:transparent)

(struct luau-table-kv-literal (pairs)
  #:transparent)

(struct luau-table-pair (key value)
  #:transparent)

;; For loops
(struct luau-for-numeric (var start end step body)
  #:transparent)

;; Predicate to check if a Luau AST node is a statement
(define (luau-stmt? node)
  (or (luau-assign? node)
      (luau-assign-local? node)
      (luau-typed-assign-local? node)
      (luau-assign-field? node)
      (luau-expr-stmt? node)
      (luau-if? node)
      (luau-block? node)
      (luau-function-def? node)
      (luau-require? node)))

;; Predicate to check if a node is a Luau expression
(define (luau-expr? node)
  (or (luau-literal? node)
      (luau-var? node)
      (luau-binop? node)
      (luau-unop? node)
      (luau-call? node)
      (luau-method-call? node)
      (luau-field-access? node)
      (luau-func? node)
      (luau-table-literal? node)
      (luau-table-kv-literal? node))) 