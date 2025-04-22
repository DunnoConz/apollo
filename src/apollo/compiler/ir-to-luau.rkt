#lang racket/base

(require racket/match
         "syntax-to-ir.rkt") ; For IR struct definitions

(provide ir->luau-ast
         ;; Export Luau AST structs as they are defined
         struct:luau-block
         struct:luau-literal
         struct:luau-variable
         struct:luau-app
         struct:luau-set
         struct:luau-local
         struct:luau-if
         struct:luau-local-function
         struct:luau-return
         )

;; --- Luau AST Node Definitions ---
;; Represents a sequence of statements
(struct luau-block (stats) #:transparent)
;; Literals (string, number, boolean, nil)
(struct luau-literal (value) #:transparent)
;; Variable reference
(struct luau-variable (name) #:transparent)
;; Function call
(struct luau-app (func args) #:transparent)
;; Assignment (can be multiple vars/vals, simplified for now)
(struct luau-set (vars vals) #:transparent)
;; Local variable definition (can be multiple)
(struct luau-local (vars vals) #:transparent)
;; If statement
(struct luau-if (test then else) #:transparent) ; `else` can be another luau-if or luau-block
;; Function definition (local function ... end)
(struct luau-local-function (name params body) #:transparent) ; body is luau-block
;; Return statement
(struct luau-return (exprs) #:transparent)


;; --- IR to Luau AST Conversion ---
(define (ir->luau-ast ir-node)
  ;; Implementation using match or struct dispatch
  (match ir-node
    [(ir-literal v span)
     (luau-literal (match v
                      [#t 'true]
                      [#f 'false]
                      [_ v]))] ; Keep numbers/strings/etc.
    [(ir-variable name span)
     (luau-variable name)]
    [(ir-begin body span)
     (luau-block (map ir->luau-ast body))] ; Convert body elements recursively
    [(ir-app fn args span)
     (luau-app (ir->luau-ast fn)
               (map ir->luau-ast args))]
    ;; TODO: Add clauses for other ir-* types
    [_ (error 'ir->luau-ast "Unsupported IR node: ~s" ir-node)])) 