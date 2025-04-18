#lang racket/base

(require racket/match
         (except-in racket/string string-contains?)
         racket/list  ; For last and drop-right
         racket/hash  ; For hash operations
         racket/struct-info  ; For struct type information
         (prefix-in ir: (submod "./ir.rkt" ir))
         "./types.rkt"  ; Import all types directly
         )

(provide ir->luau
         luau-ast->string
         )

;; Dispatcher for converting IR nodes to Luau AST nodes
(define (ir->luau ir-node)
  (match ir-node
    ;; Simple literals
    [(ir:ir-literal val)
     (luau-literal val)]

    ;; Variable reference
    [(ir:ir-var-ref name)
     (luau-var (symbol->string name))]

    ;; Quasiquote pattern - just pass through
    [(ir:ir-pat-quasiquote pattern)
     (ir->luau pattern)]
     
    ;; Unquote pattern - just pass through
    [(ir:ir-pat-unquote pattern)
     (ir->luau pattern)]

    ;; Default handler for any other node
    [_ (luau-literal 'placeholder)]))

;; Simplified stringifier
(define (luau-ast->string node)
  (cond
    [(luau-literal? node) 
     (let ([val (luau-literal-val node)])
       (cond
         [(string? val) (format "\"~a\"" val)]
         [(number? val) (number->string val)]
         [(boolean? val) (if val "true" "false")]
         [(symbol? val) (symbol->string val)]
         [else "nil"]))]
    [(luau-var? node) (luau-var-name node)]
    [else "unknown"])) 