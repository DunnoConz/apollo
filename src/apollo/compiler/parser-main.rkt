#lang racket/base

(require racket/match
         racket/syntax
         racket/list
         racket/port
         racket/string
         racket/path
         (only-in "parser-new.rkt" parse-program parse-expr parse-racket-string)
         (only-in "lexer.rkt" tokenize)
         (prefix-in ir: (submod "./ir.rkt" ir))
         "ir-types.rkt"
         "ast.rkt")

(provide parse-program
         parse-expr
         parse-racket-string
         parse-pattern
         racket-to-ir)

;; Module dependency tracking
(define module-deps (make-hash))
(define current-module-path (make-parameter #f))

(define (add-module-dep! from to)
  (hash-set! module-deps from (cons to (hash-ref module-deps from '()))))

(define (get-module-deps path)
  (hash-ref module-deps path '()))

(define (resolve-module-path path [relative-to (current-module-path)])
  (cond
    [(not (or (string? path) (path? path)))
     (error 'resolve-module-path "Invalid path type: ~a" path)]
    [(absolute-path? path) 
     (if (file-exists? path)
         path
         (error 'resolve-module-path "Absolute path does not exist: ~a" path))]
    [(and relative-to (path? relative-to))
     (let ([resolved (build-path (path-only relative-to) path)])
       (if (file-exists? resolved)
           resolved
           (error 'resolve-module-path "Resolved path does not exist: ~a" resolved)))]
    [else
     (let ([resolved (build-path (current-directory) path)])
       (if (file-exists? resolved)
           resolved
           (error 'resolve-module-path "Path does not exist: ~a" resolved)))]))

;; Convert AST to IR
(define (ast->ir ast)
  (match ast
    [(struct ast-module (name lang body))
     (ir:convert-module-to-ir (datum->syntax #f `(module ,name ,lang ,@body)))]
    [(struct ast-define (name value))
     (ir:convert-expr-to-ir (datum->syntax #f `(define ,name ,value)))]
    [(struct ast-lambda (params body))
     (ir:convert-expr-to-ir (datum->syntax #f `(lambda ,params ,@body)))]
    [(struct ast-app (func args))
     (ir:convert-expr-to-ir (datum->syntax #f (cons func args)))]
    [(struct ast-var-ref (name))
     (ir:convert-expr-to-ir (datum->syntax #f name))]
    [(struct ast-literal (value))
     (ir:convert-expr-to-ir (datum->syntax #f value))]
    [other (error 'ast->ir "Unsupported AST node: ~a" other)]))

;; Main entry points that use the new parser-tools implementation
(define (parse-racket-string str [source-name "string-input"])
  (parameterize ([current-module-path source-name])
    (parse-program str)))

(define (parse-program code-input [source-path #f])
  (parameterize ([current-module-path source-path])
    (let ([ast (if (string? code-input)
                   (parse-racket-string code-input source-path)
                   (error "parse-program: expected string input"))])
      (ast->ir ast))))

(define (parse-expr expr)
  (if (string? expr)
      (ast->ir (parse-racket-string expr))
      (error "parse-expr: expected string input")))

(define (parse-pattern pat)
  (if (string? pat)
      (ast->ir (parse-racket-string pat))
      (error "parse-pattern: expected string input")))

;; Convert Racket syntax to IR
(define (racket-to-ir stx)
  (if (syntax? stx)
      (ir:convert-to-ir stx)
      (error "racket-to-ir: expected syntax object"))) 