#lang racket/base

; Simplified top-level requires
(require racket/match
         racket/set
         racket/string
         racket/path
         racket/list
         syntax/srcloc
         syntax/parse
         "ast.rkt"
         "ir-types.rkt")

;; Pattern matching cache
(define pattern-cache (make-hash))
(define (cache-pattern key value)
  (hash-set! pattern-cache key value)
  value)

(define (get-cached-pattern key)
  (hash-ref pattern-cache key #f))

;; Module path handling with caching
(define current-module-path (make-parameter #f))
(define module-paths (make-hash))
(define resolved-paths (make-hash))

(define (set-module-path! name path)
  (hash-set! module-paths name path)
  (hash-remove! resolved-paths name))

(define (get-module-path name)
  (or (hash-ref resolved-paths name #f)
      (let ([path (hash-ref module-paths name #f)])
        (when path
          (let ([resolved (resolve-module-path path)])
            (hash-set! resolved-paths name resolved)
            resolved)))))

(define (resolve-module-path path)
  (cond
    [(absolute-path? path) path]
    [(and (current-module-path)
          (path? (current-module-path)))
     (build-path (path-only (current-module-path)) path)]
    [else (build-path (current-directory) path)]))

;; Fast path for common literal types
(define (fast-literal? v)
  (or (number? v)
      (string? v)
      (boolean? v)
      (null? v)
      (symbol? v)))

;; Fast path for common expression types
(define (fast-expr? expr)
  (and (pair? expr)
       (symbol? (car expr))
       (memq (car expr) '(lambda if begin let letrec cond define-struct struct-ref match quasiquote unquote ctfe))))

;; Optimized keyword argument handling
(define (extract-kw-args stx)
  (let* ([args-list (syntax->list stx)]
         [kw-args (let loop ([args args-list]
                            [pos-args '()]
                            [kw-args '()])
                   (cond
                     [(null? args) (values (reverse pos-args) (reverse kw-args))]
                     [(and (syntax? (car args))
                           (keyword? (syntax-e (car args))))
                      (if (null? (cdr args))
                          (error 'extract-kw-args "Missing value for keyword argument: ~a" (car args))
                          (loop (cddr args)
                                pos-args
                                (cons (cons (syntax-e (car args)) (cadr args)) kw-args)))]
                     [else
                      (loop (cdr args)
                            (cons (car args) pos-args)
                            kw-args)]))])
    kw-args))

;; Fast path for keyword argument extraction
(define (fast-extract-kw-args args)
  (let loop ([args args]
             [pos-args '()]
             [kw-args '()])
    (cond
      [(null? args) (values (reverse pos-args) (reverse kw-args))]
      [(keyword? (car args))
       (if (null? (cdr args))
           (error 'fast-extract-kw-args "Missing value for keyword argument: ~a" (car args))
           (loop (cddr args)
                 pos-args
                 (cons (cons (car args) (cadr args)) kw-args)))]
      [else
       (loop (cdr args)
             (cons (car args) pos-args)
             kw-args)])))

;; Helper to check if a value is a literal
(define (literal? v)
  (or (fast-literal? v)
      (and (pair? v)
           (eq? (car v) 'quote)
           (pair? (cdr v))
           (null? (cddr v)))))

;; Expression cache for frequently used conversions
(define expr-cache (make-hash))
(define (cache-expr key value)
  (hash-set! expr-cache key value)
  value)

(define (get-cached-expr key)
  (hash-ref expr-cache key #f))

(provide (all-from-out "ir-types.rkt")
         convert-to-ir
         convert-module-to-ir
         convert-expr-to-ir
         convert-literal-to-ir
         convert-lambda-to-ir
         convert-app-to-ir
         convert-if-to-ir
         convert-begin-to-ir
         convert-let-to-ir
         convert-letrec-to-ir
         convert-cond-to-ir
         convert-define-struct-to-ir
         convert-struct-ref-to-ir
         convert-match-to-ir
         convert-pattern-to-ir
         convert-quasiquote-to-ir
         convert-unquote-to-ir
         convert-ctfe-to-ir
         racket-to-ir)

;; Main conversion functions
(define (convert-to-ir stx)
  (unless (syntax? stx)
    (error 'convert-to-ir "Expected syntax object, got: ~a" stx))
  (ir-program (list (convert-module-to-ir stx))))

(define (convert-module-to-ir stx)
  (unless (syntax? stx)
    (error 'convert-module-to-ir "Expected syntax object, got: ~a" stx))
  (let ([module-name (or (syntax-property stx 'module-name) 'anonymous)]
        [module-path (or (syntax-source stx) (current-module-path))])
    (ir-module module-name
               module-path
               (map convert-expr-to-ir (syntax->list stx)))))

;; Optimized pattern matching
(define (match-pattern pat)
  (or (get-cached-pattern pat)
      (cache-pattern pat
        (match pat
          [(? fast-literal?) (convert-literal-to-ir pat)]
          [(list 'lambda formals body ...) (convert-lambda-to-ir formals body)]
          [(list func args ...) (convert-app-to-ir func args)]
          [(list 'if test then else) (convert-if-to-ir test then else)]
          [(list 'begin exprs ...) (convert-begin-to-ir exprs)]
          [(list 'let bindings body) (convert-let-to-ir bindings body)]
          [(list 'letrec bindings body) (convert-letrec-to-ir bindings body)]
          [(list 'cond clauses ... else-clause) (convert-cond-to-ir clauses else-clause)]
          [(list 'define-struct name fields) (convert-define-struct-to-ir name fields)]
          [(list 'struct-ref instance index name) (convert-struct-ref-to-ir instance index name)]
          [(list 'match target clauses ...) (convert-match-to-ir target clauses)]
          [(list 'quasiquote pattern) (convert-quasiquote-to-ir pattern)]
          [(list 'unquote pattern) (convert-unquote-to-ir pattern)]
          [(list 'ctfe expr) (convert-ctfe-to-ir expr)]
          [other (error "Unsupported expression type: ~a" other)]))))

;; Optimized expression conversion with caching
(define (convert-expr-to-ir expr)
  (cond
    [(syntax? expr) (convert-expr-to-ir (syntax->datum expr))]
    [(fast-literal? expr) (convert-literal-to-ir expr)]
    [(fast-expr? expr) (match-pattern expr)]
    [else (error "Unsupported expression type: ~a" expr)]))

;; Optimized literal conversion with caching
(define (convert-literal-to-ir lit)
  (or (get-cached-expr lit)
      (cache-expr lit (ir-literal lit))))

(define (convert-lambda-to-ir formals body)
  (let-values ([(pos-formals kw-formals) (extract-kw-args formals)])
    (ir-lambda pos-formals kw-formals (map convert-expr-to-ir body))))

(define (convert-app-to-ir func args)
  (let-values ([(pos-args kw-args) (extract-kw-args args)])
    (ir-app (convert-expr-to-ir func)
            (map convert-expr-to-ir pos-args)
            kw-args)))

(define (convert-if-to-ir test then else)
  (ir-if (convert-expr-to-ir test)
         (convert-expr-to-ir then)
         (convert-expr-to-ir else)))

(define (convert-begin-to-ir exprs)
  (ir-begin (map convert-expr-to-ir exprs)))

(define (convert-let-to-ir bindings body)
  (ir-let (map (lambda (b) (cons (car b) (convert-expr-to-ir (cadr b)))) bindings)
          (map convert-expr-to-ir body)))

(define (convert-letrec-to-ir bindings body)
  (ir-letrec (map (lambda (b) (cons (car b) (convert-expr-to-ir (cadr b)))) bindings)
             (map convert-expr-to-ir body)))

(define (convert-cond-to-ir clauses else-clause)
  (ir-cond (map (lambda (c) (cons (convert-expr-to-ir (car c))
                                 (map convert-expr-to-ir (cdr c))))
                clauses)
           (and else-clause (map convert-expr-to-ir else-clause))))

(define (convert-define-struct-to-ir name fields)
  (ir-define-struct name fields))

(define (convert-struct-ref-to-ir instance index name)
  (ir-struct-ref (convert-expr-to-ir instance)
                 (convert-expr-to-ir index)
                 name))

(define (convert-match-to-ir target clauses)
  (ir-match (convert-expr-to-ir target)
            (map (lambda (c) (cons (convert-pattern-to-ir (car c))
                                 (map convert-expr-to-ir (cdr c))))
                 clauses)))

(define (convert-pattern-to-ir pat)
  (match pat
    [(? literal?) (ir-pat-literal pat)]
    [(list 'var name) (ir-pat-var name)]
    ['_ (ir-pat-wildcard)]
    [(list 'list elems ... rest) (ir-pat-list (map convert-pattern-to-ir elems)
                                             (and rest (convert-pattern-to-ir rest)))]
    [(list 'struct name fields) (ir-pat-struct name (map convert-pattern-to-ir fields))]
    [_ (error "Unsupported pattern type")]))

(define (convert-quasiquote-to-ir pattern)
  (ir-pat-quasiquote (convert-pattern-to-ir pattern)))

(define (convert-unquote-to-ir pattern)
  (ir-pat-unquote (convert-pattern-to-ir pattern)))

(define (convert-ctfe-to-ir expr)
  (ir-ctfe (convert-expr-to-ir expr)))

;; Alias for convert-to-ir
(define (racket-to-ir expr)
  (convert-to-ir expr))