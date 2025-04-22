#lang racket/base

(require racket/match
         racket/set
         racket/string
         racket/path
         racket/list
         racket/function
         syntax/srcloc
         syntax/parse
         "ast.rkt"
         "ir-types.rkt"
         "private/error.rkt"
         "dsl-helpers.rkt"
         "language-registry.rkt")

(require (for-syntax racket/base
                     syntax/parse
                     "dsl-helpers.rkt"))

;; Ensure get-syntax->ir-clauses is available at runtime
(require "dsl-helpers.rkt")

;; --- TEMPORARY DSL REGISTRATION --- 
;; Ideally, this happens elsewhere based on project config
(register-apollo-language! 'shout-dsl "../dsls/shout-dsl.rkt")
;; ----------------------------------

(module+ ir
  (provide convert-to-ir
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
           convert-ctfe-to-ir))

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
                          (error 'extract-kw-args "Missing value for keyword argument: ~a" (syntax-e (car args)))
                          (loop (cddr args)
                                pos-args
                                (cons (cons (syntax-e (car args)) (cadr args)) kw-args)))]
                     [else
                      (loop (cdr args)
                            (cons (car args) pos-args)
                            kw-args)]))])
    kw-args))

;; Fast path for keyword argument extraction (operates on data, not syntax)
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

(provide (all-from-out "ir-types.rkt"))

;; Main conversion functions
(define (convert-to-ir stx)
  (unless (syntax? stx)
    (error 'convert-to-ir "Expected syntax object, got: ~a" stx))
  
  ;; --- LOAD DSL MODULES --- 
  ;; Load modules for active languages before conversion
  ;; TODO: Determine active languages dynamically (e.g., from config)
  (load-language-modules '(shout-dsl))
  ;; ------------------------

  (parameterize ([current-module-path (syntax-source stx)])
    (ir-program (list (convert-module-to-ir stx)))))

(define (convert-module-to-ir stx)
  (unless (syntax? stx)
    (error 'convert-module-to-ir "Expected syntax object, got: ~a" stx))
  (let ([module-name (or (syntax-property stx 'module-name) 'anonymous)]
        [module-path (or (syntax-source stx) (current-module-path))])
    (parameterize ([current-module-path module-path]) ; Set path for nested resolution
      (ir-module module-name
                 module-path
                 (map convert-expr-to-ir (syntax->list stx))))))

;; Optimized pattern matching on syntax data
(define (match-pattern dat stx)
  (or (get-cached-pattern dat) ; Cache based on data
      (cache-pattern dat
        (match dat
          [(? fast-literal?) (convert-literal-to-ir dat stx)]
          [(list 'lambda formals body ...) (convert-lambda-to-ir formals body stx)]
          [(list 'if test then else) (convert-if-to-ir test then else stx)]
          [(list 'begin exprs ...) (convert-begin-to-ir exprs stx)]
          [(list 'let bindings body ...) (convert-let-to-ir bindings body stx)]
          [(list 'letrec bindings body ...) (convert-letrec-to-ir bindings body stx)]
          [(list 'cond clauses ... maybe-else) (convert-cond-to-ir clauses maybe-else stx)]
          [(list 'define-struct name fields) (convert-define-struct-to-ir name fields stx)]
          ;; struct-ref has non-symbol index, handled in convert-expr-to-ir
          [(list 'match target clauses ...) (convert-match-to-ir target clauses stx)]
          [(list 'quasiquote pattern) (convert-expr-to-ir stx)]
          [(list 'unquote pattern) (convert-expr-to-ir stx)]
          [(list 'ctfe expr) (convert-ctfe-to-ir expr stx)]
          ;; Application must be handled after keywords
          [(list func args ...) (convert-app-to-ir func args stx)]
          [_ #f] ; Return #f if no keyword matches
          ))))

;; Main conversion function using syntax-parse
(define (convert-expr-to-ir stx)
  (let ([original-stx stx]) ; Capture input syntax
    (define dat (syntax->datum original-stx))
    (or (get-cached-expr dat) ; Check cache first
        (cache-expr dat ; Cache the result
          (syntax-parse original-stx #:context #'convert-expr-to-ir ; Parse captured syntax
            ;; Bind the original syntax object for use in clauses
            ;; Clauses for typed/racket removed temporarily to resolve syntax-parse error
            
            ;; --- Splice in clauses generated by define-syntax->ir --- 
            ;; These handle specific forms like define, lambda, if, etc.
            #,@(get-syntax->ir-clauses) ; <-- Uncommented

            ;; Fallback / Catch-all Clause
            [_ ; Match anything, use original-stx in the body
             ;; Handle literals, symbols, or error out
             (let ([datum (syntax->datum original-stx)]) ; Use captured syntax for datum
               (cond
                 [(literal? datum)
                  ;; Inlined body of convert-literal-to-ir
                  (ir-literal datum #:span (syntax-span original-stx))]
                 [(symbol? datum)
                  ;; Use captured syntax object for span info
                  (ir-var-ref datum #:span (syntax-span original-stx))]
                 [else
                  (error 'convert-expr-to-ir "Unsupported expression type or form: ~a" datum)]))])))))

;; Optimized literal conversion with caching
;; This function might now be unused if only called from the fallback clause
(define (convert-literal-to-ir lit stx)
  (ir-literal lit #:span (syntax-span stx)))

(define (convert-lambda-to-ir formals-stx body-stx stx)
  (let-values ([(pos-formals kw-formals) (extract-kw-args formals-stx)])
    (ir-lambda (map syntax->datum pos-formals) ; Formals are symbols
               (map (lambda (pair) (cons (car pair) (convert-expr-to-ir (cdr pair)))) kw-formals) ; Convert kw-arg default exprs
               (map convert-expr-to-ir body-stx)
               #:span (syntax-span stx))))

(define (convert-app-to-ir func-stx args-stx stx)
  ;; Application handles syntax directly
  (let-values ([(pos-args kw-args) (extract-kw-args args-stx)])
    (ir-app (convert-expr-to-ir func-stx)
            (map convert-expr-to-ir pos-args)
            (map (lambda (pair) (cons (car pair) (convert-expr-to-ir (cdr pair)))) kw-args) ; Convert kw-arg exprs
            #:span (syntax-span stx))))

(define (convert-if-to-ir test-stx then-stx else-stx stx)
  (ir-if (convert-expr-to-ir test-stx)
         (convert-expr-to-ir then-stx)
         (convert-expr-to-ir else-stx)
         #:span (syntax-span stx)))

(define (convert-begin-to-ir exprs-stx stx)
  (ir-begin (map convert-expr-to-ir exprs-stx)
            #:span (syntax-span stx)))

(define (convert-let-to-ir bindings-stx body-stx stx)
  (define (convert-binding b)
    (match b
      [(list name-stx val-stx) (cons (syntax-e name-stx) (convert-expr-to-ir val-stx))]
      [_ (error 'convert-let-to-ir "Invalid let binding form: ~a" b)]))
  (ir-let (map convert-binding bindings-stx)
          (map convert-expr-to-ir body-stx)
          #:span (syntax-span stx)))

(define (convert-letrec-to-ir bindings-stx body-stx stx)
  (define (convert-binding b)
     (match b
      [(list name-stx val-stx) (cons (syntax-e name-stx) (convert-expr-to-ir val-stx))]
      [_ (error 'convert-letrec-to-ir "Invalid letrec binding form: ~a" b)]))
  (ir-letrec (map convert-binding bindings-stx)
             (map convert-expr-to-ir body-stx)
             #:span (syntax-span stx)))

(define (convert-cond-to-ir clauses-stx maybe-else-stx stx)
  (define (convert-clause c)
    (match c
      [(list test-stx body-stx ...) (cons (convert-expr-to-ir test-stx) (map convert-expr-to-ir body-stx))]
      [_ (error 'convert-cond-to-ir "Invalid cond clause: ~a" c)]))
  (define else-clause
    (match maybe-else-stx
      [(list (list 'else body-stx ...)) (map convert-expr-to-ir body-stx)]
      ['() #f] ; No else clause
      [_ (error 'convert-cond-to-ir "Invalid else clause in cond" maybe-else-stx)]))
  (ir-cond (map convert-clause clauses-stx)
           else-clause
           #:span (syntax-span stx)))

(define (convert-define-struct-to-ir name-stx fields-stx stx)
  (ir-define-struct (syntax-e name-stx)
                    (map syntax-e fields-stx)
                    #:span (syntax-span stx)))

(define (convert-struct-ref-to-ir instance-stx index-stx name-stx stx)
  (ir-struct-ref (convert-expr-to-ir instance-stx)
                 (convert-expr-to-ir index-stx) ; Index is an expression
                 (syntax-e name-stx)
                 #:span (syntax-span stx)))

(define (convert-match-to-ir target-stx clauses-stx stx)
  (define (convert-clause c)
    (match c
      [(list pat-stx body-stx ...) (cons (convert-pattern-to-ir pat-stx) (map convert-expr-to-ir body-stx))]
      [_ (error 'convert-match-to-ir "Invalid match clause: ~a" c)]))
  (ir-match (convert-expr-to-ir target-stx)
            (map convert-clause clauses-stx)
            #:span (syntax-span stx)))

(define (convert-pattern-to-ir stx)
  (define dat (syntax->datum stx))
  (cond
    [(string? dat) (ir-pat-literal dat #:span (syntax-span stx))]
    [(number? dat) (ir-pat-literal dat #:span (syntax-span stx))]
    [(boolean? dat) (ir-pat-literal dat #:span (syntax-span stx))]
    [(symbol? dat) 
     (if (eq? dat '_)
         (ir-pat-wildcard #:span (syntax-span stx))
         (ir-pat-var dat #:span (syntax-span stx)))]
    [(pair? dat)
     (syntax-parse stx
       #:literals (quasiquote unquote unquote-splicing)
       [(quasiquote pat)
        (convert-expr-to-ir stx)]
       [(unquote pat)
        (convert-expr-to-ir stx)]
       [(unquote-splicing pat)
        (convert-expr-to-ir stx)]
       [(a . d)
        (ir-pat-cons
         (convert-pattern-to-ir #'a)
         (convert-pattern-to-ir #'d)
         #:span (syntax-span stx))])]
    [else (error 'convert-pattern-to-ir "Unsupported pattern type: ~a" dat)]))

;; Quasiquote
(define-syntax->ir (quasiquote expr:expr)
  #:convert
  (let ([expr-stx #'expr])
    (define (convert-qq-expr stx depth)
      (syntax-parse stx
        #:literals (unquote unquote-splicing quasiquote)
        [(unquote e)
         #:when (= depth 1)
         (convert-expr-to-ir #'e)]
        [(unquote-splicing e)
         #:when (= depth 1)
         (error 'convert-quasiquote-to-ir "unquote-splicing not supported")]
        [(quasiquote e)
         (ir-literal (list 'quasiquote
                          (syntax->datum (convert-qq-expr #'e (add1 depth)))))]
        [(unquote e)
         (ir-literal (list 'unquote
                          (syntax->datum (convert-qq-expr #'e (sub1 depth)))))]
        [(unquote-splicing e)
         (ir-literal (list 'unquote-splicing
                          (syntax->datum (convert-qq-expr #'e (sub1 depth)))))]
        [(a . d)
         (ir-literal (cons (syntax->datum (convert-qq-expr #'a depth))
                          (syntax->datum (convert-qq-expr #'d depth))))]
        [other
         (ir-literal (syntax->datum #'other))]))
    
    (convert-qq-expr expr-stx 1)))

;; Unquote (only valid inside quasiquote)
(define-syntax->ir (unquote expr:expr)
  #:convert
  (error 'convert-unquote-to-ir "unquote not allowed outside of quasiquote"))

;; Unquote-splicing (only valid inside quasiquote)
(define-syntax->ir (unquote-splicing expr:expr)
  #:convert
  (error 'convert-unquote-splicing-to-ir "unquote-splicing not allowed outside of quasiquote"))

(define (convert-ctfe-to-ir expr-stx stx)
  (ir-ctfe (convert-expr-to-ir expr-stx) #:span (syntax-span stx)))

;; Module path helper (retained from original)
(define (handle-module-path path)
  (match path
    [(? symbol? s) s]
    [(? string? s) (string->symbol s)]
    [_ (error 'handle-module-path "Invalid module path: ~a" path)]))

;; --- Define Syntax->IR Conversion Rules using the DSL --- 

;; Define
(define-syntax->ir (define name:id expr:expr)
  #:convert 
  (ir-define (syntax->datum #'name)
             (convert-expr-to-ir #'expr)
             #:span (syntax-span stx)))

;; Define Struct
(define-syntax->ir (define-struct name:id fields:expr)
  #:convert
  (let ([fields-stx #'fields])
    (unless (list? (syntax-e fields-stx))
      (raise-syntax-error #f "Expected a list of field identifiers" stx fields-stx))
    (let ([field-syntax-list (syntax->list fields-stx)])
      (unless (andmap identifier? field-syntax-list)
        (raise-syntax-error #f "Expected a list of field identifiers" stx fields-stx))
      (ir-define-struct (syntax->datum #'name)
                       (map syntax->datum field-syntax-list)
                       #:span (syntax-span stx)))))

;; Lambda 
(define-syntax->ir (lambda formals:expr . body)
  #:convert 
  (let-values ([(pos-formals kw-formals) (extract-kw-args #'formals)])
    (ir-lambda (map syntax->datum pos-formals)
               (map (lambda (pair) (cons (car pair) (convert-expr-to-ir (cdr pair)))) kw-formals)
               (map convert-expr-to-ir (syntax->list #'body))
               #:span (syntax-span stx))))

;; If
(define-syntax->ir (if test:expr then:expr else:expr)
  #:convert 
  (ir-if (convert-expr-to-ir #'test)
         (convert-expr-to-ir #'then)
         (convert-expr-to-ir #'else)
         #:span (syntax-span stx)))

;; Begin
(define-syntax->ir (begin . exprs)
  #:convert 
  (ir-begin (map convert-expr-to-ir (syntax->list #'exprs))
            #:span (syntax-span stx)))

;; CTFE
(define-syntax->ir (ctfe expr:expr)
  #:convert 
  (ir-ctfe (convert-expr-to-ir #'expr) 
           #:span (syntax-span stx)))

;; Typed Racket Annotation (: name type) - Type Erasure
(define-syntax->ir (: name:id type:expr)
  #:convert 
  (ir-literal (void) #:span (syntax-span stx)))

;; Let
(define-syntax->ir (let bindings:expr . body)
  #:where (and (list? (syntax-e #'bindings))
               (for/and ([b (in-list (syntax->list #'bindings))])
                 (match (syntax->list b)
                   [(list _ _) #t]
                   [_ #f])))
  #:convert
  (ir-let (for/list ([b (in-list (syntax->list #'bindings))])
            (match (syntax->list b)
              [(list name-stx val-stx)
               (cons (syntax-e name-stx) (convert-expr-to-ir val-stx))]
              [_ (raise-syntax-error #f "Invalid let binding structure" stx b)]))
          (map convert-expr-to-ir (syntax->list #'body))
          #:span (syntax-span stx)))

;; Letrec
(define-syntax->ir (letrec bindings:expr . body)
  #:where (and (list? (syntax-e #'bindings))
               (for/and ([b (in-list (syntax->list #'bindings))])
                 (match (syntax->list b)
                   [(list _ _) #t]
                   [_ #f])))
  #:convert
  (ir-letrec (for/list ([b (in-list (syntax->list #'bindings))])
               (match (syntax->list b)
                 [(list name-stx val-stx)
                  (cons (syntax-e name-stx) (convert-expr-to-ir val-stx))]
                 [_ (raise-syntax-error #f "Invalid letrec binding structure" stx b)]))
             (map convert-expr-to-ir (syntax->list #'body))
             #:span (syntax-span stx)))

;; Cond
(define-syntax->ir (cond . clauses)
  #:convert
  (let ([clauses (syntax->list #'clauses)])
    (define (convert-clause c)
      (syntax-parse c
        [(test:expr . body)
         (cons (convert-expr-to-ir #'test)
               (map convert-expr-to-ir (syntax->list #'body)))]
        [_ (error 'convert-cond-to-ir "Invalid cond clause")]))
    
    (define (find-else-clause clauses)
      (and (pair? clauses)
           (syntax-parse (car clauses)
             [(else . body)
              (map convert-expr-to-ir (syntax->list #'body))]
             [_ #f])))
    
    (let-values ([(regular-clauses maybe-else)
                  (if (null? clauses)
                      (values '() #f)
                      (let ([last (car (reverse clauses))]
                            [rest (reverse (cdr (reverse clauses)))])
                        (if (syntax-parse last
                              [(else . _) #t]
                              [_ #f])
                            (values rest (find-else-clause (list last)))
                            (values clauses #f))))])
      (ir-cond (map convert-clause regular-clauses)
               maybe-else
               #:span (syntax-span stx)))))

;; Match
(define-syntax->ir (match value:expr . clauses)
  #:convert
  (let ([clauses (syntax->list #'clauses)])
    (define (convert-match-clause c)
      (syntax-parse c
        [(pattern:expr . body)
         (cons (convert-pattern-to-ir #'pattern)
               (map convert-expr-to-ir (syntax->list #'body)))]
        [other (error 'convert-match-to-ir "Invalid match clause: ~a" (syntax->datum #'other))]))
    
    (ir-match (convert-expr-to-ir #'value)
              (map convert-match-clause clauses)
              #:span (syntax-span stx))))

;; Application (Must come AFTER keywords)
(define-syntax->ir (func:id . args)
  #:where (not (keyword? (syntax-e #'func)))
  #:convert 
  (let-values ([(pos-args kw-args) (extract-kw-args #'args)])
    (ir-app (convert-expr-to-ir #'func)
            (map convert-expr-to-ir pos-args)
            (map (lambda (pair) (cons (car pair) (convert-expr-to-ir (cdr pair)))) kw-args)
            #:span (syntax-span stx))))

;; --- End of DSL Rules --- 

;; Expression cache (kept for now, maybe integrate with syntax-parse later)