#lang racket/base

(require racket/match
         racket/syntax
         racket/list
         racket/port
         (prefix-in ir: (submod "./ir.rkt" ir)))

(provide parse-program parse-expr parse-racket-string parse-pattern)

(define (parse-program code-input)
  (if (string? code-input)
      ;; Parse from string input
      (let* ([input-port (open-input-string code-input)]
             [read-expressions (let loop ([exprs '()])
                                (let ([expr (read input-port)])
                                  (if (eof-object? expr)
                                      (reverse exprs)
                                      (loop (cons expr exprs)))))]
             [parsed-exprs (map parse-expr read-expressions)])
        (ir:ir-program parsed-exprs))
      ;; Parse from s-expression
      (match code-input
        [`(module ,_ racket/base ,body ...)
         (ir:ir-program (map parse-expr body))]
        [other
         (ir:ir-program (map parse-expr (if (list? other) other (list other))))])))

(define (parse-expr expr)
  (cond
    ;; Literals
    [(number? expr) (ir:ir-literal expr)]
    [(string? expr) (ir:ir-literal expr)]
    [(boolean? expr) (ir:ir-literal expr)]
    [(eq? expr '#true) (ir:ir-literal #true)]
    [(eq? expr '#false) (ir:ir-literal #false)]
    [(null? expr) (ir:ir-literal '())]
    [(eq? expr 'null) (ir:ir-literal 'null)]
    
    ;; Variables
    [(symbol? expr) (ir:ir-var-ref expr)]
    
    ;; Not a simple expression, must be a list form
    [(not (pair? expr)) 
     (error 'parse-expr "Unsupported Racket expression: ~s" expr)]
    
    ;; Handle struct definitions
    [(and (list? expr) (>= (length expr) 3) (eq? (car expr) 'define-struct))
     (let ([name (cadr expr)]
           [fields (caddr expr)])
       (if (and (list? fields) (not (null? fields)))
           (ir:ir-define-struct name fields)
           (error 'parse-expr "Invalid struct fields: ~s" fields)))]
    
    ;; Assignment
    [(and (list? expr) (= (length expr) 3) (eq? (car expr) 'set!))
     (let ([var (cadr expr)]
           [val-expr (caddr expr)])
       (ir:ir-var-set var (parse-expr val-expr)))]
    
    ;; Function definition
    [(and (list? expr) (>= (length expr) 3) (eq? (car expr) 'define) 
          (list? (cadr expr)) (not (null? (cadr expr))))
     (let ([name (car (cadr expr))]
           [formals (cdr (cadr expr))]
           [body (cddr expr)])
       (ir:ir-define name (ir:ir-lambda formals (parse-body body))))]
    
    ;; Variable definition
    [(and (list? expr) (= (length expr) 3) (eq? (car expr) 'define))
     (let ([name (cadr expr)]
           [val-expr (caddr expr)])
       (ir:ir-define name (parse-expr val-expr)))]
    
    ;; Lambda
    [(and (list? expr) (>= (length expr) 3) (eq? (car expr) 'lambda)
          (list? (cadr expr)))
     (let ([formals (cadr expr)]
           [body (cddr expr)])
       (ir:ir-lambda formals (parse-body body)))]
    
    ;; if expressions
    [(and (list? expr) (= (length expr) 4) (eq? (car expr) 'if))
     (let ([test (cadr expr)]
           [then (caddr expr)]
           [else (cadddr expr)])
       (ir:ir-if (parse-expr test) (parse-expr then) (parse-expr else)))]
     
    [(and (list? expr) (= (length expr) 3) (eq? (car expr) 'if))
     (let ([test (cadr expr)]
           [then (caddr expr)])
       (ir:ir-if (parse-expr test) (parse-expr then) (ir:ir-literal #false)))]
    
    ;; Begin expressions
    [(and (list? expr) (>= (length expr) 2) (eq? (car expr) 'begin))
     (ir:ir-begin (map parse-expr (cdr expr)))]
    
    ;; Let bindings
    [(and (list? expr) (>= (length expr) 3) (eq? (car expr) 'let)
          (list? (cadr expr)))
     (let ([bindings (cadr expr)]
           [body (cddr expr)])
       (ir:ir-let (map parse-let-binding bindings) (parse-body body)))]
    
    ;; letrec bindings
    [(and (list? expr) (>= (length expr) 3) (eq? (car expr) 'letrec)
          (list? (cadr expr)))
     (let ([bindings (cadr expr)]
           [body (cddr expr)])
       (ir:ir-letrec (map parse-let-binding bindings) (parse-body body)))]
    
    ;; cond expressions with else clause
    [(and (list? expr) (>= (length expr) 3) (eq? (car expr) 'cond)
          (list? (car (reverse expr))) (eq? (car (car (reverse expr))) 'else))
     (let* ([clauses (reverse (cdr (reverse (cdr expr))))]
            [else-clause (car (reverse (cdr expr)))]
            [parsed-clauses (map parse-cond-clause clauses)]
            [parsed-else (parse-body (cdr else-clause))])
       (ir:ir-cond parsed-clauses parsed-else))]
    
    ;; cond expressions without else clause
    [(and (list? expr) (>= (length expr) 2) (eq? (car expr) 'cond))
     (let ([clauses (cdr expr)])
       (ir:ir-cond (map parse-cond-clause clauses) #f))]
    
    ;; quasiquote and unquote
    [(and (list? expr) (= (length expr) 2) (eq? (car expr) 'quasiquote))
     (parse-quasiquoted (cadr expr))]
    
    [(and (list? expr) (= (length expr) 2) (eq? (car expr) 'unquote))
     (parse-expr (cadr expr))]
    
    ;; quote
    [(and (list? expr) (= (length expr) 2) (eq? (car expr) 'quote))
     (ir:ir-literal (cadr expr))]

    ;; match expressions
    [(and (list? expr) (>= (length expr) 3) (eq? (car expr) 'match))
     (let ([target-expr (cadr expr)]
           [clauses (cddr expr)])
       (ir:ir-match (parse-expr target-expr)
                    (map parse-match-clause clauses)))]

    ;; List literal (special case for '())
    [(and (list? expr) (= (length expr) 1) (eq? (car expr) 'list))
     (ir:ir-literal '())]
    
    ;; List construction
    [(and (list? expr) (>= (length expr) 1) (eq? (car expr) 'list))
     (ir:ir-app (ir:ir-var-ref 'list) (map parse-expr (cdr expr)))]
    
    ;; Cons literal
    [(and (list? expr) (= (length expr) 3) (eq? (car expr) 'cons))
     (let ([a (cadr expr)]
           [b (caddr expr)])
       (ir:ir-app (ir:ir-var-ref 'cons) (list (parse-expr a) (parse-expr b))))]
    
    ;; Association list handling
    [(and (list? expr) (= (length expr) 3) (eq? (car expr) 'cons)
          (list? (cadr expr)) (= (length (cadr expr)) 3) (eq? (car (cadr expr)) 'cons))
     (let ([k (cadr (cadr expr))]
           [v (caddr (cadr expr))]
           [rest (caddr expr)])
       (ir:ir-app (ir:ir-var-ref 'cons) 
                  (list (ir:ir-app (ir:ir-var-ref 'cons) 
                                  (list (parse-expr k) (parse-expr v)))
                        (parse-expr rest))))]
    
    ;; Define struct
    [(and (list? expr) (= (length expr) 3) (eq? (car expr) 'struct))
     (let ([name (cadr expr)]
           [fields (caddr expr)])
       (if (and (list? fields) (not (null? fields)))
           (ir:ir-define-struct name (cdr fields))
           (error 'parse-expr "Invalid struct fields: ~s" fields)))]

    ;; Define struct with options (like #:transparent)
    [(and (list? expr) (>= (length expr) 4) (eq? (car expr) 'struct))
     (let ([name (cadr expr)]
           [fields (caddr expr)])
       (if (and (list? fields) (not (null? fields)))
           (ir:ir-define-struct name (cdr fields))
           (error 'parse-expr "Invalid struct fields: ~s" fields)))]
    
    ;; Generic function application (must be last)
    [(list? expr)
     (let ([func-expr (car expr)]
           [arg-exprs (cdr expr)])
       (ir:ir-app (parse-expr func-expr) (map parse-expr arg-exprs)))]

    ;; Default case - we don't know how to parse this
    [else (error 'parse-expr "Unsupported Racket expression: ~s" expr)]))

(define (parse-body body-exprs)
  (let ([parsed (map parse-expr body-exprs)])
    (if (= (length parsed) 1)
        (first parsed)
        (ir:ir-begin parsed))))

(define (parse-let-binding binding)
  (match binding
    [`[,name ,init-expr]
     (list name (parse-expr init-expr))]
    [_ (error 'parse-let-binding "Invalid let binding format: ~s" binding)]))

(define (parse-cond-clause clause)
  (match clause
    [`[,test-expr ,result-exprs ...]
     (list (parse-expr test-expr) (parse-body result-exprs))]
    [_ (error 'parse-cond-clause "Invalid cond clause format: ~s" clause)]))

(define (parse-match-clause clause)
  (match clause
    ;; Basic match pattern with single expression
    [`[,pattern ,result-expr]
     (list (parse-pattern pattern) (parse-expr result-expr))]
     
    ;; Match pattern with multiple expressions
    [`[,pattern ,result-exprs ...]
     (list (parse-pattern pattern) (parse-body result-exprs))]
     
    ;; Match pattern with #:when guard
    [`[,pattern #:when ,_ ,result-exprs ...]
     (list (parse-pattern pattern) (parse-body result-exprs))]
     
    [_ (error 'parse-match-clause "Invalid match clause format: ~s" clause)]))

(define (parse-pattern pattern)
  (cond
    [(number? pattern) (ir:ir-pat-literal pattern)]
    [(string? pattern) (ir:ir-pat-literal pattern)]
    [(boolean? pattern) (ir:ir-pat-literal pattern)]
    [(eq? pattern '#true) (ir:ir-pat-literal #true)]
    [(eq? pattern '#false) (ir:ir-pat-literal #false)]
    [(eq? pattern 'null) (ir:ir-pat-literal 'null)]

    [(eq? pattern '_) (ir:ir-pat-wildcard)]
    [(symbol? pattern) (ir:ir-pat-var pattern)]

    [(and (list? pattern) (not (null? pattern)))
     (case (car pattern)
       [(list) 
        (ir:ir-pat-list (map parse-pattern (cdr pattern)) #f)]
       [(list*) 
        (let* ([elements (drop-right (cdr pattern) 1)]
               [rest (last (cdr pattern))])
          (ir:ir-pat-list (map parse-pattern elements) (parse-pattern rest)))]
       [(cons) 
        (if (= (length pattern) 3)
            (ir:ir-pat-list (list (parse-pattern (cadr pattern))) (parse-pattern (caddr pattern)))
            (error 'parse-pattern "Invalid cons pattern format: ~s" pattern))]
            
       [(struct)
        (if (>= (length pattern) 2)
            (ir:ir-pat-struct (cadr pattern) (map parse-pattern (cddr pattern)))
            (error 'parse-pattern "Invalid struct pattern format: ~s" pattern))]
            
       [(quasiquote) 
        (if (= (length pattern) 2)
            (ir:ir-pat-quasiquote (parse-quasiquoted-pattern (cadr pattern)))
            (error 'parse-pattern "Invalid quasiquote pattern format: ~s" pattern))]
       [(unquote)
        (if (= (length pattern) 2)
            (ir:ir-pat-unquote (parse-pattern (cadr pattern)))
            (error 'parse-pattern "Invalid unquote pattern format: ~s" pattern))]
       [(quote)
        (if (= (length pattern) 2)
            (ir:ir-pat-literal (cadr pattern))
            (error 'parse-pattern "Invalid quote pattern format: ~s" pattern))]
            
       [else 
        (ir:ir-pat-list (map parse-pattern pattern) #f)])]
    
    [(and (pair? pattern) (eq? (car pattern) 'quasiquote))
     (ir:ir-pat-quasiquote (parse-quasiquoted-pattern (cadr pattern)))]
    
    [(and (pair? pattern) (eq? (car pattern) 'unquote))
     (ir:ir-pat-unquote (parse-pattern (cadr pattern)))]
    
    [(and (pair? pattern) (eq? (car pattern) 'quote))
     (ir:ir-pat-literal (cadr pattern))]
    
    [else (error 'parse-pattern "Unsupported match pattern: ~s" pattern)]))

(define (parse-quasiquoted-pattern pattern)
  (cond
    [(and (list? pattern) (not (null? pattern)) (eq? (car pattern) 'unquote))
     (ir:ir-pat-unquote (parse-pattern (cadr pattern)))]
    
    [(list? pattern)
     (ir:ir-pat-list 
      (map parse-quasiquoted-pattern pattern)
      #f)]
    
    [else (ir:ir-pat-literal pattern)]))

(define (parse-quasiquoted datum)
  (cond
    [(number? datum) (ir:ir-literal datum)]
    [(string? datum) (ir:ir-literal datum)]
    [(boolean? datum) (ir:ir-literal datum)]
    [(symbol? datum) (ir:ir-literal datum)]
    [(null? datum) (ir:ir-literal '())]
    
    ;; Handle unquote inside quasiquote
    [(and (list? datum) 
          (= (length datum) 2) 
          (eq? (car datum) 'unquote))
     (parse-expr (cadr datum))]
    
    ;; Handle nested quasiquote
    [(and (list? datum) 
          (= (length datum) 2) 
          (eq? (car datum) 'quasiquote))
     (ir:ir-literal (list 'quasiquote (strip-syntax (cadr datum))))]
    
    ;; Handle list with unquotes
    [(list? datum)
     (ir:ir-app (ir:ir-var-ref 'list)
                (map (lambda (element)
                      (if (and (pair? element)
                               (eq? (car element) 'unquote))
                          (parse-expr (cadr element))
                          (parse-quasiquoted element)))
                    datum))]
    
    ;; Handle improper list with unquotes
    [(pair? datum)
     (ir:ir-app (ir:ir-var-ref 'cons)
                (list (parse-quasiquoted (car datum))
                      (parse-quasiquoted (cdr datum))))]
    
    ;; Default case
    [else (ir:ir-literal datum)]))

(define (strip-syntax stx)
  (cond
    [(syntax? stx) (strip-syntax (syntax->datum stx))]
    [(pair? stx) (cons (strip-syntax (car stx))
                      (strip-syntax (cdr stx)))]
    [(vector? stx) (list->vector (map strip-syntax (vector->list stx)))]
    [else stx]))

(define (parse-racket-string str [source-name "string-input"])
  (with-handlers ([exn:fail? (lambda (e)
                               (error 'parse-racket-string
                                      "Error parsing Racket code from string: ~a"
                                      (exn-message e)))])
    (with-input-from-string str
      (lambda ()
        (let ([stx (read-syntax source-name (current-input-port))])
          (if (eof-object? stx)
              (datum->syntax #f '(module default racket/base) #f #f)
              (let ([datum (syntax->datum stx)])
                (if (and (list? datum)
                         (pair? datum)
                         (eq? (car datum) 'module))
                    stx
                    (datum->syntax stx
                                  `(module default racket/base ,stx)
                                  stx
                                  stx)))))))))