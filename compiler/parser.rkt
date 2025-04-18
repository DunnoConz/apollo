#lang racket/base

(require racket/match
         racket/syntax
         racket/list
         racket/port
         racket/string
         racket/path
         syntax/parse
         (prefix-in ir: "./ir.rkt"))

(provide parse-program parse-expr parse-racket-string parse-pattern)

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

;; Keyword argument handling
(define (parse-kw-args args)
  (let loop ([args args]
             [pos-args '()]
             [kw-args '()])
    (cond
      [(null? args) (values (reverse pos-args) (reverse kw-args))]
      [(keyword? (car args))
       (if (null? (cdr args))
           (error 'parse-kw-args "Missing value for keyword argument: ~a" (car args))
           (loop (cddr args)
                 pos-args
                 (cons (list (car args) 
                           (if (syntax? (cadr args))
                               (syntax->datum (cadr args))
                               (cadr args)))
                       kw-args)))]
      [(syntax? (car args))
       (loop (cdr args)
             (cons (syntax->datum (car args)) pos-args)
             kw-args)]
      [else
       (loop (cdr args)
             (cons (car args) pos-args)
             kw-args)])))

(define (remove-lang-directive str)
  (let* ([lines (string-split str "\n")]
         [filtered-lines (filter (lambda (line)
                                 (not (string-prefix? (string-trim line) "#lang")))
                               lines)]
         [processed-lines (map (lambda (line)
                               (if (string-prefix? (string-trim line) "#%module-begin")
                                   (string-replace line "#%module-begin" "")
                                   line))
                             filtered-lines)])
    (string-join processed-lines "\n")))

(define (parse-racket-string str [source-name "string-input"])
  (let ([processed-str (remove-lang-directive str)])
    (with-input-from-string processed-str
      (lambda ()
        (let ([port (current-input-port)])
          (let loop ([exprs '()])
            (let ([expr (read-syntax source-name port)])
              (if (eof-object? expr)
                  (reverse exprs)
                  (loop (cons expr exprs))))))))))

(define (parse-program code-input [source-path #f])
  (parameterize ([current-module-path source-path])
    (if (string? code-input)
        ;; Parse from string input
        (let ([exprs (parse-racket-string code-input source-path)])
          (map parse-expr exprs))
        ;; Parse from s-expression
        (match code-input
          [`(module ,name racket/base ,body ...)
           (let ([stx (datum->syntax #f `(module ,name racket/base ,@body))])
             (syntax-property stx 'module-name name)
             (syntax-source stx source-path)
             (map parse-expr (syntax->list stx)))]
          [other (error "Invalid program input: ~a" other)]))))

(define (parse-expr expr)
  (cond
    [(syntax? expr) (parse-expr (syntax->datum expr))]
    
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
       (ir:ir-define-struct name fields))]
    
    ;; Handle lambda expressions with keyword arguments
    [(and (list? expr) (>= (length expr) 3) (eq? (car expr) 'lambda))
     (let* ([formals (cadr expr)]
            [body (cddr expr)]
            [pos-args (filter (lambda (x) (not (keyword? x))) formals)]
            [kw-args (filter keyword? formals)])
       (ir:ir-lambda (append pos-args kw-args) (map parse-expr body)))]
    
    ;; Handle define expressions
    [(and (list? expr) (>= (length expr) 3) (eq? (car expr) 'define))
     (let ([name (cadr expr)]
           [value (caddr expr)])
       (ir:ir-define name (parse-expr value)))]
    
    ;; Handle set! expressions
    [(and (list? expr) (>= (length expr) 3) (eq? (car expr) 'set!))
     (let ([name (cadr expr)]
           [value (caddr expr)])
       (ir:ir-var-set name (parse-expr value)))]
    
    ;; Handle if expressions
    [(and (list? expr) (>= (length expr) 4) (eq? (car expr) 'if))
     (let ([test (cadr expr)]
           [then (caddr expr)]
           [else (cadddr expr)])
       (ir:ir-if (parse-expr test) (parse-expr then) (parse-expr else)))]
    
    ;; Handle begin expressions
    [(and (list? expr) (>= (length expr) 2) (eq? (car expr) 'begin))
     (let ([body (cdr expr)])
       (ir:ir-begin (map parse-expr body)))]
    
    ;; Handle let expressions
    [(and (list? expr) (>= (length expr) 3) (eq? (car expr) 'let))
     (let ([bindings (cadr expr)]
           [body (cddr expr)])
       (ir:ir-let bindings (map parse-expr body)))]
    
    ;; Handle letrec expressions
    [(and (list? expr) (>= (length expr) 3) (eq? (car expr) 'letrec))
     (let ([bindings (cadr expr)]
           [body (cddr expr)])
       (ir:ir-letrec bindings (map parse-expr body)))]
    
    ;; Handle cond expressions
    [(and (list? expr) (>= (length expr) 2) (eq? (car expr) 'cond))
     (let ([clauses (cdr expr)])
       (ir:ir-cond (map (lambda (c) (list (parse-expr (car c)) (parse-expr (cadr c))))
                       (take clauses (- (length clauses) 1)))
                   (parse-expr (cadr (last clauses)))))]
    
    ;; Handle match expressions
    [(and (list? expr) (>= (length expr) 3) (eq? (car expr) 'match))
     (let ([target (cadr expr)]
           [clauses (cddr expr)])
       (ir:ir-match (parse-expr target)
                    (map (lambda (c) (list (parse-pattern (car c)) (parse-expr (cadr c))))
                         clauses)))]
    
    ;; Handle function applications with keyword arguments
    [else
     (let* ([func (car expr)]
            [args (cdr expr)]
            [pos-args (filter (lambda (x) (not (keyword? x))) args)]
            [kw-args (filter keyword? args)])
       (ir:ir-app (parse-expr func) 
                 (append (map parse-expr pos-args)
                        (map (lambda (kw) (ir:ir-literal kw)) kw-args))))]))

(define (parse-pattern pat)
  (cond
    [(syntax? pat) (parse-pattern (syntax->datum pat))]
    [(symbol? pat) (ir:ir-pat-var pat)]
    [(null? pat) (ir:ir-pat-literal '())]
    [(or (number? pat) (string? pat) (boolean? pat))
     (ir:ir-pat-literal pat)]
    [(and (list? pat) (eq? (car pat) '_))
     (ir:ir-pat-wildcard)]
    [(and (list? pat) (eq? (car pat) 'list))
     (let ([elements (cdr pat)])
       (ir:ir-pat-list (map parse-pattern elements) #f))]
    [(and (list? pat) (eq? (car pat) 'list*))
     (let ([elements (cdr pat)])
       (ir:ir-pat-list (map parse-pattern (take elements (- (length elements) 1)))
                       (parse-pattern (last elements))))]
    [(and (list? pat) (symbol? (car pat)))
     (ir:ir-pat-struct (car pat) (map parse-pattern (cdr pat)))]
    [else
     (error 'parse-pattern "Unsupported pattern: ~s" pat)]))