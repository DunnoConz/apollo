#lang racket/base

(require racket/match
         racket/syntax
         racket/list
         racket/path
         "ir-types.rkt")

(provide (all-defined-out))

;; Identity function for use in builtins
(define (identity x) x)

;; Environment Types
(struct shared-env (bindings) #:transparent)
(struct ctfe-env (bindings cache) #:transparent)

;; Environment Functions
(define (make-shared-env)
  (shared-env (make-hash)))

(define (shared-env-add! env name value)
  (hash-set! (shared-env-bindings env) name value))

(define (shared-env-get env name)
  (hash-ref (shared-env-bindings env) name #f))

(define (make-ctfe-env)
  (ctfe-env (make-hash) (make-hash)))

(define (ctfe-env-add! env name value)
  (hash-set! (ctfe-env-bindings env) name value))

(define (ctfe-env-get env name)
  (hash-ref (ctfe-env-bindings env) name #f))

;; Memoization for CTFE evaluation
(define (ctfe-eval expr env)
  (let ([cache (ctfe-env-cache env)])
    (or (hash-ref cache expr #f)
        (let ([result (ctfe-eval* expr env)])
          (hash-set! cache expr result)
          result))))

(define (ctfe-eval* expr env)
  (match expr
    [(? ir-literal? lit) (ir-literal-value lit)]
    [(? ir-var-ref? var)
     (let ([value (ctfe-env-get env (ir-var-ref-name var))])
       (if value
           value
           (error 'ctfe-eval "Undefined variable: ~a" (ir-var-ref-name var))))]
    [(? ir-app? app)
     (let ([func-value (ctfe-eval (ir-app-func app) env)]
           [arg-values (map (lambda (arg) (ctfe-eval arg env)) (ir-app-args app))])
       (if (procedure? func-value)
           (apply func-value arg-values)
           (error 'ctfe-eval "Not a function: ~a" func-value)))]
    [(? ir-if? if-expr)
     (if (ctfe-eval (ir-if-test if-expr) env)
         (ctfe-eval (ir-if-then if-expr) env)
         (ctfe-eval (ir-if-else if-expr) env))]
    [(? ir-begin? begin-expr)
     (let loop ([exprs (ir-begin-exprs begin-expr)])
       (if (null? (cdr exprs))
           (ctfe-eval (car exprs) env)
           (begin
             (ctfe-eval (car exprs) env)
             (loop (cdr exprs)))))]
    [(? ir-let? let-expr)
     (let ([new-env (make-ctfe-env)])
       (for ([binding (ir-let-bindings let-expr)])
         (let ([name (car binding)]
               [value (ctfe-eval (cadr binding) env)])
           (ctfe-env-add! new-env name value)))
       (ctfe-eval (car (ir-let-body let-expr)) new-env))]
    [_ (error 'ctfe-eval "Unsupported expression: ~a" expr)]))

;; CTFE Annotation
(struct ctfe-annotation (expr) #:transparent)

;; Mark an expression for CTFE
(define (mark-for-ctfe expr)
  (ctfe-annotation expr))

;; Check if an expression is marked for CTFE
(define (ctfe-marked? expr)
  (ctfe-annotation? expr))

;; CTFE Optimizer
(define (optimize-ctfe expr env)
  (match expr
    [(? ctfe-annotation? ann)
     (let ([result (ctfe-eval (ctfe-annotation-expr ann) env)])
       (ir-literal result))]
    [(? ir-app? app)
     (ir-app (optimize-ctfe (ir-app-func app) env)
             (map (lambda (arg) (optimize-ctfe arg env)) (ir-app-args app))
             (ir-app-kw-args app))]
    [(? ir-if? if-expr)
     (ir-if (optimize-ctfe (ir-if-test if-expr) env)
            (optimize-ctfe (ir-if-then if-expr) env)
            (optimize-ctfe (ir-if-else if-expr) env))]
    [(? ir-begin? begin-expr)
     (ir-begin (map (lambda (expr) (optimize-ctfe expr env)) (ir-begin-exprs begin-expr)))]
    [(? ir-let? let-expr)
     (ir-let (map (lambda (binding)
                    (list (car binding)
                          (optimize-ctfe (cadr binding) env)))
                  (ir-let-bindings let-expr))
             (map (lambda (expr) (optimize-ctfe expr env)) (ir-let-body let-expr)))]
    [_ expr]))

;; Built-in CTFE functions
(define ctfe-builtins
  (make-hash
   `((+ . ,+)
     (- . ,-)
     (* . ,*)
     (/ . ,/)
     (= . ,=)
     (< . ,<)
     (> . ,>)
     (<= . ,<=)
     (>= . ,>=)
     (not . ,not)
     (and . ,(lambda args (andmap identity args)))
     (or . ,(lambda args (ormap identity args)))
     (list . ,list)
     (cons . ,cons)
     (car . ,car)
     (cdr . ,cdr)
     (null? . ,null?)
     (length . ,length)
     (append . ,append)
     (map . ,map)
     (filter . ,filter)
     (foldl . ,foldl)
     (foldr . ,foldr))))

;; Initialize CTFE environment with builtins
(define (init-ctfe-env)
  (let ([env (make-ctfe-env)])
    (for ([(name func) (in-hash ctfe-builtins)])
      (ctfe-env-add! env name func))
    env)) 