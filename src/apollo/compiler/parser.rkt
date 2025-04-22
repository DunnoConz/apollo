#lang racket/base

(require racket/match
         racket/syntax
         racket/list
         racket/port
         racket/string
         racket/path
         syntax/parse
         (prefix-in ir: (submod "./ir.rkt" ir))
         "ir-types.rkt")

(provide parse-program parse-expr parse-racket-string parse-pattern racket-to-ir)

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
  (let* ([processed-str (remove-lang-directive str)]
         [exprs (with-input-from-string processed-str
                 (lambda ()
                   (let ([port (current-input-port)])
                     (let loop ([exprs '()])
                       (let ([expr (read-syntax source-name port)])
                         (if (eof-object? expr)
                             (reverse exprs)
                             (loop (cons expr exprs))))))))])
    (if (null? exprs)
        (error 'parse-racket-string "Empty input")
        (let ([body (if (= 1 (length exprs))
                       (car exprs)
                       (datum->syntax #f (cons 'begin exprs)))])
          (datum->syntax #f `(module default racket/base ,body))))))

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
  ((dynamic-require 'apollo/compiler/ir 'convert-expr-to-ir) expr))

(define (parse-pattern pat)
  ((dynamic-require 'apollo/compiler/ir 'convert-pattern-to-ir) pat))

;; Convert Racket syntax to IR
(define (racket-to-ir stx)
  ((dynamic-require 'apollo/compiler/ir 'convert-to-ir) stx))

(provide (all-defined-out))