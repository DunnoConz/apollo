#lang racket/base

(require racket/port
         racket/string
         syntax/parse
         apollo/compiler/ir ; For convert-expr-to-ir and IR types
         apollo/compiler/parser ; For parse-racket-string utility?
         apollo/compiler/ir-types)

(provide parse expand)

;; Simple Parser for #lang apollo/test-dsl
;; Reads standard Racket S-expressions after the #lang line.
(define (parse source-string source-path)
  (displayln (format "[test-dsl] Parsing ~a" source-path))
  ;; Reuse existing parser logic but skip the #lang line handling inside it
  (define exprs
    (with-input-from-string source-string
      (lambda ()
        (port->list read-syntax source-path))))
  ;; We expect a single top-level expression usually for simple DSL tests
  ;; Or perhaps wrap in a begin?
  (displayln (format "[test-dsl] Parsed: ~s" exprs))
  exprs) ; Return list of syntax objects


;; Simple Expander for #lang apollo/test-dsl
;; Converts parsed syntax to Apollo IR.
(define (expand parsed-syntax source-path)
  (displayln (format "[test-dsl] Expanding syntax from ~a: ~s" source-path parsed-syntax))
  
  ;; Define recursive helper for transformation
  (define (transform stx)
    (syntax-parse stx
      ;; Handle (shout arg ...)
      [(shout arg ...)
       (displayln "[test-dsl] Found (shout ...)")
       ;; Convert to (ir-app (ir-var-ref 'print) (list-of-uppercased-string-literals))
       (ir-app (ir-var-ref 'print #:span (syntax-span stx))
               (for/list ([a (in-syntax #'(arg ...))])
                 (let ([arg-datum (syntax->datum a)])
                   (if (string? arg-datum)
                       (ir-literal (string-upcase arg-datum) #:span (syntax-span a))
                       ;; If not a string, convert normally (or error?)
                       (convert-expr-to-ir a))))
               '() ; No keyword args for print
               #:span (syntax-span stx))]
      
      ;; Handle other forms by deferring to the standard IR converter
      [other
       (displayln (format "[test-dsl] Defaulting to standard IR conversion for: ~s" (syntax->datum other)))
       (convert-expr-to-ir other)]))

  ;; The parser returns a list of syntax objects.
  ;; We map our transformer over them and wrap in an ir-program/ir-module.
  (let ([body-irs (map transform parsed-syntax)])
    ;; Construct a module IR
    (let ([module-name (path->launch-file-name source-path #t)]) ; Get a sensible module name
        (ir-program (list (ir-module module-name source-path body-irs)))))) 