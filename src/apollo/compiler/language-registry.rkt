#lang racket/base

(require racket/hash
         racket/match
         racket/function ; For dynamic-require
         racket/list
         racket/path)

(provide register-apollo-language! ; Register a language name -> module path mapping
         load-language-modules   ; Load modules for given language names
         )

;; Registry to store language module paths
;; Maps language-symbol -> module-path-string-or-path
(define apollo-language-registry (make-hash))

;; Registers the module path for a custom Apollo language.
(define (register-apollo-language! lang-sym module-path)
  (unless (or (string? module-path) (path? module-path))
    (error 'register-apollo-language! "module-path must be a string or path, got: ~a" module-path))
  (hash-set! apollo-language-registry lang-sym module-path))

;; Looks up the module path for a given Apollo language symbol.
(define (lookup-language-module-path lang-sym)
  (hash-ref apollo-language-registry lang-sym #f))

;; Loads the modules associated with the given language symbols.
;; This executes the top-level code in the modules, including
;; define-syntax->ir calls which should register their clauses.
(define (load-language-modules lang-syms)
  (for ([lang-sym (in-list lang-syms)])
    (let ([module-path (lookup-language-module-path lang-sym)])
      (if module-path
          (begin
            (printf "Loading DSL module for language '~a' from: ~a\n" lang-sym module-path)
            ;; Use dynamic-require to load the module and run its top level.
            ;; We don't need the result, just the side-effect of running the code.
            (dynamic-require module-path #f)) ; #f means don't require a specific export
          (eprintf "Warning: No module path registered for language '~a'\n" lang-sym)))))

;; Example usage (can be moved elsewhere):
;; (register-apollo-language! 'shout-dsl "../dsls/shout-dsl.rkt")
;; (load-language-modules '(shout-dsl)) 