#lang racket/base

;; This file is used as the entry point for `raco exe`
;; It requires the main package module and command-line processing.

(require racket/file
         racket/port
         racket/string
         racket/path
         racket/system) ;; Added for filesystem-change-evt

;; Use relative paths
(require "../../src/apollo/compiler/parser.rkt"
         "../../src/apollo/compiler/codegen.rkt"
         (submod "../../src/apollo/compiler/ir.rkt" ir))

;; Optional import of Rojo integration if available
(define use-rojo? #f)
(define rojo-integrate #f)

(with-handlers ([exn:fail? (lambda (e) (void))])
  ;; Use package require for Rojo integration
  (dynamic-require 'apollo/rojo/integration 0)
  (set! use-rojo? #t)
  (set! rojo-integrate (dynamic-require 'apollo/rojo/integration 'compile-rojo-project)))

;; == Compilation Logic ==

;; Compile a single Racket file
(define (compile-single-file input-file output-file)
  (displayln (format "Compiling ~a to ~a" input-file output-file))
  (with-handlers ([exn:fail? (lambda (e)
                               (displayln (format "Error compiling ~a: ~a" 
                                                  input-file 
                                                  (exn-message e))))])
    (define racket-code (file->string input-file))
    (define racket-ast (parse-racket-string racket-code))
    (define ir (racket-to-ir racket-ast))
    (define luau-ast (ir->luau ir))
    (define luau-code (luau-ast->string luau-ast))
    (with-output-to-file output-file #:exists 'replace (lambda () (display luau-code)))
    (displayln (format "Successfully compiled to ~a" output-file))))

;; Compile a Rojo project
(define (compile-project input-dir output-dir)
  (displayln (format "Compiling Rojo project from ~a to ~a" input-dir output-dir))
   (with-handlers ([exn:fail? (lambda (e)
                                (displayln (format "Error compiling project ~a: ~a" 
                                                   input-dir 
                                                   (exn-message e))))])
     (displayln (rojo-integrate input-dir output-dir))))

;; == Main Execution ==
(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (define input-path #f)      ;; Can be file or directory
  (define output-path #f)     ;; Can be file or directory
  (define is-rojo-mode? #f)
  (define watch-mode? #f)
  
  ;; Parse command line arguments manually
  (let loop ([remaining-args args])
    (cond
      [(null? remaining-args) (void)] ; Done processing
      [(or (equal? (car remaining-args) "-o") 
           (equal? (car remaining-args) "--output"))
       (if (null? (cdr remaining-args))
           (begin (displayln "Error: Missing argument for -o/--output option") (exit 1))
           (begin (set! output-path (cadr remaining-args)) (loop (cddr remaining-args))))]
      [(or (equal? (car remaining-args) "-h") 
           (equal? (car remaining-args) "--help"))
       (displayln "Usage: apollo [options] <racket-file-or-directory>")
       (displayln "Options:")
       (displayln "  -o, --output <path>  Output file/directory path")
       (displayln "  --rojo               Compile as Rojo project (input and output must be directories)")
       (displayln "  --watch              Watch for changes and recompile automatically")
       (displayln "  -h, --help           Show this help message")
       (exit 0)]
      [(equal? (car remaining-args) "--rojo")
       (if use-rojo?
           (begin (set! is-rojo-mode? #t) (loop (cdr remaining-args)))
           (begin (displayln "Error: Rojo integration not available.") (exit 1)))]
      [(equal? (car remaining-args) "--watch")
       (set! watch-mode? #t)
       (loop (cdr remaining-args))]
      [else ; Assume it's the input path
       (if input-path
           (begin (displayln "Error: Multiple input paths specified") (exit 1))
           (begin (set! input-path (car remaining-args)) (loop (cdr remaining-args))))]))
  
  ;; === Input Validation ===
  (unless input-path
    (displayln "Error: Input path (file or directory) required") (exit 1))
  
  (when (and is-rojo-mode? (not (directory-exists? input-path)))
    (displayln "Error: In Rojo mode (--rojo), input must be a directory") (exit 1))
  (when (and is-rojo-mode? (not output-path))
    (displayln "Error: In Rojo mode (--rojo), output directory (-o) is required") (exit 1))
  (when (and (not is-rojo-mode?) (directory-exists? input-path))
     (displayln "Error: Input path is a directory, but --rojo flag was not specified") (exit 1))
  
  ;; Set default output file for single-file mode if not specified
  (when (and (not is-rojo-mode?) (not output-path))
    (set! output-path (string-append (path->string (path-replace-extension input-path "")) ".luau")))

  ;; === Compilation Function ===
  (define (perform-compilation)
    (if is-rojo-mode?
        (compile-project input-path output-path)
        (compile-single-file input-path output-path)))

  ;; === Initial Compilation ===
  (perform-compilation)

  ;; === Watch Mode ===
  (when watch-mode?
    (displayln (format "\nWatching for changes in ~a... (Press Ctrl+C to stop)" input-path))
    (let loop ()
      (define change-evt 
        (filesystem-change-evt input-path #:exists (if (directory-exists? input-path) #f #t)))
      (sync change-evt) ;; Wait for a filesystem change event
      (displayln "Change detected, recompiling...")
      (perform-compilation)
      (loop)))) 