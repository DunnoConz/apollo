#lang racket/base

;; Command-line tool for Apollo's Rojo integration
;; Allows compiling entire Rojo projects to Luau

(require racket/file
         racket/path
         racket/system ;; Added for filesystem-change-evt
         apollo/rojo/integration)

;; == Compilation Logic (Copied from build-exe.rkt for standalone use) ==
;; Compile a Rojo project
(define (compile-project input-dir output-dir)
  (displayln (format "Compiling Rojo project from ~a to ~a" input-dir output-dir))
   (with-handlers ([exn:fail? (lambda (e)
                                (displayln (format "Error compiling project ~a: ~a" 
                                                   input-dir 
                                                   (exn-message e))))])
     (displayln (compile-rojo-project input-dir output-dir)))) ; Use function from rojo-integration

;; == Main Execution ==
(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (define source-dir #f)
  (define output-dir #f)
  (define verbose? #f)
  (define watch-mode? #f)
  
  ;; Parse command line arguments manually
  (let loop ([remaining-args args])
    (cond
      [(null? remaining-args) (void)] ; Done processing
      [(or (equal? (car remaining-args) "-o") 
           (equal? (car remaining-args) "--output"))
       (if (null? (cdr remaining-args))
           (begin (displayln "Error: Missing argument for -o/--output option") (exit 1))
           (begin (set! output-dir (cadr remaining-args)) (loop (cddr remaining-args))))]
      [(or (equal? (car remaining-args) "-v") 
           (equal? (car remaining-args) "--verbose"))
       (set! verbose? #t)
       (loop (cdr remaining-args))]
      [(or (equal? (car remaining-args) "-h") 
           (equal? (car remaining-args) "--help"))
       (displayln "Usage: apollo-rojo [options] <source-directory>")
       (displayln "Options:")
       (displayln "  -o, --output <dir>    Output directory (required)")
       (displayln "  -v, --verbose         Enable verbose output")
       (displayln "  --watch              Watch for changes and recompile automatically")
       (displayln "  -h, --help            Show this help message")
       (exit 0)]
      [(equal? (car remaining-args) "--watch")
       (set! watch-mode? #t)
       (loop (cdr remaining-args))]
      [else ; Assume it's the source directory
       (if source-dir
           (begin (displayln "Error: Multiple source directories specified") (exit 1))
           (begin (set! source-dir (car remaining-args)) (loop (cdr remaining-args))))]))
  
  ;; === Input Validation ===
  (unless (and source-dir (directory-exists? source-dir))
    (displayln "Error: Source directory does not exist or was not specified") (exit 1))
  
  (unless output-dir
    (displayln "Error: Output directory must be specified with -o or --output") (exit 1))
  
  (when (and output-dir (not (directory-exists? output-dir)))
    (make-directory* output-dir))
  
  ;; === Compilation Function ===
  (define (perform-compilation)
      (compile-project source-dir output-dir))

  ;; === Initial Compilation ===
  (when verbose?
    (displayln (format "Starting initial compilation for ~a" source-dir)))
  (perform-compilation)

  ;; === Watch Mode ===
  (when watch-mode?
    (displayln (format "\nWatching for changes in ~a... (Press Ctrl+C to stop)" source-dir))
    (let loop ()
      (define change-evt (filesystem-change-evt source-dir #:exists #f)) ;; Watch directory recursively
      (sync change-evt) ;; Wait for a filesystem change event
      (when verbose? (displayln "Change detected, recompiling..."))
      (perform-compilation)
      (loop)))) 