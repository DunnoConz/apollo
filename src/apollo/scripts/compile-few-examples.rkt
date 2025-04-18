#lang racket

(require racket/file
         racket/path
         racket/string
         apollo) ; Use package path

;; Function to extract actual code from a Racket example file
(define (extract-racket-code racket-code)
  (define lines (string-split racket-code "\n"))
  
  ;; Remove the #lang line if present
  (when (and (not (null? lines)) 
             (regexp-match #rx"^#lang" (car lines)))
    (set! lines (cdr lines)))
  
  ;; Remove comment lines that start with ;;
  (set! lines (filter (lambda (line) 
                       (not (regexp-match #rx"^\\s*;;.*" line)))
                     lines))
  
  ;; Remove expected Luau output section
  (define in-comment-block #f)
  (set! lines
        (filter 
         (lambda (line)
           (cond
             [(regexp-match #rx"^\\s*#\\|" line) (set! in-comment-block #t) #f]
             [(regexp-match #rx"\\|#\\s*$" line) (set! in-comment-block #f) #f]
             [in-comment-block #f]
             [else #t]))
         lines))
  
  ;; Remove leading and trailing empty lines
  (let remove-leading ([ls lines])
    (if (and (pair? ls) (string=? (string-trim (car ls)) ""))
        (remove-leading (cdr ls))
        (set! lines ls)))
  
  ;; Join back into a string and wrap in a module form if not already
  (let ([code (string-join lines "\n")])
    (if (regexp-match #rx"^\\s*\\(module" code)
        code
        (format "(module example racket/base\n~a)" code))))

;; Function to preprocess and compile a single file
(define (compile-file file)
  (printf "\n\n==== Compiling ~a ====\n" file)
  (with-handlers 
      ([exn:fail? 
        (lambda (e) 
          (printf "FATAL ERROR processing ~a: ~a~n" file (exn-message e)))])
    (let* ([racket-code (file->string file)]
           [_ (printf "  - Read ~a bytes~n" (string-length racket-code))]
           
           [cleaned-code (extract-racket-code racket-code)]
           [_ (printf "  - Extracted ~a bytes of clean code~n" (string-length cleaned-code))]
           [_ (printf "  - Cleaned code preview (first 100 chars): ~a~n" 
                     (substring cleaned-code 0 (min 100 (string-length cleaned-code))))]
           
           [luau-code 
            (with-handlers 
                ([exn:fail? 
                  (lambda (e) 
                    (printf "  - ERROR during compilation: ~a~n" (exn-message e))
                    "")])
              (compile-racket-string-to-luau cleaned-code))]
           [_ (printf "  - Generated ~a bytes of Luau code~n" (string-length luau-code))]
           
           [output-file (string-append (path->string (path-replace-extension file "")) ".luau")])
      
      (if (string=? luau-code "")
          (printf "  - No output generated for ~a~n" file)
          (begin
            (printf "  - Writing output to: ~a~n" output-file)
            (with-output-to-file output-file
              #:exists 'replace
              (lambda () (display luau-code))))))))

;; List of example files to compile
(define example-files 
  (list "examples/01-basics/hello-world.rkt"
        "examples/01-basics/arithmetic.rkt"
        "examples/02-pattern-matching/simple-match.rkt"
        "examples/03-quasiquote-patterns/basic-quasiquote.rkt"
        "examples/04-data-structures/binary-tree.rkt"))

;; Main function - compile specific files
(for-each compile-file example-files)
(printf "\nCompilation complete!~n") 