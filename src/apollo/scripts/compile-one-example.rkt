#lang racket

(require racket/file
         racket/path
         racket/string
         apollo) ; Use package path

;; Function to extract actual code from a Racket example file
;; This is a more robust approach that extracts just the relevant code parts
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
  (printf "Compiling ~a...~n" file)
  (let* ([racket-code (file->string file)]
         [_ (printf "Read ~a bytes of Racket code~n" (string-length racket-code))]
         [_ (printf "First 100 chars: ~a~n" (substring racket-code 0 (min 100 (string-length racket-code))))]
         
         ;; Extract just the relevant code
         [cleaned-code (extract-racket-code racket-code)]
         [_ (printf "Extracted code (~a bytes):~n~a~n" (string-length cleaned-code) cleaned-code)]
         
         ;; Try to compile with error handling
         [luau-code 
          (with-handlers 
              ([exn:fail? 
                (lambda (e) 
                  (printf "ERROR compiling ~a: ~a~n" file (exn-message e))
                  "")])  ; Return empty string on error
            (printf "About to call compile-racket-string-to-luau~n")
            (compile-racket-string-to-luau cleaned-code))]
         [_ (printf "Generated ~a bytes of Luau code~n" (string-length luau-code))]
         [_ (when (not (string=? luau-code ""))
              (printf "Luau code:~n~a~n" luau-code))]
         [output-file (string-append (path->string (path-replace-extension file "")) ".luau")])
    (if (string=? luau-code "")
        (printf "No output generated for ~a~n" file)
        (begin
          (printf "Writing output to: ~a~n" output-file)
          (with-output-to-file output-file
            #:exists 'replace
            (lambda () (display luau-code)))))))

;; Main function - compile one specific file
(define input-file
  (if (= (vector-length (current-command-line-arguments)) 1)
      (vector-ref (current-command-line-arguments) 0)
      "examples/01-basics/hello-world.rkt"))
(compile-file input-file)
(printf "Compilation complete!~n") 