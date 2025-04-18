#lang racket

(require racket/file
         racket/path
         racket/string
         apollo) ; Use package path

;; Function to extract Racket code from a file, removing comments and header
(define (extract-racket-code code)
  (define lines (string-split code "\n"))
  
  ;; Print original code for debugging
  (printf "DEBUG - Original code:~n~a~n~n" code)
  
  ;; Remove #lang line if present
  (define lines-without-lang
    (if (and (not (null? lines))
             (regexp-match? #rx"^\\s*#lang" (car lines)))
        (cdr lines)
        lines))
  
  ;; Filter out comment lines and expected output sections
  (define filtered-lines
    (let loop ([lines lines-without-lang]
               [in-expect-section? #f]
               [result '()])
      (cond
        [(null? lines) (reverse result)]
        [(regexp-match? #rx"^\\s*;;\\s*Expected Luau" (car lines))
         (loop (cdr lines) #t result)]
        [in-expect-section? (loop (cdr lines) #t result)]
        [(regexp-match? #rx"^\\s*;;.*$" (car lines)) 
         (loop (cdr lines) #f result)]
        [else (loop (cdr lines) #f (cons (car lines) result))])))
  
  (define code-str (string-join filtered-lines "\n"))
  
  ;; First: remove #:transparent completely
  (define no-transparent-code 
    (regexp-replace* #rx"#:transparent" code-str ""))
  
  ;; Print intermediate processed code
  (printf "DEBUG - After removing #:transparent:~n~a~n~n" no-transparent-code)
  
  ;; Ensure code is wrapped in a module form
  (define module-code
    (if (regexp-match? #rx"^\\s*\\(module" no-transparent-code)
        no-transparent-code
        (string-append "(module binary-tree racket\n" no-transparent-code "\n)")))
  
  ;; Print final processed code
  (printf "DEBUG - Final processed code:~n~a~n~n" module-code)
  
  module-code)

;; Compile the binary-tree.rkt file
(define (compile-binary-tree)
  (define file "examples/04-data-structures/binary-tree.rkt")
  
  (printf "Compiling file: ~a~n" file)
  
  (with-handlers 
      ([exn:fail? 
        (lambda (e) 
          (printf "ERROR: ~a~n" (exn-message e))
          (exit 1))])
    
    (define racket-code (file->string file))
    (define cleaned-code (extract-racket-code racket-code))
    
    (printf "Compiling cleaned code...~n")
    (define luau-code (compile-racket-string-to-luau cleaned-code))
    
    (printf "~nGenerated Luau Code:~n~a~n" luau-code)
    
    (define output-file (string-append (path->string (path-replace-extension file "")) ".luau"))
    (printf "Writing output to: ~a~n" output-file)
    (with-output-to-file output-file
      #:exists 'replace
      (lambda () (display luau-code)))))

;; Run the compilation
(compile-binary-tree) 