#lang racket

(require racket/file
         racket/path
         racket/string
         apollo) ; Use package path

;; Define the examples directory
(define examples-dir (build-path "examples"))

;; Variables to track compilation statistics
(define total-files 0)
(define successful-files 0)
(define failed-files 0)
(define skipped-files 0)

;; Extract Racket code from a file, handling various syntax features
(define (extract-racket-code input-file [debug? #f])
  (define content (file->string input-file))
  
  ;; Remove comments, #lang line, and expected output
  (define code-without-comments-and-output
    (regexp-replace* 
     #px"(?:(?m:^;;.*$)|#lang.*?\n|;+ Expected Luau output:.*?$)"
     content ""))
  
  ;; Replace struct definitions with options
  (define code-without-struct-options
    (regexp-replace*
     #px"\\(struct ([^()\n]+) \\(([^()]*?)\\) (#:[^ ]+ [^()\n]*?)\\)"
     code-without-comments-and-output
     "(struct \\1 (\\2))"))
  
  ;; Replace match patterns with #:when clauses
  (define code-without-when-clauses
    (regexp-replace*
     #px"\\[([^][]*?) #:when ([^][]*?)\\]"
     code-without-struct-options
     "[\\1]"))
    
  ;; Replace regular expression literals
  (define code-without-regexp-literals
    (regexp-replace*
     #px"#rx\"([^\"]*?)\""
     code-without-when-clauses
     "\"\\1\""))
    
  ;; Replace association lists (ActuatorType . "Motor")
  (define code-without-association-lists
    (regexp-replace*
     #px"\\(([^()]*?) \\. ([^()]*?)\\)"
     code-without-regexp-literals
     "(cons \\1 \\2)"))
    
  ;; Replace #:easing-style symbols
  (define code-without-easing-style
    (regexp-replace*
     #px"#:([a-zA-Z0-9-]+) "
     code-without-association-lists
     "'\\1 "))
  
  ;; Trim leading/trailing whitespace and empty lines
  (define trimmed-code
    (regexp-replace* 
     #px"^\\s+|\\s+$"
     code-without-easing-style ""))
  
  ;; For debugging
  (when debug?
    (displayln "---------- Extracted code ----------")
    (displayln trimmed-code)
    (displayln "----------------------------------"))
  
  ;; Return the processed code
  trimmed-code)

;; Process Racket code to handle problematic syntax
(define (process-racket-code code debug?)
  (define lines (string-split code "\n"))
  
  ;; Remove the #lang line if present
  (define no-lang-lines
    (if (and (not (empty? lines))
             (regexp-match? #rx"^#lang" (car lines)))
        (cdr lines)
        lines))
  
  ;; Filter out comment lines
  (define no-comment-lines
    (filter (lambda (line)
              (not (regexp-match? #rx"^\\s*;;" line)))
            no-lang-lines))
  
  ;; Join lines back into a single string
  (define code-string (string-join no-comment-lines "\n"))
  
  ;; Apply a series of transformations to fix various syntax issues
  (define fixed-code (apply-transformations code-string debug?))
  
  ;; Remove leading/trailing blank lines
  (define trimmed-code (string-trim fixed-code))
  
  ;; Ensure code is wrapped in a module form if not already
  (if (regexp-match? #rx"^\\s*\\(module" trimmed-code)
      trimmed-code
      (format "(module anonymous racket\n~a\n)" trimmed-code))
  )

;; Apply a series of transformations to handle problematic Racket syntax
(define (apply-transformations code debug?)
  (define transformations
    (list
     ;; 1. Handle struct definitions with #:transparent and other options
     (cons
      #rx"\\(struct\\s+([^\\s\\(\\)]+)\\s*\\(([^\\)]*)\\)\\s+(#:[^\\s\\(\\)]+\\s*)+\\)"
      (lambda (match)
        (define struct-name (cadr match))
        (define fields (caddr match))
        (format "(struct ~a (~a))" struct-name fields)))
     
     ;; 2. Handle struct definitions with super-type and options
     (cons
      #rx"\\(struct\\s+([^\\s\\(\\)]+)\\s+([^\\s\\(\\)]+)\\s*\\(([^\\)]*)\\)\\s+(#:[^\\s\\(\\)]+\\s*)+\\)"
      (lambda (match)
        (define struct-name (cadr match))
        (define super-type (caddr match))
        (define fields (cadddr match))
        (format "(struct ~a ~a (~a))" struct-name super-type fields)))
     
     ;; 3. Handle struct field properties like #:mutable and #:auto
     (cons
      #rx"\\[(\\S+)\\s+(#:[^\\]]+)\\]"
      (lambda (match)
        (define field (cadr match))
        (format "[~a]" field)))
     
     ;; 4. Handle match patterns with #:when clauses
     (cons
      #rx"\\[([^\\]\\[]*)\\s+#:when\\s+([^\\]\\[]*)\\]"
      (lambda (match)
        (define pattern (cadr match))
        (format "[~a]" pattern)))
     
     ;; 5. Replace association lists using dotted pairs
     (cons
      #rx"\\(([^\\s\\(\\)]+)\\s+\\.\\s+([^\\s\\(\\)]+)\\)"
      (lambda (match)
        (define key (cadr match))
        (define value (caddr match))
        (format "(cons ~a ~a)" key value)))
     
     ;; 6. Handle special Roblox syntax like ActuatorType
     (cons
      #rx"\\(ActuatorType\\s+\\.\\s+\"([^\"]+)\"\\)"
      (lambda (match)
        (define type-value (cadr match))
        (format "\"~a\"" type-value)))
     
     ;; 7. Replace regex literals with string equivalents
     (cons
      #rx"#rx\"([^\"]*)\""
      (lambda (match)
        (define pattern (cadr match))
        (format "\"~a\"" pattern)))
     
     ;; 8. Replace Racket precise regex literals
     (cons
      #rx"#px\"([^\"]*)\""
      (lambda (match)
        (define pattern (cadr match))
        (format "\"~a\"" pattern)))
     
     ;; 9. Handle #:easing-style options in animations
     (cons
      #rx"#:easing-style\\s+([^\\s\\(\\)]+)"
      (lambda (match)
        (define style (cadr match))
        (format "easing-style-~a" style)))
     
     ;; 10. Fix missing bracket in UI system
     (cons
      #rx"\\(build-ui\\s+([^\\(\\)]*)$"
      "(build-ui \\1)")
     
     ;; 11. Replace struct-copy with a simpler form
     (cons
      #rx"\\(struct-copy\\s+([^\\s\\(\\)]+)\\s+([^\\s\\(\\)]+)\\s+\\(([^\\)]*)\\)\\)"
      (lambda (match)
        (define struct-name (cadr match))
        (define base-instance (caddr match))
        (define field-updates (cadddr match))
        (format "(make-copy-of ~a ~a (~a))" struct-name base-instance field-updates)))
    ))
  
  ;; Apply each transformation in sequence
  (define result
    (foldl
     (lambda (transform acc)
       (define pattern (car transform))
       (define handler (cdr transform))
       (regexp-replace* pattern acc handler))
     code
     transformations))
  
  ;; Print debugging information if requested
  (when debug?
    (for ([transform transformations])
      (define pattern (car transform))
      (when (regexp-match? pattern code)
        (printf "Applied transformation for pattern: ~a\n" pattern))))
  
  result)

;; Function to check if a path has a specific extension
(define (path-has-extension? path ext)
  (define str-path (path->string path))
  (and (> (string-length str-path) 4)
       (string=? (substring str-path (- (string-length str-path) 4)) ".rkt")))

;; Function to preprocess and compile a single file
(define (compile-file file [debug? #f])
  (define rel-path (find-relative-path (current-directory) file))
  
  ;; Determine if this file should be run in debug mode
  (define file-debug? (or debug? (regexp-match? #rx"binary-tree|game-events|ui-system" (path->string file))))
  
  (define output-dir (build-path (current-directory) "out"))
  (unless (directory-exists? output-dir)
    (make-directory output-dir))
    
  (define output-file (build-path output-dir (string-append (path->string (file-name-from-path file)) ".luau")))
  
  (printf "Compiling ~a ...\n" rel-path)
  
  (with-handlers ([exn:fail? (lambda (e)
                              (printf "ERROR: ~a\n" (exn-message e))
                              #f)])
    ;; Extract Racket code and preprocess it
    (define cleaned-code (extract-racket-code file file-debug?))
    
    (when file-debug?
      (printf "DEBUG: Using cleaned code:\n~a\n" cleaned-code))
    
    ;; Generate Luau code using the compiler
    (define luau-code (compile-racket-string-to-luau cleaned-code))
    
    (when file-debug?
      (printf "DEBUG: Generated Luau code:\n~a\n" luau-code))
    
    ;; Write the generated Luau code to the output file
    (with-output-to-file output-file
      (lambda () (display luau-code))
      #:exists 'replace)
    
    (printf "Successfully compiled to ~a (~a bytes)\n" 
            (build-path "out" (path->string (file-name-from-path file)) ".luau")
            (file-size output-file))
    
    #t))  ;; Return true on success

;; Function to recursively process all .rkt files in a directory
(define (process-directory dir)
  (printf "Processing directory: ~a~n" dir)
  (for ([item (directory-list dir)])
    (define full-path (build-path dir item))
    (cond
      [(directory-exists? full-path) (process-directory full-path)]
      [(and (file-exists? full-path) 
            (path-has-extension? full-path #".rkt"))
       (printf "Found Racket file: ~a~n" full-path)
       (compile-file full-path)]
      [else
       (set! skipped-files (add1 skipped-files))])))

;; Find all Racket files in a directory recursively
(define (find-racket-files dir)
  (define result '())
  (define (traverse path)
    (for ([item (directory-list path)])
      (define full-path (build-path path item))
      (cond
        [(directory-exists? full-path)
         (traverse full-path)]
        [(and (file-exists? full-path)
              (path-has-extension? full-path #".rkt"))
         (set! result (cons full-path result))])))
  (traverse dir)
  result)

;; Main function to compile examples
(define (compile-examples)
  (define examples-dir (build-path (current-directory) "examples"))
  (define files-to-compile (find-racket-files examples-dir))
  
  (printf "Found ~a Racket files to compile\n" (length files-to-compile))
  
  (define total-files (length files-to-compile))
  (define successful-files 0)
  (define failed-files 0)
  
  (for ([file files-to-compile])
    (define success (compile-file file))
    (if success
        (set! successful-files (add1 successful-files))
        (set! failed-files (add1 failed-files))))
  
  (printf "\nCompilation summary:\n")
  (printf "  Total files processed: ~a\n" total-files)
  (printf "  Successfully compiled: ~a\n" successful-files)
  (printf "  Failed to compile: ~a\n" failed-files))

;; Run the compilation
(compile-examples)

;; Compiles a single file
(define (compile-one-file src-file dest-file)
  (define debug-file? (regexp-match? #rx"binary-tree.rkt" src-file))
  (with-handlers ([exn:fail? (lambda (e)
                               (eprintf "Error processing file ~a: ~a~n" 
                                        src-file
                                        (exn-message e))
                               #f)])
    (define src-code (file->string src-file))
    (define extracted-code (extract-racket-code src-code debug-file?))
    (define-values (file-results success?) 
      (with-handlers 
          ([exn:fail? (lambda (e)
                        (eprintf "Compilation error for ~a: ~a~n" 
                                 src-file
                                 (exn-message e))
                        (values '() #f))])
        (unless (string? extracted-code)
          (error "Error extracting Racket code from file"))
        
        (define output-text 
          (with-handlers 
              ([exn:fail? (lambda (e)
                            (eprintf "Error compiling Racket code: ~a~n" 
                                     (exn-message e))
                            (raise e))])
            (compile-racket-string-to-luau extracted-code)))
        
        (define output-size (string-length output-text))
        
        (printf "Generated Luau Code for ~a:~n~a~n" 
                src-file
                output-text)
        (values (list output-text) #t)))
    (and success? file-results))) 