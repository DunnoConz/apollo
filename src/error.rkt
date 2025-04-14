#lang racket

(require racket/string)

;; Define custom error structs
(struct compiler-error (message location) #:transparent)
(struct parse-error compiler-error () #:transparent)
(struct typecheck-error compiler-error () #:transparent)
(struct codegen-error compiler-error () #:transparent)

;; Error location information
(struct location (file line column span) #:transparent)

;; Function to create a location from a Racket syntax object
(define (syntax->location stx)
  (if (syntax? stx)
      (let ([source (syntax-source stx)]
            [line (syntax-line stx)]
            [column (syntax-column stx)]
            [span (syntax-span stx)])
        (location source line column span))
      (location #f #f #f #f)))

;; Error formatting functions
(define (format-location loc)
  (if (and (location-file loc) (location-line loc))
      (format "~a:~a:~a" 
              (location-file loc) 
              (location-line loc) 
              (location-column loc))
      "unknown location"))

(define (format-error err)
  (format "~a: ~a at ~a" 
          (cond
            [(parse-error? err) "Parse Error"]
            [(typecheck-error? err) "Type Error"]
            [(codegen-error? err) "Code Generation Error"]
            [else "Compiler Error"])
          (compiler-error-message err)
          (format-location (compiler-error-location err))))

;; Error raising functions
(define (raise-parse-error message stx)
  (raise (parse-error message (syntax->location stx))))

(define (raise-typecheck-error message stx)
  (raise (typecheck-error message (syntax->location stx))))

(define (raise-codegen-error message stx)
  (raise (codegen-error message (syntax->location stx))))

(define (raise-compiler-error message [loc (location #f #f #f #f)])
  (raise (compiler-error message loc)))

;; Error handling function
(define (with-error-handling thunk)
  (with-handlers ([exn:fail? (λ (e)
                               (cond
                                 [(parse-error? e) 
                                  (error "Parse error: ~a" (format-error e))]
                                 [(typecheck-error? e) 
                                  (error "Type error: ~a" (format-error e))]
                                 [(codegen-error? e) 
                                  (error "Code generation error: ~a" (format-error e))]
                                 [(compiler-error? e) 
                                  (error "Compiler error: ~a" (format-error e))]
                                 [else (error "Unexpected error: ~a" (exn-message e))]))])
    (thunk)))

(provide compiler-error parse-error typecheck-error codegen-error
         location syntax->location
         format-location format-error
         raise-parse-error raise-typecheck-error 
         raise-codegen-error raise-compiler-error
         with-error-handling) 