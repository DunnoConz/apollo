#lang racket/base

(require setup/getinfo
         setup/collection-name)

(define install-collection "main.rkt")
(define compile-omit-paths '("tests" "examples" "private"))
(define collection-search-dirs '("."))
(define compile-collection-paths '("."))

(define (install)
  (displayln "Installing Apollo package...")
  (displayln "Installation complete!"))

(module+ main
  (install)) 