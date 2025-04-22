#lang racket/base

(require racket/system
         racket/port
         racket/path
         racket/list
         setup/dirs)

;; Define collection links - kept in sync with info.rkt
(define collection-links
  '((apollo "src/apollo")
    (apollo/cmd "src/apollo/cmd")
    (apollo/cmd/apollo "src/apollo/cmd/apollo")
    (apollo/cmd/apollo-rojo "src/apollo/cmd/apollo-rojo")
    (apollo/compiler "src/apollo/compiler")
    (apollo/compiler/ir "src/apollo/compiler/ir")
    (apollo/compiler/parser "src/apollo/compiler/parser")
    (apollo/compiler/quasiquote-patterns "src/apollo/compiler/quasiquote-patterns")
    (apollo/rojo "src/apollo/rojo")
    (apollo/rojo/integration "src/apollo/rojo/integration")
    (apollo/std "src/apollo/std")
    (apollo/dsls "src/apollo/dsls")
    (apollo/tests "src/apollo/tests")
    (apollo/tests/compiler "src/apollo/tests/compiler")))

(define (install)
  (printf "Installing Apollo...\n")
  
  ;; Get the raco path
  (define raco-path "/opt/homebrew/bin/raco")
  
  ;; Remove existing links first
  (for ([link (map first collection-links)])
    (system* raco-path "pkg" "remove" "--force" (symbol->string link)))
  
  ;; Create new collection links
  (for ([link+path collection-links])
    (define link (symbol->string (first link+path)))
    (define path (second link+path))
    (system* raco-path "link" link path))
  
  ;; Setup the collections
  (system* raco-path "setup" "apollo")
  
  (printf "Installation complete!\n"))

(define installer install)

(provide install installer)

(module+ main
  (install)) 