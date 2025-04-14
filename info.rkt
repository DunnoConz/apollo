#lang info

(define collection "apollo")
(define deps '("base"))
(define build-deps '("rackunit" "scribble-lib" "setup" "info"))
(define scribblings '(("scribblings/apollo.scrbl" ())))
(define pkg-desc "Apollo: A Racket to Luau Compiler")
(define version "0.1")
(define pkg-authors '("Apollo Team"))
(define license '(MIT))

;; Tell Racket where to find the collections
(define collection-search-dirs '("src"))

;; Define the main collection paths
(define compile-collection-zos #f)
(define compile-collection-paths '("src"))

;; Define the main collection
(define compile-omit-paths '("private"))
(define test-omit-paths '("private")) 