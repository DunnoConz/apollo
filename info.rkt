#lang info

(define collection "apollo")
(define deps '("base"
               "rackunit-lib"
               "scribble-lib"
               "racket-doc"))
(define build-deps '("scribble-lib"
                    "racket-doc"
                    "rackunit-lib"))
(define scribblings '(("scribblings/apollo.scrbl" ())))
(define pkg-desc "A Racket to Luau compiler for Roblox game development")
(define version "0.1")
(define pkg-authors '(yourusername))

;; Tell Racket where to find the collections
(define collection-search-dirs '("src"))

;; Define the main collection paths
(define compile-collection-zos #f)
(define compile-collection-paths '("src"))

;; Define the main collection
(define compile-omit-paths '("private"))
(define test-omit-paths '("private")) 