#lang info

(define collection "apollo")
(define deps '("base"
               "rackunit-lib"
               "scribble-lib"
               "racket-doc"
))
(define build-deps '("scribble-lib"
                    "racket-doc"
                    "rackunit-lib"
))
(define scribblings '(("scribblings/apollo.scrbl" ())))
(define pkg-desc "A Racket to Luau compiler for Roblox game development")
(define version "0.1")
(define pkg-authors '(yourusername))

;; Define the main collection paths
(define collection-search-dirs '("src"))
(define compile-collection-zos #t)
(define compile-collection-paths '("src"))

;; Define paths to omit from compilation
(define compile-omit-paths '("tests" "examples" "docs" "docs-hugo" "private"))
(define test-omit-paths '("private"))

;; Define the main module
(define main-module "src/apollo/main.rkt")

;; Define the collection structure
(define collection-links
  '("src/apollo/compiler"
    "src/apollo/rojo"
    "src/apollo/std"
    "src/apollo/ecs"))

;; Define the collection hierarchy
(define collection-hierarchy
  '("apollo"
    ("compiler" "src/apollo/compiler")
    ("rojo" "src/apollo/rojo")
    ("std" "src/apollo/std")
    ("ecs" "src/apollo/ecs"))) 