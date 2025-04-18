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
(define collection-search-dirs '("."))
(define compile-collection-zos #t)
(define compile-collection-paths '("."))

;; Define paths to omit from compilation
(define compile-omit-paths '("tests" "examples" "docs" "docs-hugo" "private"))
(define test-omit-paths '("private"))

;; Define the main module
(define main-module "main.rkt")

;; Define the collection structure
(define collection-links
  '("cmd"
    "compiler"
    "rojo"
    "std"
    "ecs"))

;; Define the collection hierarchy
(define collection-hierarchy
  '("apollo"
    ("cmd" "cmd")
    ("compiler" "compiler")
    ("rojo" "rojo")
    ("std" "std")
    ("ecs" "ecs"))) 