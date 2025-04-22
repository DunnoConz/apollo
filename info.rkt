#lang setup/infotab

(define install-collection "installer.rkt")
(define compile-omit-paths '("tests" "examples" "private"))
(define collection "apollo")
(define deps '("base"
              "rackunit-lib"  ; Added for testing support
              "racket-doc"    ; Added for documentation
              "scribble-lib")) ; Added for documentation generation
(define build-deps '())

;; Point to the source directory for collection search
(define collection-search-dirs '("src"))
(define compile-collection-paths '("src"))

;; Define the main module
(define main-module "main.rkt")

(define scribblings '(("scribblings/apollo.scrbl" ())))
(define version "0.1")
(define pkg-desc "A Racket to Luau compiler for Roblox game development")
(define pkg-authors '("apollo-developers"))

;; Define collection links and hierarchy
(define collection-links
  '(("apollo" "src/apollo")
    ("apollo/compiler" "src/apollo/compiler")
    ("apollo/rojo" "src/apollo/rojo")
    ("apollo/std" "src/apollo/std")
    ("apollo/dsls" "src/apollo/dsls")
    ("apollo/ecs" "src/apollo/ecs")))

;; Define the collection hierarchy
(define collection-hierarchy
  '(("apollo" "src/apollo")
    ("apollo/compiler" "src/apollo/compiler")
    ("apollo/rojo" "src/apollo/rojo")
    ("apollo/std" "src/apollo/std")
    ("apollo/dsls" "src/apollo/dsls")
    ("apollo/ecs" "src/apollo/ecs")))

;; Define the raco commands
(define raco-commands
  '(("apollo" (submod "cmd/apollo/main" main) 
             "Apollo Racket->Luau Compiler"
             #f)
    ("apollo-rojo" (submod "cmd/apollo-rojo/main" main)
                  "Apollo Rojo Integration"
                  #f))) 