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
(define collection-search-dirs '("." "src"))
(define compile-collection-paths '("." "src"))

;; Define the main module
(define main-module "main.rkt")

;; Update scribblings path to be relative to package root
(define scribblings '(("src/apollo/scribblings/apollo.scrbl" ())))
(define version "0.1.18")
(define pkg-desc "A Racket to Luau compiler for Roblox game development")
(define pkg-authors '("apollo-developers"))

;; Define collection links and hierarchy with absolute paths
(define collection-links
  '(("apollo" ".")
    ("apollo/compiler" "src/apollo/compiler")
    ("apollo/rojo" "src/apollo/rojo")
    ("apollo/std" "src/apollo/std")
    ("apollo/dsls" "src/apollo/dsls")
    ("apollo/ecs" "src/apollo/ecs")
    ("apollo/scribblings" "src/apollo/scribblings")))

;; Define the collection hierarchy with absolute paths
(define collection-hierarchy
  '(("apollo" ".")
    ("apollo/compiler" "src/apollo/compiler")
    ("apollo/rojo" "src/apollo/rojo")
    ("apollo/std" "src/apollo/std")
    ("apollo/dsls" "src/apollo/dsls")
    ("apollo/ecs" "src/apollo/ecs")
    ("apollo/scribblings" "src/apollo/scribblings")))

;; Define the raco commands with correct paths
(define raco-commands
  '(("apollo" (submod "src/apollo/cmd/apollo/main" main) 
             "Apollo Racket->Luau Compiler"
             #f)
    ("apollo-rojo" (submod "src/apollo/cmd/apollo-rojo/main" main)
                  "Apollo Rojo Integration"
                  #f))) 