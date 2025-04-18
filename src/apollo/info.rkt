#lang info

(define collection "apollo")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc"))

;; Define subcollections
(define compile-omit-paths '("private"))
(define test-omit-paths '("private"))
(define scribblings '(("scribblings/apollo.scrbl" ()))) 

;; Define collection structure
(define collection-links
  '((compiler "compiler")
    (rojo "rojo")
    (std "std")))

;; Define main module
(define main-module "main.rkt") 