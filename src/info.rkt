#lang info

(define collection "apollo")
(define collection-path 'same)
(define deps '("base" "core"))
(define build-deps '("rackunit" "scribble-lib" "setup" "info"))
(define scribblings '(("scribblings/apollo.scrbl" ())))
(define pkg-desc "Apollo: A Racket to Luau Compiler")
(define version "0.1")
(define pkg-authors '("Apollo Team"))
(define license '(MIT)) 