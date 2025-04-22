#lang apollo/test-dsl

;; This should be converted to print("HELLO", "DSL")
(shout "hello" "dsl")

;; This uses standard Racket syntax, which our DSL expander
;; should delegate to the standard IR converter.
(define (add x y) (+ x y))

;; This should also be delegated
(add 5 10) 