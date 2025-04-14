#lang racket/base

;; Simple test input file

;; Define a function that creates a greeting
(define (greet name)
  (string-append "Hello, " name "!"))

;; Call the function
(greet "World") 