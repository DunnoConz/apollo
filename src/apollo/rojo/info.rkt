#lang info

(define collection 'apollo/rojo)
(define deps '("base"))
(define compile-omit-paths '("tests" "private"))
(define test-omit-paths '("private"))

;; Define main module for rojo integration
(define main-module "integration.rkt") 