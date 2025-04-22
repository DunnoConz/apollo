#lang info

(define collection 'apollo/dsls)
(define deps '("base"))
(define compile-omit-paths '("tests" "private"))
(define test-omit-paths '("private"))

;; Define main module
(define main-module "main.rkt") 