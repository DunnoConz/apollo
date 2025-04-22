#lang info

(define collection 'apollo/compiler)
(define deps '("base"))
(define compile-omit-paths '("tests" "private"))
(define test-omit-paths '("private"))

;; Define the modules that should be available
(define module-suffixes '(".rkt"))

;; Define the main module
(define main-module "main.rkt")

;; Define the version
(define version "0.1") 