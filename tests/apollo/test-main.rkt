#lang racket/base

(require rackunit
         rackunit/text-ui)

;; Require test suites using relative paths
(require "./luau-test.rkt"
         "./parser-test.rkt"
         "./ir-test.rkt"
         "./compiler-test.rkt"
         "./struct-test.rkt"
         "./e2e-test.rkt"
         "./lexer-test.rkt"
         "./parser-tools-test.rkt"
         "./ast-ir-test.rkt")

(define all-tests
  (test-suite
   "All Apollo Compiler Tests"
   luau-tests
   parser-tests
   ir-tests
   compiler-tests
   struct-tests
   e2e-tests
   lexer-tests
   parser-tools-tests
   ast-ir-tests))

(module+ main
  (run-tests all-tests))

(provide all-tests) 