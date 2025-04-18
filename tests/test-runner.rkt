#lang racket/base

(require rackunit
         rackunit/text-ui
         ;; Core compiler tests
         "apollo/compiler-test.rkt"
         "apollo/ir-test.rkt"
         "apollo/parser-test.rkt"
         ;; Luau generation tests
         "apollo/luau-test.rkt"
         "apollo/luau-ast-test.rkt"
         "apollo/luau-pretty-print-test.rkt"
         ;; Feature-specific tests
         "apollo/struct-test.rkt"
         ;; Integration tests
         "apollo/e2e-test.rkt"
         ;; Pattern matching tests
         "apollo/compiler/quasiquote-pattern-ir-test.rkt"
         "apollo/compiler/quasiquote-pattern-parser-test.rkt"
         "apollo/compiler/quasiquote-pattern-full-flow-test.rkt")

;; Group test suites by functionality
(define core-tests
  (test-suite 
   "Core Compiler Tests"
   compiler-test-suite
   ir-tests
   parser-tests))

(define luau-tests-suite
  (test-suite
   "Luau Generation Tests"
   luau-tests
   luau-ast-tests
   luau-pretty-print-tests))

(define feature-tests
  (test-suite
   "Feature-Specific Tests"
   struct-tests))

(define pattern-matching-tests
  (test-suite
   "Pattern Matching Tests"
   quasiquote-pattern-ir-tests
   quasiquote-pattern-parser-tests
   quasiquote-pattern-full-flow-tests))

(define integration-tests
  (test-suite
   "Integration Tests"
   e2e-tests))

;; Combined test suite
(define all-tests
  (test-suite
   "All Apollo Tests"
   core-tests
   luau-tests-suite
   feature-tests
   pattern-matching-tests
   integration-tests))

;; Run all tests when this module is run directly
(module+ main
  (run-tests all-tests)) 