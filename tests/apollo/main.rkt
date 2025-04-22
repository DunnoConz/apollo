#lang racket/base

(require rackunit
         rackunit/text-ui)

;; Require test suites using relative paths
(require "./parser-test.rkt"
         "./ir-test.rkt"
         "./compiler-test.rkt"  ; Use compiler-test.rkt instead of codegen-test.rkt
         "./luau-test.rkt"
         "./struct-test.rkt"
         "./e2e-test.rkt"
         ;; Add other relative paths here
         )

;; Run all imported test suites
(run-tests parser-tests)
(run-tests ir-tests)       ; Make sure ir-test.rkt provides ir-tests
(run-tests compiler-tests) ; Run compiler tests instead of codegen tests
(run-tests luau-tests)     ; Make sure luau-test.rkt provides luau-tests
(run-tests struct-tests)
(run-tests e2e-tests) 