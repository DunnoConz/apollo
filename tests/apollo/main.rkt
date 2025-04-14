#lang racket/base

(require rackunit)

;; Require test suites using relative paths
(require "./parser-test.rkt"
         "./ir-test.rkt"
         "./codegen-test.rkt" ; Assuming this exists
         "./luau-test.rkt"
         "./struct-test.rkt"
         "./e2e-test.rkt"
         ;; Add other relative paths here
         )

;; Run all imported test suites
(run-tests parser-tests)
(run-tests ir-tests)       ; Make sure ir-test.rkt provides ir-tests
(run-tests codegen-tests)  ; Make sure codegen-test.rkt provides codegen-tests
(run-tests luau-tests)     ; Make sure luau-test.rkt provides luau-tests
(run-tests struct-tests)
(run-tests e2e-tests) 