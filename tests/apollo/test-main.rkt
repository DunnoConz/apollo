#lang racket/base

(require rackunit)

;; Require test suites using relative paths
(require "./luau-test.rkt"
         ; Add other relative paths here
         )

;; Run tests
(run-tests luau-tests)

(define all-tests
  (test-suite
   "All Apollo Compiler Tests"
   luau-tests
   e2e-tests))

(module+ main
  (run-tests all-tests))

(provide all-tests) 