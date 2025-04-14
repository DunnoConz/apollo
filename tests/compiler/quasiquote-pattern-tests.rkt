#lang racket

(require rackunit
         rackunit/text-ui
         "quasiquote-pattern-ir-test.rkt"
         "quasiquote-pattern-parser-test.rkt")

;; Combine all quasiquote pattern tests
(define quasiquote-pattern-tests
  (test-suite
   "All Quasiquote Pattern Tests"
   quasiquote-pattern-ir-tests
   quasiquote-pattern-parser-tests))

;; Run tests directly when script is executed standalone
(module+ main
  (run-tests quasiquote-pattern-tests))

;; Provide the test suite for use in other test runners
(provide quasiquote-pattern-tests) 