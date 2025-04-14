#lang racket/base

(require rackunit
         "apollo/compiler-test.rkt"
         "apollo/ir-test.rkt"
         "apollo/luau-test.rkt"
         "apollo/e2e-test.rkt"
         "apollo/struct-test.rkt"
         "apollo/luau-ast-test.rkt"
         "apollo/luau-pretty-print-test.rkt"
         "compiler/quasiquote-pattern-tests.rkt"
         "compiler/quasiquote-pattern-ir-test.rkt"
         "compiler/quasiquote-pattern-codegen-test.rkt"
         "compiler/quasiquote-pattern-parser-test.rkt"
         "compiler/quasiquote-pattern-full-flow-test.rkt")

; Run all test suites (Commented out as raco test handles this)
;(run-tests compiler-test-suite)
;(run-tests ir-tests) ; Corrected names based on provided files
;(run-tests luau-tests)
;(run-tests e2e-tests)
;(run-tests struct-tests)
;(run-tests luau-ast-tests)
;(run-tests pretty-print-tests)
;(run-tests quasiquote-pattern-tests)

; You might need a test suite for fixed-compiler functionality if it exists
; (run-tests fixed-compiler-tests) ; Uncomment if applicable 

; You can optionally combine test suites here if needed,
; but raco test will run all provided tests individually.

; Example of combining (not strictly necessary with raco test):
; (define all-apollo-tests 
;   (test-suite "Apollo Tests" 
;     compiler-test-suite ir-tests luau-tests e2e-tests 
;     struct-tests luau-ast-tests pretty-print-tests))
; 
; (define all-tests
;   (test-suite "All Project Tests"
;     all-apollo-tests
;     quasiquote-pattern-tests)) ; Add the compiler tests
;
; (module+ main (run-tests all-tests)) 