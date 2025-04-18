#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/list
         ;; Use relative paths from test directory
         "../../../src/apollo/compiler/parser.rkt"
         (submod "../../../src/apollo/compiler/ir.rkt" ir)
         "../../../src/apollo/compiler/ir-types.rkt")

(provide quasiquote-pattern-ir-tests)

;; Test suite for quasiquote pattern IR functionality
(define quasiquote-pattern-ir-tests
  (test-suite
   "Quasiquote Pattern IR Tests"

   (test-case "Create and verify simple quasiquote pattern"
     (let* ([literal-pattern (ir-pat-literal 'point)]
            [var-pattern1 (ir-pat-var 'x)]
            [var-pattern2 (ir-pat-var 'y)]
            [quasiquote-pattern (ir-pat-quasiquote
                                 (ir-pat-list
                                  (list 
                                   literal-pattern
                                   (ir-pat-unquote var-pattern1)
                                   (ir-pat-unquote var-pattern2))
                                  #f))])
       ;; Check the structure is correct
       (check-true (ir-pat-quasiquote? quasiquote-pattern)
                   "Should be a quasiquote pattern")
       
       (let ([inner-pattern (ir-pat-quasiquote-pattern quasiquote-pattern)])
         (check-true (ir-pat-list? inner-pattern)
                     "Inner pattern should be a list")
         
         (let ([elements (ir-pat-list-elements inner-pattern)])
           (check-equal? (length elements) 3
                         "List should have 3 elements")
           
           (check-true (ir-pat-literal? (first elements))
                       "First element should be a literal")
           (check-true (ir-pat-unquote? (second elements))
                       "Second element should be an unquote")
           (check-true (ir-pat-unquote? (third elements))
                       "Third element should be an unquote")))))

   (test-case "Create and verify nested quasiquote pattern"
     (let* ([var-pattern (ir-pat-var 'x)]
            [nested-quasiquote-pattern (ir-pat-quasiquote
                                        (ir-pat-list
                                         (list
                                          (ir-pat-literal 'nested)
                                          (ir-pat-unquote
                                           (ir-pat-unquote var-pattern)))
                                         #f))])
       ;; Check the structure is correct
       (check-true (ir-pat-quasiquote? nested-quasiquote-pattern)
                   "Should be a quasiquote pattern")
       
       (let ([inner-pattern (ir-pat-quasiquote-pattern nested-quasiquote-pattern)])
         (check-true (ir-pat-list? inner-pattern)
                     "Inner pattern should be a list")
         
         (let ([elements (ir-pat-list-elements inner-pattern)])
           (check-equal? (length elements) 2
                         "List should have 2 elements")
           
           (check-true (ir-pat-literal? (first elements))
                       "First element should be a literal")
           (check-true (ir-pat-unquote? (second elements))
                       "Second element should be an unquote")))))))

;; Run tests directly when script is executed standalone
(module+ main
  (run-tests quasiquote-pattern-ir-tests)) 