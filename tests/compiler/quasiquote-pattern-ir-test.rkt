#lang racket/base

(require rackunit
         rackunit/text-ui
         ;; Use package paths
         apollo/compiler/parser
         (submod apollo/compiler/ir ir))

(provide quasiquote-pattern-ir-tests)

;; Test suite for quasiquote pattern IR functionality
(define quasiquote-pattern-ir-tests
  (test-suite
   "Quasiquote Pattern IR Tests"

   (test-case "Create and verify simple quasiquote pattern"
     (let* ([literal-pattern (ir:ir-pat-literal 'point)]
            [var-pattern1 (ir:ir-pat-var 'x)]
            [var-pattern2 (ir:ir-pat-var 'y)]
            [quasiquote-pattern (ir:ir-pat-quasiquote
                                 (ir:ir-pat-list
                                  (list 
                                   literal-pattern
                                   (ir:ir-pat-unquote var-pattern1)
                                   (ir:ir-pat-unquote var-pattern2))
                                  #f))])
       ;; Check the structure is correct
       (check-true (ir:ir-pat-quasiquote? quasiquote-pattern)
                   "Should be a quasiquote pattern")
       
       (let ([inner-pattern (ir:ir-pat-quasiquote-pattern quasiquote-pattern)])
         (check-true (ir:ir-pat-list? inner-pattern)
                     "Inner pattern should be a list")
         
         (let ([elements (ir:ir-pat-list-elements inner-pattern)])
           (check-equal? (length elements) 3
                         "List should have 3 elements")
           
           (check-true (ir:ir-pat-literal? (first elements))
                       "First element should be a literal")
           (check-true (ir:ir-pat-unquote? (second elements))
                       "Second element should be an unquote")
           (check-true (ir:ir-pat-unquote? (third elements))
                       "Third element should be an unquote")))))

   (test-case "Create and verify nested quasiquote pattern"
     (let* ([var-pattern (ir:ir-pat-var 'x)]
            [nested-quasiquote-pattern (ir:ir-pat-quasiquote
                                        (ir:ir-pat-list
                                         (list
                                          (ir:ir-pat-literal 'nested)
                                          (ir:ir-pat-unquote
                                           (ir:ir-pat-unquote var-pattern)))
                                         #f))])
       ;; Check the structure is correct
       (check-true (ir:ir-pat-quasiquote? nested-quasiquote-pattern)
                   "Should be a quasiquote pattern")
       
       (let ([inner-pattern (ir:ir-pat-quasiquote-pattern nested-quasiquote-pattern)])
         (check-true (ir:ir-pat-list? inner-pattern)
                     "Inner pattern should be a list")
         
         (let ([elements (ir:ir-pat-list-elements inner-pattern)])
           (check-equal? (length elements) 2
                         "List should have 2 elements")
           
           (check-true (ir:ir-pat-literal? (first elements))
                       "First element should be a literal")
           (check-true (ir:ir-pat-unquote? (second elements))
                       "Second element should be an unquote")))))))

;; Run tests directly when script is executed standalone
(module+ main
  (run-tests quasiquote-pattern-ir-tests)) 