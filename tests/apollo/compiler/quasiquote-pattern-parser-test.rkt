#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/list
         ;; Use relative paths from test directory
         "../../../src/apollo/compiler/parser.rkt"
         (submod "../../../src/apollo/compiler/ir.rkt" ir)
         "../../../src/apollo/compiler/ir-types.rkt")

(provide quasiquote-pattern-parser-tests)

;; Test suite for quasiquote pattern parser functionality
(define quasiquote-pattern-parser-tests
  (test-suite
   "Quasiquote Pattern Parser Tests"

   (test-case "Parse backtick quasiquote notation"
     (let* ([pattern '`(point ,x ,y)]
            [parsed-pattern (parse-pattern pattern)])
       ;; Check the pattern was parsed correctly
       (check-true (ir-pat-quasiquote? parsed-pattern)
                   "Should be a quasiquote pattern")
                   
       (let ([inner (ir-pat-quasiquote-pattern parsed-pattern)])
         (check-true (ir-pat-list? inner)
                     "Inner pattern should be a list")
                     
         (let ([elements (ir-pat-list-elements inner)])
           (check-equal? (length elements) 3
                         "List should have 3 elements")
                         
           (check-true (ir-pat-literal? (first elements))
                       "First element should be a literal")
           (check-true (ir-pat-unquote? (second elements))
                       "Second element should be an unquote")
           (check-true (ir-pat-unquote? (third elements))
                       "Third element should be an unquote")))))

   (test-case "Parse explicit quasiquote/unquote notation"
     (let* ([pattern '(quasiquote (point (unquote x) (unquote y)))]
            [parsed-pattern (parse-pattern pattern)])
       ;; Check the pattern was parsed correctly
       (check-true (ir-pat-quasiquote? parsed-pattern)
                   "Should be a quasiquote pattern")
                   
       (let ([inner (ir-pat-quasiquote-pattern parsed-pattern)])
         (check-true (ir-pat-list? inner)
                     "Inner pattern should be a list")
                     
         (let ([elements (ir-pat-list-elements inner)])
           (check-equal? (length elements) 3
                         "List should have 3 elements")
                         
           (check-true (ir-pat-literal? (first elements))
                       "First element should be a literal")
           (check-true (ir-pat-unquote? (second elements))
                       "Second element should be an unquote")
           (check-true (ir-pat-unquote? (third elements))
                       "Third element should be an unquote")))))

   (test-case "Compare backtick and explicit quasiquote notation"
     (let* ([backtick-pattern '`(point ,x ,y)]
            [explicit-pattern '(quasiquote (point (unquote x) (unquote y)))]
            [parsed-backtick (parse-pattern backtick-pattern)]
            [parsed-explicit (parse-pattern explicit-pattern)])
       ;; Both forms should generate the same IR
       (check-true (ir-pat-quasiquote? parsed-backtick)
                   "Backtick form should be a quasiquote pattern")
       (check-true (ir-pat-quasiquote? parsed-explicit)
                   "Explicit form should be a quasiquote pattern")
       (let ([inner-backtick (ir-pat-quasiquote-pattern parsed-backtick)]
             [inner-explicit (ir-pat-quasiquote-pattern parsed-explicit)])
         (check-true (ir-pat-list? inner-backtick)
                     "Backtick inner pattern should be a list")
         (check-true (ir-pat-list? inner-explicit)
                     "Explicit inner pattern should be a list")
         (let ([elements-backtick (ir-pat-list-elements inner-backtick)]
               [elements-explicit (ir-pat-list-elements inner-explicit)])
           (check-equal? (length elements-backtick) (length elements-explicit)
                         "Both forms should have the same number of elements")
           (for ([b elements-backtick]
                 [e elements-explicit])
             (check-equal? (ir-pat-literal? b) (ir-pat-literal? e)
                          "Corresponding elements should have the same type")
             (check-equal? (ir-pat-unquote? b) (ir-pat-unquote? e)
                          "Corresponding elements should have the same type"))))))

   (test-case "Parse nested quasiquote pattern"
     (let* ([pattern '``(nested ,,x)]
            [parsed-pattern (parse-pattern pattern)])
       ;; Check the pattern was parsed correctly
       (check-true (ir-pat-quasiquote? parsed-pattern)
                   "Should be a quasiquote pattern")
                   
       (let ([inner (ir-pat-quasiquote-pattern parsed-pattern)])
         (check-true (ir-pat-list? inner)
                     "Inner pattern should be a list"))))))

;; Run tests directly when script is executed standalone
(module+ main
  (run-tests quasiquote-pattern-parser-tests)) 