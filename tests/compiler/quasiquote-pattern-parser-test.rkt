#lang racket/base

(require rackunit
         rackunit/text-ui
         ;; Use package paths
         apollo/compiler/parser
         (submod apollo/compiler/ir ir))

(provide quasiquote-pattern-parser-tests)

;; Test suite for quasiquote pattern parser functionality
(define quasiquote-pattern-parser-tests
  (test-suite
   "Quasiquote Pattern Parser Tests"

   (test-case "Parse backtick quasiquote notation"
     (let* ([pattern '`(point ,x ,y)]
            [parsed-pattern (parse-pattern pattern)])
       ;; Check the pattern was parsed correctly
       (check-true (ir:ir-pat-quasiquote? parsed-pattern)
                   "Should be a quasiquote pattern")
                   
       (let ([inner (ir:ir-pat-quasiquote-pattern parsed-pattern)])
         (check-true (ir:ir-pat-list? inner)
                     "Inner pattern should be a list")
                     
         (let ([elements (ir:ir-pat-list-elements inner)])
           (check-equal? (length elements) 3
                         "List should have 3 elements")
                         
           (check-true (ir:ir-pat-literal? (first elements))
                       "First element should be a literal")
           (check-true (ir:ir-pat-unquote? (second elements))
                       "Second element should be an unquote")
           (check-true (ir:ir-pat-unquote? (third elements))
                       "Third element should be an unquote")))))

   (test-case "Parse explicit quasiquote/unquote notation"
     (let* ([pattern '(quasiquote (point (unquote x) (unquote y)))]
            [parsed-pattern (parse-pattern pattern)])
       ;; Check the pattern was parsed correctly
       (check-true (ir:ir-pat-quasiquote? parsed-pattern)
                   "Should be a quasiquote pattern")
                   
       (let ([inner (ir:ir-pat-quasiquote-pattern parsed-pattern)])
         (check-true (ir:ir-pat-list? inner)
                     "Inner pattern should be a list")
                     
         (let ([elements (ir:ir-pat-list-elements inner)])
           (check-equal? (length elements) 3
                         "List should have 3 elements")
                         
           (check-true (ir:ir-pat-literal? (first elements))
                       "First element should be a literal")
           (check-true (ir:ir-pat-unquote? (second elements))
                       "Second element should be an unquote")
           (check-true (ir:ir-pat-unquote? (third elements))
                       "Third element should be an unquote")))))

   (test-case "Compare backtick and explicit quasiquote notation"
     (let* ([backtick-pattern '`(point ,x ,y)]
            [explicit-pattern '(quasiquote (point (unquote x) (unquote y)))]
            [parsed-backtick (parse-pattern backtick-pattern)]
            [parsed-explicit (parse-pattern explicit-pattern)])
       ;; Both forms should generate the same IR
       (check-equal? (ir:pattern->datum parsed-backtick)
                     (ir:pattern->datum parsed-explicit)
                     "Backtick and explicit forms should generate equivalent IR")))

   (test-case "Parse nested quasiquote pattern"
     (let* ([pattern '``(nested ,,x)]
            [parsed-pattern (parse-pattern pattern)])
       ;; Check the pattern was parsed correctly
       (check-true (ir:ir-pat-quasiquote? parsed-pattern)
                   "Should be a quasiquote pattern")
                   
       (let ([inner (ir:ir-pat-quasiquote-pattern parsed-pattern)])
         (check-true (ir:ir-pat-list? inner)
                     "Inner pattern should be a list"))))))

;; Run tests directly when script is executed standalone
(module+ main
  (run-tests quasiquote-pattern-parser-tests)) 