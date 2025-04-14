#lang racket/base

(require rackunit
         ;; Use package path
         apollo/compiler/parser
         (submod apollo/compiler/ir ir)
         apollo/compiler/codegen
         apollo/compiler/types)

(provide compiler-test-suite)

(define compiler-test-suite
  (test-suite
   "Apollo Compiler Tests"
   
   (test-case "parse-racket-string basic test"
     (let* ([actual-ast (parse-racket-string "(+ 1 2)")]
            ;; Convert the syntax object to a datum first
            [actual-datum (syntax->datum actual-ast)] 
            ; Extract body using list-ref (index 3)
            [actual-body-datum (list-ref actual-datum 3)])
       (check-equal? actual-body-datum '(+ 1 2))))
   
   (test-case "racket-to-ir for simple expression"
     (define ast #'(module #f racket/base (+ 1 2)))
     (define ir (ir:racket-to-ir ast))
     (check-true (ir:ir-program? ir)))
   
   (test-case "ir->luau basic functionality"
     (define ir (ir:ir-program (list (ir:ir-literal 42))))
     (define luau-ast (ir->luau ir))
     (check-true (luau:luau-module? luau-ast)))
   
   (test-case "ir->luau translates literals correctly"
      (check-equal? (ir->luau (ir:ir-literal 123)) "123")
      (check-equal? (ir->luau (ir:ir-literal "hello")) "\"hello\"")
      (check-equal? (ir->luau (ir:ir-literal #t)) "true")
      (check-equal? (ir->luau (ir:ir-literal #f)) "false")
      (check-equal? (ir->luau (ir:ir-literal '())) "{}")
      (check-equal? (ir->luau (ir:ir-literal 'some-symbol)) "nil -- unsupported literal: some-symbol") ; Assuming symbols aren't directly supported
   )
   
   (test-case "luau-ast->string basic functionality"
     (define luau-ast (luau:luau-module '() (list (luau:luau-literal 42)) '()))
     (define luau-str (luau-ast->string luau-ast))
     (check-true (string? luau-str))
     (check-true (> (string-length luau-str) 0)))))

;; Use provided names directly (no prefix needed)
(define racket-to-ir (dynamic-require '(submod apollo/compiler/ir ir) 'racket-to-ir))
(define luau-ast->string (dynamic-require 'apollo/compiler/codegen 'luau-ast->string))
(define ir->luau (dynamic-require 'apollo/compiler/codegen 'ir->luau))
(define parse-racket-string (dynamic-require 'apollo/compiler/parser 'parse-racket-string))

;; Test suite for the main compiler functions
(define compiler-tests
  (test-suite
   "Compiler Tests"
   
   ;; Test case 1: Simple definition
   (test-case "Simple define"
     (define racket-code "(define x 10)")
     (define luau-code (luau-ast->string (ir->luau (racket-to-ir (parse-racket-string racket-code)))))
     (check-match luau-code #rx"local x = 10"))

    ;; Test case 2: Simple function
    (test-case "Simple function"
     (define racket-code "(define (f x) (* x 2))")
     (define ir (racket-to-ir (parse-racket-string racket-code)))
     (define luau-ast (ir->luau ir))
     (define luau-code (luau-ast->string luau-ast))
     (check-match luau-code #rx"function f\\(x\\)")
     (check-match luau-code #rx"return x \\* 2"))

    ;; ... Add more specific tests based on features ...

   ))

;; Run the tests
(run-tests compiler-tests) 