#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/syntax
         ;; Use relative paths
         "../../src/apollo/compiler/parser.rkt"
         (submod "../../src/apollo/compiler/ir.rkt" ir)
         "../../src/apollo/compiler/codegen.rkt"
         "../../src/apollo/compiler/types.rkt"
         "../../src/apollo/compiler/ir-types.rkt"
         "../../src/apollo/compiler/luau-ast.rkt"
         "../../src/apollo/dsls/shout-dsl.rkt")

(provide compiler-test-suite compiler-tests)

(define (wrap-in-module stx)
  (datum->syntax #f `(module test-module racket/base ,stx)))

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
     (define ir (convert-to-ir ast))
     (check-true (ir-program? ir)))
   
   (test-case "ir->luau basic functionality"
     (define ir (ir-program (list (ir-literal 42))))
     (define luau-str (ir->luau ir))
     (check-true (string? luau-str))
     (check-true (> (string-length luau-str) 0)))
   
   (test-case "ir->luau translates literals correctly"
      (check-equal? (ir->luau (ir-literal 123)) "123")
      (check-equal? (ir->luau (ir-literal "hello")) "\"hello\"")
      (check-equal? (ir->luau (ir-literal #t)) "true")
      (check-equal? (ir->luau (ir-literal #f)) "false")
      (check-equal? (ir->luau (ir-literal '())) "{}")
      (check-equal? (ir->luau (ir-literal 'some-symbol)) "nil -- unsupported literal: some-symbol") ; Assuming symbols aren't directly supported
   )
   
   (test-case "luau-ast->string basic functionality"
     (define ir (ir-program (list (ir-literal 42))))
     (define luau-str (ir->luau ir))
     (check-true (string? luau-str))
     (check-true (> (string-length luau-str) 0)))))

;; Test suite for the main compiler functions
(define compiler-tests
  (test-suite
   "Compiler Tests"
   
   ;; Test case 1: Simple definition
   (test-case "Simple define"
     (define racket-code (wrap-in-module #'(define x 10)))
     (define luau-code (ir->luau (convert-to-ir racket-code)))
     (check-match luau-code #rx"local x = 10"))

    ;; Test case 2: Simple function
    (test-case "Simple function"
     (define racket-code (wrap-in-module #'(define (f x) (* x 2))))
     (define ir (convert-to-ir racket-code))
     (define luau-ast (ir->luau ir))
     (check-match luau-ast #rx"function f\\(x\\)")
     (check-match luau-ast #rx"return x \\* 2"))

    ;; ... Add more specific tests based on features ...

   ))

;; Run the tests
(run-tests compiler-tests) 