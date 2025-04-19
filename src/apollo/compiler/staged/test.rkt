#lang racket/base

(require rackunit
         "./main.rkt")

(define staged-tests
  (test-suite
   "Staged Compiler Tests"
   
   (test-case "Simple function definition"
     (let ([input '(define (add1 x) (+ x 1))]
           [expected "function add1(x)\n  return x + 1\nend"])
       (check-equal? (compile-staged input) expected)))
   
   (test-case "Simple lambda expression"
     (let ([input '(lambda (x) (* x 2))]
           [expected "function(x)\n  return x * 2\nend"])
       (check-equal? (compile-staged input) expected)))
   
   (test-case "Function call"
     (let ([input '(add1 5)]
           [expected "add1(5)"])
       (check-equal? (compile-staged input) expected)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests staged-tests)) 