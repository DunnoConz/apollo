#lang racket/base

(require rackunit
         "../core/main.rkt"
         racket/runtime-path)

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
       (check-equal? (compile-staged input) expected)))
   
   (test-case "Constant folding optimization"
     (let ([input '(+ (* 2 3) (- 10 5))]
           [expected "11"])  ; Should be optimized at compile time
       (check-equal? (compile-staged input) expected)))
   
   (test-case "Function inlining"
     (let ([input '(let ([double (lambda (x) (* x 2))])
                    (double 5))]
           [expected "10"])  ; Should be inlined and constant-folded
       (check-equal? (compile-staged input) expected)))
   
   (test-case "Complex expression"
     (let ([input '(let ([make-adder (lambda (x)
                                      (lambda (y)
                                        (+ x y)))])
                    ((make-adder 5) 3))]
           [expected "8"])  ; Should be optimized
       (check-equal? (compile-staged input) expected)))
   
   (test-case "Multiple optimizations"
     (let ([input '(let ([square (lambda (x) (* x x))])
                    (+ (square 4) (square 3)))]
           [expected "25"])  ; Should be fully optimized
       (check-equal? (compile-staged input) expected)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests staged-tests)
  
  ;; Print cache statistics
  (define stats (get-compilation-stats))
  (printf "Cache hits: ~a\n" (cache-stats-hits stats))
  (printf "Cache misses: ~a\n" (cache-stats-misses stats))
  (printf "Cache size: ~a bytes\n" (cache-stats-total-size stats))) 