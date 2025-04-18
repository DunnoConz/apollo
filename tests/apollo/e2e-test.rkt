#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/string
         ;; Use relative paths
         "../../src/apollo/main.rkt")

;; Helper function to test a complete Racket to Luau compilation
(define (test-compilation racket-code expected-luau)
  (test-equal? 
   "Racket to Luau compilation"
   (string-trim (compile-racket-string-to-luau racket-code))
   (string-trim expected-luau)))

;; End-to-end test suite for the compiler
(define e2e-tests
  (test-suite
   "End-to-end Compiler Tests"
   
   (test-case "Simple arithmetic expression"
     (test-compilation "(+ 1 2)"
                       "return 1 + 2"))
   
   (test-case "Nested arithmetic expressions"
     (test-compilation "(* (+ 1 2) (- 4 3))"
                       "return (1 + 2) * (4 - 3)"))
   
   (test-case "Define function"
     (test-compilation "(define (add a b) (+ a b))"
                       "local function add(a, b)\n  return a + b\nend"))
   
   (test-case "If expression"
     (test-compilation "(if (> x 0) 1 0)"
                       "if x > 0 then\n  return 1\nelse\n  return 0\nend"))
   
   (test-case "Define variable"
     (test-compilation "(define x 10)"
                       "local x = 10"))
   
   (test-case "Lambda expression"
     (test-compilation "(lambda (x) x)"
                       "function(x)\n  return x\nend"))
   
   (test-case "Begin expression"
     (test-compilation "(begin (define x 1) (define y 2) (+ x y))"
                       "do\n  local x = 1\n  local y = 2\n  return x + y\nend"))
   
   (test-case "Let expression"
     (test-compilation "(let ([x 1][y 2]) (+ x y))"
                       "do\n  local x = 1\n  local y = 2\n  return x + y\nend"))))

(provide e2e-tests) 