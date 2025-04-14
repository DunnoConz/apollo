#lang racket/base
(require rackunit
         apollo/compiler/parser)

(provide parser-tests)

(define parser-tests
  (test-suite
   "Parser Tests"

   (test-case "Parse simple literal"
     (check-equal? (parse-racket-string "42")
                  `(module default racket/base 42)
                  "Should parse a number literal into a module"))

   (test-case "Parse simple variable reference"
     (check-equal? (parse-racket-string "x")
                  `(module default racket/base x)
                  "Should parse a variable reference into a module"))

   (test-case "Parse function definition"
     (check-match (parse-racket-string "(define (add x y) (+ x y))")
                 '(module default racket/base (define (add x y) (+ x y)))
                 "Should parse a function definition into a module"))
   
   (test-case "Parse function application"
     (check-match (parse-racket-string "(+ 1 2)")
                 '(module default racket/base (+ 1 2))
                 "Should parse a function application into a module"))
   
   (test-case "Parse conditional expression"
     (check-match (parse-racket-string "(if (> x 0) 1 0)")
                 '(module default racket/base (if (> x 0) 1 0))
                 "Should parse an if expression into a module"))
   
   (test-case "Parse let expression"
     (check-match (parse-racket-string "(let ([x 1] [y 2]) (+ x y))")
                 '(module default racket/base (let ([x 1] [y 2]) (+ x y)))
                 "Should parse a let expression into a module"))
)) 