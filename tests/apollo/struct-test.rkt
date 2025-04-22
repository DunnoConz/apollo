#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/string
         ;; Use relative paths
         "../../src/apollo/compiler/parser.rkt"
         (submod "../../src/apollo/compiler/ir.rkt" ir)
         "../../src/apollo/compiler/codegen.rkt")

(provide struct-tests)

;; Helper function to compile Racket string to Luau
(define (compile-racket-string-to-luau str)
  (let* ([ast (parse-racket-string str)]
         [ir (convert-to-ir ast)])
    (ir->luau ir)))

;; Test cases for the struct handling
(define struct-tests
  (test-suite
   "Struct Handling Tests"
   
   ;; Test simple struct definition
   (test-case "Basic struct definition"
     (let ([luau-code (compile-racket-string-to-luau
                       "(struct point (x y))")])
       (check-true (string-contains? luau-code "local function make-point(x, y)"))
       (check-true (string-contains? luau-code "local function point?(v)"))
       (check-true (string-contains? luau-code "local function point-x(v)"))
       (check-true (string-contains? luau-code "local function point-y(v)"))))
   
   ;; Test struct constructor
   (test-case "Struct constructor usage"
     (let ([luau-code (compile-racket-string-to-luau
                       "(struct point (x y))
                        (define p (point 10 20))")])
       (check-true (string-contains? luau-code "local p = make-point(10, 20)"))))
   
   ;; Test struct accessors
   (test-case "Struct accessor usage"
     (let ([luau-code (compile-racket-string-to-luau
                       "(struct point (x y))
                        (define p (point 10 20))
                        (define x-val (point-x p))
                        (define y-val (point-y p))")])
       (check-true (string-contains? luau-code "local x_val = point-x(p)"))
       (check-true (string-contains? luau-code "local y_val = point-y(p)"))))
   
   ;; Test struct predicate
   (test-case "Struct predicate usage"
     (let ([luau-code (compile-racket-string-to-luau
                       "(struct point (x y))
                        (define p (point 10 20))
                        (define is-point (point? p))")])
       (check-true (string-contains? luau-code "local is_point = point?(p)"))))
   
   ;; Test struct with auto fields
   (test-case "Struct with auto fields"
     (let ([luau-code (compile-racket-string-to-luau
                       "(struct point (x y) #:auto [color \"black\"])")])
       (check-true (string-contains? luau-code "local function point-color(v)"))
       ))
   
   ;; Test nested struct usage
   (test-case "Nested struct usage"
     (let ([luau-code (compile-racket-string-to-luau
                       "(struct point (x y))
                        (struct line (p1 p2))
                        (define l (line (point 0 0) (point 10 10)))")])
       (check-true (string-contains? luau-code "local l = make-line(make-point(0, 0), make-point(10, 10))"))))
   
   ;; Test usage in functions
   (test-case "Struct usage in functions"
     (let ([luau-code (compile-racket-string-to-luau
                       "(struct point (x y))
                        (define (distance p1 p2)
                          (let ([dx (- (point-x p2) (point-x p1))]
                                [dy (- (point-y p2) (point-y p1))])
                            (sqrt (+ (* dx dx) (* dy dy)))))")])
       (check-true (string-contains? luau-code "local function distance(p1, p2)"))
       (check-true (string-contains? luau-code "local dx = (point-x(p2) - point-x(p1))"))
       (check-true (string-contains? luau-code "local dy = (point-y(p2) - point-y(p1))"))
       (check-true (string-contains? luau-code "return math.sqrt(((dx * dx) + (dy * dy)))")))))) 