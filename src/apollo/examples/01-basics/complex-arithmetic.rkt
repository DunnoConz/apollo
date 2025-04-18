#lang racket

;; Complex arithmetic operations example
;; Tests various arithmetic operations and math functions

;; Test basic arithmetic with complex expressions
(define (calculate-polynomial x)
  (+ (* 3 (expt x 2))
     (* -2 x)
     5))

;; Test math functions
(define (calculate-circle-area radius)
  (* pi (expt radius 2)))

;; Test nested arithmetic with multiple operators
(define (calculate-distance x1 y1 x2 y2)
  (sqrt (+ (expt (- x2 x1) 2)
           (expt (- y2 y1) 2))))

;; Test min/max and absolute values
(define (normalize-range value min-val max-val)
  (max min-val
       (min max-val value)))

;; Test unary minus and complex expressions
(define (quadratic-formula a b c)
  (let* ([discriminant (sqrt (- (expt b 2) 
                               (* 4 a c)))]
         [denominator (* 2 a)])
    (list (/ (+ (- b) discriminant) 
             denominator)
          (/ (- (- b) discriminant) 
             denominator))))

;; Test the functions
(display "Polynomial f(3) = 3x^2 - 2x + 5: ")
(display (calculate-polynomial 3))
(newline)

(display "Circle area (r=5): ")
(display (calculate-circle-area 5))
(newline)

(display "Distance between (1,1) and (4,5): ")
(display (calculate-distance 1 1 4 5))
(newline)

(display "Normalize 15 to range [0,10]: ")
(display (normalize-range 15 0 10))
(newline)

(display "Quadratic roots for x^2 + 5x + 6: ")
(display (quadratic-formula 1 5 6))
(newline) 