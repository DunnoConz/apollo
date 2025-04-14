#lang racket/base

;; math-utils.rkt
;; Math utilities for Roblox games

(provide distance lerp clamp)

;; Define a function to calculate the distance between two points
(define (distance x1 y1 z1 x2 y2 z2)
  (let ((dx (- x2 x1))
        (dy (- y2 y1))
        (dz (- z2 z1)))
    (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

;; Define a function to lerp between two values
(define (lerp a b t)
  (+ a (* (- b a) t)))

;; Define a function to clamp a value between min and max
(define (clamp value min-val max-val)
  (cond
    ((< value min-val) min-val)
    ((> value max-val) max-val)
    (else value))) 