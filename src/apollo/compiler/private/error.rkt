#lang racket/base

(provide (all-defined-out))

(define (error* who fmt . args)
  (raise (make-exn:fail
          (format "~a: ~a" who (apply format fmt args))
          (current-continuation-marks))))

(define (syntax-error stx fmt . args)
  (raise-syntax-error
   #f
   (apply format fmt args)
   stx)) 