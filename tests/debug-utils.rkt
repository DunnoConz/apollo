#lang racket

(provide debug-app
         debug-ir
         debug-nested-module
         debug-plus
         debug-app-direct)

;; Import necessary modules
(require "../src/compiler/ir.rkt"
         "../src/compiler/expand.rkt"
         "../src/compiler/compile.rkt"
         "../src/compiler/module.rkt"
         "../src/compiler/app.rkt"
         rackunit)

;; Debug utilities for application debugging
(define (debug-app expr)
  (displayln "Debugging application expression:")
  (pretty-print expr)
  (let ([expanded (expand-syntax expr)])
    (displayln "\nExpanded form:")
    (pretty-print expanded)
    (let ([ir (syntax->ir expanded)])
      (displayln "\nIR form:")
      (pretty-print ir))))

;; Debug utilities for IR
(define (debug-ir expr)
  (displayln "Debugging IR generation:")
  (let ([expanded (expand-syntax expr)])
    (displayln "\nExpanded form:")
    (pretty-print expanded)
    (let ([ir (syntax->ir expanded)])
      (displayln "\nIR form:")
      (pretty-print ir))))

;; Debug utilities for nested modules
(define (debug-nested-module expr)
  (displayln "Debugging nested module:")
  (pretty-print expr)
  (let ([expanded (expand-syntax expr)])
    (displayln "\nExpanded form:")
    (pretty-print expanded)))

;; Debug utilities for plus operations
(define (debug-plus expr)
  (displayln "Debugging plus operation:")
  (pretty-print expr)
  (let ([expanded (expand-syntax expr)])
    (displayln "\nExpanded form:")
    (pretty-print expanded)
    (let ([ir (syntax->ir expanded)])
      (displayln "\nIR form:")
      (pretty-print ir))))

;; Direct application debugging
(define (debug-app-direct expr)
  (displayln "Direct application debugging:")
  (pretty-print expr)
  (let ([expanded (expand-syntax expr)])
    (displayln "\nExpanded form:")
    (pretty-print expanded))) 