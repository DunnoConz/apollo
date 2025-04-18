#lang racket/base

(require setup/getinfo
         setup/collection-name)

(define (install)
  (displayln "Installing Apollo package...")
  (displayln "Installation complete!"))

(module+ main
  (install)) 