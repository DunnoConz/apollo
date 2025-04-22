#lang racket/base

(require racket/require
         racket/contract
         setup/getinfo
         (prefix-in compiler: "./src/apollo/compiler/main.rkt")
         (prefix-in rojo: "./src/apollo/rojo/main.rkt")
         (prefix-in std: "./src/apollo/std/main.rkt"))

;; Get version information
(define info (get-info/full "." #:namespace '(version)))
(define version (info 'version))

(provide version
         (all-from-out "./src/apollo/compiler/main.rkt")
         (all-from-out "./src/apollo/rojo/main.rkt")
         (all-from-out "./src/apollo/std/main.rkt"))

;; Re-export commonly used functions with documentation
(provide 
 (contract-out
  [compile-to-luau
   (->* (path-string?)  ; required argument: input file
        (#:output path-string?  ; optional argument: output file
         #:optimize? boolean?)  ; optional argument: enable optimizations
        void?)]
  [compile-rojo-project
   (->* (path-string?)  ; required argument: project directory
        (#:output path-string?)  ; optional argument: output directory
        void?)]
  [get-service
   (-> string? any/c)]))  ; service name -> service instance

(define compile-to-luau compiler:compile-to-luau)
(define compile-rojo-project rojo:compile-rojo-project)
(define get-service std:get-service) 