#lang racket/base

(require racket/require
         (prefix-in compiler: "compiler/main.rkt")
         (prefix-in rojo: "rojo/main.rkt")
         (prefix-in std: "std/main.rkt"))

(provide (all-from-out "compiler/main.rkt")
         (all-from-out "rojo/main.rkt")
         (all-from-out "std/main.rkt"))

;; Re-export commonly used functions
(provide (rename-out [compiler:compile-to-luau compile-to-luau]
                    [rojo:compile-rojo-project compile-rojo-project]
                    [std:get-service get-service])) 