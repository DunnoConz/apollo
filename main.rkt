#lang racket/base

(require racket/require
         (prefix-in compiler: apollo/compiler/main)
         (prefix-in rojo: apollo/rojo/main)
         (prefix-in std: apollo/std/main))

(provide (all-from-out apollo/compiler/main)
         (all-from-out apollo/rojo/main)
         (all-from-out apollo/std/main))

;; Re-export commonly used functions
(provide (rename-out [compiler:compile-to-luau compile-to-luau]
                    [rojo:compile-rojo-project compile-rojo-project]
                    [std:get-service get-service])) 