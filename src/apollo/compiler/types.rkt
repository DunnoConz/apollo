#lang racket/base

(provide (struct-out luau-literal)
         (struct-out luau-var)
         luau-literal?
         luau-var?)

;; Luau AST node types
(struct luau-literal (val) #:transparent)
(struct luau-var (name) #:transparent) 