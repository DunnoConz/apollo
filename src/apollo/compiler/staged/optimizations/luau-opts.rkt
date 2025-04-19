#lang racket/base

(require "../core/ir-to-luau.rkt"
         "../../luau-ast.rkt"
         racket/match
         racket/list)

(provide optimize-luau-ast
         opt-pass
         opt-pass-name
         opt-pass-fn)

;; Optimization passes
(struct opt-pass (name fn) #:transparent)

;; Constant folding optimization
(define (fold-constants ast)
  (match ast
    [(LuauBinaryOp op left right)
     (match* [(fold-constants left) (fold-constants right)]
       [((LuauNumberLiteral l) (LuauNumberLiteral r))
        (LuauNumberLiteral
         (case op
           ['+ (+ l r)]
           ['- (- l r)]
           ['* (* l r)]
           ['/ (/ l r)]
           [else ast]))]
       [(l r) (LuauBinaryOp op l r)])]
    
    [(LuauFunctionCall fn args)
     (LuauFunctionCall (fold-constants fn)
                      (map fold-constants args))]
    
    [(LuauAssignment targets values)
     (LuauAssignment (map fold-constants targets)
                    (map fold-constants values))]
    
    [other other]))

;; Function inlining for small functions
(define (should-inline? fn-ast)
  (match fn-ast
    [(LuauFunctionCall (LuauIdentifier "function") (list args body ...))
     (and (<= (length body) 1)  ; Single expression body
          (<= (length args) 2))] ; Max 2 parameters
    [_ #f]))

(define (inline-functions ast)
  (match ast
    [(LuauFunctionCall fn args)
     (if (should-inline? fn)
         (let ([params (car (LuauFunctionCall-args fn))]
               [body (cdr (LuauFunctionCall-args fn))])
           ;; Replace parameters with arguments in body
           (for/fold ([result (car body)])
                    ([param params]
                     [arg args])
             (replace-identifier result 
                               (LuauIdentifier-name param)
                               arg)))
         (LuauFunctionCall (inline-functions fn)
                          (map inline-functions args)))]
    
    [(LuauAssignment targets values)
     (LuauAssignment (map inline-functions targets)
                    (map inline-functions values))]
    
    [other other]))

;; Replace identifiers in AST
(define (replace-identifier ast old-name new-ast)
  (match ast
    [(LuauIdentifier name)
     (if (equal? name old-name)
         new-ast
         ast)]
    
    [(LuauFunctionCall fn args)
     (LuauFunctionCall (replace-identifier fn old-name new-ast)
                      (map (λ (a) (replace-identifier a old-name new-ast)) args))]
    
    [(LuauAssignment targets values)
     (LuauAssignment (map (λ (t) (replace-identifier t old-name new-ast)) targets)
                    (map (λ (v) (replace-identifier v old-name new-ast)) values))]
    
    [other other]))

;; List of optimization passes
(define optimization-passes
  (list
   (opt-pass "constant folding" fold-constants)
   (opt-pass "function inlining" inline-functions)))

;; Main optimization function
(define (optimize-luau-ast ast)
  (for/fold ([current-ast ast])
            ([pass optimization-passes])
    ((opt-pass-fn pass) current-ast))) 