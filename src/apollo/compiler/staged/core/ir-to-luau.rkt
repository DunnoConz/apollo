#lang racket/base

(require "../../ir-types.rkt"
         "../../luau-ast.rkt"
         racket/match
         racket/list
         racket/string)

(provide ir->staged-luau
         staged-luau->ast
         staged-luau-expr?
         staged-function?
         staged-function-name
         staged-function-params
         staged-function-body
         staged-call?
         staged-call-func
         staged-call-args
         staged-literal?
         staged-literal-value
         staged-var?
         staged-var-name)

;; Staged IR representation
(struct staged-luau-expr ()
  #:transparent)

(struct staged-function staged-luau-expr (name params body)
  #:transparent)

(struct staged-call staged-luau-expr (func args)
  #:transparent)

(struct staged-literal staged-luau-expr (value)
  #:transparent)

(struct staged-var staged-luau-expr (name)
  #:transparent)

;; Convert IR to staged Luau representation
(define (ir->staged-luau ir-expr)
  (match ir-expr
    [(ir-literal value)
     (staged-literal value)]
    
    [(ir-var-ref name)
     (staged-var name)]
    
    [(ir-lambda formals kw-formals body)
     (staged-function 
      'anonymous
      formals
      (map ir->staged-luau body))]
    
    [(ir-app func args kw-args)
     (staged-call
      (ir->staged-luau func)
      (map ir->staged-luau args))]
    
    [(ir-define name value)
     (staged-function
      name
      (if (ir-lambda? value)
          (ir-lambda-formals value)
          '())
      (if (ir-lambda? value)
          (map ir->staged-luau (ir-lambda-body value))
          (list (ir->staged-luau value))))]
    
    [_ (error 'ir->staged-luau "Unsupported IR expression: ~a" ir-expr)]))

;; Convert staged Luau to final Luau AST
(define (staged-luau->ast staged-expr)
  (match staged-expr
    [(staged-literal value)
     (cond
       [(number? value) (LuauNumberLiteral value)]
       [(string? value) (LuauStringLiteral value)]
       [(boolean? value) (LuauBooleanLiteral value)]
       [else (error 'staged-luau->ast "Unsupported literal type: ~a" value)])]
    
    [(staged-var name)
     (LuauIdentifier (symbol->string name))]
    
    [(staged-function name params body)
     (let ([body-ast (map staged-luau->ast body)])
       (if (eq? name 'anonymous)
           (LuauFunctionCall
            (LuauIdentifier "function")
            (cons (map (λ (p) (LuauIdentifier (symbol->string p))) params)
                  body-ast))
           (LuauAssignment
            (list (LuauIdentifier (symbol->string name)))
            (list (LuauFunctionCall
                  (LuauIdentifier "function")
                  (cons (map (λ (p) (LuauIdentifier (symbol->string p))) params)
                        body-ast))))))]
    
    [(staged-call func args)
     (LuauFunctionCall
      (staged-luau->ast func)
      (map staged-luau->ast args))]
    
    [_ (error 'staged-luau->ast "Unsupported staged expression: ~a" staged-expr)])) 