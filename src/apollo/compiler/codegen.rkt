#lang racket/base

(require racket/match
         racket/list
         "./types.rkt"
         (prefix-in ir: (submod "./ir.rkt" ir))
         racket/string)

(provide ir->luau luau-ast->string)

(struct luau-quasiquote (pattern) #:prefab)
(struct luau-unquote (pattern) #:prefab)

(define (ir->luau node)
  (match node
    [(ir:ir-literal value)
     (cond
       [(number? value) value]
       [(string? value) (format "~s" value)]
       [(boolean? value) (if value "true" "false")]
       [(null? value) "{}"]
       [else (format "nil -- unsupported literal: ~a" value)])]
    
    [(ir:ir-var-ref name) 
     (let ([name-str (symbol->string name)])
       (cond
         [(eq? name '+) "+"]
         [(eq? name '-) "-"]
         [(eq? name '*) "*"]
         [(eq? name '/) "/"]
         [(eq? name 'expt) "^"]
         [(eq? name 'sqrt) "math.sqrt"]
         [(eq? name 'floor) "math.floor"]
         [(eq? name 'ceiling) "math.ceil"]
         [(eq? name 'abs) "math.abs"]
         [(eq? name 'min) "math.min"]
         [(eq? name 'max) "math.max"]
         [(eq? name 'pi) "math.pi"]
         [(eq? name 'display) "print"]
         [(eq? name 'newline) "print"]
         [(eq? name 'first) "table.first"]
         [(eq? name 'rest) 
          "(function(t) local result = {}; for i=2,#t do result[i-1] = t[i] end; return result; end)"]
         [else name-str]))]
    
    [(ir:ir-lambda formals body)
     (let* ([param-names (map symbol->string formals)]
            [params-str (string-join param-names ", ")]
            [body-exprs (map ir->luau body)]
            [body-str (string-join body-exprs "\n")])
       (format "function(~a)\n~a\nend" params-str body-str))]
    
    [(ir:ir-app func args)
     (let* ([func-luau (ir->luau func)]
            [args-strs (map (lambda (arg)
                              (let ([result (ir->luau arg)])
                                (if (string? result)
                                    result
                                    (format "~a" result))))
                          args)]
            [args-str (string-join args-strs ", ")])
       (cond
         [(member func-luau (list "+" "-" "*" "/" "^"))
          (cond
            [(and (equal? func-luau "-") (= (length args) 1))
             (format "-~a" (car args-strs))]
            [(= (length args) 2)
             (format "(~a ~a ~a)" 
                     (car args-strs)
                     func-luau
                     (cadr args-strs))]
            [(and (member func-luau (list "+" "*")) (> (length args) 2))
             (format "(%s)" 
                     (string-join args-strs (format " ~a " func-luau)))]
            [else
             (error 'ir->luau "Invalid number of arguments for arithmetic operator ~a" func-luau)])]
         
         [(member func-luau (list "math.sqrt" "math.floor" "math.ceil" "math.abs" 
                                 "math.min" "math.max"))
          (format "~a(~a)" func-luau args-str)]
         
         [(and (ir:ir-var-ref? func) (eq? (ir:ir-var-ref-name func) 'module))
          (let* ([module-name (if (> (length args) 0) 
                               (ir->luau (first args))
                               "unnamed_module")]
                 [module-lang (if (> (length args) 1)
                               (ir->luau (second args))
                               "racket/base")]
                 [module-body (if (> (length args) 2)
                               (map (lambda (expr) 
                                     (let ([result (ir->luau expr)])
                                       (if (string? result)
                                           result
                                           (luau-ast->string result))))
                                    (drop args 2))
                               '())]
                 [module-body-str (string-join module-body "\n\n")])
            (format "-- Module: ~a\n-- Language: ~a\n\n~a" module-name module-lang module-body-str))]
         
         [(and (ir:ir-var-ref? func) (eq? (ir:ir-var-ref-name func) 'define) 
               (>= (length args) 2) (ir:ir-app? (first args)))
          (let* ([func-app (first args)]
                 [func-name (ir->luau (ir:ir-app-func func-app))]
                 [func-params (map (lambda (param)
                                    (let ([param-str (ir->luau param)])
                                      (if (string? param-str)
                                          param-str
                                          (format "~a" param-str))))
                                  (ir:ir-app-args func-app))]
                 [func-params-str (string-join func-params ", ")]
                 [func-body (ir->luau (second args))]
                 [func-body-str (if (string? func-body)
                                   func-body
                                   (format "~a" func-body))])
            (format "function ~a(~a)\n  return ~a\nend" func-name func-params-str func-body-str))]
         
         [else
          (format "~a(~a)" func-luau args-str)]))]
    
    [(ir:ir-if test then else)
     (let* ([test-str (ir->luau test)]
            [then-str (ir->luau then)]
            [else-str (ir->luau else)])
       (format "if ~a then\n~a\nelse\n~a\nend" test-str then-str else-str))]
    
    [_ (format "-- Unsupported node type: ~a" (object-name node))]))

(define (luau-ast->string node)
  (cond
    [(luau-quasiquote? node)
     (format "`%s" (luau-ast->string (luau-quasiquote-pattern node)))]
    
    [(luau-unquote? node)
     (format "${%s}" (luau-ast->string (luau-unquote-pattern node)))]
    
    [(string? node) node]
    
    [else (format "~a" node)]))

(define (luau-node-type node)
  (cond
    [(luau-quasiquote? node) 'luau-quasiquote]
    [(luau-unquote? node) 'luau-unquote]
    [else (error 'luau-node-type "Unsupported node type: ~a" node)])) 