#lang racket/base

(require racket/match
         racket/list
         racket/path
         "ir-types.rkt"
         (prefix-in ir: "./ir.rkt")
         "ctfe.rkt"
         racket/string)

(provide ir->luau luau-ast->string)

(struct luau-quasiquote (pattern) #:prefab)
(struct luau-unquote (pattern) #:prefab)

;; String caching for common values
(define string-cache (make-hash))
(define (cache-string key value)
  (hash-set! string-cache key value)
  value)

(define (get-cached-string key)
  (hash-ref string-cache key #f))

;; Symbol table for string interning
(define symbol-table (make-hash))
(define (intern-string str)
  (or (hash-ref symbol-table str #f)
      (let ([interned (string->immutable-string str)])
        (hash-set! symbol-table str interned)
        interned)))

;; Optimized string operations
(define (string-join* strs sep)
  (if (null? strs)
      ""
      (let loop ([acc (car strs)]
                 [rest (cdr strs)])
        (if (null? rest)
            acc
            (loop (string-append acc sep (car rest))
                  (cdr rest))))))

;; Optimized string escaping with caching
(define escape-cache (make-hash))
(define (escape-string str)
  (or (hash-ref escape-cache str #f)
      (let ([escaped (regexp-replace* #rx"[\"\\]" str "\\&")])
        (hash-set! escape-cache str escaped)
        escaped)))

;; Optimized function name conversion
(define fn-name-cache (make-hash))
(define (luau-function-name name)
  (or (hash-ref fn-name-cache name #f)
      (let ([converted (regexp-replace* #rx"-" (symbol->string name) "_")])
        (hash-set! fn-name-cache name converted)
        converted)))

;; Current module name for context
(define current-module-name (make-parameter ""))

;; Convert a function definition to Luau with proper formatting
(define (convert-function-def name formals kw-formals body)
  (let* ([func-name (luau-function-name name)]
         [param-names (map luau-function-name formals)]
         [params-str (string-join* param-names ", ")]
         [body-exprs (filter string? (map ir->luau body))]
         [kw-handling (if (null? kw-formals)
                         ""
                         (string-append
                          "\n    local args = {...}\n"
                          (string-join*
                           (map (lambda (kw-pair)
                                 (format "    local ~a = args[\"~a\"] or ~a"
                                         (luau-function-name (keyword->string (car kw-pair)))
                                         (keyword->string (car kw-pair))
                                         (ir->luau (cadr kw-pair))))
                               kw-formals)
                           "\n")
                          "\n"))]
         [body-str (string-join* body-exprs "\n    ")])
    (format "function ~a.~a(~a)~a    ~a\nend\n" 
            (current-module-name)
            func-name
            params-str 
            kw-handling
            body-str)))

;; Convert IR values to strings with caching
(define (ir-value->string value)
  (or (get-cached-string value)
      (let ([result
             (match value
               [(? number?) (number->string value)]
               [(? string?) (format "\"~a\"" (escape-string value))]
               [(? boolean?) (if value "true" "false")]
               ['null "nil"]
               [(? symbol?) (luau-function-name value)]
               [_ (error (format "Unsupported value type: ~a" value))])])
        (cache-string value result))))

;; Convert IR to Luau code
(define (ir->luau node)
  (match node
    [(ir:ir-ctfe expr)
     ;; Evaluate CTFE expression and convert result to Luau
     (let ([result (ctfe-eval expr (init-ctfe-env))])
       (ir-value->string result))]
    
    [(ir:ir-program modules)
     (let ([module-code (filter string? (map ir->luau modules))])
       (string-join module-code "\n\n"))]
    
    [(ir:ir-module name path body)
     (let* ([module-name (luau-function-name name)]
            [module-path (if path (path->string path) module-name)])
       (parameterize ([current-module-name module-name])
         (let* ([body-code (filter string? (map ir->luau body))]
                [body-str (string-join body-code "\n\n")])
           (format "local ~a = {}\n\n-- Module: ~a\n~a\n\nreturn ~a" 
                   module-name
                   module-path 
                   body-str 
                   module-name))))]
    
    [(ir:ir-literal value)
     (ir-value->string value)]
    
    [(ir:ir-var-ref name)
     (let ([name-str (luau-function-name name)])
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
    
    [(ir:ir-lambda formals kw-formals body)
     (let* ([param-names (map luau-function-name formals)]
            [params-str (string-join* param-names ", ")]
            [body-exprs (filter string? (map ir->luau body))]
            [kw-handling (if (null? kw-formals)
                           ""
                           (string-append
                            "\n  local args = {...}\n"
                            (string-join*
                             (map (lambda (kw-pair)
                                   (format "  local ~a = args[\"~a\"] or ~a"
                                           (luau-function-name (keyword->string (car kw-pair)))
                                           (keyword->string (car kw-pair))
                                           (ir->luau (cadr kw-pair))))
                                 kw-formals)
                             "\n")
                            "\n"))]
            [body-str (string-join* body-exprs "\n  ")])
       (format "function(~a)~a  ~a\nend" 
               params-str 
               kw-handling
               body-str))]
    
    [(ir:ir-app func args kw-args)
     (let* ([func-str (ir->luau func)]
            [args-str (filter string? (map ir->luau args))]
            [args-joined (string-join* args-str ", ")]
            [kw-str (if (null? kw-args)
                        ""
                        (string-join*
                         (map (lambda (kw-pair)
                               (format "[\"~a\"] = ~a"
                                       (keyword->string (car kw-pair))
                                       (ir->luau (cadr kw-pair))))
                             kw-args)
                         ", "))]
            [result (if (null? kw-str)
                        (format "~a(~a)" func-str args-joined)
                        (format "~a(~a, {~a})" 
                                func-str 
                                args-joined
                                kw-str))])
       result)]
    
    [(ir:ir-define name value)
     (cond
       [(ir:ir-lambda? value)
        (convert-function-def name 
                            (ir:ir-lambda-formals value)
                            (ir:ir-lambda-kw-formals value)
                            (ir:ir-lambda-body value))]
       [else
        (let* ([name-str (luau-function-name name)]
               [value-str (ir->luau value)])
          (format "~a.~a = ~a" (current-module-name) name-str value-str))])]
    
    [(ir:ir-var-set name value)
     (let ([name-str (luau-function-name name)]
           [value-str (ir->luau value)])
       (format "~a = ~a" name-str value-str))]
    
    [(ir:ir-if test then else)
     (let ([test-str (ir->luau test)]
           [then-str (ir->luau then)]
           [else-str (ir->luau else)])
       (format "if ~a then\n  ~a\nelse\n  ~a\nend" test-str then-str else-str))]
    
    [(ir:ir-begin exprs)
     (let ([exprs-str (filter string? (map ir->luau exprs))])
       (string-join* exprs-str "\n"))]
    
    [(ir:ir-let bindings body)
     (let* ([binding-strs (map (lambda (b) 
                                (format "local ~a = ~a" 
                                        (luau-function-name (car b))
                                        (ir->luau (cadr b))))
                              bindings)]
            [body-strs (filter string? (map ir->luau body))]
            [all-strs (append binding-strs body-strs)])
       (string-join* all-strs "\n"))]
    
    [(ir:ir-letrec bindings body)
     (let* ([binding-strs (map (lambda (b) 
                                (format "local ~a = ~a" 
                                        (luau-function-name (car b))
                                        (ir->luau (cadr b))))
                              bindings)]
            [body-strs (filter string? (map ir->luau body))]
            [all-strs (append binding-strs body-strs)])
       (string-join* all-strs "\n"))]
    
    [(ir:ir-cond clauses else-clause)
     (let* ([clause-strs (map (lambda (c)
                               (format "if ~a then\n  ~a\n"
                                       (ir->luau (car c))
                                       (ir->luau (cadr c))))
                             clauses)]
            [else-str (format "else\n  ~a\n" (ir->luau else-clause))]
            [all-strs (append clause-strs (list else-str))])
       (string-join* all-strs "\n"))]
    
    [(ir:ir-define-struct name fields)
     (let* ([name-str (luau-function-name name)]
            [field-strs (map luau-function-name fields)]
            [constructor (format "function ~a(~a)\n  local self = {}\n  ~a\n  return self\nend"
                               name-str
                               (string-join* field-strs ", ")
                               (string-join* (map (lambda (f) 
                                                 (format "self.~a = ~a" f f))
                                               field-strs)
                                          "\n  "))])
       constructor)]
    
    [(ir:ir-struct-ref instance index name)
     (let ([instance-str (ir->luau instance)]
           [name-str (luau-function-name name)])
       (format "~a.~a" instance-str name-str))]
    
    [(ir:ir-match target clauses)
     (let* ([target-str (ir->luau target)]
            [clause-strs (map (lambda (c)
                              (format "if ~a then\n  ~a\nend"
                                      (ir->luau (car c))
                                      (ir->luau (cadr c))))
                            clauses)])
       (string-join* (cons target-str clause-strs) "\n"))]
    
    [other (error 'ir->luau "Unsupported IR node: ~a" other)]))

(define (luau-ast->string ast)
  (if (string? ast)
      ast
      (error 'luau-ast->string "Expected string, got: ~a" ast)))

(define (luau-node-type node)
  (cond
    [(luau-quasiquote? node) 'luau-quasiquote]
    [(luau-unquote? node) 'luau-unquote]
    [else (error 'luau-node-type "Unsupported node type: ~a" node)])) 