#lang racket/base

(require racket/match
         racket/list
         racket/path
         "ir-types.rkt"
         (for-syntax racket/base)
         (prefix-in ir: (submod "./ir.rkt" ir))
         "ctfe.rkt"
         racket/string)

(provide ir->luau luau-ast->string ir-pattern->luau)

(struct luau-quasiquote (pattern) #:prefab)
(struct luau-unquote (pattern) #:prefab)

;; Data-oriented structures for code generation
(struct codegen-state (strings functions modules patterns) #:mutable)
(struct string-batch (values indices) #:mutable)
(struct function-batch (names params bodies) #:mutable)
(struct module-batch (names paths bodies) #:mutable)
(struct pattern-batch (values types) #:mutable)

;; Initialize code generation state
(define (make-codegen-state)
  (codegen-state (string-batch '() (make-hash))
                 (function-batch '() '() '())
                 (module-batch '() '() '())
                 (pattern-batch '() '())))

;; String interning with cache locality
(define (intern-string str state)
  (let ([string-batch (codegen-state-strings state)]
        [values (string-batch-values string-batch)]
        [indices (string-batch-indices string-batch)])
    (or (hash-ref indices str #f)
        (let ([idx (length values)])
          (set-string-batch-values! string-batch (cons str values))
          (hash-set! indices str idx)
          idx))))

;; Batch string operations
(define (batch-string-join strs sep state)
  (let ([string-batch (codegen-state-strings state)]
        [values (string-batch-values string-batch)])
    (if (null? strs)
        ""
        (let loop ([acc (list-ref values (intern-string (car strs) state))]
                   [rest (cdr strs)])
          (if (null? rest)
              acc
              (loop (string-append acc sep (list-ref values (intern-string (car rest) state)))
                    (cdr rest)))))))

;; Optimized function name conversion with batching
(define (batch-function-name name state)
  (let ([indices (string-batch-indices (codegen-state-strings state))])
    (or (hash-ref indices (symbol->string name) #f)
        (let ([converted (regexp-replace* #rx"-" (symbol->string name) "_")])
          (intern-string converted state)
          converted))))

;; Optimized string escaping with caching
(define escape-cache (make-hash))
(define (escape-string str)
  (or (hash-ref escape-cache str #f)
      (let ([escaped (regexp-replace* #rx"[\"\\]" str "\\&")])
        (hash-set! escape-cache str escaped)
        escaped)))

;; Current module name for context
(define current-module-name (make-parameter ""))

;; Convert IR to Luau code with DOD optimization
(define (ir->luau node [state (make-codegen-state)])
  (match node
    [(ir-ctfe expr)
     (let ([result (ctfe-eval expr (init-ctfe-env))])
       (ir-value->string result state))]
    
    [(ir-program modules)
     (let ([module-code (filter string? (map (lambda (m) (ir->luau m state)) modules))])
       (batch-string-join module-code "\n\n" state))]
    
    [(ir-module name path body)
     (let* ([module-name (batch-function-name name state)]
            [module-path (if path (path->string path) module-name)])
       (parameterize ([current-module-name module-name])
         (let* ([body-code (filter string? (map (lambda (b) (ir->luau b state)) body))]
                [body-str (batch-string-join body-code "\n\n" state)])
           (format "local ~a = {}\n\n-- Module: ~a\n~a\n\nreturn ~a" 
                   module-name
                   module-path 
                   body-str 
                   module-name))))]
    
    [(ir-literal value)
     (ir-value->string value state)]
    
    [(ir-var-ref name)
     (let ([name-str (batch-function-name name state)])
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
    
    [(ir-lambda formals kw-formals body)
     (let* ([param-names (map (lambda (f) (batch-function-name f state)) formals)]
            [params-str (batch-string-join param-names ", " state)]
            [body-exprs (filter string? (map (lambda (b) (ir->luau b state)) body))]
            [kw-handling (if (null? kw-formals)
                           ""
                           (string-append
                            "\n  local args = {...}\n"
                            (batch-string-join
                             (map (lambda (kw-pair)
                                   (format "  local ~a = args[\"~a\"] or ~a"
                                           (batch-function-name (keyword->string (car kw-pair)) state)
                                           (keyword->string (car kw-pair))
                                           (ir->luau (cadr kw-pair) state)))
                                 kw-formals)
                             "\n"
                             state)
                            "\n"))]
            [body-str (batch-string-join body-exprs "\n  " state)])
       (format "function(~a)~a  ~a\nend" 
               params-str 
               kw-handling
               body-str))]
    
    [(ir-app func args kw-args)
     (let* ([func-str (ir->luau func state)]
            [args-str (filter string? (map (lambda (a) (ir->luau a state)) args))]
            [args-joined (batch-string-join args-str ", " state)]
            [kw-str (if (null? kw-args)
                        ""
                        (batch-string-join
                         (map (lambda (kw-pair)
                               (format "[\"~a\"] = ~a"
                                       (keyword->string (car kw-pair))
                                       (ir->luau (cadr kw-pair) state)))
                             kw-args)
                         ", "
                         state))]
            [result (if (null? kw-str)
                        (format "~a(~a)" func-str args-joined)
                        (format "~a(~a, {~a})" 
                                func-str 
                                args-joined
                                kw-str))])
       result)]
    
    [(ir-define name value)
     (cond
       [(ir-lambda? value)
        (convert-function-def name 
                            (ir-lambda-formals value)
                            (ir-lambda-kw-formals value)
                            (ir-lambda-body value)
                            state)]
       [else
        (let* ([name-str (batch-function-name name state)]
               [value-str (ir->luau value state)])
          (format "~a.~a = ~a" (current-module-name) name-str value-str))])]
    
    [(ir-var-set name value)
     (let ([name-str (batch-function-name name state)]
           [value-str (ir->luau value state)])
       (format "~a = ~a" name-str value-str))]
    
    [(ir-if test then else)
     (let ([test-str (ir->luau test state)]
           [then-str (ir->luau then state)]
           [else-str (ir->luau else state)])
       (format "if ~a then\n  ~a\nelse\n  ~a\nend" test-str then-str else-str))]
    
    [(ir-begin exprs)
     (let ([exprs-str (filter string? (map (lambda (e) (ir->luau e state)) exprs))])
       (batch-string-join exprs-str "\n" state))]
    
    [(ir-let bindings body)
     (let* ([binding-strs (map (lambda (b) 
                                (format "local ~a = ~a" 
                                        (batch-function-name (car b) state)
                                        (ir->luau (cadr b) state)))
                              bindings)]
            [body-strs (filter string? (map (lambda (b) (ir->luau b state)) body))]
            [all-strs (append binding-strs body-strs)])
       (batch-string-join all-strs "\n" state))]
    
    [(ir-letrec bindings body)
     (let* ([binding-strs (map (lambda (b) 
                                (format "local ~a = ~a" 
                                        (batch-function-name (car b) state)
                                        (ir->luau (cadr b) state)))
                              bindings)]
            [body-strs (filter string? (map (lambda (b) (ir->luau b state)) body))]
            [all-strs (append binding-strs body-strs)])
       (batch-string-join all-strs "\n" state))]
    
    [(ir-cond clauses else-clause)
     (let* ([clause-strs (map (lambda (c)
                               (format "if ~a then\n  ~a\n"
                                       (ir->luau (car c) state)
                                       (ir->luau (cadr c) state)))
                             clauses)]
            [else-str (format "else\n  ~a\n" (ir->luau else-clause state))]
            [all-strs (append clause-strs (list else-str))])
       (batch-string-join all-strs "" state))]))

;; Convert IR values to strings with DOD optimization
(define (ir-value->string value state)
  (let ([string-batch (codegen-state-strings state)]
        [values (string-batch-values string-batch)]
        [indices (string-batch-indices string-batch)])
    (or (hash-ref indices value #f)
        (let ([result
               (match value
                 [(? number?) (number->string value)]
                 [(? string?) (format "\"~a\"" (escape-string value))]
                 [(? boolean?) (if value "true" "false")]
                 ['null "nil"]
                 [(? symbol?) (batch-function-name value state)]
                 [_ (error (format "Unsupported value type: ~a" value))])])
          (let ([idx (length values)])
            (set-string-batch-values! string-batch (cons result values))
            (hash-set! indices value idx)
            result)))))

;; Convert a function definition to Luau with proper formatting
(define (convert-function-def name formals kw-formals body state)
  (let* ([func-name (batch-function-name name state)]
         [param-names (map (lambda (f) (batch-function-name f state)) formals)]
         [params-str (batch-string-join param-names ", " state)]
         [body-exprs (filter string? (map (lambda (b) (ir->luau b state)) body))]
         [kw-handling (if (null? kw-formals)
                        ""
                        (string-append
                         "\n  local args = {...}\n"
                         (batch-string-join
                          (map (lambda (kw-pair)
                                (format "  local ~a = args[\"~a\"] or ~a"
                                        (batch-function-name (keyword->string (car kw-pair)) state)
                                        (keyword->string (car kw-pair))
                                        (ir->luau (cadr kw-pair) state)))
                              kw-formals)
                          "\n"
                          state)
                         "\n"))]
         [body-str (batch-string-join body-exprs "\n  " state)])
    (format "function ~a.~a(~a)~a  ~a\nend" 
            (current-module-name)
            func-name
            params-str 
            kw-handling
            body-str)))

;; Convert Luau AST to string
(define (luau-ast->string ast)
  (ir->luau ast))

(define (luau-node-type node)
  (cond
    [(luau-quasiquote? node) 'luau-quasiquote]
    [(luau-unquote? node) 'luau-unquote]
    [else (error 'luau-node-type "Unsupported node type: ~a" node)]))

;; Convert IR patterns to Luau code
(define (ir-pattern->luau ir)
  (match ir
    [(ir-pat-quasiquote pattern)
     (ir-pattern->luau pattern)]
    
    [(ir-pat-list elements tail)
     (let ([compiled-elements (map ir-pattern->luau elements)])
       (cond
         ;; Function definition
         [(and (>= (length compiled-elements) 3)
               (equal? (first compiled-elements) 'function))
          (format "local function ~a(~a)\n~a\nend"
                  (second compiled-elements)
                  (third compiled-elements)
                  (fourth compiled-elements))]
         
         ;; If expression
         [(and (>= (length compiled-elements) 4)
               (equal? (first compiled-elements) 'if))
          (format "if ~a then\n    return ~a\nelse\n    return ~a\nend"
                  (second compiled-elements)
                  (third compiled-elements)
                  (fourth compiled-elements))]
         
         ;; Binary operations
         [(and (= (length compiled-elements) 3)
               (member (first compiled-elements) '(+ - * / < > <= >= ==)))
          (format "(~a ~a ~a)"
                  (second compiled-elements)
                  (first compiled-elements)
                  (third compiled-elements))]
         
         ;; Function call
         [(>= (length compiled-elements) 2)
          (format "~a(~a)"
                  (first compiled-elements)
                  (string-join (cdr compiled-elements) ", "))]
         
         [else
          (string-join compiled-elements " ")]))]
    
    [(ir-pat-literal value)
     (format "~a" value)]
    
    [(ir-pat-var name)
     (symbol->string name)]
    
    [(ir-pat-unquote pattern)
     (ir-pattern->luau pattern)]
    
    [else ""])) 