#lang racket/base

(require syntax/parse ; Keep for syntax-case which is related
         racket/match
         racket/list)

;; Define IR node structures
(struct ir-module (name imports exports body span) #:transparent)
(struct ir-let (id value body span) #:transparent)
(struct ir-if (test then else span) #:transparent)
(struct ir-function (name params body span) #:transparent)
(struct ir-app (fn args span) #:transparent)
(struct ir-literal (value span) #:transparent)
(struct ir-variable (name span) #:transparent)
(struct ir-begin (body span) #:transparent)
(struct ir-lambda (params body span) #:transparent)
(struct ir-cond (clauses span) #:transparent)
(struct ir-set! (variable value span) #:transparent)
(struct ir-let* (bindings body span) #:transparent)
(struct ir-letrec (bindings body span) #:transparent)
(struct ir-quasiquote (template-stx span) #:transparent) ; Store raw template syntax
(struct ir-unquote (expr span) #:transparent) ; Keep struct defs for potential later use
(struct ir-unquote-splicing (expr span) #:transparent)

(provide syntax->ir ;; Export the main conversion function
         (all-defined-out)) ; Also export IR structures

;; Helper for span information
(define (get-span stx)
  (and stx (syntax-source stx)
       (list (syntax-source stx)
             (syntax-line stx)
             (syntax-column stx)
             (syntax-position stx)
             (syntax-span stx))))

;; --- Main Conversion Function --- 
;; Uses plain syntax-case for pattern matching.
(define (syntax->ir stx)
  (syntax-case stx (if let define lambda cond set! let* letrec begin 
                    quasiquote unquote unquote-splicing) ; Add unquote back to prevent fallback match
    ;; If expression
    [(if test then else)
     (ir-if (syntax->ir #'test)
            (syntax->ir #'then)
            (syntax->ir #'else)
            (get-span stx))]
    
    ;; Let expression (handles multiple bindings via nesting)
    [(let bindings-list body ...+)
     (let ([bindings-stx #'bindings-list])
       ;; Ensure bindings-list is a list syntax object
       (unless (list? (syntax-e bindings-stx))
         (raise-syntax-error #f "Bindings must be a list" stx bindings-stx))
       
       (let ([bindings (syntax->list bindings-stx)]) ; Convert syntax list to Racket list
         ;; Ensure each binding has the form [id val]
         (unless (andmap (lambda (b) (syntax-case b () ([_ _] #t) (_ #f))) bindings)
           (raise-syntax-error #f "invalid let binding form, expected [(id val) ...]" stx bindings-stx))
         
         ;; Process bindings recursively using a helper
         (define (build-nested-lets binds body-ir)
           (if (null? binds)
               body-ir
               (let* ([current-binding (first binds)]
                      [rest-bindings (rest binds)]
                      ;; Deconstruct the current binding safely
                      [id-stx (syntax-case current-binding () ([(i v)] #'i) (_ #f))]
                      [val-stx (syntax-case current-binding () ([(i v)] #'v) (_ #f))])
                 ;; Check if deconstruction succeeded
                 (unless (and id-stx val-stx)
                    (raise-syntax-error #f "Internal error: Invalid binding structure passed checks" stx current-binding))
                 ;; Build nested structure
                 (ir-let (syntax-e id-stx)
                         (syntax->ir val-stx)
                         (build-nested-lets rest-bindings body-ir)
                         (get-span current-binding))))) ; Span for the specific binding's let

         ;; Initial call to the helper with the full body converted
         (build-nested-lets bindings (syntax->ir #'(begin body ...+)))))] ; Close the main let clause

    ;; Let* expression (sequential bindings)
    [(let* bindings-list body ...+)
     (let ([bindings-stx #'bindings-list])
       ;; Validate overall structure
       (unless (list? (syntax-e bindings-stx))
         (raise-syntax-error #f "Bindings must be a list" stx bindings-stx))
       (let ([bindings (syntax->list bindings-stx)])
         ;; Validate each binding form
         (unless (andmap (lambda (b) (syntax-case b () ([_ _] #t) (_ #f))) bindings)
           (raise-syntax-error #f "invalid let* binding form, expected [(id val) ...]" stx bindings-stx))
         ;; Convert bindings to IR pairs: (list (cons 'id ir-val) ...)
         (let ([ir-bindings (map (lambda (b) 
                                    (syntax-case b () 
                                      ([id val]
                                       (cons (syntax-e #'id) (syntax->ir #'val)))))
                                  bindings)]
               ;; Convert body
               [ir-body (let ([body-s (syntax->list #'(body ...+))])
                          (match body-s
                            ['() (ir-literal #f (get-span stx))]
                            [(list single-stx) (syntax->ir single-stx)]
                            [_ (ir-begin (map syntax->ir body-s) (get-span stx))]))])
           (ir-let* ir-bindings ir-body (get-span stx)))))] ; Create ir-let* node

    ;; Letrec expression (recursive bindings)
    [(letrec bindings-list body ...+)
     (let ([bindings-stx #'bindings-list])
       ;; Validate overall structure
       (unless (list? (syntax-e bindings-stx))
         (raise-syntax-error #f "Bindings must be a list" stx bindings-stx))
       (let ([bindings (syntax->list bindings-stx)])
         ;; Validate each binding form
         (unless (andmap (lambda (b) (syntax-case b () ([_ _] #t) (_ #f))) bindings)
           (raise-syntax-error #f "invalid letrec binding form, expected [(id val) ...]" stx bindings-stx))
         ;; Convert bindings to IR pairs: (list (cons 'id ir-val) ...)
         (let ([ir-bindings (map (lambda (b) 
                                    (syntax-case b () 
                                      ([id val]
                                       (cons (syntax-e #'id) (syntax->ir #'val)))))
                                  bindings)]
               ;; Convert body
               [ir-body (let ([body-s (syntax->list #'(body ...+))])
                          (match body-s
                            ['() (ir-literal #f (get-span stx))]
                            [(list single-stx) (syntax->ir single-stx)]
                            [_ (ir-begin (map syntax->ir body-s) (get-span stx))]))])
           (ir-letrec ir-bindings ir-body (get-span stx)))))] ; Create ir-letrec node

    ;; Cond expression
    [(cond clauses ...)
     (ir-cond
      (let ([clause-stx-list (syntax->list #'(clauses ...))])
        (map (lambda (clause-stx)
               (syntax-case clause-stx (else)
                 ;; Else clause
                 [(else body ...+)
                  (let ([body-ir (let ([body-s (syntax->list #'(body ...+))])
                                   (match body-s
                                     ['() (ir-literal #f (get-span clause-stx))] ; Empty body
                                     [(list single-stx) (syntax->ir single-stx)] ; Single expression
                                     [_ (ir-begin (map syntax->ir body-s) (get-span clause-stx))]))]) ; Multiple
                    (cons (ir-literal #t (get-span clause-stx)) body-ir))] ; Use #t for else test
                 ;; Regular clause
                 [(test body ...+)
                  (let ([test-ir (syntax->ir #'test)]
                        [body-ir (let ([body-s (syntax->list #'(body ...+))])
                                   (match body-s
                                     ['() (ir-literal #f (get-span clause-stx))] ; Empty body
                                     [(list single-stx) (syntax->ir single-stx)] ; Single expression
                                     [_ (ir-begin (map syntax->ir body-s) (get-span clause-stx))]))]) ; Multiple
                    (cons test-ir body-ir))]
                 ;; Invalid clause form
                 [_ (raise-syntax-error #f "invalid cond clause" stx clause-stx)]))
             clause-stx-list))
      (get-span stx))]

    ;; Set! expression
    [(set! var val)
     (identifier? #'var) ; Guard: ensure var is an identifier
     (ir-set! (syntax-e #'var)
              (syntax->ir #'val)
              (get-span stx))]

    ;; Lambda expression
    [(lambda (params ...) body ...+)
     (ir-lambda (map syntax-e (syntax->list #'(params ...)))
                (let ([body-stx-list (syntax->list #'(body ...+))])
                  (cond
                    [(null? body-stx-list) (ir-literal #f (get-span stx))] ; Handle empty body
                    [(= (length body-stx-list) 1) (syntax->ir (first body-stx-list))] ; Single expression
                    [else (ir-begin (map syntax->ir body-stx-list) (get-span stx))])) ; Multiple expressions
                (get-span stx))]

    ;; Function Definition (simplified)
    [(define (name params ...) body ...)
      (ir-function (syntax-e #'name)
                   (map syntax-e (syntax->list #'(params ...)))
                   (let ([body-stx-list (syntax->list #'(body ...))])
                     (cond
                       [(null? body-stx-list) (ir-literal #f (get-span stx))] ; Handle empty body
                       [(= (length body-stx-list) 1) (syntax->ir (first body-stx-list))] ; Single expression
                       [else (ir-begin (map syntax->ir body-stx-list) (get-span stx))])) ; Multiple expressions
                   (get-span stx))]

    ;; Function Application (Clause enclosed in brackets)
    [(func args ...) ; Pattern
     (identifier? #'func) ; Guard Expression
     ;; Expression
     (ir-app (syntax->ir #'func) 
             (map syntax->ir (syntax->list #'(args ...)))
             (get-span stx))]

    ;; Variable reference (Clause enclosed in brackets)
    [v (identifier? #'v) ; Guard Expression
     (ir-variable (syntax-e #'v) (get-span stx))]
    
    ;; Literals (Clause enclosed in brackets)
    [lit (or (number? (syntax-e #'lit))
             (string? (syntax-e #'lit))
             (boolean? (syntax-e #'lit))) ; Guard Expression
     (ir-literal (syntax-e #'lit) (get-span stx))]

    ;; Begin expression (explicit sequencing)
    [(begin body ...+)
     (let ([body-s (syntax->list #'(body ...+))])
       (match body-s
         ['() (ir-literal #f (get-span stx))]             ; Empty begin
         [(list single-stx) (syntax->ir single-stx)]     ; Single expression in begin
         [_ (ir-begin (map syntax->ir body-s) (get-span stx))]))] ; Multiple expressions

    ;; Quasiquote expression
    [(quasiquote template)
     (ir-quasiquote #'template (get-span stx))] ; Store raw syntax

    ;; Top-level unquote (Error - should be handled within QQ processing if done)
    [(unquote _)
     (raise-syntax-error #f "unquote appeared unexpectedly outside quasiquote processing" stx)]

    ;; Top-level unquote-splicing (Error)
    [(unquote-splicing _)
     (raise-syntax-error #f "unquote-splicing appeared unexpectedly outside quasiquote processing" stx)]

    ;; Fallback for unrecognized syntax
    [_ (error 'syntax->ir "Unrecognized syntax: ~a" (syntax->datum stx))]))


;; Example usage
(module+ test
  (require rackunit)
  
  ;; Test conversion of a literal
  (define result-lit (syntax->ir #'42))
  (check-true (ir-literal? result-lit) "Should convert literal 42")
  (check-equal? (ir-literal-value result-lit) 42 "Literal value should be 42")
  
  ;; Test conversion of a variable
  (define result-var (syntax->ir #'x))
  (check-true (ir-variable? result-var) "Should convert variable x")
  (check-equal? (ir-variable-name result-var) 'x "Variable name should be x")

  ;; Test conversion of an if expression
  (define result-if (syntax->ir #'(if #t "yes" "no")))
  (check-true (ir-if? result-if) "Should convert if expression")
  (check-true (ir-literal? (ir-if-test result-if)) "If test should be literal")
  (check-equal? (ir-literal-value (ir-if-test result-if)) #t "If test value should be #t")

  ;; Test conversion of a function call
  (define result-app (syntax->ir #'(add 1 2)))
  (check-true (ir-app? result-app) "Should convert function application")
  (check-true (ir-variable? (ir-app-fn result-app)) "App fn should be variable")
  (check-equal? (ir-variable-name (ir-app-fn result-app)) 'add "App fn name should be add")
  (check-equal? (length (ir-app-args result-app)) 2 "App should have 2 args")
  (check-true (ir-literal? (first (ir-app-args result-app))) "App arg 1 should be literal")
  (check-equal? (ir-literal-value (first (ir-app-args result-app))) 1 "App arg 1 value should be 1")
  
  ;; Test conversion of a let expression with multiple bindings
  (define result-let-multi (syntax->ir #'(let ([x 10] [y 20]) (+ x y))))
  (check-true (ir-let? result-let-multi) "Outer let should be ir-let")
  (check-equal? (ir-let-id result-let-multi) 'x "Outer let id should be x")
  (check-true (ir-literal? (ir-let-value result-let-multi)) "Outer let value should be literal")
  (check-equal? (ir-literal-value (ir-let-value result-let-multi)) 10 "Outer let value should be 10")
  (let ([inner-let (ir-let-body result-let-multi)])
    (check-true (ir-let? inner-let) "Inner let should be ir-let")
    (check-equal? (ir-let-id inner-let) 'y "Inner let id should be y")
    (check-true (ir-literal? (ir-let-value inner-let)) "Inner let value should be literal")
    (check-equal? (ir-literal-value (ir-let-value inner-let)) 20 "Inner let value should be 20")
    (let ([body-app (ir-let-body inner-let)])
       (check-true (ir-app? body-app) "Let body should be app")
       (check-equal? (ir-variable-name (ir-app-fn body-app)) '+ "Let body app fn should be +")))
  
  ;; Test conversion of define with multiple body expressions
  (define result-define-multi (syntax->ir #'(define (func x) (print x) (+ x 1))))
  (check-true (ir-function? result-define-multi) "Should convert define to ir-function")
  (check-equal? (ir-function-name result-define-multi) 'func "Function name should be func")
  (check-equal? (ir-function-params result-define-multi) '(x) "Function params should be (x)")
  (let ([func-body (ir-function-body result-define-multi)])
    (check-true (ir-begin? func-body) "Function body should be ir-begin")
    (check-equal? (length (ir-begin-body func-body)) 2 "Begin body should have 2 expressions")
    (let ([first-expr (first (ir-begin-body func-body))]
          [second-expr (second (ir-begin-body func-body))])
      (check-true (ir-app? first-expr) "First expr should be app")
      (check-equal? (ir-variable-name (ir-app-fn first-expr)) 'print "First app fn should be print")
      (check-true (ir-app? second-expr) "Second expr should be app")
      (check-equal? (ir-variable-name (ir-app-fn second-expr)) '+ "Second app fn should be +")))

  ;; Test conversion of lambda with multiple body expressions
  (define result-lambda-multi (syntax->ir #'(lambda (x) (print x) (* x x))))
  (check-true (ir-lambda? result-lambda-multi) "Should convert lambda to ir-lambda")
  (check-equal? (ir-lambda-params result-lambda-multi) '(x) "Lambda params should be (x)")
  (let ([lambda-body (ir-lambda-body result-lambda-multi)])
    (check-true (ir-begin? lambda-body) "Lambda body should be ir-begin")
    (check-equal? (length (ir-begin-body lambda-body)) 2 "Begin body should have 2 expressions")
    (let ([first-expr (first (ir-begin-body lambda-body))]
          [second-expr (second (ir-begin-body lambda-body))])
      (check-true (ir-app? first-expr) "First expr should be app (print)")
      (check-equal? (ir-variable-name (ir-app-fn first-expr)) 'print "First app fn should be print")
      (check-true (ir-app? second-expr) "Second expr should be app (*)")
      (check-equal? (ir-variable-name (ir-app-fn second-expr)) '* "Second app fn should be *")))

  ;; Test conversion of cond with else and multi-body clause
  (define result-cond (syntax->ir #'(cond [(< x 0) (display "neg") (- 0 x)]
                                          [(> x 0) (display "pos") x]
                                          [else (display "zero") 0])))
  (check-true (ir-cond? result-cond) "Should convert cond to ir-cond")
  (check-equal? (length (ir-cond-clauses result-cond)) 3 "Cond should have 3 clauses")
  ;; Clause 1: (< x 0) => (display "neg"), (- 0 x)
  (let ([clause1 (first (ir-cond-clauses result-cond))])
    (check-true (ir-app? (car clause1)) "Clause 1 test should be app (<)")
    (check-true (ir-begin? (cdr clause1)) "Clause 1 body should be begin")
    (check-equal? (length (ir-begin-body (cdr clause1))) 2 "Clause 1 body should have 2 exprs"))
  ;; Clause 2: (> x 0) => x
  (let ([clause2 (second (ir-cond-clauses result-cond))])
    (check-true (ir-app? (car clause2)) "Clause 2 test should be app (>)")
    (check-true (ir-variable? (cdr clause2)) "Clause 2 body should be variable (x)"))
  ;; Clause 3: else => (display "zero"), 0
  (let ([clause3 (third (ir-cond-clauses result-cond))])
    (check-true (ir-literal? (car clause3)) "Clause 3 test should be literal (#t)")
    (check-equal? (ir-literal-value (car clause3)) #t "Clause 3 test value should be #t")
    (check-true (ir-begin? (cdr clause3)) "Clause 3 body should be begin")
    (check-equal? (length (ir-begin-body (cdr clause3))) 2 "Clause 3 body should have 2 exprs"))

  ;; Test conversion of set!
  (define result-set! (syntax->ir #'(set! count (+ count 1))))
  (check-true (ir-set!? result-set!) "Should convert set! to ir-set!")
  (check-equal? (ir-set!-variable result-set!) 'count "Set! variable should be count")
  (check-true (ir-app? (ir-set!-value result-set!)) "Set! value should be app (+)")

  ;; Test conversion of let* (sequential binding)
  (define result-let* (syntax->ir #'(let* ([x 10] [y (+ x 5)]) (* x y))))
  (check-true (ir-let*? result-let*) "Should convert let* to ir-let*")
  (check-equal? (length (ir-let*-bindings result-let*)) 2 "Let* should have 2 bindings")
  ;; Binding 1: x = 10
  (let ([binding1 (first (ir-let*-bindings result-let*))])
    (check-equal? (car binding1) 'x "Binding 1 id should be x")
    (check-true (ir-literal? (cdr binding1)) "Binding 1 value should be literal (10)"))
  ;; Binding 2: y = (+ x 5)
  (let ([binding2 (second (ir-let*-bindings result-let*))])
    (check-equal? (car binding2) 'y "Binding 2 id should be y")
    (check-true (ir-app? (cdr binding2)) "Binding 2 value should be app (+)"))
  ;; Body: (* x y)
  (check-true (ir-app? (ir-let*-body result-let*)) "Let* body should be app (*)")

  ;; Test conversion of letrec (mutual recursion)
  (define result-letrec 
    (syntax->ir #'(letrec ([is-even? (lambda (n) (if (= n 0) #t (is-odd? (- n 1))))]
                             [is-odd? (lambda (n) (if (= n 0) #f (is-even? (- n 1))))])
                    (is-even? 4))))
  (check-true (ir-letrec? result-letrec) "Should convert letrec to ir-letrec")
  (check-equal? (length (ir-letrec-bindings result-letrec)) 2 "Letrec should have 2 bindings")
  ;; Binding 1: is-even?
  (let ([binding1 (first (ir-letrec-bindings result-letrec))])
    (check-equal? (car binding1) 'is-even? "Binding 1 id should be is-even?")
    (check-true (ir-lambda? (cdr binding1)) "Binding 1 value should be lambda"))
  ;; Binding 2: is-odd?
  (let ([binding2 (second (ir-letrec-bindings result-letrec))])
    (check-equal? (car binding2) 'is-odd? "Binding 2 id should be is-odd?")
    (check-true (ir-lambda? (cdr binding2)) "Binding 2 value should be lambda"))
  ;; Body: (is-even? 4)
  (check-true (ir-app? (ir-letrec-body result-letrec)) "Letrec body should be app (is-even?)")

  ;; Test conversion of begin
  (define result-begin (syntax->ir #'(begin (display "hello") (+ 1 2))))
  (check-true (ir-begin? result-begin) "Should convert begin to ir-begin")
  (check-equal? (length (ir-begin-body result-begin)) 2 "Begin body should have 2 expressions")
  (check-true (ir-app? (first (ir-begin-body result-begin))) "Begin expr 1 should be app (display)")
  (check-true (ir-app? (second (ir-begin-body result-begin))) "Begin expr 2 should be app (+)")

  ;; Test conversion of begin with single expression
  (define result-begin-single (syntax->ir #'(begin (* 3 4))))
  (check-true (ir-app? result-begin-single) "Single-expr begin should convert to inner expr (app)")
  (check-equal? (ir-variable-name (ir-app-fn result-begin-single)) '* "App fn should be *")

  ;; Test conversion of quasiquote (stores raw template)
  (let ([x 10] [y '(20 30)]) ; Define locals for test
    (define result-qq (syntax->ir #'(quasiquote (a (unquote x) #(100 (unquote y) (unquote-splicing y)) b))))
    (check-true (ir-quasiquote? result-qq) "Should convert quasiquote to ir-quasiquote")
    ;; Check the datum representation of the stored syntax object
    (check-equal? (syntax->datum (ir-quasiquote-template-stx result-qq)) 
                  '(a (unquote x) #(100 (unquote y) (unquote-splicing y)) b) ; Compare datum
                  "Stored template syntax mismatch"))

  (displayln "Conversion successful!"))