#lang racket/base

(require rackunit
         racket/list ; For first, last
         racket/match ; For match
         syntax/parse ; Needed for syntax-e, syntax->list
         apollo/compiler/parser)
         ; No longer need apollo/compiler/ast for this basic test

(provide parser-tests)

;; Assuming parse-racket-string returns a raw Racket syntax object,
;; potentially wrapped in a default module form.

(define parser-tests
  (test-suite
   "Parser Tests"

   ; TODO: Add test cases here
   (test-case "Basic Sanity Check"
     (check-true #t "Placeholder test"))

   (test-case "Parse Number"
     (let ([stx (parse-racket-string "42")])
       (check-true (syntax? stx) "Result should be a syntax object")
       (let ([datum (syntax-e stx)])
         (check-true (list? datum) "Syntax-e should be a list (module form)")
         (check-equal? (syntax-e (first datum)) 'module "Should be a module form")
         (let ([body-stx (last datum)])
           (check-true (syntax? body-stx) "Module body should be a syntax object")
           (let ([body-datum (syntax-e body-stx)])
             (check-true (number? body-datum) "Module body datum should be a number")
             (check-equal? body-datum 42 "Parsed number should be 42"))))))
   
   ; Placeholder for string parsing - UNCOMMENTED
   (test-case "Parse String"
     (let ([stx (parse-racket-string "\"hello\"")])
       (check-true (syntax? stx) "Result should be a syntax object")
       (let ([datum (syntax-e stx)])
         (check-true (list? datum) "Syntax-e should be a list (module form)")
         (check-equal? (syntax-e (first datum)) 'module "Should be a module form")
         (let ([body-stx (last datum)])
           (check-true (syntax? body-stx) "Module body should be a syntax object")
           (let ([body-datum (syntax-e body-stx)])
             (check-true (string? body-datum) "Module body datum should be a string")
             (check-equal? body-datum "hello" "Parsed string should be 'hello'"))))))
   
   (test-case "Parse Booleans"
     ;; Test #true
     (let ([stx-true (parse-racket-string "#true")])
       (check-true (syntax? stx-true) "Result (#true) should be a syntax object")
       (let ([datum-true (syntax-e stx-true)])
         (check-true (list? datum-true) "Syntax-e (#true) should be a list (module form)")
         (check-equal? (syntax-e (first datum-true)) 'module "Should be a module form (#true)")
         (let ([body-stx (last datum-true)])
           (check-true (syntax? body-stx) "Module body (#true) should be a syntax object")
           (let ([body-datum (syntax-e body-stx)])
             (check-true (boolean? body-datum) "Module body datum (#true) should be a boolean")
             (check-true body-datum "Parsed boolean (#true) should be #true")))))
     ;; Test #false
     (let ([stx-false (parse-racket-string "#false")])
       (check-true (syntax? stx-false) "Result (#false) should be a syntax object")
       (let ([datum-false (syntax-e stx-false)])
         (check-true (list? datum-false) "Syntax-e (#false) should be a list (module form)")
         (check-equal? (syntax-e (first datum-false)) 'module "Should be a module form (#false)")
         (let ([body-stx (last datum-false)])
           (check-true (syntax? body-stx) "Module body (#false) should be a syntax object")
           (let ([body-datum (syntax-e body-stx)])
             (check-true (boolean? body-datum) "Module body datum (#false) should be a boolean")
             (check-false body-datum "Parsed boolean (#false) should be #false"))))))

   (test-case "Parse Symbol"
     (let ([stx (parse-racket-string "x")])
       (check-true (syntax? stx) "Result (symbol) should be a syntax object")
       (let ([datum (syntax-e stx)])
         (check-true (list? datum) "Syntax-e (symbol) should be a list (module form)")
         (check-equal? (syntax-e (first datum)) 'module "Should be a module form (symbol)")
         (let ([body-stx (last datum)])
           (check-true (syntax? body-stx) "Module body (symbol) should be a syntax object")
           (let ([body-datum (syntax-e body-stx)])
             (check-true (symbol? body-datum) "Module body datum (symbol) should be a symbol")
             (check-equal? body-datum 'x "Parsed symbol should be 'x"))))))

   (test-case "Parse Application"
     (let ([stx (parse-racket-string "(+ 1 2)")])
       (check-true (syntax? stx) "Result (app) should be a syntax object")
       (let ([datum (syntax-e stx)])
         (check-true (list? datum) "Syntax-e (app) should be a list (module form)")
         (check-equal? (syntax-e (first datum)) 'module "Should be a module form (app)")
         (let ([body-stx (last datum)])
           (check-true (syntax? body-stx) "Module body (app) should be a syntax object")
           (let ([body-list (syntax->list body-stx)])
             (check-equal? (length body-list) 3 "Application list should have 3 elements")
             (check-equal? (syntax-e (first body-list)) '+ "First element should be '+")
             (check-equal? (syntax-e (second body-list)) 1 "Second element should be 1")
             (check-equal? (syntax-e (third body-list)) 2 "Third element should be 2"))))))

   (test-case "Parse Define"
     (let ([stx (parse-racket-string "(define x 10)")])
       (check-true (syntax? stx) "Result (define) should be a syntax object")
       (let ([datum (syntax-e stx)])
         (check-true (list? datum) "Syntax-e (define) should be a list (module form)")
         (check-equal? (syntax-e (first datum)) 'module "Should be a module form (define)")
         (let ([body-stx (last datum)])
           (check-true (syntax? body-stx) "Module body (define) should be a syntax object")
           (let ([body-list (syntax->list body-stx)])
             (check-equal? (length body-list) 3 "Define list should have 3 elements")
             (check-equal? (syntax-e (first body-list)) 'define "First element should be 'define")
             (check-equal? (syntax-e (second body-list)) 'x "Second element should be 'x")
             (check-equal? (syntax-e (third body-list)) 10 "Third element should be 10"))))))

   (test-case "Parse If (3 args)"
     (let ([stx (parse-racket-string "(if #true 1 2)")])
       (check-true (syntax? stx) "Result (if3) should be a syntax object")
       (let ([datum (syntax-e stx)])
         (check-true (list? datum) "Syntax-e (if3) should be a list (module form)")
         (check-equal? (syntax-e (first datum)) 'module "Should be a module form (if3)")
         (let ([body-stx (last datum)])
           (check-true (syntax? body-stx) "Module body (if3) should be a syntax object")
           (let ([body-list (syntax->list body-stx)])
             (check-equal? (length body-list) 4 "If list should have 4 elements")
             (check-equal? (syntax-e (first body-list)) 'if "First element should be 'if")
             (check-true (boolean? (syntax-e (second body-list))) "Condition should be boolean")
             (check-true (syntax-e (second body-list)) "Condition should be #true")
             (check-equal? (syntax-e (third body-list)) 1 "Consequent should be 1")
             (check-equal? (syntax-e (fourth body-list)) 2 "Alternative should be 2"))))))
             
   (test-case "Parse If (2 args)"
     (let ([stx (parse-racket-string "(if #false 1)")]) ; Racket defaults missing else to #f or #<void>, check parser's handling
       (check-true (syntax? stx) "Result (if2) should be a syntax object")
       (let ([datum (syntax-e stx)])
         (check-true (list? datum) "Syntax-e (if2) should be a list (module form)")
         (check-equal? (syntax-e (first datum)) 'module "Should be a module form (if2)")
         (let ([body-stx (last datum)])
           (check-true (syntax? body-stx) "Module body (if2) should be a syntax object")
           (let ([body-list (syntax->list body-stx)])
             (check-equal? (length body-list) 3 "If list should have 3 elements") ; Note: Racket reader keeps structure
             (check-equal? (syntax-e (first body-list)) 'if "First element should be 'if")
             (check-false (syntax-e (second body-list)) "Condition should be #false")
             (check-equal? (syntax-e (third body-list)) 1 "Consequent should be 1"))))))
             
   (test-case "Parse Lambda"
     (let ([stx (parse-racket-string "(lambda (x) (+ x 1))")])
       (check-true (syntax? stx) "Result (lambda) should be a syntax object")
       (let ([datum (syntax-e stx)])
         (check-equal? (syntax-e (first datum)) 'module)
         (let ([body-stx (last datum)])
           (check-true (syntax? body-stx) "Lambda body syntax")
           (match (syntax->list body-stx)
             [(list lam params body-expr)
              (check-equal? (syntax-e lam) 'lambda)
              (match (syntax->list params)
                [(list x) (check-equal? (syntax-e x) 'x)]
                [_ (fail-check "Lambda params should be (x)")])
              (match (syntax->list body-expr)
                [(list plus x one)
                 (check-equal? (syntax-e plus) '+)
                 (check-equal? (syntax-e x) 'x)
                 (check-equal? (syntax-e one) 1)]
                [_ (fail-check "Lambda body expr should be (+ x 1)")])]
             [_ (fail-check "Lambda structure should be (lambda (params) body)")])))))

   (test-case "Parse Let"
     (let ([stx (parse-racket-string "(let ([x 1] [y 2]) (+ x y))")])
       (check-true (syntax? stx) "Result (let) should be a syntax object")
       (let ([datum (syntax-e stx)])
         (check-equal? (syntax-e (first datum)) 'module)
         (let ([body-stx (last datum)])
           (check-true (syntax? body-stx) "Let body syntax")
           (match (syntax->list body-stx)
             [(list let-kw bindings body-expr)
              (check-equal? (syntax-e let-kw) 'let)
              ; Check bindings [[x 1] [y 2]]
              (match (syntax->list bindings)
                [(list b1 b2)
                 (match (syntax->list b1) [(list x n1) (check-equal? (syntax-e x) 'x) (check-equal? (syntax-e n1) 1)])
                 (match (syntax->list b2) [(list y n2) (check-equal? (syntax-e y) 'y) (check-equal? (syntax-e n2) 2)])]
                [_ (fail-check "Let bindings format error")])
              ; Check body (+ x y)
              (match (syntax->list body-expr)
                [(list plus x y)
                 (check-equal? (syntax-e plus) '+)
                 (check-equal? (syntax-e x) 'x)
                 (check-equal? (syntax-e y) 'y)]
                [_ (fail-check "Let body expr should be (+ x y)")])]
             [_ (fail-check "Let structure error")])))))
             
   (test-case "Parse Cond"
     (let ([stx (parse-racket-string "(cond [#true 1] [#false 2] [else 3])")])
       (check-true (syntax? stx) "Result (cond) should be a syntax object")
       (let ([datum (syntax-e stx)])
         (check-equal? (syntax-e (first datum)) 'module)
         (let ([body-stx (last datum)])
           (check-true (syntax? body-stx) "Cond body syntax")
           (match (syntax->list body-stx)
             [(list cond-kw c1 c2 c3)
              (check-equal? (syntax-e cond-kw) 'cond)
              (match (syntax->list c1) [(list t v1) (check-true (syntax-e t)) (check-equal? (syntax-e v1) 1)])
              (match (syntax->list c2) [(list f v2) (check-false (syntax-e f)) (check-equal? (syntax-e v2) 2)])
              (match (syntax->list c3) [(list else v3) (check-equal? (syntax-e else) 'else) (check-equal? (syntax-e v3) 3)])]
             [_ (fail-check "Cond structure error")])))))

   (test-case "Parse Struct"
     (let ([stx (parse-racket-string "(struct posn (x y))")])
       (check-true (syntax? stx) "Result (struct) should be a syntax object")
       (let ([datum (syntax-e stx)])
         (check-equal? (syntax-e (first datum)) 'module)
         (let ([body-stx (last datum)])
           (check-true (syntax? body-stx) "Struct body syntax")
           (match (syntax->list body-stx)
             [(list struct-kw name fields)
              (check-equal? (syntax-e struct-kw) 'struct)
              (check-equal? (syntax-e name) 'posn)
              (match (syntax->list fields)
                [(list x y) (check-equal? (syntax-e x) 'x) (check-equal? (syntax-e y) 'y)])]
             [_ (fail-check "Struct structure error")])))))

   (test-case "Parse Quote"
     (let ([stx (parse-racket-string "'(a b c)")]) ; Equivalent to (quote (a b c))
       (check-true (syntax? stx) "Result (quote) should be a syntax object")
       (let ([datum (syntax-e stx)])
         (check-equal? (syntax-e (first datum)) 'module)
         (let ([body-stx (last datum)])
           (check-true (syntax? body-stx) "Quote body syntax")
           (match (syntax->list body-stx)
             [(list quote-kw quoted-list-stx) ; Renamed variable
              (check-equal? (syntax-e quote-kw) 'quote)
              ; Apply syntax-e to the quoted list and its elements
              (let ([quoted-list-datum (syntax-e quoted-list-stx)])
                (check-true (list? quoted-list-datum) "Quoted data should be a list")
                (check-equal? (map syntax-e quoted-list-datum) '(a b c) "Quoted list elements mismatch"))]
             [_ (fail-check "Quote structure error")])))))

   (test-case "Parse Quasiquote/Unquote"
     (let ([stx (parse-racket-string "`(a ,(+ 1 2) c)")])
       (check-true (syntax? stx) "Result (qq) should be a syntax object")
       (let ([datum (syntax-e stx)])
         (check-equal? (syntax-e (first datum)) 'module)
         (let ([body-stx (last datum)])
           (check-true (syntax? body-stx) "QQ body syntax")
           (match (syntax->list body-stx)
             [(list qq-kw qq-template)
              (check-equal? (syntax-e qq-kw) 'quasiquote)
              ; Check the template `(a ,(+ 1 2) c)`
              (match (syntax->list qq-template)
                [(list a unquote-expr c)
                 (check-equal? (syntax-e a) 'a)
                 (check-equal? (syntax-e c) 'c)
                 ; Check the unquoted expr `(unquote (+ 1 2))`
                 (match (syntax->list unquote-expr)
                   [(list uq-kw app-expr)
                    (check-equal? (syntax-e uq-kw) 'unquote)
                    ; Check the app expr `(+ 1 2)`
                    (match (syntax->list app-expr)
                      [(list plus one two)
                       (check-equal? (syntax-e plus) '+)
                       (check-equal? (syntax-e one) 1)
                       (check-equal? (syntax-e two) 2)]
                      [_ (fail-check "QQ Inner app structure error")])]
                   [_ (fail-check "QQ Unquote structure error")])]
                [_ (fail-check "QQ template structure error")])]
             [_ (fail-check "QQ structure error")])))))

   ))

; Run the tests if this file is executed directly
(module+ main
  (require rackunit/text-ui)
  (run-tests parser-tests)) 