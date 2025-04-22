#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/list
         "../../src/apollo/compiler/parser-new.rkt"
         "../../src/apollo/compiler/ast.rkt")

(provide parser-tools-tests)

(define parser-tools-tests
  (test-suite
   "Parser Tools Tests"
   
   (test-case "Parse simple module"
     (let ([ast (parse-racket-string "(module test racket/base 42)")])
       (check-pred ast-module? ast)
       (check-equal? (ast-module-name ast) 'test)
       (check-equal? (ast-module-lang ast) 'racket/base)
       (check-equal? (ast-module-body ast) (list (ast-literal 42)))))
   
   (test-case "Parse simple expression"
     (let ([ast (parse-expr "(+ 1 2)")])
       (check-pred ast-app? ast)
       (check-equal? (ast-app-func ast) (ast-var-ref '+))
       (check-equal? (ast-app-args ast)
                    (list (ast-literal 1)
                          (ast-literal 2)))))
   
   (test-case "Parse string literal"
     (let ([ast (parse-expr "\"hello world\"")])
       (check-pred ast-literal? ast)
       (check-equal? (ast-literal-value ast) "hello world")))
   
   (test-case "Parse boolean literals"
     (let ([ast1 (parse-expr "#t")]
           [ast2 (parse-expr "#f")])
       (check-pred ast-literal? ast1)
       (check-pred ast-literal? ast2)
       (check-equal? (ast-literal-value ast1) #t)
       (check-equal? (ast-literal-value ast2) #f)))
   
   (test-case "Parse quoted expression"
     (let ([ast (parse-expr "'(1 2 3)")])
       (check-pred ast-literal? ast)
       (check-equal? (ast-literal-value ast) '(quote (1 2 3)))))
   
   (test-case "Parse quasiquoted expression"
     (let ([ast (parse-expr "`(1 ,x 3)")])
       (check-pred ast-literal? ast)
       (check-equal? (ast-literal-value ast) '(quasiquote (1 (unquote x) 3)))))
   
   (test-case "Parse dotted pair"
     (let ([ast (parse-expr "(1 . 2)")])
       (check-pred ast-app? ast)
       (check-equal? (length (ast-app-args ast)) 2)))
   
   (test-case "Parse empty list"
     (let ([ast (parse-expr "()")])
       (check-pred ast-app? ast)
       (check-equal? (ast-app-args ast) '())))
   
   (test-case "Parse nested expressions"
     (let ([ast (parse-expr "(+ (* 2 3) (- 5 1))")])
       (check-pred ast-app? ast)
       (check-equal? (ast-app-func ast) (ast-var-ref '+))
       (check-equal? (length (ast-app-args ast)) 2)
       (let ([arg1 (first (ast-app-args ast))]
             [arg2 (second (ast-app-args ast))])
         (check-pred ast-app? arg1)
         (check-pred ast-app? arg2)
         (check-equal? (ast-app-func arg1) (ast-var-ref '*))
         (check-equal? (ast-app-func arg2) (ast-var-ref '-))
         (check-equal? (ast-app-args arg1)
                      (list (ast-literal 2) (ast-literal 3)))
         (check-equal? (ast-app-args arg2)
                      (list (ast-literal 5) (ast-literal 1))))))
   
   (test-case "Parse complex module"
     (let ([ast (parse-racket-string "(module test racket/base
                                       (define x 42)
                                       (+ x 1))")])
       (check-pred ast-module? ast)
       (check-equal? (ast-module-name ast) 'test)
       (check-equal? (ast-module-lang ast) 'racket/base)
       (check-equal? (length (ast-module-body ast)) 2)))))

(module+ test
  (run-tests parser-tools-tests)) 