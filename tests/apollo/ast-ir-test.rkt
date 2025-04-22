#lang racket/base

(require rackunit
         rackunit/text-ui
         "../../src/apollo/compiler/ast.rkt"
         "../../src/apollo/compiler/ir-types.rkt")

(provide ast-ir-tests)

(define ast-ir-tests
  (test-suite
   "AST to IR Conversion Tests"
   
   (test-case "Convert literal"
     (let ([ast (ast-literal 42)]
           [ir (ast->ir (ast-literal 42))])
       (check-pred ir-literal? ir)
       (check-equal? (ir-literal-value ir) 42)))
   
   (test-case "Convert variable reference"
     (let ([ast (ast-var-ref 'x)]
           [ir (ast->ir (ast-var-ref 'x))])
       (check-pred ir-var-ref? ir)
       (check-equal? (ir-var-ref-name ir) 'x)))
   
   (test-case "Convert lambda"
     (let* ([params '(x y)]
            [body (list (ast-var-ref 'x))]
            [ast (ast-lambda params body)]
            [ir (ast->ir ast)])
       (check-pred ir-lambda? ir)
       (check-equal? (ir-lambda-formals ir) params)
       (check-equal? (ir-lambda-kw-formals ir) '())
       (check-pred ir-var-ref? (car (ir-lambda-body ir)))
       (check-equal? (ir-var-ref-name (car (ir-lambda-body ir))) 'x)))
   
   (test-case "Convert application"
     (let* ([func (ast-var-ref '+)]
            [args (list (ast-literal 1) (ast-literal 2))]
            [ast (ast-app func args)]
            [ir (ast->ir ast)])
       (check-pred ir-app? ir)
       (check-pred ir-var-ref? (ir-app-func ir))
       (check-equal? (ir-var-ref-name (ir-app-func ir)) '+)
       (check-equal? (length (ir-app-args ir)) 2)
       (check-pred ir-literal? (car (ir-app-args ir)))
       (check-equal? (ir-literal-value (car (ir-app-args ir))) 1)))
   
   (test-case "Convert module"
     (parameterize ([current-module-path "test.rkt"])
       (let* ([name 'test-module]
              [lang 'racket/base]
              [body (list (ast-literal 42))]
              [ast (ast-module name lang body)]
              [ir (ast->ir ast)])
         (check-pred ir-module? ir)
         (check-equal? (ir-module-name ir) name)
         (check-equal? (ir-module-path ir) "test.rkt")
         (check-pred ir-literal? (car (ir-module-body ir)))
         (check-equal? (ir-literal-value (car (ir-module-body ir))) 42))))
   
   (test-case "Convert definition"
     (let* ([name 'x]
            [value (ast-literal 42)]
            [ast (ast-define name value)]
            [ir (ast->ir ast)])
       (check-pred ir-define? ir)
       (check-equal? (ir-define-name ir) name)
       (check-pred ir-literal? (ir-define-value ir))
       (check-equal? (ir-literal-value (ir-define-value ir)) 42)))))

(module+ test
  (run-tests ast-ir-tests)) 