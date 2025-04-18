#lang racket/base
(require rackunit
         racket/match
         ;; Use relative paths
         (submod "../../src/apollo/compiler/ir.rkt" ir)
         "../../src/apollo/compiler/ir-types.rkt")

(provide ir-tests)

;; Helper to simplify testing - convert IR to datum
(define (ir->datum ir)
  (match ir
    [(ir-program modules) (list 'ir-program (map ir->datum modules))]
    [(ir-module name path body) (list 'ir-module name path (map ir->datum body))]
    [(ir-literal value) (list 'ir-literal value)]
    [(ir-var-ref name) (list 'ir-var-ref name)]
    [(ir-var-set name value) (list 'ir-var-set name (ir->datum value))]
    [(ir-define name value) (list 'ir-define name (ir->datum value))]
    [(ir-lambda formals kw-formals body) (list 'ir-lambda formals kw-formals (map ir->datum body))]
    [(ir-app func args kw-args) (list 'ir-app (ir->datum func) (map ir->datum args) kw-args)]
    [(ir-if test then else) (list 'ir-if (ir->datum test) (ir->datum then) (ir->datum else))]
    [(ir-begin exprs) (list 'ir-begin (map ir->datum exprs))]
    [(ir-let bindings body) (list 'ir-let (map (lambda (b) (cons (car b) (ir->datum (cdr b)))) bindings) (map ir->datum body))]
    [(ir-letrec bindings body) (list 'ir-letrec (map (lambda (b) (cons (car b) (ir->datum (cdr b)))) bindings) (map ir->datum body))]
    [(ir-cond clauses else-clause) (list 'ir-cond (map (lambda (c) (cons (ir->datum (car c)) (map ir->datum (cdr c)))) clauses) (and else-clause (map ir->datum else-clause)))]
    [(ir-define-struct name fields) (list 'ir-define-struct name fields)]
    [(ir-struct-ref instance index name) (list 'ir-struct-ref (ir->datum instance) (ir->datum index) name)]
    [(ir-match target clauses) (list 'ir-match (ir->datum target) (map (lambda (c) (cons (ir->datum (car c)) (map ir->datum (cdr c)))) clauses))]
    [_ (error "Unsupported IR node type")]))

(define ir-tests
  (test-suite
   "IR Tests"

   (test-case "Convert simple literal to IR"
     (let ([ast #'(module #f racket/base 42)])
       (check-equal? (ir->datum (convert-to-ir ast))
                    '(ir-begin ((ir-literal "/* Inlined module: #f */") (ir-literal 42) (ir-begin ())))
                    "Should convert a number literal to IR datum")))

   (test-case "Convert variable reference to IR"
     (let ([ast #'(module #f racket/base x)])
       (check-equal? (ir->datum (convert-to-ir ast))
                    '(ir-begin ((ir-literal "/* Inlined module: #f */") (ir-var-ref x) (ir-begin ())))
                    "Should convert a variable reference to IR datum")))

   (test-case "Convert function application to IR"
     (let ([ast #'(module #f racket/base (+ 1 2))])
       (check-equal? (ir->datum (convert-to-ir ast))
                    '(ir-begin ((ir-literal "/* Inlined module: #f */") 
                               (ir-app (ir-var-ref +) ((ir-literal 1) (ir-literal 2)))
                               (ir-begin ())))
                    "Should convert a function application to IR datum")))
   
   (test-case "Convert if expression to IR"
     (let ([ast #'(module #f racket/base (if (> x 0) 1 0))])
       (check-equal? (ir->datum (convert-to-ir ast))
                    '(ir-begin ((ir-literal "/* Inlined module: #f */")
                               (ir-if (ir-app (ir-var-ref >) ((ir-var-ref x) (ir-literal 0)))
                                      (ir-literal 1)
                                      (ir-literal 0))
                               (ir-begin ())))
                    "Should convert an if expression to IR datum")))
   
   (test-case "Convert let expression to IR"
     (let ([ast #'(module #f racket/base (let ([x 1] [y 2]) (+ x y)))])
       (check-equal? (ir->datum (convert-to-ir ast))
                    '(ir-begin ((ir-literal "/* Inlined module: #f */")
                               (ir-let ((x (ir-literal 1)) (y (ir-literal 2)))
                                       ((ir-app (ir-var-ref +) ((ir-var-ref x) (ir-var-ref y)))))
                               (ir-begin ())))
                    "Should convert a let expression to IR datum")))
   
   (test-case "Convert lambda expression to IR"
     (let ([ast #'(module #f racket/base (lambda (x) x))])
       (check-equal? (ir->datum (convert-to-ir ast))
                    '(ir-begin ((ir-literal "/* Inlined module: #f */")
                               (ir-lambda (x) ((ir-var-ref x)))
                               (ir-begin ())))
                    "Should convert a lambda expression to IR datum"))))) 