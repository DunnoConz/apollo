#lang racket/base

(require rackunit
         racket/list
         apollo/compiler/luau-ast) ; Use package path

(provide luau-ast-tests) ; Export the test suite

;; Define Luau AST nodes using the struct names from apollo/compiler/types
;; (Assuming struct names like `luau-local`, `luau-assign`, etc. are defined there)
(define luau-ast-tests
  (test-suite
   "Luau AST Tests"
   
   ;; Example test: Create a local variable node
   (test-case "Create luau-local node"
     (define node (luau-local (list "x") (list (luau-literal 10))))
     (check-true (luau-local? node) "Node should be a luau-local")
     (check-equal? (luau-local-names node) (list "x"))
     (check-equal? (length (luau-local-values node)) 1)
     (check-true (luau-literal? (car (luau-local-values node))))
     (check-equal? (luau-literal-value (car (luau-local-values node))) 10))
   
   ;; Test creation and predicates for expressions
   (test-case "Luau expressions (using specific type predicates)"
     (check-true (luau-literal? (luau-literal 42)) "luau-literal check")
     (check-true (luau-var? (luau-var "x")) "luau-var check")
     (check-true (luau-binop? (luau-binop "+" (luau-literal 1) (luau-literal 2))) "luau-binop check")
     (check-true (luau-unop? (luau-unop "not" (luau-literal #t))) "luau-unop check")
     (check-true (luau-call? (luau-call (luau-var "f") '())) "luau-call check")
     (check-false (luau-literal? (luau-assign-local "x" (luau-literal 5)))) ; Check specific type false
     (check-false (luau-var? (luau-assign-local "x" (luau-literal 5))))) 

   ;; Test creation and predicates for statements (using specific type predicates)
   (test-case "Luau statements (using specific type predicates)"
     (check-true (luau-assign-local? (luau-assign-local "x" (luau-literal 5))))
     (check-true (luau-if? (luau-if (luau-literal #t) (luau-block '()) (luau-block '()))))
     (check-true (luau-return? (luau-return (luau-literal 42))))
     ;; Note: luau-call is an expression, but can appear as a statement (expr-stmt). 
     ;; The predicate luau-call? will be true. 
     ;; Checking if it's *used* as a statement is harder without luau-stmt?
     (check-true (luau-call? (luau-call (luau-var "print") (list (luau-literal "hello")))))
     ;; Similarly, literals are expressions, not statements themselves.
     (check-false (luau-assign-local? (luau-literal 42)))
     (check-true (luau-literal? (luau-literal 42)))
   )

   ;; Test Luau table literals
   (test-case "Luau table literals"
     (let ([empty-table (luau-table-literal '())]
           [array-table (luau-table-literal (list (luau-literal 1) (luau-literal 2) (luau-literal 3)))]
           [kv-table (luau-table-kv-literal (list (luau-table-pair (luau-literal "x") (luau-literal 10)) 
                                                  (luau-table-pair (luau-literal "y") (luau-literal 20))))])
       (check-true (luau-table-literal? empty-table))
       (check-true (luau-table-literal? array-table))
       (check-true (luau-table-kv-literal? kv-table))
       ; Corrected accessors and added prefixes
       (check-equal? (length (luau-table-literal-items array-table)) 3 "Array table should have 3 elements")
       (check-equal? (length (luau-table-kv-literal-pairs kv-table)) 2 "KV table should have 2 elements")))

   ;; Test Luau module
   (test-case "Luau module"
     (let ([mod (luau-module 
                 (list (luau-require "mathLib" (luau-literal "game.ReplicatedStorage.MathLib")))
                 (luau-block (list (luau-function-def "add" (list (luau-var "a") (luau-var "b")) ; Params need luau-var 
                                                    (luau-block (list (luau-return
                                                                      (luau-binop "+" (luau-var "a") (luau-var "b"))))))))
                 (luau-table-kv-literal (list (luau-table-pair (luau-literal "add") (luau-var "add")))))]) ; Key needs luau-literal
       (check-true (luau-module? mod) "Module check")
       (check-equal? (length (luau-module-requires mod)) 1 "Module should have one require")
       (check-equal? (length (luau-block-stmts (luau-module-body mod))) 1 "Module body should have 1 statement")
       (check-equal? (length (luau-table-kv-literal-pairs (luau-module-exports mod))) 1 "Module exports should have 1 export")))
   ))

;; Run the tests (optional, raco test usually handles this)
;(run-tests luau-ast-tests) 