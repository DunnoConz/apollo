#lang racket/base

(require rackunit
         ;; Use package paths
         apollo/compiler/types
         apollo/compiler/codegen) ; For codegen functions like luau-ast->string

;; Test suite for Luau AST functionality
(define luau-tests
  (test-suite
   "Luau AST Tests"
   
   (test-case "Number literal"
     (check-equal? (luau-ast->string (luau:luau-literal 42)) "42"))
   
   (test-case "String literal"
     (check-equal? (luau-ast->string (luau:luau-literal "hello")) "\"hello\""))
   
   (test-case "Boolean literal"
     (check-equal? (luau-ast->string (luau:luau-literal #t)) "true")
     (check-equal? (luau-ast->string (luau:luau-literal #f)) "false"))
   
   (test-case "Nil literal"
     (check-equal? (luau-ast->string (luau:luau-literal 'null)) "nil"))
   
   (test-case "Binary operation"
     (check-equal? (luau-ast->string 
                    (luau:luau-binop '+
                                     (luau:luau-literal 1)
                                     (luau:luau-literal 2)))
                   "(1 + 2)"))
   
   (test-case "Function definition"
     (check-equal? (luau-ast->string 
                    (luau:luau-function-def 
                     "add" 
                     (list (luau:luau-var "a") (luau:luau-var "b"))
                     (list (luau:luau-return
                            (luau:luau-binop '+
                                             (luau:luau-var "a")
                                             (luau:luau-var "b"))))))
                   "local function add(a, b)\n  return (a + b)\nend"))
   
   (test-case "If statement"
     (check-equal? (luau-ast->string 
                    (luau:luau-if
                     (luau:luau-binop '> 
                                    (luau:luau-var "x")
                                    (luau:luau-literal 0))
                     (luau:luau-block (list (luau:luau-return (luau:luau-literal 1))))
                     (luau:luau-block (list (luau:luau-return (luau:luau-literal 0))))))
                   "\tif x > 0 then\n\t\treturn 1\n\telse\n\t\treturn 0\n\tend"))
   
   (test-case "Local variable declaration"
     (check-equal? (luau-ast->string 
                    (luau:luau-assign-local "x" (luau:luau-literal 10)))
                   "local x = 10"))))

(provide luau-tests) 