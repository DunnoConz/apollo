#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/file
         racket/system
         racket/port
         racket/path
         "../../../src/apollo/compiler/codegen.rkt"
         "../../../src/apollo/compiler/parser.rkt"
         (submod "../../../src/apollo/compiler/ir.rkt" ir)
         "../../../src/apollo/compiler/ir-types.rkt")

(provide luau-codegen-tests)

;; Helper function to write Luau code to a temporary file and test it with Lute
(define (test-luau-code code #:file-name [file-name "test.luau"])
  (define temp-dir (make-temporary-file "luau-test-~a" 'directory))
  (define test-file (build-path temp-dir file-name))
  
  ;; Write the code to a temporary file
  (with-output-to-file test-file
    (lambda () (display code))
    #:exists 'replace)
  
  ;; Run Lute on the generated file
  (define-values (lute-output lute-error lute-status)
    (with-handlers ([exn:fail? (lambda (e) 
                                (values "" (exn-message e) 1))])
      (define-values (stdout stderr status)
        (system* "lute" (path->string test-file)))
      (values stdout stderr status)))
  
  ;; Clean up
  (delete-directory/files temp-dir)
  
  ;; Return results
  (values lute-output lute-error lute-status))

;; Test suite for Luau code generation
(define luau-codegen-tests
  (test-suite
   "Luau Code Generation Tests"

   ;; Basic validation tests
   (test-suite
    "Basic Validation Tests"
    
    (test-case "Simple Luau function"
      (define simple-code "
local function add(x: number, y: number): number
    return x + y
end

return add(1, 2)
")
      (define-values (output error status) (test-luau-code simple-code))
      (check-equal? status 0 "Lute should validate the code successfully")
      (check-equal? error "" "There should be no Lute errors"))

    (test-case "Luau type annotations"
      (define type-code "
type Point = {
    x: number,
    y: number
}

local function makePoint(x: number, y: number): Point
    return { x = x, y = y }
end
")
      (define-values (output error status) (test-luau-code type-code))
      (check-equal? status 0 "Lute should validate type definitions")
      (check-equal? error "" "There should be no type errors")))

   ;; Error condition tests
   (test-suite 
    "Error Condition Tests"
    
    (test-case "Invalid Luau syntax"
      (define invalid-code "
local function broken(x
    return x
end")
      (define-values (output error status) (test-luau-code invalid-code))
      (check-equal? status 1 "Lute should fail on invalid syntax")
      (check-not-equal? error "" "There should be Lute errors"))

    (test-case "Type mismatch error"
      (define type-error-code "
local function wrong_types(x: number): string
    return x  -- number instead of string
end")
      (define-values (output error status) (test-luau-code type-error-code))
      (check-equal? status 1 "Lute should catch type mismatch")
      (check-not-equal? error "" "There should be type error messages"))

    (test-case "Undefined variable"
      (define undefined-var-code "
local function uses_undefined(): number
    return undefined_x + 1
end")
      (define-values (output error status) (test-luau-code undefined-var-code))
      (check-equal? status 1 "Lute should catch undefined variable")
      (check-not-equal? error "" "There should be undefined variable error"))

    (test-case "Invalid type declaration"
      (define invalid-type-code "
type Invalid = {
    x: nonexistent_type  -- type doesn't exist
}
")
      (define-values (output error status) (test-luau-code invalid-type-code))
      (check-equal? status 1 "Lute should catch invalid type")
      (check-not-equal? error "" "There should be type definition errors"))

    (test-case "Incorrect function call"
      (define wrong-call-code "
local function f(x: number, y: number): number
    return x + y
end

return f(1)  -- missing argument
")
      (define-values (output error status) (test-luau-code wrong-call-code))
      (check-equal? status 1 "Lute should catch incorrect function calls")
      (check-not-equal? error "" "There should be argument count errors")))

   ;; Complex IR to Luau conversion tests
   (test-suite
    "Complex IR to Luau Tests"
    
    (test-case "Nested quasiquote patterns"
     (let* ([inner-var (ir-pat-var 'x)]
            [inner-pattern (ir-pat-quasiquote
                          (ir-pat-list
                           (list
                            (ir-pat-literal 'inner)
                            (ir-pat-unquote inner-var))
                           #f))]
            [outer-pattern (ir-pat-quasiquote
                          (ir-pat-list
                           (list
                            (ir-pat-literal 'outer)
                            (ir-pat-unquote inner-pattern))
                           #f))]
            [generated-code (ir->luau outer-pattern)])
       
       (define-values (output error status) 
         (test-luau-code generated-code #:file-name "nested_quasiquote.luau"))
       
       (check-equal? status 0 
                    "Generated nested quasiquote code should be valid")
       (check-equal? error "" 
                    "There should be no Lute errors in nested code")))

    (test-case "Pattern with multiple unquotes"
      (let* ([var1 (ir-pat-var 'x)]
             [var2 (ir-pat-var 'y)]
             [var3 (ir-pat-var 'z)]
             [complex-pattern (ir-pat-quasiquote
                             (ir-pat-list
                              (list
                               (ir-pat-literal 'function)
                               (ir-pat-unquote var1)
                               (ir-pat-list 
                                (list
                                 (ir-pat-unquote var2)
                                 (ir-pat-unquote var3))
                                #f))
                              #f))]
             [generated-code (ir->luau complex-pattern)])
        
        (define-values (output error status)
          (test-luau-code generated-code #:file-name "multi_unquote.luau"))
        
        (check-equal? status 0
                     "Generated multi-unquote code should be valid")
        (check-equal? error ""
                     "There should be no Lute errors in multi-unquote code")))

    (test-case "Pattern with conditional expressions"
      (let* ([condition (ir-pat-var 'cond)]
             [then-expr (ir-pat-literal 'then)]
             [else-expr (ir-pat-literal 'else)]
             [if-pattern (ir-pat-quasiquote
                         (ir-pat-list
                          (list
                           (ir-pat-literal 'if)
                           (ir-pat-unquote condition)
                           then-expr
                           else-expr)
                          #f))]
             [generated-code (ir->luau if-pattern)])
        
        (define-values (output error status)
          (test-luau-code generated-code #:file-name "conditional.luau"))
        
        (check-equal? status 0
                     "Generated conditional code should be valid")
        (check-equal? error ""
                     "There should be no Lute errors in conditional code")))

    (test-case "Pattern with table construction"
      (let* ([key1 (ir-pat-literal 'x)]
             [val1 (ir-pat-var 'x_val)]
             [key2 (ir-pat-literal 'y)]
             [val2 (ir-pat-var 'y_val)]
             [table-pattern (ir-pat-quasiquote
                           (ir-pat-list
                            (list
                             (ir-pat-literal 'table)
                             (ir-pat-list
                              (list key1 (ir-pat-unquote val1))
                              #f)
                             (ir-pat-list
                              (list key2 (ir-pat-unquote val2))
                              #f))
                            #f))]
             [generated-code (ir->luau table-pattern)])
        
        (define-values (output error status)
          (test-luau-code generated-code #:file-name "table.luau"))
        
        (check-equal? status 0
                     "Generated table construction code should be valid")
        (check-equal? error ""
                     "There should be no Lute errors in table code"))))

   ;; Luau-specific feature tests
   (test-suite
    "Luau-Specific Features"
    
    (test-case "Type inference"
      (define type-inference-code "
local function inferred(x)  -- type should be inferred
    return x + 1
end

return inferred(5)
")
      (define-values (output error status)
        (test-luau-code type-inference-code #:file-name "type_inference.luau"))
      (check-equal? status 0 "Luau should handle type inference")
      (check-equal? error "" "There should be no type errors"))

    (test-case "Generic functions"
      (define generic-code "
local function id<T>(x: T): T
    return x
end

return id(42)
")
      (define-values (output error status)
        (test-luau-code generic-code #:file-name "generic.luau"))
      (check-equal? status 0 "Luau should handle generic functions")
      (check-equal? error "" "There should be no generic type errors"))

    (test-case "Union types"
      (define union-type-code "
type NumberOrString = number | string

local function process(x: NumberOrString): string
    if type(x) == 'number' then
        return tostring(x)
    else
        return x
    end
end
")
      (define-values (output error status)
        (test-luau-code union-type-code #:file-name "union_types.luau"))
      (check-equal? status 0 "Luau should handle union types")
      (check-equal? error "" "There should be no union type errors")))
   ))

;; Run tests directly when script is executed standalone
(module+ main
  (run-tests luau-codegen-tests)) 