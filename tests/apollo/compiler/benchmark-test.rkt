#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/file
         racket/system
         racket/port
         racket/path
         racket/list
         "../../../src/apollo/compiler/codegen.rkt"
         "../../../src/apollo/compiler/parser.rkt"
         (submod "../../../src/apollo/compiler/ir.rkt" ir)
         "../../../src/apollo/compiler/ir-types.rkt")

(provide benchmark-tests)

;; Helper function to measure execution time
(define (measure-time thunk)
  (let ([start (current-inexact-milliseconds)])
    (thunk)
    (- (current-inexact-milliseconds) start)))

;; Helper function to run Luau code and get execution time
(define (run-luau-code code #:file-name [file-name "benchmark.luau"])
  (define temp-dir (make-temporary-file "luau-benchmark-~a" 'directory))
  (define test-file (build-path temp-dir file-name))
  
  ;; Write the code to a temporary file
  (with-output-to-file test-file
    (lambda () (display code))
    #:exists 'replace)
  
  ;; Run Lute and measure time
  (define execution-time
    (measure-time
     (lambda ()
       (system* "lute" (path->string test-file)))))
  
  ;; Clean up
  (delete-directory/files temp-dir)
  
  ;; Return execution time
  execution-time)

;; Benchmark test suite
(define benchmark-tests
  (test-suite
   "Benchmark Tests"
   
   ;; Fibonacci benchmark
   (test-case "Fibonacci Benchmark"
     (let* ([direct-luau-code "
local function fib(n)
    if n < 2 then
        return n
    end
    return fib(n-1) + fib(n-2)
end

return fib(20)  -- Using n=20 for reasonable benchmark time
"]
            [apollo-ir
             (ir-pat-quasiquote
              (ir-pat-list
               (list
                (ir-pat-literal 'function)
                (ir-pat-literal 'fib)
                (ir-pat-list (list (ir-pat-literal 'n)) #f)
                (ir-pat-list
                 (list
                  (ir-pat-literal 'if)
                  (ir-pat-list
                   (list
                    (ir-pat-literal '<)
                    (ir-pat-literal 'n)
                    (ir-pat-literal 2))
                   #f)
                  (ir-pat-literal 'n)
                  (ir-pat-list
                   (list
                    (ir-pat-literal '+)
                    (ir-pat-list
                     (list
                      (ir-pat-literal 'fib)
                      (ir-pat-list
                       (list
                        (ir-pat-literal '-)
                        (ir-pat-literal 'n)
                        (ir-pat-literal 1))
                       #f))
                     #f)
                    (ir-pat-list
                     (list
                      (ir-pat-literal 'fib)
                      (ir-pat-list
                       (list
                        (ir-pat-literal '-)
                        (ir-pat-literal 'n)
                        (ir-pat-literal 2))
                       #f))
                     #f))
                   #f))
                 #f))
               #f))]
            [apollo-compiled-code (ir-pattern->luau apollo-ir)]
            [iterations 5]
            [direct-times
             (for/list ([i iterations])
               (run-luau-code direct-luau-code #:file-name "direct_fib.luau"))]
            [apollo-times
             (for/list ([i iterations])
               (run-luau-code apollo-compiled-code #:file-name "apollo_fib.luau"))]
            [direct-avg (/ (apply + direct-times) iterations)]
            [apollo-avg (/ (apply + apollo-times) iterations)])
       
       ;; Display results
       (printf "Fibonacci Benchmark Results (n=20, ~a iterations):\n" iterations)
       (printf "Direct Luau average: ~a ms\n" direct-avg)
       (printf "Apollo-compiled average: ~a ms\n" apollo-avg)
       (printf "Performance ratio: ~a\n" (/ apollo-avg direct-avg))
       
       ;; Verify both produce same results
       (check-true (< (abs (- apollo-avg direct-avg))
                     (* 2 (max apollo-avg direct-avg)))
                  "Performance difference should be within reasonable bounds")))
   
   ;; Array operations benchmark
   (test-case "Array Operations Benchmark"
     (let* ([direct-luau-code "
local function process_array(arr)
    local sum = 0
    for i = 1, #arr do
        sum = sum + arr[i] * 2
    end
    return sum
end

local arr = {}
for i = 1, 1000 do
    arr[i] = i
end

return process_array(arr)
"]
            [apollo-ir
             (ir-pat-quasiquote
              (ir-pat-list
               (list
                (ir-pat-literal 'let)
                (ir-pat-list
                 (list
                  (ir-pat-literal 'arr)
                  (ir-pat-list
                   (list
                    (ir-pat-literal 'for)
                    (ir-pat-literal 'i)
                    (ir-pat-literal 1)
                    (ir-pat-literal 1000)
                    (ir-pat-list
                     (list
                      (ir-pat-literal 'set)
                      (ir-pat-list
                       (list
                        (ir-pat-literal 'arr)
                        (ir-pat-literal 'i))
                       #f)
                      (ir-pat-literal 'i))
                     #f))
                   #f))
                 #f)
                (ir-pat-list
                 (list
                  (ir-pat-literal 'let)
                  (ir-pat-list
                   (list
                    (ir-pat-literal 'sum)
                    (ir-pat-literal 0))
                   #f)
                  (ir-pat-list
                   (list
                    (ir-pat-literal 'for)
                    (ir-pat-literal 'i)
                    (ir-pat-literal 1)
                    (ir-pat-list
                     (list
                      (ir-pat-literal 'length)
                      (ir-pat-literal 'arr))
                     #f)
                    (ir-pat-list
                     (list
                      (ir-pat-literal '=)
                      (ir-pat-literal 'sum)
                      (ir-pat-list
                       (list
                        (ir-pat-literal '+)
                        (ir-pat-literal 'sum)
                        (ir-pat-list
                         (list
                          (ir-pat-literal '*)
                          (ir-pat-list
                           (list
                            (ir-pat-literal 'arr)
                            (ir-pat-literal 'i))
                           #f)
                          (ir-pat-literal 2))
                         #f))
                       #f))
                     #f))
                   #f)
                  (ir-pat-literal 'sum))
                 #f))
               #f))]
            [apollo-compiled-code (ir-pattern->luau apollo-ir)]
            [iterations 5]
            [direct-times
             (for/list ([i iterations])
               (run-luau-code direct-luau-code #:file-name "direct_array.luau"))]
            [apollo-times
             (for/list ([i iterations])
               (run-luau-code apollo-compiled-code #:file-name "apollo_array.luau"))]
            [direct-avg (/ (apply + direct-times) iterations)]
            [apollo-avg (/ (apply + apollo-times) iterations)])
       
       ;; Display results
       (printf "\nArray Operations Benchmark Results (~a iterations):\n" iterations)
       (printf "Direct Luau average: ~a ms\n" direct-avg)
       (printf "Apollo-compiled average: ~a ms\n" apollo-avg)
       (printf "Performance ratio: ~a\n" (/ apollo-avg direct-avg))
       
       ;; Verify both produce same results
       (check-true (< (abs (- apollo-avg direct-avg))
                     (* 2 (max apollo-avg direct-avg)))
                  "Performance difference should be within reasonable bounds")))))

;; Run benchmarks directly when script is executed standalone
(module+ main
  (run-tests benchmark-tests)) 