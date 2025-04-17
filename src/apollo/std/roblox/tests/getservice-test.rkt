#lang typed/racket

;; Test suite for the GetService module
;; Verifies that all services can be accessed correctly

(require typed/rackunit
         typed/racket/class
         (only-in "../getservice.rkt" get-service get-workspace service%))

(require/typed typed/racket/class
  [#:type Instance (All (A) A)])

(require/typed "../getservice.rkt"
  [#:type Service<%> Any])

(module+ test
  ; Test basic service access
  (check-not-exn
   (λ () (get-service 'Workspace)))
  
  (check-equal? 
   (send (get-service 'Workspace) get-service-name)
   "Workspace")

  ; Test helper function access
  (check-not-exn
   (λ () (get-workspace)))
  
  (check-equal?
   (send (get-workspace) get-service-name)
   "Workspace")

  ; Test service inheritance
  (let ([workspace (get-workspace)])
    (check-true (is-a? workspace Service<%>))
    (check-true (is-a? workspace Instance%)))

  ; Test unknown service error
  (check-exn
   #rx"Unknown service: InvalidService"
   (λ () (get-service 'InvalidService)))

  ; Test service method access
  (let ([workspace (get-workspace)])
    (check-equal? (send workspace get-name) "Workspace")
    (check-not-false (send workspace get-children))
    (check-not-false (send workspace get-parent))))

(test-case "get-service returns a Service instance"
  (check-true (is-a? (get-service 'Workspace) service%)))

(test-case "get-workspace returns a Service instance"
  (check-true (is-a? (get-workspace) service%)))

(test-case "Service instances have correct names"
  (check-equal? (send (get-workspace) get-service-name) "Workspace"))

(module+ test
  (test-case "get-service returns a Service instance"
    (check-true (is-a? (get-service 'Workspace) service%)))

  (test-case "get-workspace returns a Service instance"
    (check-true (is-a? (get-workspace) service%)))

  (test-case "Service instances have correct names"
    (check-equal? (send (get-workspace) get-service-name) "Workspace")))