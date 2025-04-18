#lang typed/racket

;; Test suite for the GetService module
;; Verifies that all services can be accessed correctly

(require typed/rackunit
         typed/racket/class
         "../getservice.rkt")

(module+ test
  ; Test basic service access
  (test-case "get-service returns a Service instance"
    (check-true (is-a? (get-service 'Workspace) service%))
    (check-equal? (send (get-service 'Workspace) get-service-name) "Workspace"))

  ; Test helper function access
  (test-case "get-workspace returns a Service instance"
    (check-true (is-a? (get-workspace) service%))
    (check-equal? (send (get-workspace) get-service-name) "Workspace"))

  ; Test service inheritance and methods
  (test-case "Service instances have correct inheritance and methods"
    (let ([workspace (get-workspace)])
      (check-true (is-a? workspace service%))
      (check-equal? (send workspace get-name) "Workspace")
      (check-not-false (send workspace get-children))
      (check-not-false (send workspace get-parent))))

  ; Test error handling
  (test-case "get-service handles unknown services correctly"
    (check-exn
     #rx"Unknown service: InvalidService"
     (λ () (get-service 'InvalidService)))))