#lang racket/base

(require rackunit
         racket/match)

;; Assume Point struct is defined elsewhere or implicitly available
;; We'll define it here for testing purposes, the compiler should handle
;; the definition from the input file (if it exists there) or rely on a
;; pre-existing Luau equivalent.
(struct Point (x y) #:transparent)

(define (test-match value)
  (match value
    ['() "empty list"] ; Added case for empty list
    [#true "boolean true"]
    [#false "boolean false"]
    [42 "the number forty-two"]
    ["hello" "the string hello"]
    [(list 1 2 3) "list 1 2 3"]
    [(list 1 x 3) (string-append "list with middle " (number->string x))]
    [(list* 1 2 r) (string-append "list* 1 2 with rest " (if (list? r) "list" "other"))] ; Simple check
    [(list a b) (string-append "two elements: " (number->string a) ", " (number->string b))]
    [(Point x y) (string-append "Point(" (number->string x) ", " (number->string y) ")")]
    [_ "wildcard match"]))

;; Add more tests here
(check-equal? (test-match '()) "empty list")
(check-equal? (test-match 42) "the number forty-two")
(check-equal? (test-match "hello") "the string hello")
(check-equal? (test-match (list 1 2 3)) "list 1 2 3")
(check-equal? (test-match (list 1 99 3)) "list with middle 99")
(check-equal? (test-match (list* 1 2 (list 3 4))) "list* 1 2 with rest list")
(check-equal? (test-match (list 5 6)) "two elements: 5, 6")
(check-equal? (test-match (Point 10 20)) "Point(10, 20)")
(check-equal? (test-match 'something-else) "wildcard match")

;; Example using match result
(define (describe-list lst)
  (match lst
    ['() "empty list"]
    [(list a) (string-append "single element list: " (number->string a))]
    [(list* a b rest) (string-append "list starting with " (number->string a) ", " (number->string b))]
    [_ "other list form"]))

(check-equal? (describe-list '()) "empty list")
(check-equal? (describe-list (list 10)) "single element list: 10")
(check-equal? (describe-list (list 10 20 30)) "list starting with 10, 20")


(module+ test
  (printf "Match tests loaded.\n")) 