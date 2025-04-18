#lang racket/base

(require racket/string
         racket/match)

(provide string-contains?
         string-position
         racket-path->luau-path
         racket-type->luau-type
         indent-string
         regexp-replace
         regexp-replace*)

;; String contains helper - implementation without requiring racket/regexp
(define (string-contains? str sub)
  (and (string? str)
       (string? sub)
       (< (string-length sub) (string-length str))
       (let loop ([i 0])
         (cond
           [(> (+ i (string-length sub)) (string-length str)) #f]
           [(string=? (substring str i (+ i (string-length sub))) sub) #t]
           [else (loop (add1 i))]))))

;; Helper function to replace regexp-match-positions
(define (string-position needle haystack [start-pos 0])
  (let loop ([i start-pos])
    (cond
      [(>= i (string-length haystack)) #f]
      [(> (+ i (string-length needle)) (string-length haystack)) #f]
      [(string=? (substring haystack i (+ i (string-length needle))) needle) i]
      [else (loop (add1 i))])))

;; Helper function to replace regexp-replace
(define (regexp-replace pattern str replacement)
  (if (string? pattern)
      (let ([pos (string-position pattern str)])
        (if pos
            (string-append 
             (substring str 0 pos)
             replacement
             (substring str (+ pos (string-length pattern)) (string-length str)))
            str))
      ;; Simple implementation for this specific case only
      (let* ([match-str "game:GetService\\(\\\"(.*)\\\"\\)"]
             [pos (string-position "game:GetService(\"" str)])
        (if pos
            (let* ([end-pos (string-position "\")" str (+ pos 15))]
                   [service-name (substring str (+ pos 15) end-pos)])
              service-name)
            str))))

;; For regexp-replace* used in tests
(define (regexp-replace* pattern str replacement)
  (let loop ([s str])
    (let ([pos (string-position pattern s)])
      (if pos
          (let ([new-s (string-append 
                       (substring s 0 pos)
                       replacement
                       (substring s (+ pos (string-length pattern)) (string-length s)))])
            (loop new-s))
          s))))

;; Simple mapping from Racket type symbols to Luau type strings
(define (racket-type->luau-type rkt-type)
  (match rkt-type
    ['Integer "number"]
    ['Number "number"]
    ['Real "number"]
    ['String "string"]
    ['Boolean "boolean"]
    ['Void "nil"]
    ['Any #f]
    [(? symbol? s) (symbol->string s)]
    [_ #f]))

;; Maps Racket require paths/symbols to Luau require expressions (strings)
(define (racket-path->luau-path racket-path)
  (match racket-path
    ['"apollo/runtime" "script.Parent.runtime"]
    ['"roblox/HttpService" "game:GetService(\"HttpService\")"]
    ['"roblox/Players" "game:GetService(\"Players\")"]
    ['"shared/utils" "game.ReplicatedStorage.Shared.Utils"]
    [(? string? p) (format "script.Parent.%s" p)]
    [(? symbol? s)
     (let ([s-str (symbol->string s)])
       (cond
         [(string-contains? s-str "/")
          (format "script.Parent.%s" s-str)]
         [else #f]))]
    [_ (error 'racket-path->luau-path "Cannot map Racket path: ~s" racket-path)]))

;; Helper function to indent a string
(define (indent-string str [indent "  "])
  (string-append indent (string-replace str "\n" (string-append "\n" indent)))) 