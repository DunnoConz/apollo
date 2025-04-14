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
    ['Real "number"] ; Map Real to number as well
    ['String "string"]
    ['Boolean "boolean"]
    ['Void "nil"] ; Map Void to nil type or just omit? Let's map to nil for now.
    ['Any #f] ; Map Any to no type annotation
    ;; TODO: Add mappings for list, vector, specific struct types (e.g., (P 'pos))
    [(? symbol? s) (symbol->string s)] ; Assume custom types match by name
    [_ #f])) ; Default to no type annotation if unknown

;; Maps Racket require paths/symbols to Luau require expressions (strings)
(define (racket-path->luau-path racket-path)
  (match racket-path
    ;; TODO: Implement more robust mapping (config file? heuristics?)
    ;; TODO: Handle syntax objects, not just symbols/strings
    ['"apollo/runtime" "script.Parent.runtime"] ; Example: Relative path for runtime
    ['"roblox/HttpService" "game:GetService(\"HttpService\")"] ; Example: Service
    ['"roblox/Players" "game:GetService(\"Players\")"] ; Example: Service
    ['"shared/utils" "game.ReplicatedStorage.Shared.Utils"] ; Example: ModuleScript
    [(? string? p) (format "script.Parent.%s" p)] ; Reverted to plain format
    [(? symbol? s)
     (let ([s-str (symbol->string s)])
       (cond
         [(string-contains? s-str "/") ; Likely a path string encoded as symbol
          (format "script.Parent.%s" s-str)] ; Reverted to plain format
         [else ; Assume it's a local variable/binding, not a path require
          #f]))] ; Return #f to indicate not a require path
    [_ (error 'racket-path->luau-path "Cannot map Racket path: ~s" racket-path)]))

;; Helper function to indent a string
(define (indent-string str [indent "  "])
  (string-append indent (string-replace str "\n" (string-append "\n" indent)))) 