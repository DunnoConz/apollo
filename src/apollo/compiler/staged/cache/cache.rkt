#lang racket/base

(require racket/contract
         racket/path
         racket/file
         racket/serialize
         "../../ir-types.rkt"
         "../core/ir-to-luau.rkt")

(provide cache-ir->luau
         clear-cache
         get-cache-stats
         cache-stats
         cache-stats-hits
         cache-stats-misses
         cache-stats-total-size)

;; Cache directory setup
(define cache-dir (make-parameter
                  (build-path (find-system-path 'temp-dir)
                            "apollo-compiler-cache")))

;; Cache statistics
(struct cache-stats (hits misses total-size) #:mutable #:transparent)
(define stats (cache-stats 0 0 0))

;; Ensure cache directory exists
(define (ensure-cache-dir)
  (define dir (cache-dir))
  (unless (directory-exists? dir)
    (make-directory* dir)))

;; Generate cache key from IR
(define (make-cache-key ir)
  (define-values (hash _) (equal-hash-code ir))
  (string->path (format "~a.cache" (number->string hash 16))))

;; Cache the IR->Luau conversion
(define (cache-ir->luau ir)
  (ensure-cache-dir)
  (let* ([cache-path (build-path (cache-dir) (make-cache-key ir))]
         [result (if (file-exists? cache-path)
                    ;; Cache hit - load from cache
                    (begin
                      (set-cache-stats-hits! stats (add1 (cache-stats-hits stats)))
                      (with-input-from-file cache-path deserialize))
                    ;; Cache miss - compute and store
                    (begin
                      (set-cache-stats-misses! stats (add1 (cache-stats-misses stats)))
                      (let ([computed (ir->staged-luau ir)])
                        (with-output-to-file cache-path
                          (λ () (serialize computed))
                          #:exists 'replace)
                        computed)))])
    result))

;; Clear the cache
(define (clear-cache)
  (when (directory-exists? (cache-dir))
    (delete-directory/files (cache-dir)))
  (set-cache-stats-hits! stats 0)
  (set-cache-stats-misses! stats 0)
  (set-cache-stats-total-size! stats 0))

;; Get cache statistics
(define (get-cache-stats)
  (define dir-size 
    (if (directory-exists? (cache-dir))
        (for/sum ([f (in-directory (cache-dir))])
          (file-size f))
        0))
  (set-cache-stats-total-size! stats dir-size)
  stats) 