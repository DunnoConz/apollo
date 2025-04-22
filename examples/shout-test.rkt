#lang racket/base

(require apollo/dsls/shout-dsl)

;; Test the shout DSL functionality
(shout "hello" "world")
(shout "testing" "multiple" "shouts")