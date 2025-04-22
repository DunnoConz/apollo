#lang racket/base

(require rackunit
         rackunit/text-ui
         "../../src/apollo/compiler/lexer.rkt")

(provide lexer-tests)

(define (tokens->values tokens)
  (map token-value tokens))

(define (tokens->types tokens)
  (map token-type tokens))

(define lexer-tests
  (test-suite
   "Lexer Tests"
   
   (test-case "Basic token types"
     (let ([tokens (tokenize "(+ 1 2)")])
       (check-equal? (tokens->types tokens)
                    '(LPAREN SYMBOL NUMBER NUMBER RPAREN EOF))))
   
   (test-case "Basic token values"
     (let ([tokens (tokenize "(+ 1 2)")])
       (check-equal? (tokens->values tokens)
                    '(LPAREN + 1 2 RPAREN EOF))))
   
   (test-case "String tokenization"
     (let ([tokens (tokenize "\"hello world\"")])
       (check-equal? (tokens->types tokens)
                    '(STRING EOF))
       (check-equal? (tokens->values tokens)
                    '("hello world" EOF))))
   
   (test-case "Boolean tokenization"
     (let ([tokens (tokenize "#t #f")])
       (check-equal? (tokens->types tokens)
                    '(BOOLEAN BOOLEAN EOF))
       (check-equal? (tokens->values tokens)
                    '(#t #f EOF))))
   
   (test-case "Special syntax tokens"
     (let ([tokens (tokenize "'`,,@")])
       (check-equal? (tokens->types tokens)
                    '(QUOTE QUASIQUOTE UNQUOTE UNQUOTE-SPLICING EOF))))
   
   (test-case "Complex number tokenization"
     (let ([tokens (tokenize "3.14 -2.5 1e10 -1.2e-3")])
       (check-equal? (tokens->types tokens)
                    '(NUMBER NUMBER NUMBER NUMBER EOF))
       (check-equal? (tokens->values tokens)
                    '(3.14 -2.5 1e10 -0.0012 EOF))))
   
   (test-case "Symbol tokenization"
     (let ([tokens (tokenize "define lambda if")])
       (check-equal? (tokens->types tokens)
                    '(SYMBOL SYMBOL SYMBOL EOF))
       (check-equal? (tokens->values tokens)
                    '(define lambda if EOF))))
   
   (test-case "Comment handling"
     (let ([tokens (tokenize "; this is a comment\n(+ 1 2)")])
       (check-equal? (tokens->types tokens)
                    '(LPAREN SYMBOL NUMBER NUMBER RPAREN EOF))))
   
   (test-case "Module syntax"
     (let ([tokens (tokenize "(module test racket/base (+ 1 2))")])
       (check-equal? (tokens->types tokens)
                    '(LPAREN SYMBOL SYMBOL SYMBOL LPAREN SYMBOL NUMBER NUMBER RPAREN RPAREN EOF))
       (check-equal? (tokens->values tokens)
                    '(LPAREN module test racket/base LPAREN + 1 2 RPAREN RPAREN EOF))))
   
   (test-case "Whitespace handling"
     (let ([tokens (tokenize "   (  +   1   2  )   ")])
       (check-equal? (tokens->types tokens)
                    '(LPAREN SYMBOL NUMBER NUMBER RPAREN EOF))))
   
   (test-case "Dotted pair syntax"
     (let ([tokens (tokenize "(1 . 2)")])
       (check-equal? (tokens->types tokens)
                    '(LPAREN NUMBER DOT NUMBER RPAREN EOF))
       (check-equal? (tokens->values tokens)
                    '(LPAREN 1 DOT 2 RPAREN EOF))))))

(module+ test
  (run-tests lexer-tests)) 