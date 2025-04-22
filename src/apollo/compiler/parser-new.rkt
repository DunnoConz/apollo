#lang racket/base

(require parser-tools/yacc
         (only-in parser-tools/lex token?)
         racket/match
         racket/list
         (only-in "lexer.rkt" tokenize value-tokens syntax-tokens token-value)
         "ast.rkt")

(provide parse-program
         parse-expr
         parse-racket-string)

;; Grammar definition
(define apollo-parser
  (parser
   (start program)
   (end EOF)
   (tokens value-tokens syntax-tokens)
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (error 'apollo-parser
                   "Syntax error at position ~a: ~a"
                   start-pos
                   (if tok-ok? 
                       (format "unexpected token '~a'" tok-value)
                       "invalid token"))))
   (precs
    (nonassoc UNQUOTE UNQUOTE-SPLICING)
    (left QUOTE QUASIQUOTE))
   (grammar
    [program
     [(module) $1]
     [(expr) $1]]
    
    [module
     [(LPAREN module-form RPAREN) $2]]
    
    [module-form
     [(SYMBOL SYMBOL SYMBOL body)
      (let ([mod-sym (token-value $1)]
            [name-sym (token-value $2)]
            [lang-sym (token-value $3)])
        (if (eq? mod-sym 'module)
            (ast-module name-sym lang-sym $4)
            (error 'apollo-parser "Expected 'module' keyword")))]]
    
    [body
     [() '()]
     [(expr body) (cons $1 $2)]]
    
    [expr
     [(LPAREN exprs RPAREN) $2]
     [(LBRACKET exprs RBRACKET) $2]
     [(NUMBER) (ast-literal (token-value $1))]
     [(STRING) (ast-literal (token-value $1))]
     [(BOOLEAN) (ast-literal (token-value $1))]
     [(SYMBOL) (ast-var-ref (token-value $1))]
     [(QUOTE expr) (ast-literal (list 'quote $2))]
     [(QUASIQUOTE expr) (ast-literal (list 'quasiquote $2))]
     [(UNQUOTE expr) (ast-literal (list 'unquote $2))]
     [(UNQUOTE-SPLICING expr) (ast-literal (list 'unquote-splicing $2))]]
    
    [exprs
     [() '()]
     [(expr exprs) (cons $1 $2)]
     [(expr DOT expr) (append $1 $3)]])))

;; Helper function to parse a string into AST
(define (parse-racket-string str)
  (apollo-parser (lambda () (tokenize str))))

;; Parse a program (either string or syntax object)
(define (parse-program input [source-path #f])
  (if (string? input)
      (parse-racket-string input)
      (error "parse-program: expected string input")))

;; Parse a single expression
(define (parse-expr expr)
  (if (string? expr)
      (parse-racket-string expr)
      (error "parse-expr: expected string input"))) 