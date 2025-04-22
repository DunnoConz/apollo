#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         racket/string)

(provide tokenize
         token-value
         token-type
         token?
         value-tokens
         syntax-tokens)

;; Token structure
(define-tokens value-tokens (NUMBER STRING SYMBOL BOOLEAN))
(define-empty-tokens syntax-tokens
  (LPAREN RPAREN
   LBRACKET RBRACKET
   QUOTE QUASIQUOTE UNQUOTE UNQUOTE-SPLICING
   DOT
   EOF))

;; Token predicates and accessors
(define (token? v)
  (or (position-token? v)
      (memq v (list 'LPAREN 'RPAREN 'LBRACKET 'RBRACKET 
                    'QUOTE 'QUASIQUOTE 'UNQUOTE 'UNQUOTE-SPLICING
                    'DOT 'EOF))))

(define (token-value tok)
  (if (position-token? tok)
      (position-token-token tok)
      tok))

(define (token-type tok)
  (if (position-token? tok)
      (position-token-token tok)
      tok))

;; Main lexer definition
(define apollo-lexer
  (lexer
   ;; Whitespace
   [(:+ whitespace) (apollo-lexer input-port)]
   
   ;; Comments
   [(:: ";" (:* (:~ #\newline)) #\newline) (apollo-lexer input-port)]
   [(:: "#|" (complement (:: any-string "|#" any-string)) "|#") (apollo-lexer input-port)]
   
   ;; Numbers
   [(:+ numeric) (token-NUMBER (string->number lexeme))]
   [(:: (:? (:or "+" "-"))
        (:+ numeric)
        (:? (:: "." (:+ numeric)))
        (:? (:: (:or "e" "E")
                (:? (:or "+" "-"))
                (:+ numeric))))
    (token-NUMBER (string->number lexeme))]
   
   ;; Strings
   [(:: "\"" (:* (:or (:~ "\"" "\\")
                      (:: "\\" any-char)))
        "\"")
    (token-STRING (substring lexeme 1 (sub1 (string-length lexeme))))]
   
   ;; Booleans
   ["#t" (token-BOOLEAN #t)]
   ["#f" (token-BOOLEAN #f)]
   
   ;; Symbols and keywords
   [(:: (:or alphabetic "#" "+" "-" "." "*" "/" "<" ">" "=" "!")
        (:* (:or alphabetic numeric "#" "+" "-" "_" "." "*" "/" "<" ">" "=" "!")))
    (token-SYMBOL (string->symbol lexeme))]
   
   ;; Special syntax
   ["(" 'LPAREN]
   [")" 'RPAREN]
   ["[" 'LBRACKET]
   ["]" 'RBRACKET]
   ["'" 'QUOTE]
   ["`" 'QUASIQUOTE]
   ["," 'UNQUOTE]
   [",@" 'UNQUOTE-SPLICING]
   ["." 'DOT]
   
   ;; End of file
   [(eof) 'EOF]
   
   ;; Error case
   [(special)
    (error 'apollo-lexer "Illegal character in input: ~a" lexeme)]))

;; Function to tokenize a string or port
(define (tokenize input)
  (let ([port (if (string? input)
                  (open-input-string input)
                  input)])
    (let loop ([tokens '()])
      (let ([token (apollo-lexer port)])
        (if (eq? token 'EOF)
            (reverse (cons token tokens))
            (loop (cons token tokens))))))) 