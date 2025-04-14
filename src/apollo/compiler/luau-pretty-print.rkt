#lang racket

(require "luau-ast.rkt")

(provide pretty-print-luau)

; Main entry point for pretty printing
(define (pretty-print-luau node)
  (match node
    [(LuauFunctionCall callee args)
     (string-append (pretty-print-luau callee)
                   "("
                   (string-join (map pretty-print-luau args) ", ")
                   ")")]
    [(LuauAssignment targets values)
     (string-append (string-join (map pretty-print-luau targets) ", ")
                   " = "
                   (string-join (map pretty-print-luau values) ", "))]
    [(LuauIfStatement condition then-stmts else-stmts)
     (string-append "if "
                   (pretty-print-luau condition)
                   " then\n  "
                   (string-join (map pretty-print-luau then-stmts) "\n  ")
                   "\nelse\n  "
                   (string-join (map pretty-print-luau else-stmts) "\n  ")
                   "\nend")]
    [(LuauIdentifier name) name]
    [(LuauStringLiteral value) (format "\"~a\"" value)]
    [(LuauNumberLiteral value) (number->string value)]
    [(LuauBooleanLiteral value) (if value "true" "false")]
    [(LuauBinaryOp op left right)
     (string-append (pretty-print-luau left)
                   " "
                   (symbol->string op)
                   " "
                   (pretty-print-luau right))]
    [(LuauReturn value)
     (string-append "return " (pretty-print-luau value))])) 