#lang racket

(provide (struct-out LuauNode)
         (struct-out LuauFunctionCall)
         (struct-out LuauAssignment)
         (struct-out LuauIfStatement)
         (struct-out LuauIdentifier)
         (struct-out LuauStringLiteral)
         (struct-out LuauNumberLiteral)
         (struct-out LuauBooleanLiteral)
         (struct-out LuauBinaryOp)
         (struct-out LuauReturn))

; Base node type for all Luau AST nodes
(struct LuauNode () #:transparent)

; Function call node
(struct LuauFunctionCall LuauNode (callee args) #:transparent)

; Assignment node
(struct LuauAssignment LuauNode (target value) #:transparent)

; If statement node
(struct LuauIfStatement LuauNode (condition then-branch else-branch) #:transparent)

; Identifier node
(struct LuauIdentifier LuauNode (name) #:transparent)

; String literal node
(struct LuauStringLiteral LuauNode (value) #:transparent)

; Number literal node
(struct LuauNumberLiteral LuauNode (value) #:transparent)

; Boolean literal node
(struct LuauBooleanLiteral LuauNode (value) #:transparent)

; Binary operation node
(struct LuauBinaryOp LuauNode (op left right) #:transparent)

; Return statement node
(struct LuauReturn LuauNode (value) #:transparent) 