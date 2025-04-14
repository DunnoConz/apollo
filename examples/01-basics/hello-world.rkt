#lang racket

;; Simple Hello World example
;; Demonstrates basic function definition and string operations

;; Define a function that creates a greeting
(define (greet name)
  (string-append "Hello, " name "!"))

;; Call the function with different arguments
(display (greet "World"))
(newline)
(display (greet "Roblox"))
(newline)

;; Expected Luau output:
#|
local function greet(name)
  return "Hello, " .. name .. "!"
end

print(greet("World"))
print(greet("Roblox"))
|# 