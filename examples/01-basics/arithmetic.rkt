#lang racket

;; Arithmetic operations example
;; Demonstrates basic math functions

;; Basic arithmetic functions
(define (add a b) (+ a b))
(define (subtract a b) (- a b))
(define (multiply a b) (* a b))
(define (divide a b) (/ a b))

;; A more complex calculation
(define (calculate-rectangle-properties width height)
  (let* ([area (* width height)]
         [perimeter (* 2 (+ width height))]
         [diagonal (sqrt (+ (* width width) (* height height)))])
    (list area perimeter diagonal)))

;; Test the functions
(display "Addition: ")
(display (add 5 3))
(newline)

(display "Subtraction: ")
(display (subtract 10 4))
(newline)

(display "Multiplication: ")
(display (multiply 6 7))
(newline)

(display "Division: ")
(display (divide 20 5))
(newline)

(display "Rectangle properties (width=5, height=3): ")
(display (calculate-rectangle-properties 5 3))
(newline)

;; Expected Luau output:
#|
local function add(a, b)
  return a + b
end

local function subtract(a, b)
  return a - b
end

local function multiply(a, b)
  return a * b
end

local function divide(a, b)
  return a / b
end

local function calculate_rectangle_properties(width, height)
  local area = width * height
  local perimeter = 2 * (width + height)
  local diagonal = math.sqrt(width * width + height * height)
  return {area, perimeter, diagonal}
end

print("Addition: " .. tostring(add(5, 3)))
print("Subtraction: " .. tostring(subtract(10, 4)))
print("Multiplication: " .. tostring(multiply(6, 7)))
print("Division: " .. tostring(divide(20, 5)))
print("Rectangle properties (width=5, height=3): " .. tostring(calculate_rectangle_properties(5, 3)))
|# 