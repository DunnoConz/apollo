# Examples

This tutorial provides practical examples of using Apollo to compile Racket code to Luau.

## Basic Examples

### 1. Hello World

```racket
#lang racket

(define (greet name)
  (printf "Hello, ~a!\n" name))

(greet "World")
```

Compiles to:
```lua
local function greet(name: string)
    print(string.format("Hello, %s!\n", name))
end

greet("World")
```

### 2. Simple Calculator

```racket
#lang racket

(define (calculate op a b)
  (match op
    ['+ (+ a b)]
    ['- (- a b)]
    ['* (* a b)]
    ['/ (/ a b)]
    [_ (error "Unknown operator")]))

(calculate '+ 5 3)  ; => 8
```

Compiles to:
```lua
local function calculate(op: string, a: number, b: number): number
    if op == "+" then
        return a + b
    elseif op == "-" then
        return a - b
    elseif op == "*" then
        return a * b
    elseif op == "/" then
        return a / b
    else
        error("Unknown operator")
    end
end

print(calculate("+", 5, 3))  -- => 8
```

## Intermediate Examples

### 3. Data Structures

```racket
#lang racket

(struct Point ([x : Real] [y : Real]))

(define (distance p1 p2)
  (sqrt (+ (sqr (- (Point-x p1) (Point-x p2)))
           (sqr (- (Point-y p1) (Point-y p2))))))

(define p1 (Point 0 0))
(define p2 (Point 3 4))
(distance p1 p2)  ; => 5
```

Compiles to:
```lua
local Point = {}
Point.__index = Point

function Point.new(x: number, y: number)
    local self = setmetatable({}, Point)
    self.x = x
    self.y = y
    return self
end

local function distance(p1: Point, p2: Point): number
    return math.sqrt((p1.x - p2.x)^2 + (p1.y - p2.y)^2)
end

local p1 = Point.new(0, 0)
local p2 = Point.new(3, 4)
print(distance(p1, p2))  -- => 5
```

### 4. Higher-Order Functions

```racket
#lang racket

(define (map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (map f (cdr lst)))))

(map (lambda (x) (* x x)) '(1 2 3 4))  ; => '(1 4 9 16)
```

Compiles to:
```lua
local function map(f: (any) -> any, lst: {any}): {any}
    if #lst == 0 then
        return {}
    end
    local result = {f(lst[1])}
    for i = 2, #lst do
        table.insert(result, f(lst[i]))
    end
    return result
end

local squared = map(function(x) return x * x end, {1, 2, 3, 4})
-- => {1, 4, 9, 16}
```

## Advanced Examples

### 5. Macros

```racket
#lang racket

(define-syntax-rule (repeat n body ...)
  (for ([i n])
    body ...))

(repeat 3
  (printf "Hello\n"))
```

Compiles to:
```lua
for i = 1, 3 do
    print("Hello")
end
```

### 6. Modules

```racket
#lang racket

(module math racket
  (provide square cube)
  
  (define (square x) (* x x))
  (define (cube x) (* x x x)))

(require 'math)
(square 5)  ; => 25
(cube 3)    ; => 27
```

Compiles to:
```lua
local math = {}

function math.square(x: number): number
    return x * x
end

function math.cube(x: number): number
    return x * x * x
end

print(math.square(5))  -- => 25
print(math.cube(3))    -- => 27
```

## What's Next?

*   [Advanced Features](../tutorials/advanced.md)
*   [Type System](../explanation/type-system.md)
*   [Standard Library](../reference/stdlib.md) 